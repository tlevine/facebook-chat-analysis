import           System.Environment      (getArgs)
import           Database.HDBC           
import           Database.HDBC.Sqlite3   
import qualified Data.Map                as M

data Status = LogIn | LogOut deriving (Enum, Show)

---------------------------------------
-- Query
---------------------------------------

getUsers :: Connection -> IO [(String, String)]
getUsers conn = do
  query <- quickQuery conn sql []
  return $ unSql query
  where
    sql = "SELECT uid, nick FROM log_status GROUP BY uid;"
    unSql results = map (\l -> (head l, last l)) $ map (map (\sql -> fromSql sql :: String)) results


getUserStatuses :: Connection -> String -> IO [(Integer, Status)]
getUserStatuses conn uid = do
  query <- quickQuery conn sql [(toSql uid)]
  return $ map unSql query
  where
    sql = "SELECT ts,status FROM log_status WHERE uid = ?"
    unSql row = ((fromSql $ head row) :: Integer, (status $ fromSql $ last row) :: Status)
    status s = case s of
      "avail"    -> LogIn
      "notavail" -> LogOut

---------------------------------------
-- User status features
---------------------------------------

-- Total time online today
totalTime :: [(Integer, Status)] -> Integer
totalTime = snd $ foldrWithKey folder (0, 0)
  where
    increment prevTime thisTime LogIn  = thisTime - prevTime
    increment prevTime thisTime LogOut = 0
    folder thisTime status (prevTime, soFar) = soFar + (increment prevTime thisTime status)
    s = M.fromList statuses

---------------------------------------
main = do
---------------------------------------

  -- Connect
  args <- getArgs
  conn <- connectSqlite3 $ head args

  -- Query
  users    <- getUsers conn
  statuses <- getUserStatuses conn "xmpp:-613350425@chat.facebook.com"

  -- Finish
--putStrLn $ show $ users
  putStrLn $ show $ statuses
  disconnect conn
