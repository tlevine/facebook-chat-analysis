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


getStatuses :: Connection -> String -> IO [(Integer, Status)]
getStatuses conn uid = do
  query <- quickQuery conn sql [(toSql uid)]
  return $ map unSql query
  where
    sql = "SELECT ts,status FROM log_status WHERE uid = ?"
    unSql row = ((fromSql $ head row) :: Integer, (status $ fromSql $ last row) :: Status)
    status s = case s of
      "avail"    -> LogIn
      "notavail" -> LogOut

getStatusesByUser :: Connection -> IO [(String, [(Integer, Status)])]
getStatusesByUser conn = do
  users    <- getUsers conn
  let uids = map fst users

  statuses <- mapM (getStatuses conn) uids
  let statusesByUser = zip uids statuses

  return statusesByUser

---------------------------------------
-- User status features
---------------------------------------

-- Total time online today
totalTime :: [(Integer, Status)] -> Integer
totalTime statuses = snd $ M.foldrWithKey folder (0, 0) s
  where
    increment thisTime prevTime LogIn  = thisTime - prevTime
    increment thisTime prevTime LogOut = 0
    folder prevTime status (thisTime, soFar) = (prevTime, soFar + (increment thisTime prevTime status))
    s = M.fromList statuses

---------------------------------------
main = do
---------------------------------------

  -- Connect
  args <- getArgs
  conn <- connectSqlite3 $ head args

  -- Query
  users    <- getUsers conn
  statuses <- getStatusesByUser conn


  -- Finish
--putStrLn $ show $ users
--putStrLn $ show $ statuses

  putStrLn $ show $ s
--putStrLn $ show $ M.map totalTime $ head statuses
  disconnect conn
