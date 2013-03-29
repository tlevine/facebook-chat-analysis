import           System.Environment      (getArgs)
import           Database.HDBC           
import           Database.HDBC.Sqlite3   
import qualified Data.Map                as M

{-
durations :: Connection
durations conn = 
  where

users :: Connection -> M.Map String String
users conn = M.fromList $ 
  where
    query = quickQuery conn "SELECT uid, nick FROM log_status GROUP BY uid;" []
-}

getUsers :: Connection -> IO [(String, String)]
getUsers conn = do
  query <- quickQuery conn sql []
  return $ unSql query
  where
    sql = "SELECT uid, nick FROM log_status GROUP BY uid;"
    unSql results = map (\l -> (head l, last l)) $ map (map (\sql -> fromSql sql :: String)) results

data Status = LogIn | LogOut | Other deriving (Enum, Show)

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

main = do
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
