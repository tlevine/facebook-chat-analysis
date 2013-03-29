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

getUsers :: Connection -> IO [[String]]
getUsers conn = do 
  query <- quickQuery conn "SELECT uid, nick FROM log_status GROUP BY uid;" []
  return $ unSql query
  where
    unSql :: [[SqlValue]] -> [[String]]
    unSql = map $ map (\sql -> fromSql sql :: String)

usernames :: [[String]] -> M.Map String String
usernames query = M.fromList $ map (\l -> (head l, last l)) query

main = do
  a <- getArgs
  conn <- connectSqlite3 $ head a
  users <- getUsers conn
  putStrLn $ show $ usernames users
  disconnect conn
