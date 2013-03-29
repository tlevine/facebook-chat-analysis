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

getQuery :: String -> String -> Connection -> IO [[String]]
getQuery sql values conn = do
  query <- quickQuery conn sql $ map toSql values
  return $ unSql query
  where
    unSql :: [[SqlValue]] -> [[String]]
    unSql = map $ map (\sql -> fromSql sql :: String)

getUsers :: Connection -> IO [[String]]
getUsers = getQuery "SELECT uid, nick FROM log_status GROUP BY uid;" []

{-
getUserStatus :: Connection -> IO [[String]]
getUsers conn uid = do 
  query <- quickQuery conn "SELECT * FROM log_status WHERE uid = ?" [uid]
  return $ unSql query
-}

-- All features are maps from usernames to feature values.
usernames :: [[String]] -> M.Map String String
usernames query = M.fromList $ map (\l -> (head l, last l)) query

main = do
  a <- getArgs
  conn <- connectSqlite3 $ head a
  users <- getUsers conn
  putStrLn $ show $ usernames users
  disconnect conn
