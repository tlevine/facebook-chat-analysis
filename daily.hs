import System.Environment     (getArgs)
import Database.HDBC
import Database.HDBC.Sqlite3

main = do
  dbFile <- getArgs
  putStrLn $ head dbFile
  -- conn <- connectSqlite3
