{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
import           System.Environment      (getArgs)
import           Database.HDBC           
import           Database.HDBC.Sqlite3   
import qualified Data.Map                as M

-- Import types
data Status = LogIn | LogOut deriving (Enum, Show, Eq)
type DateTime = Integer

-- Clean types
type Uid  = String
type Nick = String
type Session = (DateTime, DateTime)
data User = User { uid      :: Uid
                 , nick     :: Nick
                 , sessions :: [Session]
} deriving (Show, Eq)

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
-- Convert to the Session type
---------------------------------------
toSessions :: [(DateTime, Status)] -> [Session]
toSessions (statusUpdate:statusUpdates) = fst $ foldl folder ([], statusUpdate) statusUpdates
  where
    folder :: ([Session], (DateTime, Status)) -> (DateTime, Status) -> ([Session], (DateTime, Status))
    folder (sessions, (prevTime, LogIn )) (thisTime, LogOut) = ((prevTime, thisTime):sessions, (thisTime, LogOut))
    folder (sessions, (prevTime, LogOut)) (thisTime, LogIn ) = (sessions, (thisTime, LogIn))

-- Name by nick and uid instead of just uid.
nickTables :: [(Uid, Nick)] -> [(String, [(DateTime, Status)])] -> M.Map Nick [(DateTime, Status)]
nickTables nicks statusesByUser = M.mapKeys nickLookup $ M.fromList statusesByUser
  where
    nickLookup :: Uid -> Nick
    nickLookup uid = case (M.lookup uid $ M.fromList nicks) of
      Just x   -> x ++ " (" ++ uid ++ ")"
      Nothing  -> uid

---------------------------------------
-- User status features
---------------------------------------

{-
-- Total time online today
totalTime :: [(Integer, Status)] -> Integer
totalTime statuses = snd $ M.foldlWithKey folder (0, 0) s
  where
    increment thisTime 0 _ = 0
    increment thisTime prevTime LogIn  = 0
    increment thisTime prevTime LogOut = thisTime - prevTime
    folder (prevTime, soFar) thisTime status = (thisTime, soFar + (increment thisTime prevTime status))
    s = M.fromList statuses

-- Number of sessions that a person spent online
nSessions :: [(Integer, Status)] -> Integer
nSessions statuses = fromIntegral $ length $ filter (== LogIn) $ map snd statuses
-}

---------------------------------------
main = do
---------------------------------------

  -- Connect
  args <- getArgs
  conn <- connectSqlite3 $ head args

  -- Query
  nicks          <- getUsers conn
  statusesByUser <- getStatusesByUser conn
  -- let statuses = M.map (M.toAscList . toSessions) $ 

  {-
  -- Print all of the times for today.
  mapM_ print $ take 9 $ M.toAscList timeOnline

  -- Print the number of sessions
  putStrLn $ show $ M.map nSessions statuses
  -}

  -- return statuses
  putStrLn $ show $ nickTables nicks statusesByUser
