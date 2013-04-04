{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
import           System.Environment      (getArgs)
import           Database.HDBC           (quickQuery, fromSql, toSql)
import           Database.HDBC.Sqlite3   (Connection, connectSqlite3)
import qualified Data.Map                as M
import           Data.DateTime           as D

-- Import types
data Status = LogIn | LogOut deriving (Enum, Show, Eq)

type Uid     = String
type Nick    = String
type NickUid = String

type DateInteger = Integer
type Session = (DateInteger, DateInteger)
type Users   = M.Map NickUid [Session]

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


getStatuses :: Connection -> String -> IO [(DateInteger, Status)]
getStatuses conn uid = do
  query <- quickQuery conn sql [(toSql uid)]
  return $ map unSql query
  where
    sql = "SELECT ts,status FROM log_status WHERE uid = ?"
    unSql row = (((fromSql $ head row) :: DateInteger), ((status $ fromSql $ last row) :: Status))
    status s = case s of
      "avail"    -> LogIn
      "notavail" -> LogOut

getStatusesByUser :: Connection -> IO [(String, [(DateInteger, Status)])]
getStatusesByUser conn = do
  users    <- getUsers conn
  let uids = map fst users

  statuses <- mapM (getStatuses conn) uids
  let statusesByUser = zip uids statuses

  return statusesByUser

---------------------------------------
-- Convert to the Session type
---------------------------------------
folder :: ([Session], (DateInteger, Status)) -> (DateInteger, Status) -> ([Session], (DateInteger, Status))
folder (sessions, (prevTime, LogIn )) (thisTime, LogOut) = ((prevTime, thisTime):sessions, (thisTime, LogOut))
folder (sessions, (prevTime, LogOut)) (thisTime, LogIn ) = (sessions, (thisTime, LogIn))
folder (sessions, (prevTime, LogOut)) (thisTime, LogOut) = (sessions, (prevTime, LogOut))
folder (sessions, (prevTime, LogIn )) (thisTime, LogIn ) = (sessions, (thisTime, LogIn))

toSessions :: [(DateInteger, Status)] -> [Session]
toSessions (statusUpdate:statusUpdates) = fst $ foldl folder ([], statusUpdate) statusUpdates

-- Name by nick and uid instead of just uid.
nickTables :: [(Uid, Nick)] -> [(String, [(DateInteger, Status)])] -> Users
nickTables nicks statusesByUser = M.map toSessions $ M.mapKeys nickLookup $ M.fromList statusesByUser
  where
    nickLookup :: Uid -> Nick
    nickLookup uid = case (M.lookup uid $ M.fromList nicks) of
      Just x   -> x ++ " (" ++ uid ++ ")"
      Nothing  -> uid

---------------------------------------
-- Build user features and export
---------------------------------------

-- Round to the nearest bin
roundToBin :: Integer -> Integer -> Integer
roundToBin binWidth datetime = binWidth * (round $ datetime/bin)

-- Time span (list of bins)
timeBins :: Integer -> DateInteger -> DateInteger -> [DateInteger]
timeBins binWidth minDate maxDate = [(r minDate)..(r maxDate)]
  where
    r = roundToBin binWidth

-- Time online by bin, given an ascending list
timeOnline :: Integer -> [DateInteger] -> Integer
timeOnline binWidth sessions = map ( \ (a, b) -> (r a, r b) ) sessions
  where
    r = roundToBin binWidth

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


  ---------------------
  -- Tables for graphs
  -- x=time, group=user
  ---------------------

  ---- Raw online status ----
  -- Daily online status pdf
  -- Weekly online status pdf
  -- Absolute online status

  ---- Franticness ----


    -}

  -- return statuses
  putStrLn $ show $ nickTables nicks statusesByUser
