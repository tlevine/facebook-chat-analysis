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

type Session = (D.DateTime, D.DateTime)
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


getStatuses :: Connection -> String -> IO [(D.DateTime, Status)]
getStatuses conn uid = do
  query <- quickQuery conn sql [(toSql uid)]
  return $ map unSql query
  where
    sql = "SELECT ts,status FROM log_status WHERE uid = ?"
    unSql row = (D.fromSeconds ((fromSql $ head row) :: Integer), (status $ fromSql $ last row) :: Status)
    status s = case s of
      "avail"    -> LogIn
      "notavail" -> LogOut

getStatusesByUser :: Connection -> IO [(String, [(D.DateTime, Status)])]
getStatusesByUser conn = do
  users    <- getUsers conn
  let uids = map fst users

  statuses <- mapM (getStatuses conn) uids
  let statusesByUser = zip uids statuses

  return statusesByUser

---------------------------------------
-- Convert to the Session type
---------------------------------------
folder :: ([Session], (D.DateTime, Status)) -> (D.DateTime, Status) -> ([Session], (D.DateTime, Status))
folder (sessions, (prevTime, LogIn )) (thisTime, LogOut) = ((prevTime, thisTime):sessions, (thisTime, LogOut))
folder (sessions, (prevTime, LogOut)) (thisTime, LogIn ) = (sessions, (thisTime, LogIn))
folder (sessions, (prevTime, LogOut)) (thisTime, LogOut) = (sessions, (prevTime, LogOut))
folder (sessions, (prevTime, LogIn )) (thisTime, LogIn ) = (sessions, (thisTime, LogIn))

toSessions :: [(D.DateTime, Status)] -> [Session]
toSessions (statusUpdate:statusUpdates) = fst $ foldl folder ([], statusUpdate) statusUpdates

-- Name by nick and uid instead of just uid.
nickTables :: [(Uid, Nick)] -> [(String, [(D.DateTime, Status)])] -> Users
nickTables nicks statusesByUser = M.map toSessions $ M.mapKeys nickLookup $ M.fromList statusesByUser
  where
    nickLookup :: Uid -> Nick
    nickLookup uid = case (M.lookup uid $ M.fromList nicks) of
      Just x   -> x ++ " (" ++ uid ++ ")"
      Nothing  -> uid

---------------------------------------
-- User status features
---------------------------------------

lifeByMinute = "%Y-%m-%d %H:%M"
lifeByHour   = "%Y-%m-%d %H"
lifeByDay    = "%Y-%m-%d"

weekByMinute = "(%w) %H:%M %A"
weekByHour   = "(%w) %H %A"

dayByMinute  = "%H:%M"
dayByHour    = "%H"


-- Time span (list of bins), given an ascending list
timeBins :: Integer -> DateTime -> DateTime -> [D.DateTime]
timeBins binWidth minDate maxDate = map fromSeconds [((toSeconds minDate)/binWidth)..((toSeconds maxDate)/binWidth)]

-- Length of a session in seconds
sessionLength :: Session -> Integer
sessionLength (start, end) = (toSeconds end) - (toSeconds start)

-- Number of session beginnings by bin
nSessionBeginnings :: String -> [Session]
nSessionBeginnings bin sessions = 
  where
    map (fst . (formatDateTime bin)) sessions

 :: String -> [Session] -> Integer
 timeFormat sessions = map sessionLength sessions

-- Number of sessions that a person spent online
nSessions :: [(Integer, Status)] -> Integer
nSessions statuses = fromIntegral $ length $ filter (== LogIn) $ map snd statuses
-}

---------------------------------------
-- Build user features and export
---------------------------------------
{-
http://hackage.haskell.org/packages/archive/probability/0.2.2/doc/html/Numeric-Probability-Distribution.html

[(NickUid, DateTime, a)]

export :: Users -> ([Session] -> [Integer]) ->
type Users   = M.Map NickUid [Session]
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
