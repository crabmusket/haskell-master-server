{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai (remoteHost)
import Network.HTTP.Types.Status (status200)
import Network.Socket.Internal (SockAddr)

import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, withMVar)

import Data.Monoid (mconcat, (<>))
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (Text, pack)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

-- The master server has two responsibilities. First, it must respond to requests
-- to see the server list or update a server's information. Second, it must drop
-- servers from the list which have not updated themselves recently.
main :: IO ()
main = do
    -- We're not going to bother with a database. Instead, we'll just keep a
    -- list of active games in an MVar for concurrent access.
    games <- newMVar []
    -- Fork a thread to clean old game listings every so often.
    forkIO $ forever $ sleep 60 >> dropOldGames games
    -- Serve the web app in this thread.
    scotty 3000 $ app games

-- The web app has two posible actions: GETting the current list of games, and
-- POSTing an update to the list.
app :: GameList -> ScottyM ()
app games = do
    get "/games" $ listGames games
    post "/games" $ updateGame games

-- To list the games, we read the MVar that holds the game list, building a Text
-- with a game on each line.
listGames :: GameList -> ActionM ()
listGames games = do
    result <- liftIO $
        withMVar games (return . mconcat . fmap formatGame)
    html result

-- Eventually this should be a little more involved, for example using the query
-- string to return certain information about the game servers. But, for now,
-- our game format is simply the IP address.
formatGame :: Game -> Text
formatGame (addr, _) = pack (show addr) <> "\n"

-- To update a game, we take the IP address of the requestor and simply store
-- the current time associated with it. We return no value, but set a 200 status
-- code to denote success.
updateGame :: GameList -> ActionM ()
updateGame games = do
    host <- fmap remoteHost request
    now  <- liftIO getCurrentTime
    liftIO $ modify games $ updateAssoc host now
    status status200

-- Meanwhile, the thread that we started to drop old games will call this action
-- once a minute. We filter for only games that were updated in the last 30
-- seconds from now.
dropOldGames :: GameList -> IO ()
dropOldGames games = do
    now <- getCurrentTime
    modify games $ filter (updatedInTheLast 30 now . snd)

-- Simple time diff.
updatedInTheLast :: Int -> UTCTime -> UTCTime -> Bool
updatedInTheLast max now t = dt < max
    where dt = floor $ toRational $ now `diffUTCTime` t

-- Now we define our types, which are pretty basic.
type GameList = MVar [Game]
type Game = (SockAddr, UTCTime)

-- Modify an MVar with a pure function. Curiously missing from libraries.
modify :: MVar a -> (a -> a) -> IO ()
modify mv op = do
    v <- takeMVar mv
    putMVar mv $ op v

-- Update a value in an associative list, or add the new pair to the list if the
-- key isn't found.
updateAssoc :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
updateAssoc key val assoc =
    case lookup key assoc of
        Nothing -> (key, val) : assoc
        Just old -> map (\(k, v) -> (k, if k == key then val else v)) assoc

-- And a nicer sleep function than threadDelay.
sleep :: Float -> IO ()
sleep s = threadDelay $ floor (s * 1e6)
