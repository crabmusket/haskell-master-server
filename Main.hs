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

main :: IO ()
main = do
    games <- newMVar []
    forkIO $ forever $ sleep 60 >> dropOldGames games
    scotty 3000 $ app games
    where
        sleep s = threadDelay $ floor (s * 1e6)

app :: GameList -> ScottyM ()
app games = do
    get "/games" $ listGames games
    post "/games" $ updateGame games

listGames :: GameList -> ActionM ()
listGames games = do
    result <- liftIO $ withMVar games (return . mconcat . map formatGame)
    html result

updateGame :: GameList -> ActionM ()
updateGame games = do
    host <- fmap remoteHost request
    now <- liftIO getCurrentTime
    modifyGames $ updateAssoc host now
    status status200
    where
        modifyGames = liftIO . modify games

dropOldGames :: GameList -> IO ()
dropOldGames games = do
    now <- getCurrentTime
    modify games $ filter (updatedInTheLast 120 now . snd)

updatedInTheLast :: Int -> UTCTime -> UTCTime -> Bool
updatedInTheLast max from last = (floor $ toRational $ dt) < max
    where dt = from `diffUTCTime` last

formatGame :: Game -> Text
formatGame (addr, _) = pack $ show addr <> "\n"

type GameList = MVar [Game]
type Game = (SockAddr, UTCTime)

modify :: MVar a -> (a -> a) -> IO ()
modify mv op = do
    v <- takeMVar mv
    putMVar mv $ op v

updateAssoc :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
updateAssoc key val assoc =
    case lookup key assoc of
        Nothing -> (key, val) : assoc
        Just old -> map (\(k, v) -> (k, if k == key then val else v)) assoc
