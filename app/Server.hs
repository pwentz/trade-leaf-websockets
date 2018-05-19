{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified MessageParser as MP
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

import qualified Network.WebSockets as WS

type ServerState = Map.Map Int WS.Connection

newServerState :: ServerState
newServerState = Map.empty

acceptableRequest :: WS.AcceptRequest
acceptableRequest =
  WS.AcceptRequest (Just "patrules") []

addClient :: Int -> WS.Connection -> ServerState -> ServerState
addClient = Map.insert

removeClient :: Int -> ServerState -> ServerState
removeClient = Map.delete

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequestWith pending acceptableRequest
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar state
    case MP.parseNewUser (T.unpack msg) of
      Nothing ->
        WS.sendTextData conn ("-- INVALID FORMAT" :: Text)
      Just newUser -> do
        putStrLn (mconcat ["NEW USER: ", show newUser])
        flip finally disconnect $ do
           modifyMVar_ state (return . addClient newUser conn )
           talk conn state newUser
        where
          disconnect = do
              modifyMVar state $ \s ->
                  let s' = removeClient newUser s
                  in return (s', s')
              return ()

sendTo :: Text -> WS.Connection -> Int -> ServerState -> IO ()
sendTo msg conn recipientId state =
  case Map.lookup recipientId state of
    Nothing -> WS.sendTextData conn (mconcat ["-- ", T.pack $ show recipientId, ": DISCONNECTED"])
    Just receivingConn ->
      WS.sendTextData receivingConn (mconcat [T.pack $ show recipientId, ": ", msg])

talk :: WS.Connection -> MVar ServerState -> Int -> IO ()
talk conn state userId = forever $ do
    msg <- WS.receiveData conn
    putStrLn (mconcat [show userId, ": ", T.unpack msg])
    case MP.parseMessage (T.unpack msg) of
      Nothing -> return ()
      Just (recipientId, message) ->
        readMVar state >>= sendTo (T.pack message) conn recipientId
