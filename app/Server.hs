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

import qualified Network.WebSockets as WS

-- We represent a client by their username and a `WS.Connection`. We will see how we
-- obtain this `WS.Connection` later on.

type ServerState = Map.Map Int WS.Connection

newServerState :: ServerState
newServerState = Map.empty

-- Check if a user already exists (based on username):

acceptableRequest :: WS.AcceptRequest
acceptableRequest =
  WS.AcceptRequest (Just "patrules") []

addClient :: Int -> WS.Connection -> ServerState -> ServerState
addClient = Map.insert

removeClient :: Int -> ServerState -> ServerState
removeClient = Map.delete


-- The main function first creates a new state for the server, then spawns the
-- actual server. For this purpose, we use the simple server provided by
-- `WS.runServer`.

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.

-- When a client is succesfully connected, we read the first message. This should
-- be in the format of "Hi! I am Jasper", where Jasper is the requested username.

-- We send a "Welcome!", according to our own little protocol. We add the client to
-- the list and broadcast the fact that he has joined. Then, we give control to the
-- 'talk' function.

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequestWith pending acceptableRequest
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    clients <- readMVar state
    case MP.parseNewUser (T.unpack msg) of
      Nothing ->
        WS.sendTextData conn ("Invalid format" :: Text)
      Just newUser ->
        flip finally disconnect $ do
           modifyMVar_ state (return . addClient newUser conn )
           talk conn state newUser
        where
          disconnect = do
              modifyMVar state $ \s ->
                  let s' = removeClient newUser s
                  in return (s', s')
              return ()

-- The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.

sendTo :: Text -> WS.Connection -> Int -> ServerState -> IO ()
sendTo msg conn recipientId state =
  case Map.lookup recipientId state of
    Nothing -> WS.sendTextData conn (mconcat [T.pack $ show recipientId, " HAS DISCONNECTED"])
    Just receivingConn ->
      WS.sendTextData receivingConn msg

talk :: WS.Connection -> MVar ServerState -> Int -> IO ()
talk conn state userId = forever $ do
  -- listen for new message
    msg <- WS.receiveData conn
    case MP.parseMessage (T.unpack msg) of
      Nothing -> return ()
      Just (recipientId, message) ->
        readMVar state >>= sendTo (T.pack message) conn recipientId
