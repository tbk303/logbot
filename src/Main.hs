module Main where

import Network
import Network.XMPP
import Network.XMPP.MUC

import Control.Concurrent
import Control.Exception
import Control.Monad.Error

import System.Environment
import System.Exit
import System.IO

import Data.Time
import Data.ConfigFile

import Logbot.Backlog (LogState)
import qualified Logbot.Backlog as B

data BotCfg = BotCfg
	{ bcUsername :: String
	, bcServer :: String
	, bcPassword :: String
	, bcAdmin :: String
	, bcFile :: String
	, bcRoom :: String
	, bcNick :: String
	, bcSecret :: String
	}
	deriving (Show)

botResource :: String
botResource = "logbot"

delay :: Int
delay = 10 * 60 * 1000000

main :: IO ()
main = withSocketsDo $ do
	args <- getArgs
	if (length args) /= 1 then 
		do
			putStrLn "Please provide path to configuration file"
			exitWith $ ExitFailure 1
		else 
		do
			cfg <- runErrorT $ do
				cp <- join $ liftIO $ readfile emptyCP (head args)
				u <- get cp "account" "username"
				s <- get cp "account" "server"
				p <- get cp "account" "password"
				a <- get cp "general" "admin"
				f <- get cp "general" "file"
				r <- get cp "room" "name"
				n <- get cp "room" "nick"
				w <- get cp "room" "password"
				return $ BotCfg u s p a f r n w 
			startServer cfg
	where
	startServer (Left _) = do
		putStrLn "Error reading configuration file"
		exitWith $ ExitFailure 2
	startServer (Right cfg) = do
		c <- openStream (bcServer cfg)
		h <- openFile (bcFile cfg) AppendMode
		_ <- getStreamStart c

		runServer h c cfg `finally` (closeConnection c >> hClose h)
		startServer (Right cfg) -- Try starting again upon connection loss

	runServer h c cfg = runXMPP c $ do
		_ <- startAuth (bcUsername cfg) (bcServer cfg) (bcPassword cfg) botResource
		sendPresence Nothing Nothing
		handleVersion "logbot" "0.1" "Haskell"
		s <- liftIO $ newMVar B.init
		joinGroupchat (bcNick cfg) (bcRoom cfg) (Just $ bcSecret cfg)
		addHandler (isGroupchatMessage `conj` hasBody) (handleGroupMessage h s) True
		addHandler (isGroupchatPresence) (handleGroupPresence s) True
--		_ <- liftIO $ forkIO $ keepalive (bcServer cfg) c
		sendMessage (bcAdmin cfg) "We're back on-line!"

keepalive :: String -> TCPConnection -> IO ()
keepalive s c = runXMPP c $ do
	_ <- sendIq s "get" [ XML "ping" [("xmlns","urn:xmpp:ping")] [] ]
	liftIO $ threadDelay delay
	liftIO $ keepalive s c

handleGroupMessage :: Handle -> MVar LogState -> StanzaHandler
handleGroupMessage h ms st = do
	mf <- return $ getAttr "from" st
	mm <- return $ getMessageBody st
	updateBacklog mf mm
		where
		updateBacklog (Just f) (Just m) = do
			s <- liftIO $ takeMVar ms
			t <- liftIO $ getCurrentTime
			l <- return $ "[" ++ (show t) ++ "] " ++ (getResource f) ++ ": " ++ m
			liftIO $ putMVar ms (B.log s t l)
			liftIO $ hPutStrLn h l
			liftIO $ hFlush h
		updateBacklog _ _ = return ()

handleGroupPresence :: MVar LogState -> StanzaHandler
handleGroupPresence ms st = do
	mf <- return $ getAttr "from" st
	mr <- return $ maybe Nothing (getAttr "role") $ xmlPath ["x", "item"] st
	updatePresence mf mr
		where
		updatePresence (Just f) (Just "none") = do
			s <- liftIO $ takeMVar ms
			liftIO $ putMVar ms (B.part s f)
			liftIO $ putStrLn ("Starting logging for " ++ f)
		updatePresence (Just f) (Just _) = do
			s <- liftIO $ takeMVar ms
			b <- return $ B.backlog s f 
			liftIO $ putMVar ms (B.join s f)	
			liftIO $ putStrLn ("Finished logging for " ++ f)
		updatePresence _ _ = return ()

