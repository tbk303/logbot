module Logbot.Backlog (log, backlog, init, part, join, LogState, Backlog(..)) where

import Prelude hiding (log, init)

import Data.Time

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

type JID = String
type Message = String

data Backlog = Backlog
	{ bLog :: [(UTCTime, JID, Message)]
	}
	deriving Show

data LogState = LogState
	{ lLogs :: Map JID Backlog
	, lAbsence :: Set JID
	}
	deriving Show

-- | Log a message for absent JIDs.
log :: LogState -> UTCTime -> JID -> Message -> LogState
log s t f m = s { lLogs = S.fold insert (lLogs s) (lAbsence s) }
	where
	insert j l = M.alter update j l
		where
		update Nothing = Just $ Backlog [(t,f,m)]
		update (Just b) = Just $ b { bLog = (bLog b) ++ [(t,f,m)] }

-- | Retrieve the backlog for the given JID (if present).
backlog :: LogState -> JID -> Maybe Backlog
backlog s j = M.lookup j (lLogs s)

-- | Mark the given JID as absent.
part :: LogState -> JID -> LogState
part s j = s { lAbsence = S.insert j $ lAbsence s }

-- | Mark the given JID as present and clear the backlog.
join :: LogState -> JID -> LogState
join s j = s { lAbsence = S.delete j $ lAbsence s, lLogs = M.delete j $ lLogs s }

-- | Initialize the backlog and presence state.
init :: LogState
init = LogState M.empty S.empty


