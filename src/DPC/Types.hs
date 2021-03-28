{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
module DPC.Types where

import           Data.Serialize                 ( Serialize )
import           GHC.Generics                   ( Generic )

import           Data.Map                      as Map

type NodeID = Int

type Label = Int

data Message = Message {
  _msgFrom  :: NodeID,  -- sender id
  -- represents informatin about the request like "compute_request" it is used to filter for the right kind of messages
  _msgTag   :: String,
  _msgBody  :: [Int],   -- body made of list of ints
  _msgTo    :: NodeID,  -- target id
  -- name associated with each protlet instance associated with a node makes it possible to maintain state across several instances of the same protlet over time
  -- for e.g. the same RPC protlet can be used for multiplication and addition
  -- mul and add labels are added to distiguish to protlets on the same node
  _msgLabel :: Label
  }
  deriving (Eq, Show, Generic, Serialize)

-- Network state consists of
-- a map of global state where labels are mapped to list of protlets
-- a map of local state where labels is mapped to a node state, and
-- a message queue and if the node is active or not
data NetworkState global local = NetworkState {
  _globalState :: global,  -- user defined
  _localStates :: Map NodeID local  -- user defined
} deriving Show

-- retrieve node ids from Network state
nodes :: NetworkState g l -> [NodeID]
nodes = Map.keys . _localStates
