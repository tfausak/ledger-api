{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Ledger.Application.State where

import Ledger.Application.Model (Entry, Key)

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid (Query, Update, makeAcidic)
import Data.IxSet (IxSet, empty)
import Data.SafeCopy (base, deriveSafeCopy)

type Entries = IxSet Entry
type Keys = IxSet Key

data State = State
    { stateEntries :: Entries
    , stateKeys    :: Keys
    } deriving (Read, Show)

$(deriveSafeCopy 1 'base ''State)

defaultState :: State
defaultState = State
    { stateEntries = empty
    , stateKeys = empty
    }

queryEntries :: Query State Entries
queryEntries = fmap stateEntries ask

queryKeys :: Query State Keys
queryKeys = fmap stateKeys ask

updateEntries :: Entries -> Update State Entries
updateEntries entries = do
    _ <- modify (\ state -> state { stateEntries = entries })
    return entries

updateKeys :: Keys -> Update State Keys
updateKeys keys = do
    _ <- modify (\ state -> state { stateKeys = keys })
    return keys

$(makeAcidic ''State
    [ 'queryEntries
    , 'queryKeys
    , 'updateEntries
    , 'updateKeys
    ])
