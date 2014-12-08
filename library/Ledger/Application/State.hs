{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Ledger.Application.State where

import Ledger.Application.Model (Entry, Key, KeyId)

import Control.Monad.Reader (ask)
import Control.Monad.State (gets, modify)
import Data.Acid (Query, Update, liftQuery, makeAcidic)
import Data.IxSet (IxSet, empty, insert, getEQ, getOne)
import Data.SafeCopy (base, deriveSafeCopy)

type Entries = IxSet Entry
type Keys = IxSet Key

data State = State
    { stateEntries :: Entries
    , stateKeys    :: Keys
    } deriving (Read, Show)

defaultState :: State
defaultState = State
    { stateEntries = empty
    , stateKeys = empty
    }

-- * Keys

queryKeys :: Query State Keys
queryKeys = fmap stateKeys ask

queryKey :: KeyId -> Query State (Maybe Key)
queryKey kid = do
    keys <- queryKeys
    let maybeKey = getOne (getEQ kid keys)
    return maybeKey

updateKeys :: Keys -> Update State Keys
updateKeys keys = do
    _ <- modify (\ state -> state { stateKeys = keys })
    gets stateKeys

insertKey :: Key -> Update State Key
insertKey key = do
    oldKeys <- liftQuery queryKeys
    let newKeys = insert key oldKeys
    _ <- updateKeys newKeys
    return key

-- * Entries

queryEntries :: Query State Entries
queryEntries = fmap stateEntries ask

updateEntries :: Entries -> Update State Entries
updateEntries entries = do
    _ <- modify (\ state -> state { stateEntries = entries })
    gets stateEntries

-- TH

$(deriveSafeCopy 1 'base ''State)

$(makeAcidic ''State
    [ 'queryKeys
    , 'queryKey
    , 'updateKeys
    , 'insertKey

    , 'queryEntries
    , 'updateEntries
    ])
