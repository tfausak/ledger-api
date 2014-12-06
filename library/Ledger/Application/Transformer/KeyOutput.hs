{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Transformer.KeyOutput where

import Ledger.Application.Model.Key (Key, keyId)

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Text (Text)

data KeyOutput = KeyOutput
    { keyOutputId :: Text
    } deriving (Read, Show)

instance ToJSON KeyOutput where
    toJSON keyOutput = object
        [ "id" .= keyOutputId keyOutput
        ]

toKeyOutput :: Key -> KeyOutput
toKeyOutput key = KeyOutput
    { keyOutputId = keyId key
    }
