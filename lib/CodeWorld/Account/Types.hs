{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Account.Types
    ( Password(..)
    , PasswordHash(..)
    , SecretToken(..)
    , Status(..)
    , Store(..)
    , UserId(..)
    ) where

import           Data.ByteString (ByteString)
import           Data.Hashable (Hashable)
import           Database.SQLite.Simple (SQLData(..))
import           Database.SQLite.Simple.FromField (FromField(..), ResultError(..), fieldData, returnError)
import           Database.SQLite.Simple.Ok (Ok(..))
import           Database.SQLite.Simple.ToField (ToField(..))

data Store = Store FilePath
newtype UserId = UserId String deriving (Eq, Hashable, Show)
newtype Password = Password String deriving (Eq, Show)
newtype PasswordHash = PasswordHash ByteString deriving Eq
newtype SecretToken = SecretToken String deriving Show

data Status = Active | Expired deriving Show

instance ToField Status where
    toField Active = SQLText "active"
    toField Expired = SQLText "expired"

instance FromField Status where
    fromField f = let d = fieldData f in case d of
                    SQLText "active" -> Ok Active
                    SQLText "expired" -> Ok Expired
                    _ -> returnError ConversionFailed f ("Value must be \"active\" or \"expired\", got " ++ show d)
