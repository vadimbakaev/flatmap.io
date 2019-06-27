{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

instance FromJSON Company

instance FromJSON JobType

instance FromJSON Office

instance FromJSON Coordinate

instance FromJSON SocialNetwork

instance ToJSON Company

instance ToJSON JobType

instance ToJSON Office

instance ToJSON Coordinate

instance ToJSON SocialNetwork

data Company =
  Company
    { companyName :: !Text
    , companyWebsite :: !Text
    , companyTypes :: ![JobType]
    , companyOffice :: !Office
    , companySocials :: ![SocialNetwork]
    , companyStack :: ![Text]
    }
  deriving (Eq, Show, Generic)

data JobType
  = Product
  | Consulting
  deriving (Eq, Show, Generic)

data SocialNetwork =
  SocialNetwork
    { sNname :: !Text
    , sNurl :: !Text
    }
  deriving (Eq, Show, Generic)

data Coordinate =
  Coordinate
    { coordinateLat :: !Float
    , coordinateLon :: !Float
    }
  deriving (Eq, Show, Generic)

data Office =
  Office
    { officeAddress :: !Text
    , officeCoordinate :: !Coordinate
    }
  deriving (Eq, Show, Generic)
