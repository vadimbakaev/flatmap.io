{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Home where

import Data.Aeson
import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
-- Let's handle IOException and decodeFail
getHomeR :: Handler Html
getHomeR = do
  companies <- runDB getAllCompanies
  defaultLayout $ do
    App {..} <- getYesod
    aDomId <- newIdent
    setTitle "Discover new opportunity"
    $(widgetFile "homepage")

getAllCompanies :: DB [Entity Company]
getAllCompanies = selectList [] []
