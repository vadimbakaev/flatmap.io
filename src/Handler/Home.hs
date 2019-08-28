{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Home where

import Data.Aeson
import Import

-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  companies <- runDB getAllCompanies
  defaultLayout $ do
    App {..} <- getYesod
    aDomId <- newIdent
    setTitle "Discover new opportunity"
    $(widgetFile "homepage")

getSearchR :: Handler Html
getSearchR = do
  langMaybe <- lookupGetParam "lang"
  let lang = fromMaybe "" langMaybe
  allCompanies <- runDB getAllCompanies
  let companies = filter (elem lang . companyStack . entityVal) allCompanies
  defaultLayout $ do
    App {..} <- getYesod
    aDomId <- newIdent
    setTitle $ toHtml $ mconcat ["Discover ", lang, " opportunity"]
    $(widgetFile "homepage")

searchForR :: Text -> Handler Html
searchForR lang = redirect (SearchR, [("lang", lang)])

getAllCompanies :: DB [Entity Company]
getAllCompanies = selectList [] []
