{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

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

getHaskellR :: Handler Html
getHaskellR = getByLangR "Haskell"

getPureScriptR :: Handler Html
getPureScriptR = getByLangR "PureScript"

getByLangR :: Text -> Handler Html
getByLangR lang = do
  allCompanies <- runDB getAllCompanies
  let companies = filter (elem lang . companyStack . entityVal) allCompanies
  defaultLayout $ do
    App {..} <- getYesod
    aDomId <- newIdent
    setTitle $ toHtml $ mconcat ["Discover ", lang, " opportunity"]
    $(widgetFile "homepage")

getAllCompanies :: DB [Entity Company]
getAllCompanies = selectList [] []
