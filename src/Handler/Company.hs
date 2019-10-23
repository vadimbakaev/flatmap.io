{-
    flatmap.io IT job search based on geo and technology.
    Copyright (C) 2019 Vadim Bakaev

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Company where

import qualified Data.List.Split as LS (chunksOf)
import Data.Text as T (replace)
import Import
import Network.HTTP.Simple

spamProtection :: Int
spamProtection = 150

getCompanyR :: Handler Html
getCompanyR =
  defaultLayout $ do
    setTitle "Add Company"
    $(widgetFile "add-company-form")

postCompanyR :: Handler Html
postCompanyR = do
  App {..} <- getYesod
  muserId <- fmap entityKey <$> maybeAuth
  address <- runInputPost $ ireq textField "address"
  coordinate <- liftIO $ safe $ resolveCoordinate appMapQuestKey address
  createdAt <- liftIO getCurrentTime
  newCompany <-
    runInputPost $
    NewCompany <$> ireq textField "companyName" <*> ireq textField "website" <*>
    ireq textField "industry" <*>
    pure (Office address coordinate) <*>
    (Socials <$> (extractGithub <$> ireq textField "github") <*>
     (fmap extractLinkedin <$> iopt textField "linkedin") <*>
     (fmap extractTwitter <$> iopt textField "twitter")) <*>
    ireq checkBoxField "startup" <*>
    ireq checkBoxField "remote" <*>
    ((\checks -> [lang | (Just True, lang) <- checks `zip` langs]) <$>
     traverse (iopt checkBoxField) langs) <*>
    pure createdAt <*>
    pure muserId
  total <- runDB $ count ([] :: [Filter NewCompany])
  when (total < spamProtection) $ void $ runDB $ insert newCompany
  defaultLayout $ do
    setTitle $
      toHtml $ mconcat ["Thank you for added ", newCompanyName newCompany]
    $(widgetFile "add-company-thank-you")

replaceBackslash :: Text -> Text
replaceBackslash = T.replace "/" ""

extractGithub :: Text -> Text
extractGithub = replaceBackslash . T.replace "https://github.com/" ""

extractLinkedin :: Text -> Text
extractLinkedin =
  replaceBackslash . T.replace "https://www.linkedin.com/company/" ""

extractTwitter :: Text -> Text
extractTwitter = replaceBackslash . T.replace "https://twitter.com/" ""

resolveCoordinate :: Text -> Text -> IO Coordinate
resolveCoordinate key address = do
  initRequest <-
    parseRequest $
    unpack $
    mconcat
      [ "https://www.mapquestapi.com/geocoding/v1/address?maxResults=1&key="
      , key
      , "&location="
      , address
      ]
  response <- httpJSON initRequest
  pure $ toCoordinate $ getResponseBody response

safe :: IO Coordinate -> IO Coordinate
safe io = do
  result <- try io
  case result :: Either SomeException Coordinate of
    Left ex -> const (Coordinate 0 0) <$> print ex
    Right val -> pure val

toCoordinate :: GeoResponse -> Coordinate
toCoordinate (GeoResponse (ResultResponse [LocationResponse (CoordinateResponse lat lng)]:_)) =
  Coordinate lat lng
toCoordinate _ = Coordinate 0 0
