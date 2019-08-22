{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.AddCompany where

import qualified Data.List.Split as LS (chunksOf)
import Data.Text as T (replace)
import Import
import Network.HTTP.Simple

spamProtection :: Int
spamProtection = 150

getAddCompanyR :: Handler Html
getAddCompanyR =
  defaultLayout $ do
    setTitle "Add Company"
    $(widgetFile "add-company-form")

postAddCompanyR :: Handler Html
postAddCompanyR = do
  App {..} <- getYesod
  address <- runInputPost $ ireq textField "address"
  coordinate <- liftIO $ safe $ resolveCoordinate appMapQuestKey address
  newCompany <-
    runInputPost $
    NewCompany <$> ireq textField "companyName" <*> ireq textField "website" <*>
    ireq textField "industry" <*>
    pure (Office address coordinate) <*>
    (Socials <$> (extractGithub <$> ireq textField "github") <*>
     (extractLinkedin <$> ireq textField "linkedin")) <*>
    ireq checkBoxField "startup" <*>
    ireq checkBoxField "remote" <*>
    ((\checks -> [lang | (Just True, lang) <- checks `zip` langs]) <$>
     traverse (iopt checkBoxField) langs)
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

resolveCoordinate :: Text -> Text -> IO Coordinate
resolveCoordinate key address = do
  initRequest <-
    parseRequest $
    unpack $
    mconcat
      [ "https://www.mapquestapi.com/geocoding/v1/address?maxResults=1"
      , "&key="
      , key
      , "&location="
      , address
      ]
  response <- httpJSON initRequest
  pure $ toCoordinate $ getResponseBody response

safe :: IO Coordinate -> IO Coordinate
safe io = do
  result <- try io :: IO (Either SomeException Coordinate)
  case result of
    Left ex -> const (Coordinate 0 0) <$> print ex
    Right val -> pure val

toCoordinate :: GeoResponse -> Coordinate
toCoordinate (GeoResponse (ResultResponse [LocationResponse (CoordinateResponse lat lng)]:_)) =
  Coordinate lat lng
toCoordinate _ = Coordinate 0 0
