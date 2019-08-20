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

lang6Columns :: [[Text]]
lang6Columns = LS.chunksOf 5 langs

getAddCompanyR :: Handler Html
getAddCompanyR =
  defaultLayout $ do
    setTitle "Add Company"
    $(widgetFile "add-company-form")

postAddCompanyR :: Handler Html
postAddCompanyR = do
  App {..} <- getYesod
  newCompany <-
    runInputPost $
    NewCompany <$> ireq textField "companyName" <*> ireq textField "website" <*>
    ireq textField "industry" <*>
    ((\address -> Office address (Coordinate 0 0)) <$> ireq textField "address") <*>
    (Socials <$> (extractGithub <$> ireq textField "github") <*>
     (extractLinkedin <$> ireq textField "linkedin")) <*>
    ireq checkBoxField "startup" <*>
    ireq checkBoxField "remote" <*>
    ((\ms -> [lang | (Just True, lang) <- ms `zip` langs]) <$>
     traverse (iopt checkBoxField) langs)
  total <- runDB $ count ([] :: [Filter NewCompany])
  newCoordinate <-
    liftIO $
    resolveCoordinate
      appMapQuestKey
      (officeAddress $ newCompanyOffice newCompany)
  let companyToSave = updateCoordinate newCoordinate newCompany
  when (total < spamProtection) $ void $ runDB $ insert companyToSave
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
      [ "http://www.mapquestapi.com/geocoding/v1/address?maxResults=1"
      , "&key="
      , key
      , "&location="
      , address
      ]
  response <- httpJSON initRequest
  pure $ toCoordinate (getResponseBody response :: GeoResponse)

toCoordinate :: GeoResponse -> Coordinate
toCoordinate (GeoResponse (ResultResponse [LocationResponse (CoordinateResponse lat lng)]:_)) =
  Coordinate lat lng
toCoordinate _ = Coordinate 0 0

updateCoordinate :: Coordinate -> NewCompany -> NewCompany
updateCoordinate coordinate newCompany =
  newCompany
    { newCompanyOffice =
        (newCompanyOffice newCompany) {officeCoordinate = coordinate}
    }
