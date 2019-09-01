{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Home where

import Data.Aeson
import Data.Geospatial.Internal.BasicTypes
import Data.Geospatial.Internal.GeoFeature
import Data.Geospatial.Internal.GeoFeatureCollection
import Data.Geospatial.Internal.Geometry
import Import

-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
  companies <- toGeo <$> runDB (getAllCompanies Nothing)
  defaultLayout $ do
    App {..} <- getYesod
    aDomId <- newIdent
    setTitle "Discover new opportunity"
    $(widgetFile "homepage")

getSearchR :: Handler Html
getSearchR = do
  mlang <- lookupGetParam "lang"
  companies <- toGeo <$> runDB (getAllCompanies mlang)
  defaultLayout $ do
    App {..} <- getYesod
    aDomId <- newIdent
    setTitle $
      toHtml $ mconcat ["Discover ", fromMaybe "new" mlang, " opportunity"]
    $(widgetFile "homepage")

searchForR :: Text -> Handler Html
searchForR lang = redirect (SearchR, [("lang", lang)])

getAllCompanies :: Maybe Text -> DB [Entity Company]
getAllCompanies (Just lang) = do
  companies <- selectList [] []
  pure $ filter (elem lang . companyStack . entityVal) companies
getAllCompanies _ = selectList [] []

toGeo :: [Entity Company] -> GeoFeatureCollection (Entity Company)
toGeo companies =
  GeoFeatureCollection Nothing (fromList $ map toGeoFeature companies)

toGeoFeature :: Entity Company -> GeoFeature (Entity Company)
toGeoFeature company =
  GeoFeature
    { _bbox = Nothing
    , _geometry =
        Point $
        GeoPoint $
        GeoPointXY $
        PointXY
          (coordinateLon $ officeCoordinate $ companyOffice $ entityVal company)
          (coordinateLat $ officeCoordinate $ companyOffice $ entityVal company)
    , _properties = company
    , _featureId = Nothing
    }
