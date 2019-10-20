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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Pending where

import Data.Aeson
import Data.Geospatial.Internal.BasicTypes
import Data.Geospatial.Internal.GeoFeature
import Data.Geospatial.Internal.GeoFeatureCollection
import Data.Geospatial.Internal.Geometry

import Import

getPendingR :: Handler Html
getPendingR = do
  muser <- maybeAuth
  case muser of
    Nothing -> redirect HomeR
    Just (Entity _ user) ->
      if userIsAdmin user
        then do
          newCompanies <- runDB $ selectList [] []
          defaultLayout $ do
            let companies = toGeo newCompanies
            addScriptRemote "https://code.jquery.com/jquery-3.4.1.min.js"
            App {..} <- getYesod
            aDomId <- newIdent
            setTitle "Pending companies"
            $(widgetFile "pending")
        else redirect HomeR

postPendingR :: Handler Value
postPendingR = do
  muser <- maybeAuth
  case muser of
    Nothing -> sendResponseStatus status401 ()
    Just (Entity _ user) ->
      if userIsAdmin user
        then do
          PromoteNewCompanyRequest nId <- requireInsecureJsonBody
          newCompany <- runDB $ get404 nId
          _ <-
            runDB $ do
              _ <- insertKey (toCompanyKey nId) (toCompany newCompany)
              _ <- delete nId
              pure ()
          sendResponseStatus status200 ()
        else sendResponseStatus status403 ()

toGeo :: [Entity NewCompany] -> GeoFeatureCollection (Entity NewCompany)
toGeo companies =
  GeoFeatureCollection Nothing (fromList $ map toGeoFeature companies)

toGeoFeature :: Entity NewCompany -> GeoFeature (Entity NewCompany)
toGeoFeature company =
  GeoFeature
    { _bbox = Nothing
    , _geometry =
        Point $
        GeoPoint $
        GeoPointXY $
        PointXY
          (coordinateLon $
           officeCoordinate $ newCompanyOffice $ entityVal company)
          (coordinateLat $
           officeCoordinate $ newCompanyOffice $ entityVal company)
    , _properties = company
    , _featureId = Nothing
    }

toCompany :: NewCompany -> Company
toCompany (NewCompany companyName companyWebsite companyIndustry companyOffice companySocials companyStartup companyRemote companyStack companyCreatedAt companyAddedBy) =
  Company {..}

toCompanyKey :: Key NewCompany -> Key Company
toCompanyKey (NewCompanyKey key) = CompanyKey key
