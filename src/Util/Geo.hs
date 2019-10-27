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

module Util.Geo where

import Data.Geospatial.Internal.BasicTypes
import Data.Geospatial.Internal.GeoFeature
import Data.Geospatial.Internal.GeoFeatureCollection
import Data.Geospatial.Internal.Geometry
import Import

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
