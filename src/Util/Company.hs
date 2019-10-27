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

module Util.Company where

import Import
import Data.Time.Clock

-- Default setup one week
recentlyAdded :: UTCTime -> Entity Company -> Bool
recentlyAdded now = ((7 * 24 * 60 * 60 :: NominalDiffTime) <) . diffUTCTime now . companyCreatedAt . entityVal
