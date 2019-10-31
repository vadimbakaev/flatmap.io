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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Database where

import Import

getCompanies :: DB [Entity Company]
getCompanies = getAllCompanies Nothing Nothing Nothing

getCompaniesByIds :: [CompanyId] -> DB [Entity Company]
getCompaniesByIds ids = selectList [CompanyId <-. ids] []

getAllCompanies :: Maybe Text -> Maybe Bool -> Maybe Text -> DB [Entity Company]
getAllCompanies (Just lang) mremote mindustry = do
  companies <- getAllCompanies Nothing mremote mindustry
  pure $ filter (elem lang . companyStack . entityVal) companies
getAllCompanies _ mremote mindustry =
  selectList
    (catMaybes
       [(CompanyRemote ==.) <$> mremote, (CompanyIndustry ==.) <$> mindustry])
    []

promoteNewCompany :: Key NewCompany -> NewCompany -> DB ()
promoteNewCompany newCompanyId company = do
  _ <- insertKey (toCompanyKey newCompanyId) (toCompany company)
  _ <- delete newCompanyId
  pure ()

toCompany :: NewCompany -> Company
toCompany (NewCompany companyName companyWebsite companyIndustry companyOffice companySocials companyStartup companyRemote companyStack companyCreatedAt companyAddedBy) =
  Company {..}

toCompanyKey :: Key NewCompany -> Key Company
toCompanyKey (NewCompanyKey key) = CompanyKey key

getBookmarks :: UserId -> DB (Maybe (Entity Bookmarks))
getBookmarks userId = selectFirst [BookmarksUserId ==. userId] []

updateBookmarksItems :: BookmarksId -> [CompanyId] -> DB ()
updateBookmarksItems bookmarksId items = update bookmarksId [BookmarksItems =. items]

totalNewCompanies :: DB Int
totalNewCompanies = count ([] :: [Filter NewCompany])

getNewCompanies :: DB [Entity NewCompany]
getNewCompanies = selectList [] []
