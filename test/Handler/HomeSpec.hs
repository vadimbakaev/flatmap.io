{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.HomeSpec
  ( spec
  ) where

import TestImport

spec :: Spec
spec =
  withApp $ do
    describe "Homepage" $
        -- This is a simple example of using a database access in a test.  The
        -- test will succeed for a fresh scaffolded site with an empty database,
        -- but will fail on an existing database with a non-empty user table.
     do
      it "leaves the companies table empty" $ do
        get HomeR
        statusIs 200
        companies <- runDB $ selectList ([] :: [Filter Company]) []
        assertEq "companies table empty" 0 $ length companies
