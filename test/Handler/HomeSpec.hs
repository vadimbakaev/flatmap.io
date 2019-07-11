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
     do
      it "leaves the companies table empty" $ do
        get HomeR
        statusIs 200
        companies <- runDB $ selectList ([] :: [Filter Company]) []
        assertEq "companies table empty" 0 $ length companies
