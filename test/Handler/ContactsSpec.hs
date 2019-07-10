module Handler.ContactsSpec
  ( spec
  ) where

import TestImport

spec :: Spec
spec =
  withApp $ do
    describe "contacts" $ do
      it "gives a 200" $ do
        get ContactsR
        statusIs 200
