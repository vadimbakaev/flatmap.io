{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.AddCompany where

import Import
import Data.Text as T (replace)
import qualified Data.List.Split as LS (chunksOf)

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
  newCompany <-
    runInputPost $
    NewCompany <$> ireq textField "companyName" <*> ireq textField "website" <*>
    ireq textField "industry" <*>
    ((\address -> Office address (Coordinate 0 0)) <$> ireq textField "address") <*>
    (Socials <$>
     (extractGithub <$> ireq textField "github") <*> (extractLinkedin <$> ireq textField "linkedin")) <*>
    ireq checkBoxField "startup" <*>
    ireq checkBoxField "remote" <*>
    ((\ms -> [lang | (Just True, lang) <- ms `zip` langs]) <$>
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
extractLinkedin = replaceBackslash . T.replace "https://www.linkedin.com/company/" ""
