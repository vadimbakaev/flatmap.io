{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.AddCompany where

import Import

spamProtection :: Int
spamProtection = 50

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
    (Socials <$> ireq textField "github" <*> ireq textField "linkedin") <*>
    ireq textField "address" <*>
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
