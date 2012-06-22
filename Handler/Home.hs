{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time
import Handler.Quote
import Model.Quote
import Database.Persist.GenericSql.Raw

-- | form widget
quoteCreate :: GWidget App App () -> Enctype -> GWidget App App ()
quoteCreate formWidget enctype = $(widgetFile "quote-create")

-- | version list widget
versionCreate :: [Entity (TarballGeneric SqlPersist)] -> GWidget App App ()
versionCreate versions = $(widgetFile "quote-version")


getHomeR :: Handler RepHtml
getHomeR = do
    -- get database info
    (notApproved,versions) <- runDB $ do
        a <- count [QuoteApproved ==. False]
        b <- selectList [] [Desc TarballTimestamp]
        return (a,b)
    -- create form widet
    time <- liftIO $ zonedTimeToUTC <$> getZonedTime
    form <- quoteAForm
    (formWidget,enctype) <- generateFormPost $ renderTable form 
    authors <- getAuthorsList
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

