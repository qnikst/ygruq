{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Logendar where

import Import
import Data.Time.Calendar
import Data.Time.Clock
import Data.Text as T
import Data.Maybe
import Text.Printf

getLogendarR :: Handler RepHtml
getLogendarR = do
    form <- liftIO logDateForm
    (formWidget, formEnctype) <- generateFormPost $ renderTable form
    defaultLayout $ do
        $(widgetFile "logendar")

logsUrlPrefix :: Text
logsUrlPrefix = "http://gentoo.ru/jabber/logs/"

redirectUrlTail :: (Integer, Int, Int) -> Text
redirectUrlTail (year, month, day) = T.pack $ printf "%d/%02d/%02d.html" year month day

getCurrentDay :: IO Day
getCurrentDay = do
    getCurrentTime >>= return . utctDay

datePickerAttrs :: Day -> [(Text, Text)]
datePickerAttrs day = 
    let textDay = T.pack $ showGregorian day
        in [ ("min", "2008-07-26")
           , ("max", textDay)]

confField :: Field App App Text
confField = selectFieldList conferenceUrls
                where conferenceUrls :: [ (Text, Text) ]
                      conferenceUrls = [ ("gentoo@c.g.r", "gentoo")
                                       , ("python@c.g.r", "python")
                                       , ("awesome@c.g.r", "awesome")
                                       ]

getRedirectR :: Handler RepHtml
getRedirectR = do
    date <- toGregorian <$> (runInputGet $ ireq dayField "date")
    conf <- runInputGet $ ireq confField "conf"
    let redirectUrl = T.concat [logsUrlPrefix, conf, "@conference.gentoo.ru/"::Text, redirectUrlTail date]
    redirect (redirectUrl)


logDateForm :: IO (AForm App App (Day, Text))
logDateForm = do
    currentDay <- getCurrentDay
    let attrs = datePickerAttrs currentDay
    let confFieldSettings = FieldSettings "Конференция" Nothing Nothing (Just "conf") []
    let dayFieldSettings = FieldSettings "Дата" Nothing Nothing (Just "date") attrs
    return $ (,) <$> areq dayField dayFieldSettings (Just currentDay)
                 <*> areq confField confFieldSettings Nothing
