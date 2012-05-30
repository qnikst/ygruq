module Handler.Quote
    where

import Import
import Data.Time
import Data.Text (pack)
import Control.Applicative
import Control.Arrow
import Text.Blaze

instance ToMarkup LinkSource where
    toMarkup (Gru) = "Gentoo.ru"
    toMarkup GruConf = "getnoo@conference.gentoo.ru"
    toMarkup JRuConf = "gentoo@conference.jabber.ru"
    toMarkup RuMail  = "gentoo-user-ru@lists.gentoo.org"
    toMarkup RuWiki  = "ru.gentoo-wiki.com"
    toMarkup OtherSource = "Другое"


quoteAForm :: UTCTime -> Maybe Quote -> AForm App App Quote
quoteAForm time mquote = Quote 
            <$> aopt textField "Отправитель" (quoteSender <$> mquote)
            <*> aopt textField "Автор"       (quoteAuthor <$> mquote)
            <*> areq (selectFieldList sources) "Место" (quoteSource <$> mquote)
            <*> areq urlField "Ссылка"      (quoteProoflink <$> mquote)
            <*> areq textareaField "Текст"   (quoteText   <$> mquote)
            <*> pure time 
            <*> pure False
        where 
            sources :: [(Text,LinkSource)]
            sources = [("gentoo.ru",Gru)
                      ,("gentoo@conference.gentoo.ru",GruConf)
                      ,("gentoo@conference.jabber.ru",JRuConf)
                      ,("gentoo-user-ru@lists.gentoo.org",RuMail)
                      ,("ru.gentoo-wiki.com",RuWiki)
                      ,("Другое",OtherSource)]



postQuoteCreateR :: Handler RepHtml
postQuoteCreateR = do
    time <- liftIO getCurrentTime
    ((result,widget),enctype) <- runFormPost $ renderTable $ quoteAForm time Nothing
    case result of
        FormSuccess quote -> do
            quoteId <- runDB $ insert quote
            defaultLayout $ do
                $(widgetFile "quote-show")
        _other -> do 
            defaultLayout $ do
                $(widgetFile "quote-create")

getQuoteCreateR  :: Handler RepHtml
getQuoteCreateR  = do
    time <- liftIO $ getCurrentTime
    (widget,enctype) <- generateFormPost $ renderTable $ quoteAForm time Nothing
    defaultLayout $ do
        $(widgetFile "quote-create")


getQuoteListR    :: Handler RepHtml
getQuoteListR    = do
    quotes <- runDB $ selectList [QuoteApproved ==. True] []
    defaultLayout $ do
        $(widgetFile "quote-list")


getQuoteListPageR:: Int -> Handler RepHtml
getQuoteListPageR page = undefined

getQuoteAbyssListR :: Handler RepHtml
getQuoteAbyssListR = do
    maid <- maybeAuthId
    quotes <- runDB $ selectList [] []
    defaultLayout $ do
        $(widgetFile "quote-list")

getQuoteAbyssListPageR::Int-> Handler RepHtml
getQuoteAbyssListPageR  page  = undefined


-- getQuotesList :: Maybe Int -> Bool -> Entity
--listQuotes mpage True  = runDB $ selectList [QuoteApproved ==.True] []
--listQuotes mpage False = runDB $ selectList [] []
