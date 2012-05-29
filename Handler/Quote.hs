module Handler.Quote
    where

import Import
import Data.Time

instance RenderMessage App LinkSource where
    renderMessage _ _ Gru = "Gentoo.ru"
    renderMessage _ _ _   = "Other"


quoteAForm :: UTCTime -> Maybe Quote -> AForm App App Quote
quoteAForm time mquote = Quote 
            <$> aopt textField "Отправитель" (quoteSender <$> mquote)
            <*> aopt textField "Автор"       (quoteAuthor <$> mquote)
            <*> areq (selectFieldList sources) "Место" (quoteSource <$> mquote)
            <*> areq textField "Ссылка"      (quoteProoflink <$> mquote)
            <*> areq textareaField "Текст"   (quoteText   <$> mquote)
            <*> pure time 
            <*> pure False
        where 
            sources = [("gentoo.ru",Gru)
                      ,("gentoo@conference.gentoo.ru",GruConf)
                      ,("gentoo@conference.jabber.ru",JRuConf)
                      ,("gentoo-user-ru@lists.gentoo.org",RuMail)
                      ,("ru.gentoo-wiki.com",RuWiki)
                      ,("Другое",OtherSource)]





postQuoteCreateR :: Handler RepHtml
postQuoteCreateR = undefined
getQuoteCreateR  :: Handler RepHtml
getQuoteCreateR  = undefined

getQuoteListR    :: Handler RepHtml
getQuoteListR    = do
    quotes <- runDB $ selectList [QuoteApproved ==. True] []
    defaultLayout $ do
        $(widgetFile "quote-list")


getQuoteListPageR:: Int -> Handler RepHtml
getQuoteListPageR page = undefined

getQuoteAbyssListR :: Handler RepHtml
getQuoteAbyssListR = do
    quotes <- runDB $ selectList [] []
    defaultLayout $ do
        $(widgetFile "quote-list")

getQuoteAbyssListPageR::Int-> Handler RepHtml
getQuoteAbyssListPageR  page  = undefined


-- getQuotesList :: Maybe Int -> Bool -> Entity
--listQuotes mpage True  = runDB $ selectList [QuoteApproved ==.True] []
--listQuotes mpage False = runDB $ selectList [] []
