module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
    , Text
#if __GLASGOW_HASKELL__ < 704
    , (<>)
#endif
    , showTime
    , QuotePage(..)
    , menuWidget
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text,pack)
import Settings.StaticFiles
import Settings.Development
import Data.Time.Lens
import Data.Time
import Text.Printf
import Text.Blaze


#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

showTime :: UTCTime -> Text
showTime  utc   = 
    let (y,m,d) = getL gregorian utc
        h       = getL hours     utc
        i       = getL minutes   utc
    in pack $ printf "%04d-%02d-%02d %02d:%02d" y m d h i 

-- | Menu
data QuotePage = Approved | Abyss | Create | None deriving (Eq)

instance ToMarkup QuotePage where
  toMarkup Approved = "Утвержденные цитаты"
  toMarkup Abyss    = "Неутвержденные цитаты"
  toMarkup Create   = "Добавить цитату"
  toMarkup None     = ""

menuPages = [ (Approved,(ApprovedListR,[("page","0")]))
            , (Abyss,(AbyssListR,[("page","0")]))
            , (Create,(QuoteCreateR,[]))
            ]


menuWidget pageType = 
  let pages = menuPages 
  in $(widgetFile "quote-list-wrapper")
