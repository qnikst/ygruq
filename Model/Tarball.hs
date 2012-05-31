{-# LANGUAGE OverloadedStrings #-}
module Model.Tarball
    where

import Import
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Conduit.Zlib
import qualified Data.Text as S
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe

import Debug.Trace

createFile = do 
    r <- runDB $ selectList [QuoteApproved ==. True] []
    CL.sourceList r  
            $= CL.map (\(Entity _ quote) ->
                        S.concat [ (unTextarea $ quoteText quote)
                                 , "\n"
                                 , "   -- "
                                 , (maybe "" id $ quoteAuthor quote)
                                 , " "
                                 , (S.pack $ show $ quoteSource quote)
                                 , "\n%\n"
                                 ]
                       )
            $$ CL.map encodeUtf8
            =$ compress 1 (WindowBits 8) 
            =$ sinkFile "1"
