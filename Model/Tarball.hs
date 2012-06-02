{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Model.Tarball
    where

import Import
import Control.Monad
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Conduit.Zlib
import Database.Persist.GenericSql.Raw
import qualified Data.Text as S
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Lens
import Text.Printf


createFile :: forall master sub.
              (YesodPersist master, 
              YesodPersistBackend master ~ SqlPersist) =>
              GHandler sub master ()
createFile = do
    (qC, vC) <- runDB $ do
        a <- count [QuoteApproved ==. True]
        b <- selectFirst [] [ Desc TarballTimestamp]
        return (a,maybe 0 (tarballNumquotes.entityVal) b)
    when (qC>vC+32) $ do
         time <- liftIO $ getCurrentTime
         let y = getL year time
             m = getL month time
             d = getL day time
             s = printf "%04d%02d%02d" y m d
             f = "fortune-mod-gentoo-ru-"++s++".gz"
             t = Tarball 
                    { tarballFilename  = S.pack f
                    , tarballNumquotes = qC
                    , tarballTimestamp = time
                    }
         _ <- runDB $ insert t
         writeFortunes f
    writeFortunes "fortune-mod-gentoo-ru-99999999.ebuild"

writeFortunes :: forall master sub. 
                (YesodPersist master,
                YesodPersistBackend master ~ SqlPersist) =>
                [Char] -> GHandler sub master ()
writeFortunes f = do
         runDB $ do 
            runResourceT $ selectSource [QuoteApproved ==. True] []
                $= CL.map toText
                $$ CL.map encodeUtf8
                =$ gzip 
                =$ sinkFile ("static/files/"++f)
    





toText :: Entity Quote -> Text
toText (Entity _ quote) = 
    S.concat [ (unTextarea $ quoteText quote)
             , "\n"
             , "   -- "
             , (maybe "" id $ quoteAuthor quote)
             , " "
             , (S.pack $ show $ quoteSource quote)
             , "\n%\n"
             ]
