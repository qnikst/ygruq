{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Model.Quote
    where

import Prelude (head)
import Import
import Data.Conduit 
import Data.Maybe
import qualified Data.Conduit.List as CL
import Database.Persist.GenericSql.Raw
import Database.Persist.Store

getAuthorsList :: forall master sub. 
                  (YesodPersist master,
                  YesodPersistBackend master ~ SqlPersist) =>
                  GHandler sub master [Text]
getAuthorsList = do 
    let sql = "SELECT  sender, count(id) as cnt FROM quote WHERE is_approved = 1 AND sender IS NOT NULL GROUP by sender ORDER by cnt DESC"
    lst <- runDB $ runResourceT $ withStmt sql [] $$ CL.consume
    return $ catMaybes $ map (either (const Nothing) Just . convert .head) lst
    where convert = fromPersistValue 
