{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module JobReport.Database where

import Control.Monad.Logger
import Control.Monad.Trans.Resource

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Sqlite (open)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
    Job
        name String
        salary Int
        UniqueJob name
        deriving Show

    Payment
        job JobId
        year Int
        month Int
        UniquePayment job year month
        deriving Show

    Working
        payment PaymentId
        year Int
        month Int
        day Int
        startHour Int
        startMinute Int
        endHour Int
        endMinute Int
        UniqueWorking payment year month day startHour startMinute endHour endMinute
        deriving Show
|]

insertOrGet
    :: ( Functor m, PersistUnique m, PersistEntity entity
       , PersistMonadBackend m ~ PersistEntityBackend entity)
    => entity -> m (Key entity)
insertOrGet = fmap (either entityKey id) . insertBy

insertOrReplace
    :: ( PersistUnique m, PersistEntity entity
       , PersistMonadBackend m ~ PersistEntityBackend entity)
    => entity -> m (Key entity)
insertOrReplace e = insertBy e >>= \res -> case res of
    Left (Entity key _) -> replace key e >> return key
    Right key           -> return key

run :: Connection -> SqlPersistM a -> IO a
run conn sql = runResourceT . runNoLoggingT $ runSqlConn sql conn

openDatabase :: IO Connection
openDatabase = do
    conn <- wrapConnection =<< open "jobreport.sqlite"
    run conn $ do
        -- Enable foreign key constraint checking. This is only possible
        -- outside of transaction and the effects the whole connection.
        rawExecute "ROLLBACK" []
        rawExecute "PRAGMA foreign_keys = ON" []
        rawExecute "BEGIN" []
        runMigration migrateAll
    return conn

-- vim: set ts=4 sts=4 sw=4 et:
