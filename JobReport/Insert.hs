module JobReport.Insert where

import Control.Monad
import JobReport.Database

data InsertOptions = InsertOptions
    { insertOptionsJob          :: String
    , insertOptionsPaymentYear  :: Int
    , insertOptionsPaymentMonth :: Int
    , insertOptionsYear         :: Int
    , insertOptionsMonth        :: Int
    , insertOptionsDay          :: Int
    , insertOptionsStartHour    :: Int
    , insertOptionsStartMinute  :: Int
    , insertOptionsEndHour      :: Int
    , insertOptionsEndMinute    :: Int
    } deriving Show

insertAction :: InsertOptions -> IO ()
insertAction opts = do
    conn <- openDatabase
    run conn $ do
        job <- insertOrGet $ Job (insertOptionsJob opts) 0
        pay <- insertOrGet $ Payment job (insertOptionsPaymentYear opts)
                                         (insertOptionsPaymentMonth opts)
        void . insertOrGet $ Working pay (insertOptionsYear opts)
                                         (insertOptionsMonth opts)
                                         (insertOptionsDay opts)
                                         (insertOptionsStartHour opts)
                                         (insertOptionsStartMinute opts)
                                         (insertOptionsEndHour opts)
                                         (insertOptionsEndMinute opts)

-- vim: set ts=4 sts=4 sw=4 et:
