module JobReport.Insert where

import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
import JobReport.Database


newtype InsertOptions = InsertOptions Working
    deriving (Read, Show)

data Job = Job
    { jobName :: String
    } deriving (Read, Show)

data Payment = Payment
    { paymentJob   :: Job
    , paymentYear  :: Int
    , paymentMonth :: Int
    } deriving (Read, Show)

data Working = Working
    { workingPayment     :: Payment
    , workingYear        :: Int
    , workingMonth       :: Int
    , workingDay         :: Int
    , workingStartHour   :: Int
    , workingStartMinute :: Int
    , workingEndHour     :: Int
    , workingEndMinute   :: Int
    } deriving (Read, Show)

insertAction :: InsertOptions -> IO ()
insertAction (InsertOptions w) = withDatabase (flip withTransaction f)
  where
    f c = do
      insertPayment c (workingPayment w)
      insertWorking c w

insertPayment :: Connection -> Payment -> IO ()
insertPayment c (Payment j y m) = void $ run c stm [toSql (jobName j), toSql y, toSql m]
  where
    stm = unlines
      [ "INSERT OR IGNORE INTO Payment (job, year, month)"
      , "  SELECT id, ?2, ?3"
      , "  FROM   Job"
      , "  WHERE  name = ?1"
      ]

insertWorking :: Connection -> Working -> IO ()
insertWorking c w = void $ run c stm args
  where
    args =
      [ toSql $ jobName      $ paymentJob $ workingPayment w
      , toSql $ paymentYear  $ workingPayment w
      , toSql $ paymentMonth $ workingPayment w
      , toSql $ workingYear w
      , toSql $ workingMonth w
      , toSql $ workingDay w
      , toSql $ workingStartHour w
      , toSql $ workingStartMinute w
      , toSql $ workingEndHour w
      , toSql $ workingEndMinute w
      ]
    stm = unlines
      [ "INSERT INTO Working (payment, year, month, day, startHour, startMinute, endHour, endMinute)"
      , "  SELECT     Payment.id, ?4, ?5, ?6, ?7, ?8, ?9, ?10"
      , "  FROM       Payment"
      , "  INNER JOIN Job ON Job.id = Payment.job"
      , "  WHERE      Job.name      = ?1"
      , "  AND        Payment.year  = ?2"
      , "  AND        Payment.month = ?3"
      ]
