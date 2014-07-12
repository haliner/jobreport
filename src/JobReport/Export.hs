module JobReport.Export where

import Data.Functor
import Data.List
import Data.Monoid

import Database.HDBC
import Database.HDBC.Sqlite3

import JobReport.Database

import Text.Pandoc
import Text.Pandoc.Builder


data ReportJob = ReportJob
    { reportJobJob    :: String
    , reportJobDate   :: String
    , reportJobHours  :: String
    , reportJobSalary :: String
    } deriving (Read, Show)

reportJobCollator :: ReportJob -> ReportJob -> Bool
reportJobCollator (ReportJob j1 _ _ _) (ReportJob j2 _ _ _)
  = j1 == j2

data ReportPayment = ReportPayment
    { reportPaymentPayment :: String
    , reportPaymentDate    :: String
    , reportPaymentJob     :: String
    , reportPaymentHours   :: String
    , reportPaymentSalaray :: String
    } deriving (Read, Show)

reportPaymentCollator :: ReportPayment -> ReportPayment -> Bool
reportPaymentCollator (ReportPayment p1 _ _ _ _) (ReportPayment p2 _ _ _ _)
  = p1 == p2

data ReportTotal = ReportTotal String String


exportAction :: IO ()
exportAction = withDatabase $ \c -> do
  js  <- queryReportJob c
  js' <- queryReportJobTotal c
  ps  <- queryReportPayment c
  ps' <- queryReportPaymentTotal c
  putStrLn . writeMarkdown def $ buildDocument js js' ps ps'
  return ()

queryReportJob :: Connection -> IO [ReportJob]
queryReportJob c = map conv <$> quickQuery' c "SELECT * FROM ReportJob" []
  where
    conv [a, b, c, d] = ReportJob (fromSql a) (fromSql b) (fromSql c) (fromSql d)

queryReportJobTotal :: Connection -> IO [ReportTotal]
queryReportJobTotal c = map conv <$> quickQuery' c "SELECT totalHours, totalSalary FROM ReportJobTotal" []
  where
    conv [a, b] = ReportTotal (fromSql a) (fromSql b)

queryReportPayment :: Connection -> IO [ReportPayment]
queryReportPayment c = map conv <$> quickQuery' c "SELECT * FROM ReportPayment" []
  where
    conv [a, b, c, d, e] = ReportPayment (fromSql a) (fromSql b) (fromSql c) (fromSql d) (fromSql e)

queryReportPaymentTotal :: Connection -> IO [ReportTotal]
queryReportPaymentTotal c = map conv <$> quickQuery' c "SELECT totalHours, totalSalary FROM ReportPaymentTotal" []
  where
    conv [a, b] = ReportTotal (fromSql a) (fromSql b)

buildDocument :: [ReportJob] -> [ReportTotal] -> [ReportPayment] -> [ReportTotal] -> Pandoc
buildDocument js js' ps ps' = setTitle t $ doc bs
  where
    t = text "Nebenjob-Übersicht"
    bs = header 1 t
      <> header 2 (text "Abrechnungen")
      <> mconcat (zipWith buildJob (groupBy reportJobCollator js) js')
      <> header 2 (text "Arbeitsstunden")
      <> mconcat (zipWith buildPayment (groupBy reportPaymentCollator ps) ps')

buildJob :: [ReportJob] -> ReportTotal -> Blocks
buildJob js (ReportTotal h' s') = header 3 (text j)
                               <> simpleTable hs rs
  where
    (ReportJob j _ _ _) = head js
    hs = map (plain . text) ["Abrechnung", "Stunden", "Gehalt"]
    rs = map buildJobRow js
      ++ [[ plain $ text "Summe"
          , plain $ text h'
          , plain $ text s'
          ]]

buildJobRow :: ReportJob -> [Blocks]
buildJobRow (ReportJob _ d h s) = map (plain . text) [d, h, s]

buildPayment :: [ReportPayment] -> ReportTotal -> Blocks
buildPayment ps (ReportTotal h' s') = header 3 (text p)
                                   <> simpleTable hs rs
  where
    (ReportPayment p _ _ _ _) = head ps
    hs = map (plain . text) ["Datum", "Job", "Stunden", "Gehalt"]
    rs = map buildPaymentRows ps
      ++ [[ plain $ text "Summe"
          , plain $ text ""
          , plain $ text h'
          , plain $ text s'
          ]]

buildPaymentRows :: ReportPayment -> [Blocks]
buildPaymentRows (ReportPayment _ d j h s)
  = map (plain . text) [d, j, h, s]
