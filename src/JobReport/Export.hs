module JobReport.Export where

import Control.Monad
import Control.Monad.Trans.Writer.Strict

import Data.Functor
import Data.Maybe
import Data.Monoid

import Database.Esqueleto

import JobReport.Database

import Text.Pandoc hiding (Writer)
import Text.Pandoc.Builder
import Text.Printf

data ReportPayment = ReportPayment Job [(Payment, Int)]
data ReportWorking = ReportWorking (Int, Int) [(Job, Working)]

queryPayments :: SqlPersistM [ReportPayment]
queryPayments = do
    js <- sqlJobs
    let js' = map entityVal js
    ps <- mapM (\x -> mapMaybe conv <$> sqlPaymentsForJob x) js
    return $ zipWith ReportPayment js' ps
  where
    conv (p, Value (Just h)) = Just (entityVal p, h)
    conv _ = Nothing

queryWorkings :: SqlPersistM [ReportWorking]
queryWorkings = do
    ps <- map conv1 <$> sqlPayments
    ws <- mapM (\x -> map conv2 <$> sqlWorkingsForPayment x) ps
    return $ zipWith ReportWorking ps ws
  where
    conv1 (Value y, Value m) = (y, m)
    conv2 (j, w) = (entityVal j, entityVal w)

exportAction :: IO ()
exportAction = do
    conn <- openDatabase
    (ps, ms) <- run conn $ liftM2 (,) queryPayments queryWorkings
    putStrLn . writeMarkdown def $ buildDocument ps ms

sqlJobs :: SqlPersistM [Entity Job]
sqlJobs = select $
    from $ \job -> do
    orderBy [asc (job ^. JobName)]
    return $ job

sqlPayments :: SqlPersistM [(Value Int, Value Int)]
sqlPayments = selectDistinct $
    from $ \pay -> do
    orderBy [asc (pay ^. PaymentYear), asc (pay ^.PaymentMonth)]
    return (pay ^. PaymentYear, pay ^. PaymentMonth)

sqlPaymentsForJob :: Entity Job -> SqlPersistM [(Entity Payment, Value (Maybe Int))]
sqlPaymentsForJob j = select $
    from $ \(wor `InnerJoin` pay) -> do
    on (pay ^. PaymentId ==. wor ^. WorkingPayment)
    where_ (pay ^. PaymentJob ==. (val $ entityKey j))
    groupBy $ pay ^. PaymentId
    orderBy [ asc (pay ^. PaymentYear)
            , asc (pay ^. PaymentMonth) ]
    let hs = wor ^. WorkingEndHour -. wor ^. WorkingStartHour
    let ms = wor ^. WorkingEndMinute -. wor ^. WorkingStartMinute
    let duration = sum_ (hs *. val 60 -. ms)
    return (pay, duration)

sqlWorkingsForPayment :: (Int, Int) -> SqlPersistM [(Entity Job, Entity Working)]
sqlWorkingsForPayment (y, m) = select $
    from $ \(wor `InnerJoin` pay `InnerJoin` job) -> do
    on (job ^. JobId ==. pay ^. PaymentJob)
    on (pay ^. PaymentId ==. wor ^. WorkingPayment)
    where_ (pay ^. PaymentYear ==. val y)
    where_ (pay ^. PaymentMonth ==. val m)
    orderBy [ asc (wor ^. WorkingYear)
            , asc (wor ^. WorkingMonth)
            , asc (wor ^. WorkingDay)
            ]
    return (job, wor)

buildDocument :: [ReportPayment] -> [ReportWorking] -> Pandoc
buildDocument ps ws = setTitle t $ doc bs
  where
    t = text "Nebenjob-Übersicht"
    bs = header 1 t
      <> header 2 (text "Abrechnungen")
      <> mconcat (map buildPayment ps)
      <> header 2 (text "Arbeitsstunden")
      <> mconcat (map buildWorking ws)

buildPayment :: ReportPayment -> Blocks
buildPayment (ReportPayment j ps) = header 3 (text $ jobName j)
                                 <> simpleTable hs rs'
  where
    hs = [ plain $ text "Abrechnung"
         , plain $ text "Stunden"
         , plain $ text "Gehalt"
         ]
    (rs, Sum s) = runWriter $ mapM (buildPaymentRows j) ps
    lr = map (plain . text)
        [ "Summe"
        , printf "%.1f h" (fromIntegral s / 60 :: Double)
        , printf "%.2f €" (fromIntegral (jobSalary j * s) / 6000 :: Double)
        ]
    rs' = rs ++ [lr]

buildPaymentRows :: Job -> (Payment, Int) -> Writer (Sum Int) [Blocks]
buildPaymentRows j (p, h) = do
    tell $ Sum h
    return $ map (plain . text) [col1, col2, col3]
  where
    col1 = printf "%04d-%02d" (paymentYear p) (paymentMonth p)
    col2 = printf "%.1f h" (fromIntegral h / 60 :: Double)
    col3 = printf "%.2f €" (fromIntegral (jobSalary j * h) / 6000 :: Double)

buildWorking :: ReportWorking -> Blocks
buildWorking (ReportWorking (y, m) js) = header 3 t
                                      <> simpleTable hs rs'
  where
    t = text $ printf "%04d-%02d" y m
    hs = [ plain $ text "Job"
         , plain $ text "Datum"
         , plain $ text "Stunden"
         , plain $ text "Gehalt"
         ]
    (rs, (Sum d, Sum s)) = runWriter $ mapM buildWorkingRows js
    lr = map (plain . text)
        [ "Summe", ""
        , printf "%.1f h" d
        , printf "%.2f €" s
        ]
    rs' = rs ++ [lr]

buildWorkingRows :: (Job, Working) -> Writer (Sum Double, Sum Double) [Blocks]
buildWorkingRows (j, w) = do
    tell (Sum duration, Sum salary)
    return $ map (plain . text) [col1, col2, col3, col4]
  where
    start = fromIntegral (workingStartHour w * 60 + workingStartMinute w)
    end = fromIntegral (workingEndHour w * 60 + workingEndMinute w)
    duration = (end - start) / 60 :: Double
    salary = duration * fromIntegral (jobSalary j) / 100
    col1 = jobName j
    col2 = printf "%04d-%02d-%02d" (workingYear w) (workingMonth w) (workingDay w)
    col3 = printf "%.1f h" duration
    col4 = printf "%.2f €" salary

-- vim: set ts=4 sts=4 sw=4 et:
