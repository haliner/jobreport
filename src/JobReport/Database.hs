module JobReport.Database where

import Control.Exception (bracket)
import Database.HDBC
import Database.HDBC.Sqlite3


withDatabase :: (Connection -> IO a) -> IO a
withDatabase = bracket openDatabase disconnect

openDatabase :: IO Connection
openDatabase = do
  c <- connectSqlite3 "jobreport.sqlite"
  mapM_ (runRaw c)
    [ "ROLLBACK"
    , "PRAGMA foreign_keys = ON"
    , "PRAGMA journal_mode = WAL"
    , "BEGIN"
    ]
  createSchema c
  return c

createSchema :: Connection -> IO ()
createSchema c = mapM_ (runRaw c) ss
  where
    ss = [ tableJob, tablePayment, tableWorking, viewJob
         , viewJobTotal, viewPayment, viewPaymentTotal
         ]

    tableJob = unlines
      [ "CREATE TABLE IF NOT EXISTS Job ("
      , "  id     INTEGER PRIMARY KEY,"
      , "  name   TEXT NOT NULL,"
      , "  salary INTEGER NOT NULL,"
      , ""
      , "  CONSTRAINT UniqueJob UNIQUE (name)"
      , ")"
      ]

    tablePayment = unlines
      [ "CREATE TABLE IF NOT EXISTS Payment ("
      , "  id    INTEGER PRIMARY KEY,"
      , "  job   INTEGER NOT NULL REFERENCES Job,"
      , "  year  INTEGER NOT NULL,"
      , "  month INTEGER NOT NULL,"
      , ""
      , "  CONSTRAINT UniquePayment UNIQUE (job, year, month)"
      , ")"
      ]

    tableWorking = unlines
      [ "CREATE TABLE IF NOT EXISTS Working ("
      , "  id          INTEGER PRIMARY KEY,"
      , "  payment     INTEGER NOT NULL REFERENCES Payment,"
      , "  year        INTEGER NOT NULL,"
      , "  month       INTEGER NOT NULL,"
      , "  day         INTEGER NOT NULL,"
      , "  startHour   INTEGER NOT NULL,"
      , "  startMinute INTEGER NOT NULL,"
      , "  endHour     INTEGER NOT NULL,"
      , "  endMinute   INTEGER NOT NULL,"
      , ""
      , "  CONSTRAINT UniqueWorking UNIQUE (payment, year, month , day,"
      , "                                   startHour, startMinute, endHour, endMinute)"
      , ")"
      ]

    viewJob = unlines
      [ "CREATE VIEW IF NOT EXISTS ReportJob AS"
      , "  SELECT     j.name AS job,"
      , "             p.year || '-' || PRINTF('%02i', p.month) AS month,"
      , "             ROUND(SUM(w.endHour - w.startHour + (w.endMinute - w.startMinute) / 60.0), 2) || ' h' AS hours,"
      , "             PRINTF('%.02f', SUM(w.endHour - w.startHour + (w.endMinute - w.startMinute) / 60.0) * j.salary / 100.0) || ' €' AS salary"
      , "  FROM       payment p"
      , "  INNER JOIN job j     ON j.id = p.job"
      , "  INNER JOIN working w ON p.id = w.payment"
      , "  GROUP BY   j.name, p.year, p.month"
      , "  ORDER BY   j.name  ASC,"
      , "             p.year  ASC,"
      , "             p.month ASC;"
      ]

    viewJobTotal = unlines
      [ "CREATE VIEW IF NOT EXISTS ReportJobTotal AS"
      , "  SELECT     j.name AS job,"
      , "             ROUND(SUM(w.endHour - w.startHour + (w.endMinute - w.startMinute) / 60.0), 2) || ' h' AS totalHours,"
      , "             PRINTF('%.02f', SUM(w.endHour - w.startHour + (w.endMinute - w.startMinute) / 60.0) * j.salary / 100.0) || ' €' AS totalSalary"
      , "  FROM       payment p"
      , "  INNER JOIN job j     ON j.id = p.job"
      , "  INNER JOIN working w ON p.id = w.payment"
      , "  GROUP BY   j.name"
      , "  ORDER BY   j.name  ASC,"
      , "             p.year  ASC,"
      , "             p.month ASC;"
      ]

    viewPayment = unlines
      [ "CREATE VIEW IF NOT EXISTS ReportPayment AS"
      , "  SELECT     p.year || '-' || PRINTF('%02i', p.month) AS payment,"
      , "             w.year || '-' || PRINTF('%02i', w.month) || '-' || PRINTF('%02i', w.day) AS day,"
      , "             j.name  AS job,"
      , "             ROUND(w.endHour - w.startHour + (w.endMinute - w.startMinute) / 60.0, 2) || ' h' AS hours,"
      , "             PRINTF('%.02f', (w.endHour - w.startHour + (w.endMinute - w.startMinute) / 60.0) * j.salary / 100.0) || ' €' AS salary"
      , "  FROM       payment p"
      , "  INNER JOIN job j     ON j.id = p.job"
      , "  INNER JOIN working w ON p.id = w.payment"
      , "  ORDER BY   p.year  ASC,"
      , "             p.month ASC,"
      , "             w.year  ASC,"
      , "             w.month ASC,"
      , "             w.day   ASC,"
      , "             j.name  ASC;"
      ]

    viewPaymentTotal = unlines
      [ "CREATE VIEW IF NOT EXISTS ReportPaymentTotal AS"
      , "  SELECT     p.year || '-' || PRINTF('%02i', p.month) AS payment,"
      , "             ROUND(SUM(w.endHour - w.startHour + (w.endMinute - w.startMinute) / 60.0), 2) || ' h' AS totalHours,"
      , "             PRINTF('%.02f', SUM((w.endHour - w.startHour + (w.endMinute - w.startMinute) / 60.0) * j.salary / 100.0)) || ' €' AS totalSalary"
      , "  FROM       payment p"
      , "  INNER JOIN job j     ON j.id = p.job"
      , "  INNER JOIN working w ON p.id = w.payment"
      , "  GROUP BY   p.year, p.month"
      , "  ORDER BY   p.year  ASC,"
      , "             p.month ASC,"
      , "             w.year  ASC,"
      , "             w.month ASC,"
      , "             w.day   ASC,"
      , "             j.name  ASC;"
      ]
