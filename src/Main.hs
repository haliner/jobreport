module Main where

import Database.HDBC

import JobReport.Database
import JobReport.Export
import JobReport.Insert

import Options.Applicative

data Command = Init | Export | Insert InsertOptions deriving Show

commandParser :: Parser Command
commandParser = subparser $ initCommand <> exportCommand <> insertCommand
  where
    initCommand   = command "init"   $ info (pure Init) initInfo
    exportCommand = command "export" $ info (pure Export) exportInfo
    insertCommand = command "insert" $ info (Insert <$> insertParser) insertInfo

    initInfo      = progDesc "Initialize database."
    exportInfo    = progDesc "Export data."
    insertInfo    = progDesc "Insert new data."

insertParser :: Parser InsertOptions
insertParser = InsertOptions <$> workingParser

jobParser :: Parser Job
jobParser = Job <$> strOption (long "job" <> metavar "STRING")

paymentParser :: Parser Payment
paymentParser = Payment
    <$> jobParser
    <*> option (long "payment-year"  <> metavar "INT")
    <*> option (long "payment-month" <> metavar "INT")

workingParser :: Parser Working
workingParser = Working
    <$> paymentParser
    <*> option (long "year" <> metavar "INT")
    <*> option (long "month" <> metavar "INT")
    <*> option (long "day" <> metavar "INT")
    <*> option (long "start-hour" <> metavar "INT")
    <*> option (long "start-min" <> metavar "INT")
    <*> option (long "end-hour" <> metavar "INT")
    <*> option (long "end-min" <> metavar "INT")

commandLine :: ParserInfo Command
commandLine = info (helper <*> commandParser) commandInfo
  where
    commandInfo = header "jobsi - My personal working time calculator!"

runCommand :: Command -> IO ()
runCommand Init          = withDatabase commit
runCommand Export        = exportAction
runCommand (Insert opts) = insertAction opts

main :: IO ()
main = execParser commandLine >>= runCommand
