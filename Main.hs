module Main where

import JobReport.Export
import JobReport.Insert
import Options.Applicative

data Command = Export | Insert InsertOptions deriving Show

commandParser :: Parser Command
commandParser = subparser $ exportCommand <> insertCommand
  where
    exportCommand = command "export" $ info (pure Export) exportInfo
    insertCommand = command "insert" $ info (Insert <$> insertParser) insertInfo

    exportInfo    = progDesc "Export data."
    insertInfo    = progDesc "Insert new data."

insertParser :: Parser InsertOptions
insertParser
    = InsertOptions
  <$> strOption (long "job" <> metavar "STRING")
  <*> option (long "payment-year" <> metavar "INT")
  <*> option (long "payment-month" <> metavar "INT")
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
runCommand Export        = exportAction
runCommand (Insert opts) = insertAction opts

main :: IO ()
main = execParser commandLine >>= runCommand

-- vim: set ts=4 sts=4 sw=4 et:
