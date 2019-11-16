{-# LANGUAGE OverloadedStrings #-}

module CliParser
  ( Args(..)
  , parseArgs
  , HasNotion(..)
  , HasTempDir(..)
  )
where

import           Options.Applicative
import           Data.Text                      ( Text )

data Args = Args { tempPath :: FilePath, notionToken :: Text,  schedule :: Maybe Int }
class HasTempDir a where
  path :: a  -> FilePath

instance HasTempDir Args where
  path = tempPath

class HasNotion r where
  notionConf :: r -> Text

instance HasNotion Args where
  notionConf = notionToken

parseArgs :: FilePath -> IO Args
parseArgs tmpDir = execParser opts
 where
  opts = info
    (argsParser tmpDir <**> helper)
    (  fullDesc
    <> progDesc
         "Add ocr (optical character recognition) to your Notion images"
    <> header "notion-ocr - enrich images with the text in the image"
    )


argsParser :: FilePath -> Parser Args
argsParser tmpDir =
  Args tmpDir
    <$> strOption
          (  long "token"
          <> short 't'
          <> help
               "Your notion token, found in the token_v2 cookie when you open notion in the browser"
          )
    <*> optional
          (option
            auto
            (  long "schedule"
            <> short 's'
            <> help "Can be used to schedule a run every x minutes."
            <> metavar "Minutes"
            )
          )

