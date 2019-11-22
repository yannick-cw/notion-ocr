{-# LANGUAGE OverloadedStrings #-}

module CliParser
  ( Args(..)
  , parseArgs
  , HasNotion(..)
  , HasTempDir(..)
  , CliOpts(..)
  )
where

import           Options.Applicative
import           Data.Text                      ( Text )

data CliOpts = CliVersion | CliArgs  Text ( Maybe Int ) Bool

data Args = Args { tempPath :: FilePath, notionToken :: Text,  schedule :: Maybe Int, verbose :: Bool }

class HasTempDir a where
  path :: a  -> FilePath

instance HasTempDir Args where
  path = tempPath

class HasNotion r where
  notionConf :: r -> Text

instance HasNotion Args where
  notionConf = notionToken

parseArgs :: IO CliOpts
parseArgs = execParser opts
 where
  opts = info
    (argsParser <**> helper)
    (  fullDesc
    <> progDesc
         "Add ocr (optical character recognition) to your Notion images"
    <> header "notion-ocr - enrich images with the text in the image"
    )


argsParser :: Parser CliOpts
argsParser =
  (   CliArgs
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
    <*> switch (long "verbose" <> short 'v' <> help "Log extra information")
    )
    <|> (CliVersion <$ switch (long "version" <> help "Show the version number")
        )

