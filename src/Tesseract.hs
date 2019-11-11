module Tesseract where

import           AppM

class Tesseract m where
    ocrFile :: FilePath -> m FilePath

instance Tesseract AppM where
  ocrFile = pure
