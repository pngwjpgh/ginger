{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Ginger.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.FilePath

import System.IO.Error
import Control.Exception

import Text.Ginger.Parse


instance Lift SourcePos


gingerFile :: FilePath -> Q Exp
gingerFile baseTemplate = do
  let
    resolver :: IncludeResolver Q
    resolver fPath = do
        addDependentFile fPath
        qRunIO $
          catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
          (Just <$> readFile fPath)
          (\_ -> return Nothing)
  template <- either (qRunIO . throwIO) return =<< parseGingerFile resolver baseTemplate
  lift template
