{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}

module MoreText where

import Test.Inspection

import MoreTextTH

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.ByteString (ByteString)
import Data.List
import Control.Applicative

definePipelines
defineTests
