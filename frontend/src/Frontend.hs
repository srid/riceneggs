{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend where

import Control.Applicative (liftA2)
import Data.Default (def)
import Data.Profunctor (lmap)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Reflex.Dom

import Static

frontend :: Widget x ()
frontend = do
  el "h1" $ text "Nutrition calculator"
  eggs <- el "div" $ do
    el "label" $ text "Eggs"
    value <$> textInput def
  rice <- el "div" $ do
    el "label" $ text "Rice (grams)"
    value <$> textInput def

  let eggs'  = fmap (readMaybe . T.unpack) eggs
  let rice'  = fmap (readMaybe . T.unpack)  rice
  let total = fmap (T.pack . show) $ zipDynWith (liftA2 calc) eggs' rice'

  el "div" $ do
    el "b" $ text "Total calories:"
    dynText $ total
  return ()

calc :: Fractional a => a -> a -> a
calc eggs riceGrams = eggs * 78 + riceGrams * 3.65
