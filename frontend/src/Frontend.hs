{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend where

import Control.Applicative (liftA2)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Reflex.Dom

import Static

data Food = Food
  { _food_desc :: Text
  , _food_calories :: Double
  , _food_fat :: Double
  , _food_carbs :: Double
  , _food_protein :: Double
  }
  deriving (Eq, Show, Ord)

basmatiRice :: Food
basmatiRice = Food "Rice (per gram)" 3.555 0 0.8 0.0888

largeEgg :: Food
largeEgg = Food "One large egg" 70 5 1 6

getFoodInput :: MonadWidget t m => Food -> m (Dynamic t (Maybe Double))
getFoodInput food = do
  val <- el "div" $ do
    el "label" $ text $ _food_desc food
    showFood food
    value <$> textInput def
  return $ fmap (readMaybe . T.unpack) val

repeatFood :: Food -> Double -> Food
repeatFood food n = Food
  (_food_desc food <> " (" <> (T.pack $ show n) <> " times)")
  (n * _food_calories food )
  (n * _food_fat food )
  (n * _food_carbs food )
  (n * _food_protein food )

addFood :: Food -> Food -> Food
addFood f1 f2 = Food
  (_food_desc f1 <> " + " <> _food_desc f2)
  (_food_calories f1 + _food_calories f2)
  (_food_fat f1 + _food_fat f2)
  (_food_carbs f1 + _food_carbs f2)
  (_food_protein f1 + _food_protein f2)

showFood :: MonadWidget t m => Food -> m ()
showFood food = el "table" $ do
  el "tr" $ do
    el "td" $ text "Food"
    el "td" $ text $ _food_desc food
  el "tr" $ do
    el "td" $ text "Calories"
    el "td" $ text $ T.pack $ show $ _food_calories food
  el "tr" $ do
    el "td" $ text "Fat"
    el "td" $ text $ T.pack $ show $ _food_fat food
  el "tr" $ do
    el "td" $ text "Carbs"
    el "td" $ text $ T.pack $ show $ _food_carbs food
  el "tr" $ do
    el "td" $ text "Protein"
    el "td" $ text $ T.pack $ show $ _food_protein food

frontend :: Widget x ()
frontend = do
  el "h1" $ text "Nutrition calculator"
  eggs <- fmap (fmap $ repeatFood largeEgg) <$> getFoodInput largeEgg
  rice <- fmap (fmap $ repeatFood basmatiRice) <$> getFoodInput basmatiRice

  let total = zipDynWith (liftA2 addFood) eggs rice

  el "div" $ do
    el "b" $ text "Total calories:"
    dyn $ ffor total $ \case
      Nothing -> text "nothing available"
      Just f -> showFood f
  return ()
