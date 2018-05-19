{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Applicative (liftA2)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core

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

ribeyeSteak :: Food
ribeyeSteak = Food "Ribeye steak 100g" 291 22 0 24

getFoodInput :: MonadWidget t m => Food -> m (Dynamic t (Maybe Double))
getFoodInput food = do
  val <- divClass "ui card" $ divClass "content" $ do
    divClass "header" $ text $ _food_desc food
    divClass "description" $ do
      showFood food
      value <$> textInput (def & textInputConfig_initialValue .~ "0")
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

instance Monoid Food where
  mempty = Food "Nothing" 0 0 0 0
  mappend = addFood

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

frontend :: JSM ()
frontend = mainWidgetWithHead' (const h, const b)
  where
    h = elAttr "link" ("rel" =: "stylesheet" <> "href" =: static @"semantic.min.css") blank
    b = divClass "ui container" $ do
      elClass "h1" "ui header"  $ text "Rice 'n eggs"
      divClass "ui content" $ divClass "ui segment" $ do
        eggs <- fmap (fmap $ repeatFood largeEgg) <$> getFoodInput largeEgg
        rice <- fmap (fmap $ repeatFood basmatiRice) <$> getFoodInput basmatiRice
        steak <- fmap (fmap $ repeatFood ribeyeSteak) <$> getFoodInput ribeyeSteak

        -- let foods = sequence [eggs, rice, steak]

        -- let total = zipDynWith (liftA2 addFood) eggs rice
        let total = fmap (fmap mconcat) $ fmap sequence $ sequence [eggs, rice, steak]

        el "div" $ do
          el "b" $ text "Total calories:"
          dyn $ ffor total $ \case
            Nothing -> text "nothing available"
            Just f -> showFood f
        return ()
