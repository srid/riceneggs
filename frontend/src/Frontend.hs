{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Control.Applicative (liftA2)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Text.Read (readMaybe)

import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core

import Static

data Food = Food
  { _food_desc :: Text
  , _food_units :: Double
  , _food_unit :: Text
  , _food_calories :: Double
  , _food_fat :: Double
  , _food_carbs :: Double
  , _food_protein :: Double
  }
  deriving (Eq, Show, Ord)

basmatiRice :: Food
basmatiRice = Food "Rice" 10 "grams" 35.56 0 8 0.889

largeEgg :: Food
largeEgg = Food "Large egg" 1 "units" 70 5 1 6

ribeyeSteak :: Food
ribeyeSteak = Food "Ribeye steak" 50 "grams" 145 11 0 12

getFoodInput :: MonadWidget t m => Food -> m (Dynamic t (Maybe Double))
getFoodInput food = do
  divClass "ui segment" $ do
    rec _ <- divClass "ui header" $ el "h3" $ dynText $ fmap (maybe "" mkTitle) val
        val <- divClass "description" $ do
          fmap (fmap $ readMaybe . T.unpack) $ divClass "ui input" $ value <$> textInput
                  (def & textInputConfig_initialValue .~ "0"
                       & textInputConfig_inputType .~ "range"
                       & textInputConfig_attributes .~ constDyn ("min" =: "0" <> "max" =: "100"))
    return val
  where
    mkTitle units = T.unwords
      [ T.pack $ show $ (units * _food_units food)
      , _food_unit food
      , "of"
      , _food_desc food
      ]

repeatFood :: Food -> Double -> Food
repeatFood food n = Food
  (_food_desc food <> " (" <> (T.pack $ show n) <> " times)")
  (n * _food_units food )
  (_food_unit food)
  (n * _food_calories food )
  (n * _food_fat food )
  (n * _food_carbs food )
  (n * _food_protein food )

addFood :: Food -> Food -> Food
addFood f1 f2 = Food
  (_food_desc f1 <> " + " <> _food_desc f2)
  (_food_units f1 + _food_units f2) -- XXX: this makes no sense
  ("N/A")
  (_food_calories f1 + _food_calories f2)
  (_food_fat f1 + _food_fat f2)
  (_food_carbs f1 + _food_carbs f2)
  (_food_protein f1 + _food_protein f2)

instance Monoid Food where
  mempty = Food "Nothing" 1 "units" 0 0 0 0
  mappend = addFood

showFood :: MonadWidget t m => Food -> m ()
showFood food = elClass "table" "ui definition table" $ do
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
      el "p" $ text "Nutrition calculator"
      totalDyn <- divClass "ui raised segments" $ do
        eggs <- fmap (fmap $ repeatFood largeEgg) <$> getFoodInput largeEgg
        rice <- fmap (fmap $ repeatFood basmatiRice) <$> getFoodInput basmatiRice
        steak <- fmap (fmap $ repeatFood ribeyeSteak) <$> getFoodInput ribeyeSteak
        return $ fmap (fmap mconcat . sequence) $ sequence [eggs, rice, steak]

      dyn_ $ ffor totalDyn $ \total -> do
        showAttr "Calories" "purple" $ fmap _food_calories total
        showAttr "Fat" "yellow" $ fmap _food_fat total
        showAttr "Carbs" "red" $ fmap _food_carbs total
        showAttr "Protein" "brown" $ fmap _food_protein total

    showAttr name color attr = circularSegment color name $ do

      text $ maybe "N/A" T.pack $ fmap (printf "%0.2v" :: Double -> String) attr

circularSegment :: MonadWidget t m => Text -> Text -> m () -> m ()
circularSegment color h b = divClass ("ui " <> color <> " inverted circular segment") $ do
  elClass "h2" "ui inverted header" $ do
    text h
    divClass "sub header" $ el "p" $ b
