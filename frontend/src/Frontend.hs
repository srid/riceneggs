{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend where

import Reflex.Dom

import Static

frontend :: Widget x ()
frontend = do
  el "h1" $ text "Nutrition calculator"
  return ()
