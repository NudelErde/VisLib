{-# LANGUAGE TemplateHaskell #-}
module Light where

import Linear.V3 (V3 (..))
import Control.Lens

data Light = Light {
  _lightPos :: V3 Float,
  _ambientStrengthLight :: Float,
  _diffuseStrengthLight :: Float,
  _specularStrengthLight :: Float,
  _shininessLight :: Int
}

$(makeLenses ''Light)