{-# LANGUAGE TemplateHaskell #-}
module World where

import Object
import Light
import Control.Lens

data World = World {
  _objects :: [ObjectContainer],
  _lights :: [Light]
}

$(makeLenses ''World)