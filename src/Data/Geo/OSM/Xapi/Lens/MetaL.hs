module Data.Geo.OSM.Xapi.Lens.MetaL where

import Data.Lens.Common
import Data.Geo.OSM.Xapi.Meta

class MetaL a where
  metaL :: Lens a Meta
