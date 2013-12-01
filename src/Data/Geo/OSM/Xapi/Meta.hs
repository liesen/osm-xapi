-- | The @meta@ element of a OSM file.
module Data.Geo.OSM.Xapi.Meta
(
  Meta
) where

import Text.XML.HXT.Arrow.Pickle

data Meta = Meta String
  deriving (Eq, Show)

instance XmlPickler Meta where
  xpickle = xpElem "meta" (xpWrap (Meta, \(Meta osmBase') -> osmBase')
                          (xpAttr "osm_base" xpText))
