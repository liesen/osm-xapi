-- | The @note@ element of a OSM file.
module Data.Geo.OSM.Xapi.Note
(
  Note
) where

import Text.XML.HXT.Arrow.Pickle

data Note = Note String
  deriving Eq

instance XmlPickler Note where
  xpickle = xpElem "note" (xpWrap (Note, \(Note n) -> n) xpText0)
