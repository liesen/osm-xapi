module Data.Geo.OSM.Xapi.Lens.NoteL where

import Data.Lens.Common
import Data.Geo.OSM.Xapi.Note

class NoteL a where
  noteL :: Lens a Note
