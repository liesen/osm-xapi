{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

-- | The @osm@ element of an Xapi file, as served by the Overpass XAPI
-- compatibility layer
module Data.Geo.OSM.Xapi.Xapi where

import Control.Comonad.Trans.Store
import Data.Geo.OSM.Children
import Data.Geo.OSM.Lens.ChildrenL
import Data.Geo.OSM.Lens.GeneratorL
import Data.Geo.OSM.Lens.VersionL
import Data.Geo.OSM.NodeWayRelation
import Data.Geo.OSM.Xapi.Lens.MetaL
import Data.Geo.OSM.Xapi.Lens.NoteL
import Data.Geo.OSM.Xapi.Meta
import Data.Geo.OSM.Xapi.Note
import Data.Lens.Common
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Core

data Xapi = Xapi String (Maybe String) Note Meta [NodeWayRelation]
  deriving Eq

instance XmlPickler Xapi where
  xpickle =
    xpElem "osm" (xpWrap (\(version, generator, note, meta, nwrs) -> Xapi version generator note meta nwrs, \(Xapi version generator note meta nwrs) -> (version, generator, note, meta, nwrs))
                         (xp5Tuple (xpTextAttr "version")
                                   (xpOption (xpTextAttr "generator"))
                                   xpickle
                                   xpickle
                                   xpickle))

instance Show Xapi where
  show = showPickled []

instance VersionL Xapi String where
  versionL =
    Lens $ \(Xapi version generator note meta nwrs) -> store (\version -> Xapi version generator note meta nwrs) version

instance GeneratorL Xapi where
  generatorL =
    Lens $ \(Xapi version generator note meta nwrs) -> store (\generator -> Xapi version generator note meta nwrs) generator

instance NoteL Xapi where
  noteL =
    Lens $ \(Xapi version generator note meta nwrs) -> store (\note -> Xapi version generator note meta nwrs) note

instance MetaL Xapi where
  metaL =
    Lens $ \(Xapi version generator note meta nwrs) -> store (\meta -> Xapi version generator note meta nwrs) meta

instance ChildrenL Xapi where
  childrenL =
    Lens $ \x@(Xapi version generator note meta nwrs) ->
      store (foldChildren (const x) (const x) (const x) (const x) (const x) (\nwrs -> Xapi version generator note meta nwrs))
            (osmNodeWayRelation nwrs)

readXapiFile :: FilePath -> IO [Xapi]
readXapiFile = runX . xunpickleDocument xpickle [withRemoveWS yes, withFileMimeType v_1]
