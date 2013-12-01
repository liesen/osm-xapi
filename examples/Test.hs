module Test where

import Control.Monad
import Data.Geo.OSM.Children
import Data.Geo.OSM.NodeWayRelation
import Data.Geo.OSM.Lens.ChildrenL
import Data.Geo.OSM.Lens.TagsL
import Data.Geo.OSM.Lens.KL
import Data.Geo.OSM.Lens.VL
import Data.Geo.OSM.Xapi
import Data.Lens.Common
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as C
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do [x] <- readXapiFile "sl_stop_id.osm"
          let children = x ^! childrenL
              skip = const (fail "no nodes found")
          mapM_ Text.putStrLn $ foldChildren skip skip skip skip skip (nub . sort . concatMap name) children

name nwr = do node <- maybeToList $ foldNodeWayRelation nwr Just skip skip
              tag <- node ^! tagsL
              guard (tag ^! kL == "name")
              let v = tag ^! vL
              return $ Text.decodeUtf8 (C.pack v)
  where
    skip = const Nothing
