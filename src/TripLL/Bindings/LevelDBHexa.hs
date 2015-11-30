{-# LANGUAGE    TypeSynonymInstances
              , OverloadedStrings
              , MultiParamTypeClasses
              , FlexibleInstances #-}
{-|
Module      : TripLL.Bindings.LevelDBHexa
Description : An implementation for the tripLL interface.
Copyright   : (c) Philipp Pfeiffer, 2015
License     : MIT
Maintainer  : pfiff@hax-f.net
Stability   : experimental
Portability : POSIX

Gives an implementation for the tripLL interface based upon the `LevelDB` database.
-}
module TripLL.Bindings.LevelDBHexa (

  -- * Base Types
  LevelDBHexa (..),
  -- Re-Exports:
  module TripLL
) where

---- LevelDB: ----
import qualified Database.LevelDB.Base      as LDB
import qualified Database.LevelDB.Streaming as S
import qualified Database.LevelDB.Internal  as LDBI
---- Intern: ----
import TripLL
---- Data: ----
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.Serialize       as BIN
import Data.Either (rights)
---- Control: ----
import Control.Applicative
import Control.Monad



-------------
-- Base Types
-------------
-- | The main handle of this implementation, 'LevelDBHexa'.
type LevelDBHexa = LDB.DB
type ByTriple = Triple BS.ByteString
type QueryByTriple = QueryTriple BS.ByteString


-------------------
-- Encoding:
-------------------
instance BIN.Serialize ByTriple where
  put (Triple s p o) = BIN.put (s,p,o)
  get = fmap (\(s,p,o) -> Triple s p o) BIN.get


----------------
-- Intern Misc
----------------
tupleKey :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
tupleKey i v1 v2 v3 = i
           `BS.append` "::"
           `BS.append` v1
           `BS.append` "::"
           `BS.append` v2
           `BS.append` "::"
           `BS.append` v3


queryOpener :: Maybe BS.ByteString -> Maybe BS.ByteString -> Maybe BS.ByteString -> BS.ByteString
queryOpener Nothing Nothing Nothing    = "spo::"
queryOpener (Just s) Nothing Nothing   = "spo::" `BS.append` s
queryOpener Nothing (Just p) Nothing   = "pso::" `BS.append` p
queryOpener Nothing Nothing (Just o)   = "ops::" `BS.append` o
queryOpener (Just s) (Just p) Nothing  = "spo::" `BS.append` s `BS.append` "::" `BS.append` p
queryOpener (Just s) Nothing (Just o)  = "osp::" `BS.append` o `BS.append` "::" `BS.append` s
queryOpener Nothing (Just p) (Just o)  = "ops::" `BS.append` o `BS.append` "::" `BS.append` p
queryOpener (Just s) (Just p) (Just o) = "spo::" `BS.append` s `BS.append` "::" `BS.append` p `BS.append` "::" `BS.append` o


---------------------
-- Hexastore Instance
---------------------
instance Triplestore LevelDBHexa BS.ByteString where
  -------------------
  -- PUT into
  -------------------
  put (Triple s p o) db = let
                                encodedTrip = BIN.encode (Triple s p o)
                           in
                                LDB.write db LDB.defaultWriteOptions
                                  [ LDB.Put (tupleKey "spo" s p o) encodedTrip
                                  --, LDB.Put (tupleKey "sop" s o p) encodedTrip
                                  , LDB.Put (tupleKey "pso" p s o) encodedTrip
                                  --, LDB.Put (tupleKey "pos" p o s) encodedTrip
                                  , LDB.Put (tupleKey "osp" o s p) encodedTrip
                                  , LDB.Put (tupleKey "ops" o p s) encodedTrip ]

  -------------------
  -- DELETE
  -------------------
  del (Triple s p o) db = LDB.write db LDB.defaultWriteOptions
                                 [ LDB.Del (tupleKey "spo" s p o)
                                 --, LDB.Del (tupleKey "sop" s o p)
                                 , LDB.Del (tupleKey "pso" p s o)
                                 --, LDB.Del (tupleKey "pos" p o s)
                                 , LDB.Del (tupleKey "osp" o s p)
                                 , LDB.Del (tupleKey "ops" o p s) ]

  -----------------------
  -- BATCH operations
  -----------------------
  batch [] _ = return ()
  batch (Put t:xs) db = do { TripLL.put t db; batch xs db }
  batch (Delete t:xs) db = do { TripLL.del t db; batch xs db }

  ----------------------
  -- QUERY
  ----------------------
  query (Triple s p o) db  =
    let
        op = queryOpener s p o
    in
      LDB.withIter db LDB.defaultReadOptions $ \iter ->
        do
          entries <- S.toList $ S.entrySlice iter
                        (S.KeyRange
                            op
                            (\bs -> compare bs (op `BS.append` "\xff")))
                        S.Asc
          return $ rights (fmap (BIN.decode . snd) entries)

  --------------------------
  -- WITHHEXA bracket
  --------------------------
  withTrip path = LDB.withDB path (LDB.defaultOptions { LDB.createIfMissing = False, LDB.errorIfExists = False })

  -------------------------
  -- CREATE
  -------------------------
  createAndOpen path = LDB.open path (LDB.defaultOptions { LDB.createIfMissing = True, LDB.errorIfExists = True })
  open path = LDB.open path (LDB.defaultOptions { LDB.createIfMissing = False, LDB.errorIfExists = False})
  close = LDBI.unsafeClose
