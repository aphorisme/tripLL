{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies #-}

module TripLL (
  -- * Base Types for Accesing and Querying
  Triplestore (..),
  Triple (..),
  TriplePosition (..),
  flatten,
  QueryTriple (..),
  BatchAction (..)
) where

----- System: -----
import System.FilePath

class Triplestore h b | h -> b where
  -- | most simple actions, a triple store has to support, 'put' and 'del' of triples:
  put      :: Triple b -> h -> IO ()
  del      :: Triple b -> h -> IO ()
  -- | doing several simple actions in one go, one can use 'batch'.
  batch    :: [BatchAction b] -> h -> IO ()
  -- | querying triples is done by filling in the known fields with 'Just x'.
  query    :: QueryTriple b -> h -> IO [Triple b]
  -- bracket:
  withTrip :: FilePath -> (h -> IO a) -> IO a
  -- system:
  createAndOpen :: FilePath -> IO h
  open          :: FilePath -> IO h
  close         :: h ->  IO ()


-- | 'TriplePosition' gives a type for the fields within a 'Triple'.
data TriplePosition = Subject | Predicate | Object deriving (Eq, Show)

-- | A 'Triple' is just anything consisting of three ordered pieces.
data Triple a = Triple { subject :: a
                       , predicate :: a
                       , object :: a }

-- | 'QueryTriple' is a shorthand for 'Triple' where the argument is encapsulated within a 'Maybe'.
type QueryTriple a = Triple (Maybe a)


-- | With 'within' one can query accessor invariant the different elements of a triple. Meant to be used in infix: @Subject `within` (Triple "s" "p" "o") == "s"@.
within :: TriplePosition -> Triple a -> a
within Subject = subject
within Predicate = predicate
within Object = object

-- | 'into' can be used as a annotated setter.
into :: Triple a -> TriplePosition -> a -> Triple a
into t Subject x = t { subject = x }
into t Predicate x = t { predicate = x }
into t Object x = t { object = x }

-- | 'flatten' forgets about the description of the fields.
flatten :: Triple a -> (a, a, a)
flatten (Triple s p o) = (s,p,o)

-- | A 'BatchAction' is either a 'Put' or a 'Delete', i.e. either a `write given triple into database` or a `delete given triple from database`.
data BatchAction a = Put (Triple a) | Delete (Triple a)
