# tripLL

## Introduction

`tripLL` is a very simple triple store, written in Haskell. It provides a basic interface to write, delete and query triples -- in fact, that's it. On top, there are some bindings to realizations of triple stores, such that this tiny library can be used out of box.

Right now it is thought that exactly one binding is imported within a project and only this binding (since it re-exports the interface).

## A simple example

```{.Haskell}
{-# LANGUAGE OverloadedStrings #-}

{- This examples uses the leveldb realization. This means that triples become triples of strict bytestrings and the leveldb lib has to be installed. -}

import TripLL.Bindings.LevelDBHexa

main = do -- open the database:
          handle <- createAndOpen "data/store.tripdb" :: IO LevelDBHexa
          -- batch something into it:
          batch [ Put (Triple "Frenzy" "likes" "Jealousy")
                , Put (Triple "Hate" "loves" "Hatred")
                , Put (Triple "Frenzy" "likes" "Hate")]
                handle
          -- querying the database:
          res <- query (Triple (Just "Frenzy") Nothing Nothing) handle
          -- now, `res :: [Triple Strict.ByteString]`, do something with it...
          -- ...
          -- and clean up:
          close handle
```

## Contributions

The LevelDB Binding is based upon (http://nodejsconfit.levelgraph.io/), which is based upon the idea to build up a hexastore. Freely interpreted.
