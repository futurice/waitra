{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
------------------------------------------------------------
-- |
-- Module      :  Network.Waitra.Embedded
-- Copyright   :  (c) 2015 Futurice
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability   :  experimental
--
-- @Network.Waitra.Embedded@ is a missing part from @wai-app-static@.
----------------------------------------------------------------------------
module Network.Waitra.Embedded (mkRecursiveEmbedded) where

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Language.Haskell.TH
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath ((</>), makeRelative)

getRecursiveContents :: FilePath -> IO [(FilePath, BL.ByteString)]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = Prelude.filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else do contents <- BL.readFile path
              return [(path, contents)]
  return (concat paths)

makeAllRelative :: FilePath -> [(FilePath, a)] -> [(FilePath, a)]
makeAllRelative topdir = map (first (("/" ++) . makeRelative topdir))

bytestringE :: B.ByteString -> Q Exp
bytestringE b = [| B8.pack $s |]
  where s = litE $ stringL $ B8.unpack b

makeEmbeddedEntry :: (FilePath, BL.ByteString) -> Q Exp
makeEmbeddedEntry (path, bs) = [| (path, $(bytestringE $ BL.toStrict bs)) |]

-- | Create a @[(FilePath, ByteString)]@ list, recursively traversing given directory path.
--
-- > staticApp $ embeddedSettings $(mkRecursiveEmbedded "static")
-- > -- is an in-memory equivalent of
-- > staticApp $ defaultFileServerSettings "static" 
mkRecursiveEmbedded :: FilePath -> Q Exp
mkRecursiveEmbedded topdir = do
  pairs <- runIO $ makeAllRelative topdir <$> getRecursiveContents topdir
  listE $ map makeEmbeddedEntry pairs
