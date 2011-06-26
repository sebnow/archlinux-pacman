-- |
-- Module:      Distribution.Archlinux.Package
-- Stability:   Experimental
-- Portability: Portable
--
-- Provides an abstract datatype representing and reading information
-- about binary Archlinux packages.
--
-- @
--  import qualified Data.ByteString as B
--  import Distribution.Archlinux.Package
--  import Distribution.Package (packageId)
--  
--  main = do
--      tar <- Data.ByteString.readFile path
--      let (Just pkg) = readPackage tar
--      print $ packageId pkg
-- @
module Distribution.Archlinux.Package
    ( Package(..)
    , readPackage
    ) where
import Data.List (isPrefixOf)
import Distribution.Archlinux.PackageDescription
import qualified Distribution.Package as D
import qualified Codec.Archive.Tar as T
import qualified Codec.Archive.Tar.Util as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- |Represents a binary ALPM package, which contains package
-- metadata along with the files.
data Package = Package
    { pkgInfo :: PackageDescription
    , pkgFiles :: T.Entries
    }

instance D.Package Package where
    packageId = pkgId . pkgInfo

-- |Read a 'Package' from the raw tar contents.
readPackage :: B.ByteString -> Maybe Package
readPackage raw = do
    -- TODO: Support compressed archives
    pkginfo <- pkginfoEntry es
    pkg     <- parsePackageInfoMaybe =<< fileContent pkginfo
    return $ Package pkg es'
    where es = T.read $ BL.fromChunks [raw]
          -- Entries without metadata files
          es' = T.filterEntries (("." `isPrefixOf`) . T.entryPath) es

-- |Return the .PKGINFO 'T.Entry' if one is found.
pkginfoEntry :: T.Entries -> Maybe T.Entry
pkginfoEntry es = case T.filterEntries ((".PKGINFO" ==) . T.entryPath) es of
    T.Next e _ -> Just e
    otherwise  -> Nothing

-- |Same as 'parsePackageInfo' but use 'Maybe' instead of 'MonadError'.
parsePackageInfoMaybe :: B.ByteString -> Maybe PackageDescription
parsePackageInfoMaybe p = case pkg of
    Left _   -> Nothing
    Right p' -> Just p'
    where pkg = parsePackageInfo p :: Either String PackageDescription

-- |Return 'Just' the 'T.NormalFile' content or 'Nothing'
fileContent :: T.Entry -> Maybe B.ByteString
fileContent e = case T.entryContent e of
    (T.NormalFile c _) -> Just . B.concat . BL.toChunks $ c
    otherwise          -> Nothing

