{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Distribution.Archlinux.PackageDescription
-- Portability: Portable
--
-- Provides an abstract datatype representing information about Archlinux
-- packages, along with functions to read PKGINFO files.
--
module Distribution.Archlinux.PackageDescription
    ( PackageDescription(..)
    , readPackageInfo
    , parsePackageInfo
    ) where
import Control.Monad.Error
import Control.Monad (liftM)
import Data.Char (toLower)
import Data.Version (parseVersion)
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Char8 as B
import qualified Distribution.Archlinux.PackageInfo.Parse as P
import qualified Distribution.License as D
import qualified Distribution.Package as D
import qualified Distribution.System as D
import qualified Distribution.Version as D
import qualified Text.ParserCombinators.ReadP as ReadP

data PackageDescription = PackageDescription
    { pkgId          :: D.PackageIdentifier -- ^Unique identifier
    , pkgDescription :: String              -- ^One-line summary
    , pkgDepends     :: [D.Dependency]
    , pkgOptDepends  :: [D.Dependency]
    , pkgPackager    :: String
    , pkgLicense     :: D.License
    , pkgArch        :: D.Arch
    , pkgUrl         :: Maybe String
    } deriving (Show, Eq)

instance D.Package PackageDescription where
    packageId = pkgId

-- |Read and parse a PKGINFO file at the given path.
readPackageInfo :: (MonadError e m, Error e) => FilePath -> IO (m PackageDescription)
readPackageInfo = liftM parsePackageInfo . B.readFile

-- |Parse a PKGINFO file .
parsePackageInfo :: (MonadError e m, Error e) => B.ByteString -> m PackageDescription
parsePackageInfo s = case A.parseOnly P.pkginfo s of
    Left err -> throwError (strMsg err)
    Right ds -> fromDefinitions ds

fromDefinitions :: (MonadError e m, Error e) =>  [P.Definition] -> m PackageDescription
fromDefinitions ds = do
    name <- liftM D.PackageName $ lookupField "pkgname" ds
    ver <- lookupField "pkgver" ds
    desc <- lookupField "pkgdesc" ds
    packager <- lookupField "packager" ds
    license <- liftM readLicense $ lookupField "license" ds
    arch <- liftM readArch $ lookupField "arch" ds
    let ver' = fst . last . ReadP.readP_to_S parseVersion $ ver
        depends = mkDepends $ lookupFields "depend" ds
        optdepends = mkDepends $ lookupFields "optdepend" ds
        mkDepends deps = map (\x -> D.Dependency (D.PackageName x) D.anyVersion) deps
    return PackageDescription
        { pkgId          = D.PackageIdentifier name ver'
        , pkgDescription = desc
        , pkgPackager    = packager
        , pkgLicense     = license
        , pkgDepends     = depends
        , pkgOptDepends  = optdepends
        , pkgArch        = arch
        , pkgUrl         = case lookupField "url" ds :: Either String String of
              (Left  _) -> Nothing
              (Right u) -> Just u
        }

-- Utility {{{

-- | Same as 'lookupField', but returning a list of all matches rather
-- than just the first value.
--
-- @
--  xs = zipWith (,) (cycle ["a","b"]) [1,2..5]
--  lookupField "a" xs -- Just 1
--  lookupFields "a" xs -- [1,3,5]
-- @
lookupFields :: B.ByteString -> [P.Definition] -> [String]
lookupFields x = map (B.unpack . snd) . filter ((x ==) . fst)

-- |Same behaviour as 'lookup', but using 'MonadError' instead. If an
-- element is found the value is 'unpack'ed to a 'String'.
lookupField :: (MonadError e m, Error e) => B.ByteString -> [P.Definition] -> m String
lookupField s ds = case lookup s ds of
    Just x -> return $ B.unpack x
    Nothing -> throwError . strMsg $ (B.unpack s) ++ " missing"

-- |Parse architecture. If the architecture is not recognised, it will
-- be used verbatim in 'OtherArch'
readArch :: String -> D.Arch
readArch x = case lowercase x of
    "i386"   -> D.I386
    "x86_64" -> D.X86_64
    "arm"    -> D.Arm
    "sparc"  -> D.Sparc
    "ppc"    -> D.PPC
    "ppc64"  -> D.PPC64
    _        -> D.OtherArch x

-- |Convert a string to lowercase.
lowercase = map toLower

-- |Parse a license. If the license is not recognised, the string value
-- will be used verbatim in 'UnknownLicense'
readLicense :: String -> D.License
readLicense "GPL" = D.GPL Nothing
readLicense "BSD" = D.BSD3
readLicense "MIT" = D.MIT
readLicense l = D.UnknownLicense l

-- }}}

