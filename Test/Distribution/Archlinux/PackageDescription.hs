module Test.Distribution.Archlinux.PackageDescription where
import Distribution.Archlinux.PackageDescription
import qualified Data.ByteString.Char8 as B8
import qualified Distribution.Package as D
import qualified Distribution.Version as D
import qualified Distribution.License as D
import qualified Distribution.System as D
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

tests = [ testGroup "PKGINFO Parser"
            [ testCase "sample" test_sample
            ]
        ]

test_sample :: Assertion
test_sample = do
    pkg <- readPackageInfo "fixture/PKGINFO/sample_01.txt" :: IO (Either String PackageDescription)
    case pkg of
        Left s     -> assertFailure ("failed to parse: " ++ s)
        Right pkg' -> exp @=? pkg'
    where exp = PackageDescription {
          pkgId          = D.PackageIdentifier (D.PackageName "pcre") (D.Version [8, 12] ["1"])
        , pkgDescription = "A library that implements Perl 5-style regular expressions"
        , pkgDepends     = [D.Dependency (D.PackageName "gcc-libs") D.anyVersion]
        , pkgOptDepends  = []
        , pkgPackager    = "Allan McRae <allan@archlinux.org>"
        , pkgLicense     = D.BSD3
        , pkgArch        = D.X86_64
        , pkgUrl         = Just "http://www.pcre.org/"
        }

