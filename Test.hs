module Main where
import Test.Framework (defaultMain)
import qualified Test.Distribution.Archlinux.Package.Parse as Parse
import qualified Test.Distribution.Archlinux.PackageDescription as PackageDescription

main = defaultMain $ concat
    [ Parse.tests
    , PackageDescription.tests
    ]

