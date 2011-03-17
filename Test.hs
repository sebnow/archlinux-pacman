module Main where
import Test.Framework (defaultMain)
import qualified Test.Distribution.Archlinux.Package.Parse as Parse

main = defaultMain $ concat [Parse.tests]

