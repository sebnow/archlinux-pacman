{-# LANGUAGE OverloadedStrings #-}
module Test.Distribution.Archlinux.Package.Parse where
import Control.Applicative ((<*>))
import Distribution.Archlinux.PackageInfo.Parse
import qualified Data.Attoparsec as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

instance Arbitrary Field where
    arbitrary = (Field . B8.pack) `fmap` ((listOf (oneof [choose ('>', '~'), choose ('!', '<')])) `suchThat` ((> 0) . length))

instance Arbitrary Value where
    arbitrary = (Value . B8.pack) `fmap` (listOf (choose (' ', '~')))

tests = [ testGroup "PKGINFO Parser"
            [ testProperty "field" prop_field_parsable
            , testProperty "value" prop_value_parsable
            , testProperty "definition" prop_definition_parsable
            ]
        ]

prop_field_parsable :: Field -> Bool
prop_field_parsable f = Just f == parseMaybe field (getField f)

prop_value_parsable :: Value -> Bool
prop_value_parsable v = Just v == parseMaybe value (getValue v `B8.append` "\n")

prop_definition_parsable :: Definition -> Bool
prop_definition_parsable d = Just d == parseMaybe definition (getDef d)

parseMaybe :: P.Parser a -> B.ByteString -> Maybe a
parseMaybe p s = case P.parseOnly p s of
    Left _  -> Nothing
    Right x -> Just x

getDef :: Definition -> B8.ByteString
getDef (f, v) = (getField f) `B8.append` " = " `B8.append` (getValue v) `B8.append` "\n"

