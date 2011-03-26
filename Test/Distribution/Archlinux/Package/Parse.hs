{-# LANGUAGE OverloadedStrings #-}
module Test.Distribution.Archlinux.Package.Parse where
import Data.Maybe (isJust)
import Distribution.Archlinux.PackageInfo.Parse hiding (Definition)
import qualified Data.Attoparsec as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

newtype Field = Field { getField :: B8.ByteString }
    deriving (Show, Read, Eq)

instance Arbitrary Field where
    arbitrary = fmap (Field . B8.pack) gen
        where gen   = listOf (oneof [choose ('>', '~'), choose ('$', '<')]) `suchThat` req
              req s = length s > 0 && head s /= '#'

newtype Value = Value { getValue :: B8.ByteString }
    deriving (Show, Read, Eq)

instance Arbitrary Value where
    arbitrary = (Value . B8.pack) `fmap` (listOf (choose (' ', '~')))

newtype Comment = Comment { getComment :: B8.ByteString }
    deriving (Show, Eq);

instance Arbitrary Comment where
    arbitrary = (Comment . B8.pack . wrap "#" "\n") `fmap` (listOf (choose (' ', '~')))

type Definition = (Field, Value)

tests = [ testGroup "PKGINFO Parser"
            [ testProperty "field" prop_field_parsable
            , testProperty "value" prop_value_parsable
            , testProperty "comment" prop_comment_parsable
            , testProperty "definition" prop_definition_parsable
            , testProperty "definitions" prop_definitions_parsable
            ]
        ]

prop_field_parsable :: Field -> Bool
prop_field_parsable f = Just (getField f) == parseMaybe field (getField f)

prop_value_parsable :: Value -> Bool
prop_value_parsable v = Just (getValue v) == parseMaybe value (getValue v `B8.append` "\n")

prop_comment_parsable :: Comment -> Bool
prop_comment_parsable = isJust . parseMaybe comment . getComment

prop_definition_parsable :: Definition -> Bool
prop_definition_parsable d@(f, v) = Just d' == parseMaybe definition (getDef d)
    where d' = (getField f, getValue v)

prop_definitions_parsable :: [Definition] -> Bool
prop_definitions_parsable ds = Just ds'' == parseMaybe pkginfo ds'
    where ds'  = B.concat . map getDef $ ds
          ds'' = map (\(f, v) -> (getField f, getValue v)) ds

parseMaybe :: P.Parser a -> B.ByteString -> Maybe a
parseMaybe p s = case P.parseOnly p s of
    Left _  -> Nothing
    Right x -> Just x

getDef :: Definition -> B8.ByteString
getDef (f, v) = (getField f) `B8.append` " = " `B8.append` (getValue v) `B8.append` "\n"

wrap :: String -> String -> String -> String
wrap l r s = l ++ s ++ r

