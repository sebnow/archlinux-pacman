{-# LANGUAGE OverloadedStrings #-}
module Distribution.Archlinux.PackageInfo.Parse
    ( comment
    , definition
    , Definition
    , field
    , Field(..)
    , pkginfo
    , value
    , Value(..)
    ) where
import Data.Attoparsec.Char8
import Control.Applicative ((<*), (<|>))
import qualified Data.Attoparsec.Char8 as A8
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B

newtype Field = Field { getField :: B.ByteString }
    deriving (Show, Read, Eq)
newtype Value = Value { getValue :: B.ByteString }
    deriving (Show, Read, Eq)
type Definition = (Field, Value)

comment :: Parser B.ByteString
comment = char '#' >> A.takeTill isEndOfLine <* endOfLine <?> "comment"

skipSpaces :: Parser ()
skipSpaces = skipWhile isSpace

field :: Parser Field
field = Field `fmap` (A.takeTill isHorizontalSpace <?> "field")

value :: Parser Value
value = Value `fmap` (A.takeTill isEndOfLine <* endOfLine <?> "value")

-- |Parses a key-value pair (e.g. \"foo = bar\").
definition :: Parser Definition
definition = do
    f <- field
    string " = "
    v <- value
    return (f, v) <?> "definition"

-- |Parses the PKGINFO format.
pkginfo :: Parser [Definition]
pkginfo = definition `sepBy` (skip comment <|> endOfLine)

-- |Ignore the return value of 'Parser' /p/.
skip :: Parser a -> Parser ()
skip p = p >> return ()

