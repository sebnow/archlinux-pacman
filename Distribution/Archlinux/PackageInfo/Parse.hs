{-# LANGUAGE OverloadedStrings #-}
module Distribution.Archlinux.PackageInfo.Parse
    ( comment
    , definition
    , Definition
    , field
    , pkginfo
    , value
    ) where
import Data.Attoparsec.Char8
import Control.Applicative ((<*), (<|>))
import qualified Data.Attoparsec.Char8 as A8
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B

type Definition = (B.ByteString, B.ByteString)

comment :: Parser B.ByteString
comment = char '#' >> A.takeTill isEndOfLine <* endOfLine <?> "comment"

skipSpaces :: Parser ()
skipSpaces = skipWhile isSpace

field :: Parser B.ByteString
field = A.takeTill isHorizontalSpace <?> "field"

value :: Parser B.ByteString
value = A.takeTill isEndOfLine <* endOfLine <?> "value"

-- |Parses a key-value pair (e.g. \"foo = bar\").
definition :: Parser Definition
definition = do
    f <- field
    string " = "
    v <- value
    return (f, v) <?> "definition"

-- |Parses the PKGINFO format.
pkginfo :: Parser [Definition]
pkginfo = skipMany comment >> many definition

-- |Ignore the return value of 'Parser' /p/.
skip :: Parser a -> Parser ()
skip p = p >> return ()

