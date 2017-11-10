module B21.Redirects
( b21Redirects
, parseRedirects
, B21Redirects
, Redirect
, Redirects
, renderRedirects
) where

import Control.Exception hiding ( try )
import Data.Monoid ( (<>) )
import Data.Proxy
import qualified Data.Text as T
import Data.Void
import Network.URI
import Servant.API
import System.IO ( FilePath )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Text
import qualified Text.PrettyPrint.ANSI.Leijen as P

type B21Redirects
  = "update" :> Get '[PlainText] T.Text

b21Redirects :: Proxy B21Redirects
b21Redirects = Proxy

type Redirects = [Redirect]
data Redirect
  = Redirect
    { redirectFrom :: T.Text
    , redirectTo :: URI
    }

parseRedirects :: FilePath -> T.Text -> Either T.Text Redirects
parseRedirects p t = left (T.pack . show) $ parse parser p t where
  parser = (redirect `sepBy` eol) <* optional eol <*  eof
  redirect :: Parser Redirect
  redirect = do
    path <- T.concat <$> some pathComponent
    skipSome spaceChar
    string ("->" :: String)
    skipSome spaceChar
    dest <- some (alphaNumChar <|> symbolChar)
    uri <-
      maybe
      (fail $ "invalid URI: " <> dest)
      pure
      (parseURI dest)
    pure Redirect
      { redirectFrom = path
      , redirectTo = uri
      }

  -- | Parse a path component of the form:
  -- @slash (non-slash alphanumeric or symbol)+@.
  pathComponent :: Parser T.Text
  pathComponent = do
    c <- char '/'
    -- the path component body is any alphanumeric character or punctuation
    -- (like a dash) but *not* a slash!
    let s = "alphanumeric or symbol character (excluding slash)"
    let p = alphaNumChar <|> punctuationChar <* notFollowedBy (char '/') <?> s
    body <- some p
    pure (T.pack $ c : body)

-- | Renders a list of redirects as nginx @location@ directives creating
-- temporary redirects.
renderRedirects :: Redirects -> T.Text
renderRedirects = T.pack . ($ "") . P.displayS . P.renderCompact . go where
  go :: Redirects -> P.Doc
  go = P.cat . P.punctuate P.hardline . map redirect

  redirect :: Redirect -> P.Doc
  redirect Redirect{..} =
    P.text "location" P.<+> P.equals P.<+> text redirectFrom P.<+> bracing 4 (
      P.text "rewrite" P.<+> caret P.<+>
      P.text (show redirectTo) P.<+>
      P.text "redirect" P.<> P.semi
    )

  bracing n d = P.lbrace P.<$> P.indent n d P.<$> P.rbrace

  caret = P.text "^"

  text = P.text . T.unpack

left :: (e -> e') -> Either e a -> Either e' a
left f ei = case ei of
  Left e -> Left (f e)
  Right x -> Right x
