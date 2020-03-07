{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Skylighting.Format.HTML.Lucid
  ( formatHtmlInline
  , formatHtmlBlock
  ) where

import           Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.Text as T
import           Lucid
import           Skylighting.Types

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

-- | Format tokens using HTML spans inside @code@ tags. For example,
-- A @KeywordTok@ is rendered as a span with class @kw@.
-- Short class names correspond to 'TokenType's as follows:
-- 'KeywordTok'        = @kw@,
-- 'DataTypeTok'       = @dt@,
-- 'DecValTok'         = @dv@,
-- 'BaseNTok'          = @bn@,
-- 'FloatTok'          = @fl@,
-- 'CharTok'           = @ch@,
-- 'StringTok'         = @st@,
-- 'CommentTok'        = @co@,
-- 'OtherTok'          = @ot@,
-- 'AlertTok'          = @al@,
-- 'FunctionTok'       = @fu@,
-- 'RegionMarkerTok'   = @re@,
-- 'ErrorTok'          = @er@,
-- 'ConstantTok'       = @cn@,
-- 'SpecialCharTok'    = @sc@,
-- 'VerbatimStringTok' = @vs@,
-- 'SpecialStringTok'  = @ss@,
-- 'ImportTok'         = @im@,
-- 'DocumentationTok'  = @do@,
-- 'AnnotationTok'     = @an@,
-- 'CommentVarTok'     = @cv@,
-- 'VariableTok'       = @va@,
-- 'ControlFlowTok'    = @cf@,
-- 'OperatorTok'       = @op@,
-- 'BuiltInTok'        = @bu@,
-- 'ExtensionTok'      = @ex@,
-- 'PreprocessorTok'   = @pp@,
-- 'AttributeTok'      = @at@,
-- 'InformationTok'    = @in@,
-- 'WarningTok'        = @wa@.
-- A 'NormalTok' is not marked up at all.
formatHtmlInline :: FormatOptions -> [SourceLine] -> Html ()
formatHtmlInline opts =
  -- wrapCode opts . mconcat . L.intersperse (toHtml "\n") . map (traverse_ (tokenToHtml opts))
  wrapCode opts . mconcat . map (traverse_ (tokenToHtml opts))

-- | Format tokens as an HTML @pre@ block. Each line is wrapped in an a element
-- with the class ‘source-line’. If line numbering is selected, the surrounding
-- pre is given the class ‘numberSource’, and the resulting html will display
-- line numbers thanks to the included CSS. See the documentation for
-- 'formatHtmlInline' for information about how tokens are encoded.
formatHtmlBlock :: FormatOptions -> [SourceLine] -> Html ()
formatHtmlBlock opts ls =
  div_ [class_ "sourceCode"]
  $ pre_ [classes_ classes]
    $ wrapCode opts
    $ mconcat
    $ L.intersperse "\n"
    $ zipWith (sourceLineToHtml opts) [startNum..] ls
  where
    classes :: [T.Text]
    classes = "sourceCode"
      :  ["numberSource" | numberLines opts]
      ++ [x | x <- containerClasses opts, x /= "sourceCode"]

    startNum :: LineNo
    startNum = LineNo $ startNumber opts

wrapCode :: FormatOptions -> Html () -> Html ()
wrapCode opts h =
  code_ [classes_ $ "sourceCode" : codeClasses opts] h
                         -- !? (startZero /= 0, A.style (toValue counterOverride))
                         -- $ h
  -- where
  --   counterOverride :: String
  --   counterOverride = "counter-reset: source-line " <> show startZero <> ";"

  --   startZero :: Int
    -- startZero = startNumber opts - 1

-- | Each line of source is wrapped in an (inline-block) anchor that makes
-- subsequent per-line processing (e.g. adding line numnbers) possible.
sourceLineToHtml :: FormatOptions -> LineNo -> SourceLine -> Html ()
sourceLineToHtml opts lno cont = span_ [id_ prefixedLineNo] $ do
  a_ [href_ lineRef] ""
  traverse_ (tokenToHtml opts) cont
  where
    lineRef :: T.Text
    lineRef = T.cons '#' prefixedLineNo

    prefixedLineNo :: T.Text
    prefixedLineNo = lineIdPrefix opts <> T.pack (show $ lineNo lno)

tokenToHtml :: FormatOptions -> Token -> Html ()
tokenToHtml _ (NormalTok, txt) = toHtml txt
tokenToHtml opts (toktype, txt)
  | titleAttributes opts = sp -- ! A.title (toValue $ show toktype)
  | otherwise = sp
  where
    sp :: Html ()
    sp = span_ [class_ $ short toktype] $ toHtml txt

short :: TokenType -> T.Text
short KeywordTok        = "kw"
short DataTypeTok       = "dt"
short DecValTok         = "dv"
short BaseNTok          = "bn"
short FloatTok          = "fl"
short CharTok           = "ch"
short StringTok         = "st"
short CommentTok        = "co"
short OtherTok          = "ot"
short AlertTok          = "al"
short FunctionTok       = "fu"
short RegionMarkerTok   = "re"
short ErrorTok          = "er"
short ConstantTok       = "cn"
short SpecialCharTok    = "sc"
short VerbatimStringTok = "vs"
short SpecialStringTok  = "ss"
short ImportTok         = "im"
short DocumentationTok  = "do"
short AnnotationTok     = "an"
short CommentVarTok     = "cv"
short VariableTok       = "va"
short ControlFlowTok    = "cf"
short OperatorTok       = "op"
short BuiltInTok        = "bu"
short ExtensionTok      = "ex"
short PreprocessorTok   = "pp"
short AttributeTok      = "at"
short InformationTok    = "in"
short WarningTok        = "wa"
short NormalTok         = ""
