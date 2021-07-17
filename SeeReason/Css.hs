{-# LANGUAGE AllowAmbiguousTypes, CPP, FunctionalDependencies, LambdaCase, OverloadedStrings, RecordWildCards, TemplateHaskell #-}

module SeeReason.Css
  ( CssClass(cssClass)
  , withHash
  , protectCSSIdentifier
#if SERVER
  , CssStyle(cssStyle)
  , byClass'
  , reifyCss
#endif
#if CLIENT
  , _class
#endif
  ) where

import Data.ByteString.Lazy as Lazy (fromStrict)
import Data.Char (isAlphaNum, isAscii, isSpace)
import Data.Digest.Pure.MD5 (md5)
import Data.Serialize (encode)
import Data.Text as Text ({-map,-} pack, singleton, Text)
import qualified Data.Text as Text (concatMap)
import GHC.Stack
import GHC.Stack.Types
#if SERVER
import Clay hiding (not, s)
import Data.Default (Default(def))
import Language.Haskell.TH (listE, litE, stringL, Type(AppT, ConT))
import Language.Haskell.TH.Syntax (Dec(..), Exp, mkName, Q, reifyInstances, Type(VarT))
import System.Directory ()
#endif
#if CLIENT
import Alderon.Alderon  (Attribute, classes_)
#endif

-- | Instances of 'CssClass' can be converted to css class names.
class CssClass a where
  cssClass :: HasCallStack => a -> Text

-- FIXME - protect leading digits and hyphens
protectCSSIdentifier :: Text -> Text
protectCSSIdentifier t = Text.concatMap protectCSSChar t
  where
    protectCSSChar :: Char -> Text
    protectCSSChar '-' = "-"
    protectCSSChar '_' = "_"
    protectCSSChar '.' = "_" -- the backslash escaping doesn't seem to work
    protectCSSChar c | isAlphaNum c = singleton c
    protectCSSChar c | not (isAscii c) = singleton c
    protectCSSChar '\n' = "_" -- hexEscape c
    protectCSSChar '\r' = "_" -- hexEscape c
    protectCSSChar c | isSpace c = "_" -- hexEscape c
    protectCSSChar c = "\\" <> singleton c

withHash :: HasCallStack => Text -> Text
withHash s = do
  s <> locHash callStack
  where
    -- This is the module we want, where withHash was called
    locHash (PushCallStack "withHash" SrcLoc{..} _more) =
      "_" <> pack (take 7 (show (md5 (fromStrict (encode srcLocModule)))))
       -- <> protectCSSIdentifier (pack (srcLocPackage <> "_" <> srcLocModule)) -- this actually works too
    -- Some other module - this probably won't happen, withHash should come first
    locHash (PushCallStack _fn _loc more) = locHash more
    locHash (FreezeCallStack more) = locHash more
    locHash EmptyCallStack = ""
#if 0
    protect :: Text -> Text
    protect = Text.map protectChar

    protectChar '-' = '-'
    protectChar '_' = '_'
    protectChar c | isAlphaNum c = c

    protectChar '.' = '_'
    protectChar c | isSpace c = '_'
    protectChar c = '_'
#endif

#if SERVER
-- | Instances of 'CssStyle' generate a Css value.
class (CssClass a, Default prefs) => CssStyle a prefs | a -> prefs where
  cssStyle :: prefs -> Css

byClass' :: CssClass a => a -> Refinement
byClass' = byClass . cssClass

-- | Collect all the in scope instances of CssStyle and turn them into
-- pairs that can be used to build scss files.  Result expression type
-- is [(FilePath, Css)].
reifyCss :: Q Exp
reifyCss = do
  insts <- reifyInstances ''CssStyle [VarT (mkName "a"), VarT (mkName "prefs")]
  listE (concatMap (\case InstanceD _ _cxt (AppT (AppT _cls typ@(ConT tname)) prefs) _decs ->
                            [ [|($(litE (stringL (show tname))), cssStyle @ $(pure typ) @ $(pure prefs) def)|] ]
                          _ -> []) insts)
#endif

#if CLIENT
class_ :: CssClass c => c -> Attribute
class_ c = classes_ [cssClass c]
#endif
