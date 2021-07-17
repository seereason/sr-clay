{-# LANGUAGE AllowAmbiguousTypes, CPP, FunctionalDependencies, LambdaCase, OverloadedStrings, RecordWildCards, TemplateHaskell #-}

module SeeReason.Css
  ( CssClass(cssClass)
  , withHash
#if SERVER
  , CssStyle(cssStyle)
  , byClass'
  , reifyCss
#endif
  ) where

import Data.Hashable
import Data.Text (pack, Text)
import GHC.Stack
import GHC.Stack.Types
import Text.Printf
#if SERVER
import Clay hiding (s)
import Data.Default (Default(def))
import Language.Haskell.TH (listE, litE, stringL, Type(AppT, ConT))
import Language.Haskell.TH.Syntax (Dec(..), Exp, mkName, Q, reifyInstances, Type(VarT))
import System.Directory ()
#endif

-- | Instances of 'CssClass' can be converted to css class names.
class CssClass a where
  cssClass :: HasCallStack => a -> Text

instance Hashable SrcLoc where
  hash (SrcLoc{..}) =
    hash srcLocPackage `hashWithSalt` srcLocPackage
                       `hashWithSalt` srcLocModule
                       -- `hashWithSalt` srcLocFile
                       `hashWithSalt` srcLocStartLine
                       `hashWithSalt` srcLocStartCol
  hashWithSalt s (SrcLoc{..}) =
    s `hashWithSalt` srcLocPackage
      `hashWithSalt` srcLocModule
      -- `hashWithSalt` srcLocFile
      `hashWithSalt` srcLocStartLine
      `hashWithSalt` srcLocStartCol

withHash :: HasCallStack => Text -> Text
withHash s = do
  locHash callStack
  where
    locHash EmptyCallStack = s
    locHash (PushCallStack "withHash" _loc more) = locHash more
    locHash (PushCallStack _fn loc _more) = s <> "_" <> pack (printf "%07x" (mod (hash loc) 0x10000000))
    locHash (FreezeCallStack more) = locHash more

-- data TestHash = TestHash deriving Show
--
-- instance CssClass TestHash where
--   cssClass = withHash . pack . show
--
-- > cssClass TestHash
-- "TestHash_84f295f"

#if SERVER
-- | Instances of 'CssStyle' generate a Css value.
class Default prefs => CssStyle a prefs | a -> prefs where
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
