{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, LambdaCase, OverloadedStrings, TemplateHaskell #-}

module SeeReason.Css
  ( CssClass(cssClass)
#if SERVER
  , CssStyle(cssStyle)
  , byClass'
  , reifyCss
#endif
  ) where

import Data.Text (Text)
#if SERVER
import Clay
import Data.Default (Default(def))
import Language.Haskell.TH (listE, litE, stringL, Type(AppT, ConT))
import Language.Haskell.TH.Syntax (Dec(..), Exp, mkName, Q, reifyInstances, Type(VarT))
import System.Directory ()
#endif

-- | Instances of 'CssClass' can be converted to css class names.
class CssClass a where
  cssClass :: a -> Text

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
