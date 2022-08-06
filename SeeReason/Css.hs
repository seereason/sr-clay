{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module SeeReason.Css
  ( CssClass(cssClass)
  , withHash
  , protectCSSIdentifier
#if SERVER
  , CssStyle(cssStyle)
  , byClass'
  , reifyCss
  , putCss'
#endif
  ) where

import Data.ByteString.Lazy as Lazy (fromStrict)
import Data.Char (isAlphaNum, isAscii, isSpace)
import Data.Digest.Pure.MD5 (md5)
import Data.Serialize (encode)
import Data.Text as Text (pack, singleton, Text)
import qualified Data.Text as Text (concatMap)
import GHC.Stack
import GHC.Stack.Types
import Language.Haskell.TH.Syntax (Name(..), ModName(..), NameFlavour(..), NameSpace(..), OccName(..))

#if SERVER
import Clay hiding (not, s, space)
import Data.Text.Lazy as Lazy (unpack)
import Language.Haskell.TH (appTypeE, listE, litE, stringL, Type(AppT, ConT))
import Language.Haskell.TH.Syntax (Dec(..), Exp, mkName, Q, reifyInstances, Type(VarT))
import System.Directory ()
#endif

-- | Instances of 'CssClass' can be converted to css class names.
class CssClass a where
  cssClass :: HasCallStack => a -> Text

-- | Any reasonable haskell 'Name' can be a 'CssClass'.
instance CssClass Name where
  cssClass (Name (OccName o) (NameG space _ (ModName m))) =
    protectCSSIdentifier (pack (m <> "-" <> o <>
                               case space of
                                 TcClsName -> "_T"
                                 DataName -> "_D"
                                 VarName -> "_V"))
  cssClass (Name (OccName o) (NameQ (ModName m))) = protectCSSIdentifier (pack (m <> "-" <> o))
  cssClass (Name (OccName o) NameS) = pack o
  cssClass (Name o f) = error ("cssClass (Name " <> show o <> " " <> show f <> ")")

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
class CssStyle a where
  cssStyle :: Css

byClass' :: CssClass a => a -> Refinement
byClass' = byClass . cssClass

-- | Collect all the in scope instances of CssStyle and turn them into
-- pairs that can be used to build scss files.  Result expression type
-- is [(FilePath, Css)].
reifyCss :: Q Exp
reifyCss = do
  insts <- reifyInstances ''CssStyle [VarT (mkName "a")]
  listE (concatMap (\case InstanceD _ _cxt (AppT _cls typ@(ConT tname)) _decs ->
                            [ [|($(litE (stringL (show tname))), $(appTypeE [|cssStyle|] (pure typ)))|] ]
                          _ -> []) insts)

-- Render and print in a compact format, for debugging
putCss' :: Css -> IO ()
putCss' = putStrLn . Lazy.unpack . renderWith compact []
#endif
