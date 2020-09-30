{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiModule where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isSpace)
import Data.List (sort, (\\))
import Control.Applicative (some, Alternative(..))
import Control.Monad (void)

newtype Module = Module { getModuleName :: Text }

data ApiModule = ApiModule
  { moduleName :: Module
  , imports :: [Module]
  }

renderApiModule :: ApiModule -> Text
renderApiModule ApiModule { moduleName, imports } =
  (T.unlines
    $ "{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}"
    : "{-# LANGUAGE PartialTypeSignatures #-}"
    : "{-# LANGUAGE ExplicitNamespaces #-}"
    : "{-# LANGUAGE TypeOperators #-}"
    : ""
    : "module " <> getModuleName moduleName <> " (type Api, server) where"
    : ""
    : "import Servant ((:<|>)((:<|>)))"
    : "import qualified GHC.Stack as Stack"
    : "import qualified Servant"
    : ""
    : fmap renderImport imports
    )
    <> "\n"
    <> renderApiType imports
    <> "\n\n"
    <> renderServerFunction imports
  where
    renderImport :: Module -> Text
    renderImport modu = "import qualified " <> getModuleName modu
    renderApiType :: [Module] -> Text
    renderApiType modules = "type Api\n  = "
      <> T.intercalate "\n  :<|> " (fmap (\modul -> getModuleName modul <> ".Route") modules)
    renderServerFunction :: [Module] -> Text
    renderServerFunction modules =
      "server :: Stack.HasCallStack => Servant.ServerT Api _\n"
        <> "server\n  = "
        <> server
      where
        server =
          T.intercalate "\n  :<|> " (fmap (\modul -> getModuleName modul <> ".handler") modules)

-- | used to calculate the difference between discovered handler modules
-- and imported modules at the call sight of @makeApi@ splice
difference :: Ord a => [a] -> [a] -> [a]
difference listA listB =
  let
    sortedA = sort listA
    sortedB = sort listB
  in sortedA \\ sortedB

decodeApiModule :: Text -> Either String ApiModule
decodeApiModule input = Atto.parseOnly parserApiModule input

parserApiModule :: Atto.Parser ApiModule
parserApiModule = ApiModule
  <$> parserModule
  <*> parserImports

skipBlockComment ::
  -- | Start of block comment
  Text ->
  -- | End of block comment
  Text ->
  Atto.Parser ()
skipBlockComment start end = p *> void (Atto.manyTill Atto.anyChar n)
  where
    p = Atto.string start
    n = Atto.string end

skipLineComment ::
  Text -> Atto.Parser ()
skipLineComment prefix =
  Atto.string prefix *> (Atto.skipWhile (/= '\n'))
{-# INLINEABLE skipLineComment #-}

comment :: Atto.Parser ()
comment = 
      skipBlockComment "{-" "-}"
  <|> skipLineComment "--"

spaceConsumer :: Atto.Parser () 
spaceConsumer = Atto.skipSpace
  *> Atto.skipMany comment 
  *> Atto.skipSpace

parserModule :: Atto.Parser Module
parserModule = do
  spaceConsumer
  void $ Atto.string "module "
  moduleName <- Atto.takeWhile1 (not . isSpace)
  Atto.skipWhile (/= '\n')
  spaceConsumer
  pure $ Module moduleName

parserImports :: Atto.Parser [Module]
parserImports = some parserImport
  where
  parserImport = do
    void $ Atto.string "import "
    moduleName <- Atto.takeWhile1 (not . isSpace)
    spaceConsumer
    pure $ Module moduleName

failModule :: Text -> [Text] -> Text
failModule modName errMsgs =
  let typeErrors = fmap (\err -> "TypeLits.Text \"" <> err <> "\"") errMsgs
  in
    T.unlines
      [ "{-# LANGUAGE ExplicitNamespaces #-}"
      , "{-# LANGUAGE TypeOperators #-}"
      , "{-# LANGUAGE DataKinds #-}"
      , ""
      , "module " <> modName <> " where"
      , ""
      , "import qualified GHC.TypeLits as TypeLits"
      , ""
      , "server :: TypeLits.TypeError (" <> T.intercalate " TypeLits.:$$: " typeErrors <> ")"
      , "server = undefined"
      ]
