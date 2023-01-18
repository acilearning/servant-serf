module ServantSerf.Type.Flag where

import qualified System.Console.GetOpt as Console

data Flag
  = ApiName String
  | Depth String
  | ExcludeSuffix String
  | Help
  | ModuleName String
  | ServerName String
  | Version
  deriving (Eq, Show)

options :: [Console.OptDescr Flag]
options =
  [ Console.Option
      ['h', '?']
      ["help"]
      (Console.NoArg Help)
      "Shows this help message, then exits.",
    Console.Option
      []
      ["version"]
      (Console.NoArg Version)
      "Shows the version number, then exits.",
    Console.Option
      []
      ["api-name"]
      (Console.ReqArg ApiName "API_NAME")
      "Sets the name to use for the API type. Defaults to `API`.",
    Console.Option
      []
      ["depth"]
      (Console.ReqArg Depth "DEPTH")
      "Controls whether to search through only one directory (`shallow`) or recursively (`deep`). Defaults to `deep`.",
    Console.Option
      []
      ["exclude-suffix"]
      (Console.ReqArg ExcludeSuffix "SUFFIX")
      "Sets the module suffix to exclude. Defaults to the empty string.",
    Console.Option
      []
      ["module-name"]
      (Console.ReqArg ModuleName "MODULE_NAME")
      "Sets the name of the generated module. By default this is generated from the source file name.",
    Console.Option
      []
      ["server-name"]
      (Console.ReqArg ServerName "SERVER_NAME")
      "Sets the name to use for the server value. Defaults to `server`."
  ]
