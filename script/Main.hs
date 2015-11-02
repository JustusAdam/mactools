{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where


import           Control.Monad
import           Data.Either        (lefts)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import           Data.Yaml
import           System.Directory
import           System.Environment (getArgs)
import           System.Exit
import           System.FilePath
import           System.IO          (hPutStr, hPutStrLn, stderr)
import           System.Process


cacheFileName :: FilePath
cacheFileName = "cache.yaml"


data InstallStatus
  = Installed
  | InstallationPending
  | Errored String
  | ErrorIgn String

data Installation
  = Brew            [String]
  | BrewCask        [String]
  | Font            String
  | RawShellCommand String
  | Compound        [Installation]
  | Manual          String
  | Cabal           [String]

data Tool = Tool
  { name         :: String
  , description  :: String
  , url          :: Maybe String
  , installation :: Installation
  , tStatus      :: InstallStatus
  }

data ToolSection = ToolSection
  { sName        :: String
  , sDescription :: Maybe String
  , sTools       :: [Tool]
  , haltOnFail   :: Bool
  }

type ToolFile = [ToolSection]


putErr :: String -> IO ()
putErr = hPutStr stderr


putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr


instance FromJSON Installation where

  parseJSON (Object o) =
    o .: "type" >>= installationFromType
    where
      getPackages a@(Array _)  = parseJSON a
      getPackages b@(String _) = (: []) <$> parseJSON b
      getPackages _            = mzero

      installationFromType :: String -> Parser Installation
      installationFromType "brew"        = Brew <$> (o .: "value" >>= getPackages)
      installationFromType "raw_command" = RawShellCommand <$> o .: "value"
      installationFromType "font"        = Font <$> o .: "value"
      installationFromType "compound"    = Compound <$> o .: "value"
      installationFromType "brew cask"   = BrewCask <$> (o .: "value" >>= getPackages)
      installationFromType "manual"      = Manual <$> o .: "value"
      installationFromType "cabal"       = Cabal <$> (o .: "value" >>= getPackages)
      installationFromType _             = mzero

  parseJSON _ = mzero


instance FromJSON InstallStatus where
  parseJSON (Object o) =
    (o .: "type" :: Parser String) >>= \case
      "errored" -> Errored <$> o .: "reason"
      "ignored" -> ErrorIgn <$> o .: "reason"
      _ -> mzero
  parseJSON (String "installed") = return Installed
  parseJSON (String "not installed") = return InstallationPending
  parseJSON (String "pending") = return InstallationPending
  parseJSON (Bool True) = return Installed
  parseJSON (Bool False) = return InstallationPending
  parseJSON _ = mzero


instance FromJSON Tool where
  parseJSON (Object o) = Tool
    <$> o .: "name"
    <*> o .: "description"
    <*> o .:? "url" .!= Nothing
    <*> o .: "installation"
    <*> o .:? "status" .!= InstallationPending
  parseJSON _ = mzero


instance FromJSON ToolSection where
  parseJSON (Object o) = ToolSection
    <$> o .: "name"
    <*> o .:? "description" .!= Nothing
    <*> o .: "items"
    <*> o .: "halt_on_file" .!= False
  parseJSON _ = mzero

instance ToJSON Tool where
  toJSON (Tool {..}) = object
    [ "name" .= name
    , "description" .= description
    , "url" .= url
    , "installation" .= installation
    , "status" .= tStatus
    ]

instance ToJSON ToolSection where
  toJSON (ToolSection {..}) = object
    [ "name" .= sName
    , "description" .= sDescription
    , "items" .= sTools
    , "halt_on_fail" .= haltOnFail
    ]

instance ToJSON Installation where
  toJSON (Brew pack) = object
    [ "type" .= ("brew" :: Text) , "value" .= pack ]
  toJSON (BrewCask pack) = object
    [ "type" .= ("brew cask" :: Text) , "value" .= pack ]
  toJSON (Font s) = object
    [ "type" .= ("font" :: Text), "value" .= s ]
  toJSON (RawShellCommand cmd) = object
    [ "type" .= ("raw_command" :: Text), "value" .= cmd ]
  toJSON (Compound i) = object
    [ "type" .= ("compound" :: Text), "value" .= i ]
  toJSON (Manual str) = object
    [ "type" .= ("manual" :: Text), "value" .= str ]
  toJSON (Cabal pack) = object
    [ "type" .= ("cabal" :: Text), "value" .= pack ]

instance ToJSON InstallStatus where
  toJSON Installed = "installed"
  toJSON InstallationPending = "pending"
  toJSON (Errored reason) = object
    [ "type" .= ("errored" :: Text), "reason" .= reason ]
  toJSON (ErrorIgn reason) = object
    [ "type" .= ("ignored" :: Text), "reason" .= reason ]


installWithShellCommand :: String -> [String] -> [String] -> IO (Either String ())
installWithShellCommand command defaults extras =
  readProcessWithExitCode command (defaults <> extras) "" >>= \case
    (ExitSuccess, _, _) -> return (Right ())
    (ExitFailure c, _, err) ->
      return $ Left $
        "Command `"
        <> unwords defaults
        <> "` failed with code: "
        <> show c
        <> " and stderr: \n"
        <> err

install :: Installation -> IO (Either String ())
install (Brew packages) = installWithShellCommand "brew" ["install"] packages
install (BrewCask packages) = installWithShellCommand "brew" ["cask", "install"] packages
install (Font _) = return $ Left "Font install not supported yet."
install (RawShellCommand command) = do
  putErrLn "Note that executing raw shell commands is always dangerous!"
  (<$> system command) $ \case
    ExitSuccess -> Right ()
    (ExitFailure code) -> Left $ "Command failed with " <> show code
install (Compound insts) =
  (<$> traverse install insts) $ (. lefts) $ \case
    [] -> Right ()
    failiures -> Left (unlines failiures)
install (Manual descr) = do
  putErrLn "Manual installation required:"
  putErrLn descr
  return $ Right ()
install (Cabal packages) = installWithShellCommand "cabal" ["install"] packages


toolFileToAdoc :: ToolFile -> String
toolFileToAdoc toolFile = unlines $
  [ "= Essential Software for my Mac"
  , "Justus Adam <me@justus.science>"
  ] ++ join (map (\t -> ["", "", toolSectionToAdoc t]) toolFile)


toolToAdoc :: Tool -> String
toolToAdoc (Tool { name, description, url, installation }) = unlines $ map ("| " <>)
  [ maybe id (\u n -> u <> "[" <> n <> "]") url name
  , description
  , installToAdoc installation
  ]


installToAdoc :: Installation -> String
installToAdoc (Brew packages) = "`brew install " <> unwords packages <> "`"
installToAdoc (BrewCask packages) = "`brew cask install " <> unwords packages <> "`"
installToAdoc (Font url) = "Download latest release " <> url <> "[here]"
installToAdoc (RawShellCommand c) = "`" <> c <> "`"
installToAdoc (Compound installs) = foldl (\a b -> a <> ", then " <> b) "" $ map installToAdoc installs
installToAdoc (Manual instruction) = instruction
installToAdoc (Cabal packages) = "`cabal install " <> unwords packages <> "`"


toolSectionToAdoc :: ToolSection -> String
toolSectionToAdoc (ToolSection { sName, sDescription, sTools }) = unlines $
  ("== " <> sName)
  : ([""] <> maybe [] (: [""]) sDescription)
  ++ [ "|==="
     , "| Name | What it is | How to get it"
     ]
  ++ join (map (\t -> ["", toolToAdoc t]) sTools)
  ++ ["|==="]


getCacheDir :: IO FilePath
getCacheDir = do
  appDir <- getAppUserDataDirectory "mactools"
  createDirectoryIfMissing True appDir
  return appDir


installTools :: Maybe String -> IO ()
installTools Nothing = do
  cache <- getCacheFile
  installTools' cache
installTools (Just file) = do
  fileExists <- doesFileExist file
  if fileExists
    then installTools' file
    else putErrLn "Tool file not found" >> exitWith (ExitFailure 2)


installTools' :: String -> IO ()
installTools' file =
  decodeFileEither file >>= \case
    Left err -> do
      putErrLn $ show err
      exitWith (ExitFailure 2)
    Right toolFile -> do
      cache <- go [] toolFile
      cacheDir <- getCacheDir
      encodeFile (cacheDir </> cacheFileName) cache
  where
    go acc [] = return acc
    go acc (sec@(ToolSection { haltOnFail }) : rest) = do
      (hsec, res) <- installSection sec
      let cont = go (hsec : acc) rest
      case res of
        Left _ ->
          if haltOnFail
            then return (reverse acc <> (hsec : rest))
            else cont
        Right _ -> cont


genToolFile :: String -> IO ()
genToolFile file =
  decodeFileEither file >>= \case
    Left err -> do
      putErrLn $ show err
      exitWith (ExitFailure 2)
    Right toolFile ->
      putStrLn $ toolFileToAdoc toolFile


installSection :: ToolSection -> IO (ToolSection, Either String ())
installSection t@(ToolSection { sName, sTools, haltOnFail }) = do
  putStrLn $ "Installing section \"" <> sName <> "\""
  go [] sTools
  where
    go processed [] = return (t { sTools = reverse processed }, Right ())
    go processed (ct@(Tool { name, tStatus = Installed }) : ts) = do
      putErrLn $ "Skipping \"" <> name <> "\""
      go (ct : processed) ts
    go processed (ct@(Tool { name, installation }) : ts) = do
      putErr $ "Installing \"" <> name <> "\" ... "
      install installation >>= \case
        (Right ()) -> putErrLn "success" >> go (ct { tStatus = Installed } : processed) ts
        (Left err) -> do
          putErrLn "failed"
          putErrLn err
          if haltOnFail
            then return (t { sTools = reverse $ ct { tStatus = Errored err } : processed}, Left err)
            else go (ct { tStatus = ErrorIgn err } : processed) ts


getCacheFile :: IO FilePath
getCacheFile = do
  appDir <- getCacheDir
  let cacheFilePath = appDir </> cacheFileName
  cacheExists <- doesFileExist cacheFilePath
  if cacheExists
    then return cacheFilePath
    else do
      putErrLn "No cache file found, aborting"
      exitWith (ExitFailure 2)


validCacheCommands :: [String]
validCacheCommands = ["ignore-blocking"]


setFirst :: (Tool -> Tool) -> [Tool] -> Maybe [Tool]
setFirst f l =
  case span pred' l of
    (lbegin, elem' : ltail) -> Just $ lbegin <> (f elem' : ltail)
    _ -> Nothing
  where
    pred' = \case
      (Tool { tStatus = (Errored _)}) -> True
      _ -> False


setCache :: String -> ToolFile -> ToolFile
setCache "ignore-blocking" tf =
  map (\s -> maybe s (\i -> s { sTools = i })
          $ setFirst (\elem'@(Tool { tStatus = (Errored err) }) -> elem' { tStatus = ErrorIgn err })
          $ sTools s) tf
setCache _ tf = tf


main :: IO ()
main =
  getArgs >>= \case
    ["set-cache", cmd] ->
      if cmd `elem` validCacheCommands
        then getCacheFile >>= \cacheFile -> decodeFileEither cacheFile >>= \case
          (Left err) -> putErrLn $ show err
          (Right tf) -> encodeFile cacheFile $ setCache cmd tf
        else putErrLn $ "Invalid cache command \"" <> cmd <> "\""
    ("set-cache":cmd) -> putErrLn $ "Invalid cache command \"" <> unwords cmd <> "\""
    ["generate", file] -> genToolFile file
    ["generate"] -> genToolFile "tools.yaml"
    ["install", "continue"] -> installTools Nothing
    ["install", file] -> installTools (Just file)
    [] -> do
      putErrLn "Expected command"
      exitWith (ExitFailure 1)
    cmds -> do
      putErrLn ("Unknown command \"" <> unwords cmds <> "\"")
      exitWith (ExitFailure 1)
