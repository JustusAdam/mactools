{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Monad
import           Data.Either        (lefts)
import           Data.Monoid
import           Data.Yaml
import           System.Environment (getArgs)
import           System.Exit
import           System.IO
import           System.Process


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
  , tInstall     :: Bool
  }

data ToolSection = ToolSection
  { sName        :: String
  , sDescription :: Maybe String
  , sTools       :: [Tool]
  }

type ToolFile = [ToolSection]


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


instance FromJSON Tool where
  parseJSON (Object o) = Tool
    <$> o .: "name"
    <*> o .: "description"
    <*> o .:? "url" .!= Nothing
    <*> o .: "installation"
    <*> o .:? "install" .!= True
  parseJSON _ = mzero


instance FromJSON ToolSection where
  parseJSON (Object o) = ToolSection
    <$> o .: "name"
    <*> o .:? "description" .!= Nothing
    <*> o .: "items"
  parseJSON _ = mzero


installWithShellCommand :: String -> [String] -> [String] -> IO (Either String ())
installWithShellCommand command defaults extras =
  readProcessWithExitCode command (defaults <> extras) "" >>= \case
    (ExitSuccess, _, err) -> putErrLn err >> return (Right ())
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
install (Font _) = do
  putErrLn "Font install not supported yet."
  return (Right ())
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


installTools :: IO ()
installTools = return ()


genToolFile :: String -> IO ()
genToolFile file =
  decodeFileEither file >>= \case
    Left err -> do
      putErrLn $ show err
      exitWith (ExitFailure 2)
    Right toolFile ->
      putStrLn $ toolFileToAdoc toolFile


main :: IO ()
main =
  getArgs >>= \case
    ["generate", file] -> genToolFile file
    ["generate"] -> genToolFile "tools.yaml"
    ["install"] -> installTools
    [] -> do
      putErrLn "Expected command"
      exitWith (ExitFailure 1)
    cmds -> do
      putErrLn ("Unknown command \"" <> unwords cmds <> "\"")
      exitWith (ExitFailure 1)
