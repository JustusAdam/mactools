{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Monad
import           Data.Aeson
import System.Process
import System.Exit
import Data.Monoid
import Data.Either (lefts)


data Installation
  = Brew [String]
  | BrewCask [String]
  | Font String
  | RawShellCommand String
  | Compound [Installation]
  | Manual String
  | Custom String Value

data Tool = Tool
  { name         :: String
  , description  :: String
  , url          :: Maybe String
  , installation :: Installation
  , tInstall      :: Bool
  }

data ToolSection = ToolSection
  { sName        :: String
  , sDescription :: Maybe String
  , sTools       :: [Tool]
  }

data ToolFile = ToolFile
  { management     :: ToolSection
  , cliTools       :: ToolSection
  , graphicalTools :: ToolSection
  , fonts          :: ToolSection
  , utilities      :: ToolSection
  }


instance FromJSON Installation where

  parseJSON (Object o) =
    o .: "type" >>= installationFromType
    where
      getPackages a@(Array _)  = parseJSON a
      getPackages b@(String _) = (: []) <$> parseJSON b
      getPackages _            = mzero

      installationFromType "brew"        = Brew <$> (o .: "value" >>= getPackages)
      installationFromType "raw_command" = RawShellCommand <$> o .: "value"
      installationFromType "font"        = Font <$> o .: "value"
      installationFromType "compound"    = Compound <$> o .: "value"
      installationFromType "brew cask"   = BrewCask <$> o .: "value"
      installationFromType "manual"      = Manual <$> o .: "value"
      installationFromType a             = Custom a <$> o .: "value"

  parseJSON _ = mzero


instance FromJSON Tool where
  parseJSON (Object o) = Tool
    <$> o .: "name"
    <*> o .: "description"
    <*> o .: "url"
    <*> o .: "installation"
    <*> o .:? "install" .!= True
  parseJSON _ = mzero


instance FromJSON ToolSection where
  parseJSON (Object o) = ToolSection
    <$> o .: "name"
    <*> o .: "description"
    <*> o .: "items"
  parseJSON _ = mzero


installWithShellCommand :: String -> [String] -> [String] -> IO (Either String ())
installWithShellCommand command defaults extras =
  readProcessWithExitCode command (defaults <> extras) "" >>= \case
    (ExitSuccess, o, _) -> putStrLn o >> return (Right ())
    (ExitFailure c, _, err) ->
      return $ Left $
        "Command `"
        <> foldl (\a b -> a <> " " <> b) "" defaults
        <> "` failed with code: "
        <> show c
        <> " and stderr: \n"
        <> err

install :: Installation -> IO (Either String ())
install (Brew packages) = installWithShellCommand "brew" ["install"] packages
install (BrewCask packages) = installWithShellCommand "brew" ["cask", "install"] packages
install (Font _) = do
  putStrLn "Font install not supported yet."
  return (Right ())
install (RawShellCommand command) = do
  putStrLn "Note that executing raw shell commands is always dangerous!"
  system command >>= \case
    ExitSuccess -> return $ Right ()
    (ExitFailure code) -> return $ Left $ "Command failed with " <> show code
install (Compound insts) =
  flip fmap (traverse install insts) $ (. lefts) $ \case
    [] -> Right ()
    failiures -> Left (unlines failiures)
install (Manual descr) = do
  putStrLn "Manual installation required:"
  putStrLn descr
  return $ Right ()
install (Custom cmd _) = do
  putStrLn $ "Unsupported install method " <> cmd
  return $ Right ()


main :: IO ()
main = return ()
