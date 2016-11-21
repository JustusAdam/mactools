{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where


import           Control.Monad
import           Data.List          (intersperse)
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
    | Errored CallResult
    | ErrorIgn CallResult

data Installation
    = Brew            [String]
    | BrewCask        [String]
    | Font            String
    | RawShellCommand String
    | Compound        [Installation]
    | Manual          String
    | Cabal           [String]
    | Apm             [String]

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


type Key = Text


data CallResult = CallResult
    { commandName :: String
    , returnCode :: Maybe Int
    , resultStderr :: String
    } deriving (Show, Eq)


putErr :: String -> IO ()
putErr = hPutStr stderr


putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr


installationTypeKey :: Key
installationTypeKey = "type"
installationValueKey :: Key
installationValueKey = "value"
instBrewType :: Key
instBrewType = "brew"
instBrewCaskType :: Key
instBrewCaskType = "brew cask"
instRawCommandType :: Key
instRawCommandType = "raw_command"
instCompoundType :: Key
instCompoundType = "compound"
instManualType :: Key
instManualType = "manual"
instCabalType :: Key
instCabalType = "cabal"
instFontType :: Key
instFontType = "font"
instApmType :: Key
instApmType = "apm"

instance FromJSON Installation where

    parseJSON (Object o) =
        o .: installationTypeKey >>= installationFromType
      where
        packages = o .: installationValueKey >>= \case
                    a@(Array _) -> parseJSON a
                    b@(String _) -> (: []) <$> parseJSON b
                    _ -> mzero

        installationFromType :: Text -> Parser Installation
        installationFromType type'
            | type' == instBrewType = Brew <$> packages
            | type' == instRawCommandType = RawShellCommand <$> o .: installationValueKey
            | type' == instFontType = Font <$> o .: installationValueKey
            | type' == instCompoundType = Compound <$> o .: installationValueKey
            | type' == instBrewCaskType = BrewCask <$> packages
            | type' == instManualType = Manual <$> o .: installationValueKey
            | type' == instCabalType = Cabal <$> packages
            | type' == instApmType = Apm <$> packages
            | otherwise = mzero

    parseJSON _ = mzero


unPackage :: Text -> [String] -> Value
unPackage key [val] = toInstallation key val
unPackage key val = toInstallation key val

toInstallation :: ToJSON a => Text -> a -> Value
toInstallation type' value = object
    [ installationTypeKey .= type'
    , installationValueKey .= value
    ]

instance ToJSON Installation where
    toJSON (Brew pack) = unPackage instBrewType pack
    toJSON (BrewCask pack) = unPackage instBrewCaskType pack
    toJSON (Font s) = toInstallation instFontType s
    toJSON (RawShellCommand cmd) = toInstallation instRawCommandType cmd
    toJSON (Compound i) = toInstallation instCompoundType i
    toJSON (Manual str) = toInstallation instManualType str
    toJSON (Cabal pack) = unPackage instCabalType pack
    toJSON (Apm pack) = unPackage instApmType pack


instStatTypeKey :: Key
instStatTypeKey = "type"
instStatReasonKey :: Key
instStatReasonKey = "value"

instance FromJSON InstallStatus where
    parseJSON (Object o) =
        (o .: instStatTypeKey :: Parser String) >>= \case
            "errored" -> Errored <$> reason
            "ignored" -> ErrorIgn <$> reason
            _ -> mzero
      where
        reason = o .: instStatReasonKey
    parseJSON (String "installed") = return Installed
    parseJSON (String "not installed") = return InstallationPending
    parseJSON (String "pending") = return InstallationPending
    parseJSON (Bool True) = return Installed
    parseJSON (Bool False) = return InstallationPending
    parseJSON _ = mzero

toInstallStatus :: ToJSON a => Text -> a -> Value
toInstallStatus type' value =
    object [ instStatTypeKey .= type', instStatReasonKey .= value ]

instance ToJSON InstallStatus where
    toJSON Installed = "installed"
    toJSON InstallationPending = "pending"
    toJSON (Errored reason) = toInstallStatus "errored" reason
    toJSON (ErrorIgn reason) = toInstallStatus "ignored" reason


toolNameKey :: Key
toolNameKey = "name"
toolDescKey :: Key
toolDescKey = "description"
toolUrlKey :: Key
toolUrlKey = "url"
toolInstKey :: Key
toolInstKey = "installation"
toolStatusKey :: Key
toolStatusKey = "status"

instance FromJSON Tool where
    parseJSON (Object o) = Tool
        <$> o .: toolNameKey
        <*> o .: toolDescKey
        <*> o .:? toolUrlKey .!= Nothing
        <*> o .: toolInstKey
        <*> o .:? toolStatusKey .!= InstallationPending
    parseJSON _ = mzero

instance ToJSON Tool where
    toJSON Tool {..} = object $
        [ toolNameKey .= name
        , toolDescKey .= description
        , toolInstKey .= installation
        , toolStatusKey .= tStatus
        ] ++ maybe [] (return . (toolUrlKey .=)) url


callResCommandKey :: Text
callResCommandKey = "command"
callResRetCodeKey :: Text
callResRetCodeKey = "returncode"
callResStderrKey :: Text
callResStderrKey = "stderr"

instance FromJSON CallResult where
    parseJSON (Object o) = CallResult
        <$> o .: callResCommandKey
        <*> o .:? callResRetCodeKey .!= Nothing
        <*> o .:? callResStderrKey .!= ""
    parseJSON _ = mzero


instance ToJSON CallResult where
    toJSON CallResult{..} = object $
        [ callResCommandKey .= commandName
        ]
        ++ maybe [] (return . (callResRetCodeKey .=)) returnCode
        ++ if null resultStderr 
               then [] 
               else [callResStderrKey .= resultStderr] 


toolSecNameKey :: Key
toolSecNameKey = "name"
toolSecDescKey :: Key
toolSecDescKey = "description"
toolSecItemsKey :: Key
toolSecItemsKey = "items"
toolHaltOnFailKey :: Key
toolHaltOnFailKey = "halt_on_fail"

instance FromJSON ToolSection where
    parseJSON (Object o) = ToolSection
        <$> o .: toolSecNameKey
        <*> o .:? toolSecDescKey .!= Nothing
        <*> o .: toolSecItemsKey
        <*> o .:? toolHaltOnFailKey .!= False
    parseJSON _ = mzero

instance ToJSON ToolSection where
    toJSON ToolSection{..} = object
        [ toolSecNameKey .= sName
        , toolSecDescKey .= sDescription
        , toolSecItemsKey .= sTools
        , toolHaltOnFailKey .= haltOnFail
        ]


class ToAdoc a where
    toAdoc :: a -> String

instance ToAdoc ToolFile where
    toAdoc toolFile = unlines $
        [ "= Essential Software for my Mac"
        , "Justus Adam <me@justus.science>"
        ] ++ join (map (\t -> ["", "", toAdoc t]) toolFile)

instance ToAdoc Tool where
    toAdoc Tool{ name, description, url, installation } = unlines $ map ("| " <>)
        [ maybe id (\u n -> u <> "[" <> n <> "]") url name
        , description
        , toAdoc installation
        ]

instance ToAdoc ToolSection where
    toAdoc ToolSection{ sName, sDescription, sTools } = unlines $
        ("== " <> sName)
        : ([""] <> maybe [] (: [""]) sDescription)
        ++ [ "|==="
        , "| Name | What it is | How to get it"
        ]
        ++ join (map (\t -> ["", toAdoc t]) sTools)
        ++ ["|==="]

instance ToAdoc Installation where
    toAdoc (Brew packages) = "`brew install " <> unwords packages <> "`"
    toAdoc (BrewCask packages) = "`brew cask install " <> unwords packages <> "`"
    toAdoc (Font url) = "Download latest release " <> url <> "[here]"
    toAdoc (RawShellCommand c) = "`" <> c <> "`"
    toAdoc (Compound installs) = unwords $ intersperse "and then" $ map toAdoc installs
    toAdoc (Manual instruction) = instruction
    toAdoc (Cabal packages) = "`cabal install " <> unwords packages <> "`"
    toAdoc (Apm packages) = "`apm install " <> unwords packages <> "`"


installWithShellCommand :: String -> [String] -> [String] -> IO (Either CallResult ())
installWithShellCommand command defaults extras =
    readProcessWithExitCode command (defaults <> extras) "" >>= \case
        (ExitSuccess, _, _) -> return (Right ())
        (ExitFailure c, _, err) ->
            return $ Left $ CallResult (unwords defaults) (return c) err


install :: Installation -> IO (Either CallResult ())
install (Brew packages) = installWithShellCommand "brew" ["install"] packages
install (BrewCask packages) = installWithShellCommand "brew" ["cask", "install"] packages
install (Font f) = return $ Left $ CallResult ("install font " <> f) Nothing "Font install not supported yet."
install (RawShellCommand command) = do
    putErrLn "Note that executing raw shell commands is always dangerous!"
    (<$> system command) $ \case
        ExitSuccess -> Right ()
        (ExitFailure code) -> Left $ CallResult command (return code) ""
install (Compound insts) =
    go insts
  where
    go [] = return $ Right ()
    go (curr:remaining) = install curr >>= \case
        (Right _) -> go remaining
        err -> return err
install (Manual descr) = do
    putErrLn "Manual installation required:"
    putErrLn descr
    return $ Right ()
install (Cabal packages) = installWithShellCommand "cabal" ["install"] packages
install (Apm packages) = installWithShellCommand "apm" ["install"] packages


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
    go acc (sec@ToolSection{ haltOnFail } : rest) = do
        (hsec, res) <- installSection sec
        let cont = go (hsec : acc) rest
        case res of
            Left _ | haltOnFail -> return (reverse acc <> (hsec : rest))      
            _ -> cont


genToolFile :: String -> IO ()
genToolFile file =
    decodeFileEither file >>= \case
        Left err -> do
            putErrLn $ show err
            exitWith (ExitFailure 2)
        Right toolFile ->
            putStrLn $ toAdoc (toolFile :: ToolFile)


installSection :: ToolSection -> IO (ToolSection, Either CallResult ())
installSection t@ToolSection{ sName, sTools, haltOnFail } = do
    putStrLn $ "Installing section \"" <> sName <> "\""
    go [] sTools
  where
    go processed [] = return (t { sTools = reverse processed }, Right ())
    go processed (ct@Tool{ name, tStatus = Installed } : ts) = do
        putErrLn $ "Skipping \"" <> name <> "\""
        go (ct : processed) ts
    go processed (ct@Tool { name, installation } : ts) = do
        putErr $ "Installing \"" <> name <> "\" ... "
        install installation >>= \case
            Right () -> putErrLn "success" >> go (ct { tStatus = Installed } : processed) ts
            Left err -> do
                putErrLn "failed"
                putErrLn (show err)
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
        Tool{ tStatus = (Errored _)} -> True
        _ -> False

setCache :: String -> ToolFile -> ToolFile
setCache "ignore-blocking" tf =
    map (\s -> maybe s (\i -> s { sTools = i })
        $ setFirst (\elem'@Tool{ tStatus = Errored err } -> elem' { tStatus = ErrorIgn err })
        $ sTools s) tf
setCache _ tf = tf


main :: IO ()
main =
    getArgs >>= \case
        ["set-cache", cmd] ->
            if cmd `elem` validCacheCommands
                then getCacheFile >>= \cacheFile -> decodeFileEither cacheFile >>= \case
                    Left err -> putErrLn $ show err
                    Right tf -> encodeFile cacheFile $ setCache cmd tf
                else putErrLn $ "Error: invalid cache command \"" <> cmd <> "\""
        ("set-cache":cmd) -> putErrLn $ "Error: invalid cache command \"" <> unwords cmd <> "\""
        ["generate", file] -> genToolFile file
        ["generate"] -> genToolFile "tools.yaml"
        ["install", "continue"] -> installTools Nothing
        ["install", file] -> installTools (Just file)
        ["test", in', out] ->
            decodeFile in' >>= maybe (return ()) (encodeFile out :: ToolFile -> IO ())
        [] -> do
            putErrLn "Error: expected command"
            exitWith (ExitFailure 1)
        cmds -> do
            putErrLn ("Error: unknown command \"" <> unwords cmds <> "\"")
            exitWith (ExitFailure 1)
