{-# LANGUAGE OverloadedStrings #-}
module Config ( Config
              , readConfig
              , filesFromConfig
              ) where

import           Data.Aeson          (withObject)
import           Data.Monoid         (Last (..), (<>))
import           Data.Yaml           (FromJSON (..), decodeFileEither, (.:?))
import           Options.Applicative
import           System.Directory

data Config = Config String [String] deriving (Eq, Show)

readConfig :: IO Config
readConfig = do
  cmdLineConfig <- execParser $ info (partialConfigParser <**> helper) mempty
  fileConfig <- either fail return =<< readPartialConfig
  let combinedConfig = defaultPartialConfig
        <> fileConfig
        <> cmdLineConfig
  either fail return $ mkConfig combinedConfig

filesFromConfig :: Config -> [String]
filesFromConfig (Config rootFolder modules) = ((rootFolder ++ "/modules/") ++) <$> modules


-- partial options pattern
data PartialConfig = PartialConfig
  { pcIdeaFolderDir :: Last String
  , pcModules       :: Last [String]
  }

instance Monoid PartialConfig where
  mempty = PartialConfig mempty mempty
  mappend l r = PartialConfig
    { pcIdeaFolderDir = pcIdeaFolderDir l <> pcIdeaFolderDir r
    , pcModules       = pcModules l       <> pcModules r
    }

valueOrMissingMessage :: String -> Last a -> Either String a
valueOrMissingMessage errorMsg (Last x) = maybe (Left errorMsg) Right x

mkConfig :: PartialConfig -> Either String Config
mkConfig (PartialConfig pcIFD pcM) = do
  cIFD <- valueOrMissingMessage "Missing .idea folder location" pcIFD
  cM <- valueOrMissingMessage "Missing module .iml paths" pcM
  return $ Config cIFD cM

-- default
defaultPartialConfig :: PartialConfig
defaultPartialConfig = PartialConfig mempty (Last $ Just defaultModules)


defaultModules :: [String]
defaultModules = ["clinical-app/clinical-app_main.iml",
                  "clinical-app/clinical-app_test.iml",
                  "go-clinical/clinical_main.iml",
                  "go-clinical/clinical_test.iml",
                  "go-validation/go-validation_main.iml",
                  "go-validation/go-validation_test.iml",
                  "repair-man/repair-man_main.iml",
                  "repair-man/repair-man_test.iml",
                  "zz-one-off/zz-one-off_main.iml",
                  "zz-one-off/zz-one-off_test.iml"]

-- config file
instance FromJSON PartialConfig where
  parseJSON = withObject "FromJSON PartialConfig" $ \obj -> do
    pcIFD <- Last <$> obj .:? "idea-path"
    pcM <- Last <$> obj .:? "modules"
    return $ PartialConfig pcIFD pcM

readPartialConfig :: IO (Either String PartialConfig)
readPartialConfig = do
  homeDir <- getHomeDirectory
  let configFilePath = homeDir ++ "/.intellij-facet-remover/config.yaml"
  exists <- doesFileExist configFilePath
  if exists
    then either (Left . show) Right <$> decodeFileEither configFilePath
    else return $ Right mempty

-- command line
lastConfig :: Parser a -> Parser (Last a)
lastConfig parser = Last <$> (optional parser)

partialConfigParser :: Parser PartialConfig
partialConfigParser = PartialConfig
  <$> lastConfig (option str (long "idea-path" <> short 'p'))
  <*> (pure $ mempty)
