{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, TupleSections #-}
module Lib
    ( makeBackups
    , normalizeDB
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Resource (MonadBaseControl, ResourceT, runResourceT)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.Persist
import Database.Persist.MySQL as DB
import Database.Persist.TH
import qualified Data.Text as T
import Data.Text (Text)
import Shelly

import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Backup
    time Int
    file Text
    type BackupType
    deriving Show
FtpServer
    host Text
    port Int
    user Text
    password Text
    deriving Show
BackupTarget
    server FtpServerId
    folder Text
    name Text
    deriving Show
BackupCount
    type BackupType
    count Int
    UniqueType type
|]

numBackups :: BackupType -> Int
numBackups Daily = 12
numBackups Weekly = 5
numBackups Monthly = 12
numBackups Yearly = -1

mySQLConn :: ConnectInfo
mySQLConn = defaultConnectInfo { connectHost = "192.168.0.104"
                               , connectUser = "MinervaBackup"
                               , connectPassword = ""
                               , connectDatabase = "MinervaBackup"
                               }

runMySQL :: (MonadBaseControl IO m, MonadIO m)
         => ConnectInfo -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runMySQL conninfo = runResourceT . runNoLoggingT . withMySQLConn conninfo . runReaderT . (<* transactionSave)

data FTPConn = FTPConn { host :: T.Text
                       , port :: Int
                       , user :: T.Text
                       , password :: T.Text
                       }

ftpServerToFTPConn :: FtpServer -> FTPConn
ftpServerToFTPConn (FtpServer {..}) = FTPConn { host     = ftpServerHost
                                              , port     = ftpServerPort
                                              , user     = ftpServerUser
                                              , password = ftpServerPassword
                                              }

normalizeDB :: IO ()
normalizeDB = runMySQL mySQLConn $ runMigration migrateAll

downloadFile :: FTPConn -> T.Text -> T.Text -> IO ()
downloadFile (FTPConn{..}) src dst = shelly $ do
    cmd "ncftpget" "-u" user "-p" password "-P" (T.pack $ show port) "-R" "-T" host dst src

makeBackups :: IO ()
makeBackups = runMySQL mySQLConn $ do
    targets <- DB.selectList ([] :: [Filter BackupTarget]) []
    -- For each target, get the appropriate FtpServer and store them in a Map
    -- servers :: Map (Key BackupTarget) BackupTarget
    servers <- Map.map fromJust . Map.filter isJust . Map.fromList <$> (sequence $ (\targ -> (targ,) <$> DB.get targ) . backupTargetServer . entityVal <$> targets)
    liftIO $ sequence_ $ do -- List monad
        target <- entityVal <$> targets
        let server = Map.lookup (backupTargetServer target) servers
        case server of
          Nothing -> pure $ pure ()
          Just server -> pure $ makeBackup (ftpServerToFTPConn server) (backupTargetFolder target) (backupTargetName target)
    liftIO $ getTodaysBackupType >>= cleanBackups

makeBackup :: FTPConn -> T.Text -> T.Text -> IO ()
makeBackup ftpConn file backupName = do
    downloadFile ftpConn file "temp"
    date <- show . utctDay <$> getCurrentTime
    let filename = backupName <> "-" <> T.pack date <> ".tar.gz"
        worldname = last $ T.splitOn "/" file
    shelly $ do
        cd "temp"
        cmd "tar" "czf" filename worldname
        cmd "mv" filename "../backups"
        cmd "rm" "-r" worldname
    backupType <- getTodaysBackupType
    timestamp <- fromEnum . utcTimeToPOSIXSeconds <$> getCurrentTime
    runMySQL mySQLConn $ do
        insert_ $ Backup timestamp filename backupType

cleanBackups :: BackupType -> IO ()
cleanBackups backupType = when (numBackups backupType >= 0) $ runMySQL mySQLConn $ do
    curNum <- DB.count ([] :: [Filter Backup])
    let diff = curNum - numBackups backupType
    when (diff > 0) $ do
        toDelete <- DB.selectList [] [Asc BackupTime, LimitTo diff]
        mapM_ DB.delete . fmap entityKey $ toDelete
        
        let filesToDelete = backupFile . entityVal <$> toDelete
        shelly $ do
            cd "backups"
            mapM_ (cmd "rm") filesToDelete

getTodaysBackupType :: IO BackupType
getTodaysBackupType = do
    day <- utctDay <$> getCurrentTime
    let (dayOfMonth, month, _) = toGregorian day
        (_, weekday) = mondayStartWeek day
    case (dayOfMonth, month, weekday) of
      (1, 1, _) -> pure Yearly
      (1, _, _) -> pure Monthly
      (_, _, 1) -> pure Weekly
      (_, _, _) -> pure Daily
