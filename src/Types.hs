{-# LANGUAGE TemplateHaskell #-}
module Types where

import Database.Persist.TH

data BackupType = Daily | Weekly | Monthly | Yearly
    deriving (Show, Read, Eq)

derivePersistField "BackupType"
