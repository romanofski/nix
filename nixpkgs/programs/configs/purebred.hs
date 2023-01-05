-- This file is part of purebred
-- Copyright (C) 2023 Róman Joost
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-
Example configuration, currently used for testing which demonstrates various
ways to overwrite the configuration.
-}
import Control.Monad.IO.Class (liftIO)
import Purebred
import qualified Purebred.Plugin.ICU
import qualified Data.ByteString as B
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory)
import Data.Maybe (fromMaybe)
import Data.List (union)
import Data.List.NonEmpty (NonEmpty(..), fromList)

import Data.MIME (matchContentType)

myBrowseThreadsKbs :: [Keybinding 'Threads 'ListOfThreads]
myBrowseThreadsKbs =
  [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"])
  , Keybinding (EvKey (KChar 'S') []) (setTags [RemoveTag "inbox", AddTag "spam"])
  ]

myMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView]
myMailKeybindings =
    [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"])
    ]

fromMail :: [Mailbox]
fromMail =
    [ Mailbox
          (Just "Roman Joost")
          (AddrSpec "roman" (DomainDotAtom $ "bromeco" :| ["de"]))
    ]

addressRetriever :: FilePath -> AddressBookSettings
addressRetriever aliasfile = AddressBookSettings {
          _abRetrievers = [
            AddressRetriever
            { _runAddressRetriever = runMuttAliasRetriever aliasfile }
          ]
        }

main :: IO ()
main = do
  cwd <- getCurrentDirectory

  addressBook <-
    initMuttAliasFileAddressBook "/home/rjoost/.config/purebred/alias"
    >>= either (die . show) pure

  purebred
    [ usePlugin . tweakConfig $
          over (confIndexView . ivBrowseThreadsKeybindings) (`union` myBrowseThreadsKbs)
          . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
          . set (confComposeView . cvIdentities) fromMail
          . set (confComposeView . cvSendMailCmd) (sendmail "/home/rjoost/.nix-profile/bin/msmtp")
          . over confTheme (applyAttrMappings myColoredTags)
          . set confAddressBook [addressBook]
          . Purebred.Plugin.ICU.enable
    , usePlugin $ tweakConfigWithIO $ \conf -> do
      pure $ set (confFileBrowserView . fbHomePath) cwd conf
    ]
  where
    tweak cwd =
      over (confIndexView . ivBrowseThreadsKeybindings) (`union` myBrowseThreadsKbs)
      . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
      . set (confComposeView . cvIdentities) fromMail
      . set (confComposeView . cvSendMailCmd) (sendmail "/home/rjoost/.nix-profile/bin/msmtp")
      . over confTheme (applyAttrMappings myColoredTags)
      . set confAddressBook (addressRetriever "/home/rjoost/.config/purebred/alias")
      . Purebred.Plugin.ICU.enable

myColoredTags :: [(AttrName, Attr)]
myColoredTags =
  [ (mailTagAttr <> attrName "inbox", fg blue)
  , (mailTagAttr <> attrName "archive", fg cyan)
  , (mailTagAttr <> attrName "signed", fg green)
  , (mailTagAttr <> attrName "attachment", fg magenta)
  ]
