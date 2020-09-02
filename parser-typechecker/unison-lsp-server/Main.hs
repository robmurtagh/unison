{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Lens ((^.), use, uses, assign, modifying)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Except (ExceptT, throwE, catchE, runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, execStateT)
import           Data.Default (Default(def))
import           Data.Text (Text)
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import qualified Language.Haskell.LSP.Messages as LSP.Messages
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Utility as U
import           Lens.Family (LensLike')
import qualified Language.Haskell.LSP.Types.Lens as J
import qualified System.Log.Logger

------------------------------------------------------------------------------
-- Logger
------------------------------------------------------------------------------
-- if no filename is provided then logger is disabled, if input is string `[OUTPUT]` then log goes to stderr,
-- which then redirects inside VSCode to the output pane of the plugin.
setupLogger :: Maybe FilePath -> IO ()
setupLogger Nothing          = pure ()
setupLogger (Just "[OUTPUT]") = LSP.Core.setupLogger Nothing [] System.Log.Logger.DEBUG
setupLogger file              = LSP.Core.setupLogger file [] System.Log.Logger.DEBUG

------------------------------------------------------------------------------
-- ServerConfig - CURRENTLY UNUSED
------------------------------------------------------------------------------
-- The ServerConfig will be generated onInitialise, and then will be passed to every subsequent handler function as context
-- It is the `config` paramterising `InitializeCallbacks` datatype
-- https://hackage.haskell.org/package/haskell-lsp-0.22.0.0/docs/Language-Haskell-LSP-Core.html#t:InitializeCallbacks
data ServerConfig = ServerConfig
  { foo :: Bool
  } deriving Show

instance Default ServerConfig where
  def = ServerConfig { foo = False }

------------------------------------------------------------------------------
-- ServerState
------------------------------------------------------------------------------
-- The state which is shared between server callbacks
data ServerState = ServerState
  { _bar :: Bool
  , _lspFuncs :: LSP.Core.LspFuncs ServerConfig
  }

makeLenses ''ServerState


------------------------------------------------------------------------------
-- lspHandlers
------------------------------------------------------------------------------
-- Inside a handler we have access to the ServerState. The exception layer
-- allows us to fail gracefully, displaying a message to the user via the
-- "ShowMessage" mechanism of the lsp standard.
-- ExceptT is a Monad Transformer which adds exceptions to other monads
-- http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html#t:ExceptT
-- similarly StateT is a Monday Transformer which adds state to the inner Monad
-- type HandlerM = ExceptT (Severity, Text) (StateT ServerState IO)

data Severity = Error
              -- ^ Error displayed to the user.
              | Warning
              -- ^ Warning displayed to the user.
              | Info
              -- ^ Information displayed to the user.
              | Log
              -- ^ Log message, not displayed by default.

nullHandler ::  MVar ServerState -> a -> IO ()
nullHandler _ _ = do 
  -- liftIO $ U.logs "I've been hit 🙀"
  return ()

hoverHandler ::  MVar ServerState -> LSP.Core.Handler J.HoverRequest
hoverHandler state request = do
    -- let uri = request ^. J.params . J.textDocument . J.uri
    -- liftIO $ U.logs (show uri)
    let _range = Just $ J.Range (J.Position 1 1) (J.Position 6 14)
    let _contents = J.HoverContents $ J.unmarkedUpContent "lsp-hello"
    let hover = Just $ J.Hover{ .. }
    let response = LSP.Core.makeResponseMessage request hover
    lf <- readMVar state
    LSP.Core.sendFunc (_lspFuncs lf) $ LSP.Messages.RspHover response
  

lspHandlers :: MVar ServerState -> LSP.Core.Handlers
lspHandlers state = def {
          LSP.Core.initializedHandler                       = Just $ nullHandler state
        , LSP.Core.hoverHandler                             = Just $ hoverHandler state
        , LSP.Core.didOpenTextDocumentNotificationHandler   = Just $ nullHandler state
        , LSP.Core.didChangeTextDocumentNotificationHandler = Just $ nullHandler state
        , LSP.Core.didSaveTextDocumentNotificationHandler   = Just $ nullHandler state
        , LSP.Core.didCloseTextDocumentNotificationHandler  = Just $ nullHandler state
        , LSP.Core.cancelNotificationHandler                = Just $ nullHandler state
        , LSP.Core.responseHandler                          = Just $ nullHandler state
        , LSP.Core.documentFormattingHandler                = Just $ nullHandler state
        , LSP.Core.documentLinkHandler                      = Just $ nullHandler state
        , LSP.Core.completionHandler                        = Just $ nullHandler state
        }

------------------------------------------------------------------------------
-- Server Capabilities
------------------------------------------------------------------------------
-- Tells the LSP client to notify us about file changes. Handled behind the
-- scenes by haskell-lsp (in Language.Haskell.LSP.VFS); we don't handle the
-- corresponding notifications ourselves.
-- documented here: https://microsoft.github.io/language-server-protocol/specification#textDocument_synchronization
-- and here: https://github.com/alanz/haskell-lsp/blob/51c578f3559f462660d49e52bec1b3b73e6ad942/haskell-lsp-types/src/Language/Haskell/LSP/Types/DataTypesJSON.hs#L450
-- in the source code there is some weird '' syntax. this is Template Haskell
syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

-- Tells the LSP client what capabilities the server has
lspOptions :: LSP.Core.Options
lspOptions = def { LSP.Core.textDocumentSync = Just syncOptions }

run :: Maybe FilePath -> IO ()
run mlog = do
  liftIO $ U.logs "Log 1"
  setupLogger mlog
  state <- newEmptyMVar

  -- handle message of form: https://microsoft.github.io/language-server-protocol/specification#initialize
  -- { method: 'initialize', params: { processId: 1, rootUri: '...', initializationOptions: ... } }
  let onInitialConfiguration :: J.InitializeRequest -> Either Text ServerConfig
      onInitialConfiguration _ = Right def

  -- handle message of form: https://microsoft.github.io/language-server-protocol/specification#workspace_didChangeConfiguration
  -- { method: 'initialize', params: { processId: 1, rootUri: '...', initializationOptions: ... } }
  -- it looks like this can only be done if the Client has a 'Capability' 'DidChangeConfigurationClientCapabilities' with 'dynamicRegistration': true
  let onConfigurationChange :: J.DidChangeConfigurationNotification -> Either Text ServerConfig
      onConfigurationChange _ = Right def

  -- Callback that is called when the LSP server is started; makes the lsp
  -- state (LspFuncs) available to the message handlers through the `state` MVar.
  let onStartup :: LSP.Core.LspFuncs ServerConfig -> IO (Maybe J.ResponseError)
      onStartup lspFuncs = do
        putMVar state $ ServerState {_lspFuncs=lspFuncs, _bar=False}
        return Nothing

  _ <- LSP.Control.run (LSP.Core.InitializeCallbacks { LSP.Core.onInitialConfiguration=onInitialConfiguration, LSP.Core.onConfigurationChange=onConfigurationChange, LSP.Core.onStartup=onStartup })
                       (lspHandlers state)
                       lspOptions
                       Nothing
  return ()

-- Run the server
main :: IO ()
main = do
    run (Just "[OUTPUT]")