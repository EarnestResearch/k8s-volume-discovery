module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_k8s_volume_discovery
import Control.Monad.Trans.AWS
import Kubernetes.Client
import Kubernetes.OpenAPI

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_k8s_volume_discovery.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> strOption ( long "discovery-tag-key"
                 <> help "Tag key to discover"
                  )
       <*> option auto (long "discovery-tag-value" <> help "Tag value to discover")
       <*> strOption ( long "masterURI"
                 <> short 'u'
                 <> help "URI of the k8s master"
                  )
       <*> switch ( long "dry-run"
                 <> help "Dry run?"
                 )
        )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  env <- liftIO $ newEnv Discover
  kC <- newConfig
    & fmap (setMasterURI (optionsMasterURI options))
  tlsParams <- defaultTLSClientParams & fmap disableServerCertValidation
  mgr <- newManager tlsParams
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , awsEnv = env
          , k8sClientConfig = kC
          , k8sClientManager = mgr
          }
     in forever (runRIO app (withNewToken run) >> threadDelay 60000000)

withNewToken :: RIO App () -> RIO App ()
withNewToken inner = do
  tok <- readFileUtf8 "/var/run/secrets/kubernetes.io/serviceaccount/token"
  local (over k8sClientConfigL (setTokenAuth tok)) inner
