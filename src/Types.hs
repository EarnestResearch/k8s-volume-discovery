module Types where

import RIO
import RIO.Process
import Control.Monad.Trans.AWS
import Kubernetes.OpenAPI.Core
import Network.HTTP.Client

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool,
    optionsDiscoveryTagKey :: !Text,
    optionsDiscoveryTagValue :: !(Maybe Text),
    optionsMasterURI :: !Text,
    optionsDryRun :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , awsEnv :: !Env
  , k8sClientConfig :: !KubernetesClientConfig
  , k8sClientManager :: !Manager
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

instance HasEnv App where
  environment = lens awsEnv (\x y -> x { awsEnv = y })

class HasK8S env where 
  k8sClientConfigL :: Lens' env KubernetesClientConfig
  k8sClientManagerL :: Lens' env Manager
instance HasK8S App where
  k8sClientConfigL = lens k8sClientConfig (\x y -> x { k8sClientConfig = y})
  k8sClientManagerL = lens k8sClientManager (\x y -> x { k8sClientManager = y})

class HasOptions env where
  optionsL :: Lens' env Options
  dryRunL :: Lens' env Bool
  discoveryTagKeyL :: Lens' env Text
  discoveryTagValueL :: Lens' env (Maybe Text)

instance HasOptions App where
  optionsL = lens appOptions (\x y -> x { appOptions = y})
  dryRunL = optionsL . lens optionsDryRun (\x y -> x { optionsDryRun = y})
  discoveryTagKeyL = optionsL . lens optionsDiscoveryTagKey (\x y -> x { optionsDiscoveryTagKey = y})
  discoveryTagValueL = optionsL . lens optionsDiscoveryTagValue (\x y -> x { optionsDiscoveryTagValue = y})

data Volume = Volume {
    ebsVolumeId :: !Text
  , persistentVolumeName :: !Text
  , persistentVolumeClaimName :: !Text
  , persistentVolumeClaimNamespace :: !Text
  , storageClassName :: !(Maybe Text)
  , availabilityZone :: !Text
  , capacity :: !Text
} deriving (Show, Eq)

newtype TagMissing = TagMissing Text
data VolumeNotParsedReason = MissingRequiredTags Text [Text] deriving (Show, Eq)
 