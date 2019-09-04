module Types where

import RIO
import RIO.Process
import Control.Monad.Trans.AWS
import Kubernetes.OpenAPI.Core
import Network.HTTP.Client

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool,
    optionsClusterName :: !Text,
    optionsMasterURI :: !Text,
    optionsDryRun :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , awsEnv :: !Env
  , k8sClusterName :: !Text
  , k8sClientConfig :: !KubernetesClientConfig
  , k8sClientManager :: !Manager
  , dryRun :: !Bool
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

instance HasEnv App where
  environment = lens awsEnv (\x y -> x { awsEnv = y })

class HasClusterName env where 
  clusterNameL :: Lens' env Text
instance HasClusterName App where
  clusterNameL = lens k8sClusterName (\x y -> x { k8sClusterName = y})

class HasK8S env where 
  k8sClientConfigL :: Lens' env KubernetesClientConfig
  k8sClientManagerL :: Lens' env Manager
instance HasK8S App where
  k8sClientConfigL = lens k8sClientConfig (\x y -> x { k8sClientConfig = y})
  k8sClientManagerL = lens k8sClientManager (\x y -> x { k8sClientManager = y})

class HasDryRun env where 
  dryRunL :: Lens' env Bool
instance HasDryRun App where
  dryRunL = lens dryRun (\x y -> x { dryRun = y})

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
 