module Util where

import Import
import qualified Network.AWS.EC2 as AWS
import qualified Kubernetes.OpenAPI as K8S
import Lens.Micro
import Lens.Micro.Extras (preview)
import Data.Foldable
import Data.Map.Strict

extractVolumeInfo :: AWS.Volume -> Either VolumeNotParsedReason Volume
extractVolumeInfo v = do 
  let 
    tags = view AWS.vTags v
    volumeName = readTag "kubernetes.io/created-for/pv/name" tags
    pvcName = readTag "kubernetes.io/created-for/pvc/name" tags
    pvcNamespace = readTag "kubernetes.io/created-for/pvc/namespace" tags
    scName = readTag "kubernetes.io/created-for/sc/name" tags
    volumeId = view AWS.vVolumeId v
  
  case (volumeName, pvcName, pvcNamespace) of 
    (Right pv, Right pvcN, Right pvcNs) ->
      pure $ Volume {
          ebsVolumeId = volumeId,
          persistentVolumeName = pv,
          persistentVolumeClaimName = pvcN,
          persistentVolumeClaimNamespace = pvcNs,
          storageClassName = preview _Right scName,
          availabilityZone = view AWS.vAvailabilityZone v,
          capacity = textDisplay (view AWS.vSize v) <> "Gi"
        }
    (pv, pvcN, pvcNs) ->
      let xs = (\(TagMissing x) -> x) <$> lefts [pv, pvcN, pvcNs] in
        Left $ MissingRequiredTags volumeId xs

readTag :: Text -> [AWS.Tag] -> Either TagMissing Text
readTag tagKey tags =
    view AWS.tagValue <$> eitherTag
  where
    eitherTag :: Either TagMissing AWS.Tag
    eitherTag = maybe (Left $ TagMissing tagKey) Right $ find (\t -> view AWS.tagKey t == tagKey) tags

makePV :: Volume -> K8S.V1PersistentVolume
makePV vol = 
  K8S.mkV1PersistentVolume 
    & volumeNameL ?~ persistentVolumeName vol
    & accessModesL ?~ ["ReadWriteOnce"]
    & volumeFsTypeL ?~ "ext4"
    & storageClassNameL .~ storageClassName vol
    & volumeCapacityL ?~ singleton "storage" (K8S.Quantity $ capacity vol)
  where
    volumeNameL = 
      K8S.v1PersistentVolumeMetadataL
        . non K8S.mkV1ObjectMeta
        . K8S.v1ObjectMetaNameL
    accessModesL = 
      K8S.v1PersistentVolumeSpecL 
        . non K8S.mkV1PersistentVolumeSpec
        . K8S.v1PersistentVolumeSpecAccessModesL
    volumeId = 
      "aws://" <> availabilityZone vol <> "/" <> ebsVolumeId vol
    volumeSource = 
      K8S.mkV1AWSElasticBlockStoreVolumeSource volumeId
    volumeFsTypeL = 
      K8S.v1PersistentVolumeSpecL
        . non K8S.mkV1PersistentVolumeSpec
        . K8S.v1PersistentVolumeSpecAwsElasticBlockStoreL
        . non volumeSource
        . K8S.v1AWSElasticBlockStoreVolumeSourceFsTypeL
    storageClassNameL = 
      K8S.v1PersistentVolumeSpecL 
        . non K8S.mkV1PersistentVolumeSpec
        . K8S.v1PersistentVolumeSpecStorageClassNameL
    volumeCapacityL = 
      K8S.v1PersistentVolumeSpecL 
        . non K8S.mkV1PersistentVolumeSpec
        . K8S.v1PersistentVolumeSpecCapacityL
    
makePVC :: Volume -> K8S.V1PersistentVolumeClaim
makePVC vol =
    K8S.mkV1PersistentVolumeClaim
      & pvcNameL ?~ persistentVolumeClaimName vol
      & pvcVolumeNameL ?~ persistentVolumeName vol
      & accessModesL ?~ ["ReadWriteOnce"]
      & storageL ?~ singleton "storage" (K8S.Quantity $ capacity vol)
      & storageClassNameL .~ storageClassName vol
  where
    pvcNameL = K8S.v1PersistentVolumeClaimMetadataL
      . non K8S.mkV1ObjectMeta
      . K8S.v1ObjectMetaNameL
    pvcVolumeNameL = K8S.v1PersistentVolumeClaimSpecL
      . non K8S.mkV1PersistentVolumeClaimSpec
      . K8S.v1PersistentVolumeClaimSpecVolumeNameL
    accessModesL = 
      K8S.v1PersistentVolumeClaimSpecL 
        . non K8S.mkV1PersistentVolumeClaimSpec
        . K8S.v1PersistentVolumeClaimSpecAccessModesL
    storageL = K8S.v1PersistentVolumeClaimSpecL 
      . non K8S.mkV1PersistentVolumeClaimSpec
      . K8S.v1PersistentVolumeClaimSpecResourcesL
      . non K8S.mkV1ResourceRequirements
      . K8S.v1ResourceRequirementsRequestsL
    storageClassNameL = 
      K8S.v1PersistentVolumeClaimSpecL 
        . non K8S.mkV1PersistentVolumeClaimSpec
        . K8S.v1PersistentVolumeClaimSpecStorageClassNameL

dispatchMime :: (HasK8S env, K8S.Produces req accept, K8S.MimeUnrender accept res, K8S.MimeType contentType) 
             => K8S.KubernetesRequest req contentType res accept -> RIO env (Either K8S.MimeError res)
dispatchMime req = do 
  conf <- view k8sClientConfigL
  mgr <- view k8sClientManagerL
  liftIO $ K8S.dispatchMime' mgr conf req