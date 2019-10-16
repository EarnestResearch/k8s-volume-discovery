module Run (run) where

import Import
import Util
import Control.Monad.Trans.AWS
import qualified Network.AWS.EC2 as AWS
import qualified Kubernetes.OpenAPI as K8S
import Kubernetes.OpenAPI.Core ()
import qualified Kubernetes.OpenAPI.API.CoreV1 as CoreV1
import Lens.Micro
import RIO.Orphans ()
import qualified RIO.Set as Set

run :: RIO App ()
run = do
    logInfo "Starting main loop"
    logDebug "AWS Volumes"
    volumes <- getVolumes
    logDebug (displayShow volumes)
    logDebug "K8S PersistentVolumes"
    persistentVolumes <- listPersistentVolumes
    logDebug (displayShow persistentVolumes)
    case persistentVolumes of
      Left e -> 
        logError (displayShow e)
      Right alreadyCreated -> do
        let volumeNames =
              Set.fromList $ mapMaybe (view persistentVolumeNamesL) (K8S.v1PersistentVolumeListItems alreadyCreated)
            toCreate =
              filter (\v -> persistentVolumeName v `notElem` volumeNames) volumes
        logInfo "Creating new persistent volumes"
        traverse_ createPersistentVolume toCreate
    logDebug "K8S PersistentVolumeClaims"
    persistentVolumeClaims <- listPersistentVolumeClaims
    logDebug (displayShow persistentVolumeClaims)   
    case persistentVolumeClaims of
      Left e -> 
        logError (displayShow e)
      Right alreadyCreated -> do
        let volumeClaimsNames =
              Set.fromList $ mapMaybe (view persistentVolumeClaimNamesL) (K8S.v1PersistentVolumeClaimListItems alreadyCreated)
            toCreate = 
              filter (\v -> persistentVolumeClaimName v `notElem` volumeClaimsNames) volumes
        logInfo "Creating new persistent volume claims"
        traverse_ createPersistentVolumeClaim toCreate
  where
    persistentVolumeNamesL = 
      K8S.v1PersistentVolumeMetadataL 
      . traverse 
      . K8S.v1ObjectMetaNameL
    persistentVolumeClaimNamesL = 
      K8S.v1PersistentVolumeClaimMetadataL 
      . traverse 
      . K8S.v1ObjectMetaNameL

getVolumes :: RIO App [Volume]
getVolumes = do
  k <- view discoveryTagKeyL
  v <- view discoveryTagValueL 

  let 
    tagFilter = 
      maybe (AWS.filter' "tag-key" & AWS.fValues .~ [k])
      (\x -> AWS.filter' ("tag:"<>k) & AWS.fValues .~ [x])
      v
    filters = 
      [
        tagFilter,
        AWS.filter' "status"  & AWS.fValues .~ ["available"]
      ]
  awsResponse <- runResourceT $ send $ AWS.describeVolumes & AWS.desFilters .~ filters
  let 
    volumes = view AWS.dvvrsVolumes awsResponse
    extractedVolumes = fmap extractVolumeInfo volumes
    parsedVolumes = rights extractedVolumes
    notParsedVolumes = lefts extractedVolumes

  traverse_ logVolumeNotParsed notParsedVolumes

  pure parsedVolumes

logVolumeNotParsed :: VolumeNotParsedReason -> RIO App ()
logVolumeNotParsed reason = case reason of
  MissingRequiredTags volumeId tags ->
    logError $ "Volume " <> displayShow volumeId <> "not parsed due to missing required tags: " <> displayShow tags

listPersistentVolumes :: RIO App (Either K8S.MimeError K8S.V1PersistentVolumeList)
listPersistentVolumes =
  dispatchMime $ CoreV1.listPersistentVolume (K8S.Accept K8S.MimeJSON)

listPersistentVolumeClaims :: RIO App (Either K8S.MimeError K8S.V1PersistentVolumeClaimList)
listPersistentVolumeClaims = 
  dispatchMime $ CoreV1.listPersistentVolumeClaimForAllNamespaces  (K8S.Accept K8S.MimeJSON)

createPersistentVolume :: Volume -> RIO App (Either K8S.MimeError K8S.V1PersistentVolume)
createPersistentVolume vol = do
  dryRun' <- view dryRunL
  let pv = makePV vol
  let 
    req =
      CoreV1.createPersistentVolume 
        (K8S.ContentType K8S.MimeJSON)
        (K8S.Accept K8S.MimeJSON)
        pv
    dryReq =
      if dryRun' then req `K8S.applyOptionalParam` K8S.DryRun "All" else req
  result <- dispatchMime dryReq
  case result of
    Left e ->
      logError (displayShow e)
    Right _ ->
      logInfo $ "Persistent volume " <> displayShow (persistentVolumeName vol) <> " create request sent."
  logDebug (displayShow pv)
  pure result


createPersistentVolumeClaim :: Volume -> RIO App (Either K8S.MimeError K8S.V1PersistentVolumeClaim)
createPersistentVolumeClaim vol = do
  dryRun' <- view dryRunL
  let 
    pvc = makePVC vol
    namespace = K8S.Namespace $ persistentVolumeClaimNamespace vol 
    req =
      CoreV1.createNamespacedPersistentVolumeClaim
        (K8S.ContentType K8S.MimeJSON)
        (K8S.Accept K8S.MimeJSON)
        pvc
        namespace
    dryReq =
      if dryRun' then req `K8S.applyOptionalParam` K8S.DryRun "All" else req
  result <- dispatchMime dryReq
  case result of
    Left e ->
      logError (displayShow e)
    Right _ ->
      logInfo $ "Persistent volume claim " <> displayShow (persistentVolumeClaimName vol) <> " create request sent."
  logDebug (displayShow pvc)
  pure result