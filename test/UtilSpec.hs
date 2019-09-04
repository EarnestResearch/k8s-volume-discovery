module UtilSpec (spec) where

import Import
import Util
import Test.Hspec
import qualified Kubernetes.OpenAPI as K8S
import qualified Network.AWS.EC2 as AWS
import RIO.Time
import Lens.Micro
import Data.Map.Strict

spec :: Spec
spec = do
  describe "extract volume info" $ do
    it "should return the missing tags if it doesn't have any required tags" $ extractVolumeInfo awsVol `shouldBe` Left (MissingRequiredTags "a" ["kubernetes.io/created-for/pv/name", "kubernetes.io/created-for/pvc/name", "kubernetes.io/created-for/pvc/namespace"])
    it "should return the missing tags if it doesn't have some required tags" $ extractVolumeInfo awsVolWithSomeRequiredTags `shouldBe` Left (MissingRequiredTags "a" ["kubernetes.io/created-for/pvc/namespace"])
    it "should match the volume if it has all the tags" $ extractVolumeInfo awsVolWithRequiredTags `shouldBe` Right volWithoutSC
    it "should match the volume if it has all the tags" $ extractVolumeInfo awsVolWithAllTags `shouldBe` Right vol
  describe "make persistent volume" $ do
    it "should match the pv" $ makePV vol `shouldBe` pv
    it "should match the pvc" $ makePVC vol `shouldBe` pvc

awsVol :: AWS.Volume
awsVol = 
  (AWS.volume 
    "f"
    (UTCTime (ModifiedJulianDay 58723) 0)
    False
    19
    ""
    AWS.VAvailable
    "a"
    AWS.GP2)

awsVolWithSomeRequiredTags :: AWS.Volume
awsVolWithSomeRequiredTags = 
  awsVol & AWS.vTags .~ [
      AWS.tag "kubernetes.io/created-for/pv/name" "b"
    , AWS.tag "kubernetes.io/created-for/pvc/name" "c"
  ]

awsVolWithRequiredTags :: AWS.Volume
awsVolWithRequiredTags = 
  awsVol & AWS.vTags .~ [
      AWS.tag "kubernetes.io/created-for/pv/name" "b"
    , AWS.tag "kubernetes.io/created-for/pvc/name" "c"
    , AWS.tag "kubernetes.io/created-for/pvc/namespace" "d"
  ]

awsVolWithAllTags :: AWS.Volume
awsVolWithAllTags = 
  awsVol
    & AWS.vTags .~ [
        AWS.tag "kubernetes.io/created-for/pv/name" "b"
      , AWS.tag "kubernetes.io/created-for/pvc/name" "c"
      , AWS.tag "kubernetes.io/created-for/pvc/namespace" "d"
      , AWS.tag "kubernetes.io/created-for/sc/name" "e"
    ]

volWithoutSC :: Volume
volWithoutSC = Volume "a" "b" "c" "d" Nothing "f" "19Gi"

vol :: Volume
vol = Volume "a" "b" "c" "d" (Just "e") "f" "19Gi"

pv :: K8S.V1PersistentVolume
pv = 
  K8S.mkV1PersistentVolume {
    K8S.v1PersistentVolumeMetadata = Just K8S.mkV1ObjectMeta {
      K8S.v1ObjectMetaName = Just "b"
    },
    K8S.v1PersistentVolumeSpec = Just K8S.mkV1PersistentVolumeSpec {
      K8S.v1PersistentVolumeSpecAccessModes = Just ["ReadWriteOnce"],
      K8S.v1PersistentVolumeSpecAwsElasticBlockStore = 
        Just ((K8S.mkV1AWSElasticBlockStoreVolumeSource "aws://f/a") {
          K8S.v1AWSElasticBlockStoreVolumeSourceFsType = Just "ext4"
        }),
      K8S.v1PersistentVolumeSpecStorageClassName = Just "e",
      K8S.v1PersistentVolumeSpecCapacity = Just $ singleton "storage" (K8S.Quantity "19Gi")
    }
  }

pvc :: K8S.V1PersistentVolumeClaim
pvc = 
  K8S.mkV1PersistentVolumeClaim {
    K8S.v1PersistentVolumeClaimMetadata = Just K8S.mkV1ObjectMeta {
      K8S.v1ObjectMetaName = Just "c"
    },
    K8S.v1PersistentVolumeClaimSpec = Just K8S.mkV1PersistentVolumeClaimSpec {
      K8S.v1PersistentVolumeClaimSpecVolumeName = Just "b",
      K8S.v1PersistentVolumeClaimSpecAccessModes = Just ["ReadWriteOnce"],
      K8S.v1PersistentVolumeClaimSpecResources = Just K8S.mkV1ResourceRequirements {
        K8S.v1ResourceRequirementsRequests = Just $ singleton "storage" (K8S.Quantity "19Gi")
      },
      K8S.v1PersistentVolumeClaimSpecStorageClassName = Just "e"
    }
  }