# k8s-volume-discovery

## What is k8s-volume-discovery
k8s-volume-discovery is a Kubernetes operator that discovers EBS volumes in AWS
tagged with the cluster identifiers and attaches them to the cluster in the form of 
PersistentVolume and PersistentVolumeClaim.

## Why k8s-volume-discovery?
When rebuilding a Kubernetes cluster (e.g. in case of disaster), we might want to reattach the volumes that were
attached to the previous cluster. This operator allows for an easy and automatic volume recovery.
The cluster will automatically rediscover the volumes and attach them.

## Deployment
The operator deployment files are expressed in `dhall` for type safety and customizability.
To apply it to your cluster you need to install the `dhall-to-yaml` tool (inside `dhall-json`). 
See https://github.com/dhall-lang/dhall-haskell#pre-built-binaries for more information on installing it.

Once you have dhall-to-yaml installed, you can apply the files to your cluster by doing
```sh
export AWS_CLUSTER_NAME=example #set it to your cluster name
make deploy
```

## IAM Role
By default, the operator tries to assume a IAM role called `App-k8s-volume-discovery` using kube2iam or kiam.
You can rename the role by changing `k8s/config.dhall`.
The role needs to have the `ec2:DescribeVolumes` permission.

## Tags
k8s-volume-discovery operates by retrieving the AWS EBS volumes and using the information that Kubernetes puts in the tags.
k8s-volume-discovery will only consider volumes with the `kubernetes.io/cluster/{clusterName}` tag that are in the `available` state.

The following tags are required for k8s-volume-discovery to add the EBS volume to Kubernetes:
- `kubernetes.io/created-for/pv/name`
- `kubernetes.io/created-for/pvc/name`
- `kubernetes.io/created-for/pvc/namespace`
These tags are usually automatically created by Kubernetes when creating PersistentVolumes/PersistentVolumeClaims.

If any of these tags is missing, the volume is not added to the cluster.

Optionally, users can add the tag `kubernetes.io/created-for/sc/name` to signal which storage class should be assigned to the volume.
To add the storage class tag, we recommend using [k8s-metadata-injector](https://github.com/almariah/k8s-metadata-injector).

## Development
The application is developed in `Haskell`. To build the project you need [stack](https://docs.haskellstack.org/en/stable/README/).
Run `make build` to build the project locally.
Run `make test` to test the project locally.

You can deploy a `dry run` version of the project by adding the `--dry-run` flag to the deployment.

To build a new docker image, run `make build-docker-image`.

## Maintainers
The project is actively maintained but it's in alpha stage, expect breaking changes.
