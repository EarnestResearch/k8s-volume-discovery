let defaults =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/4ad58156b7fdbbb6da0543d8b314df899feca077/defaults.dhall sha256:4450e23dc81975d111650e06c0238862944bf699537af6cbacac9c7e471dfabe

let types =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/4ad58156b7fdbbb6da0543d8b314df899feca077/types.dhall sha256:e48e21b807dad217a6c3e631fcaf3e950062310bfb4a8bbcecc330eb7b2f60ed

let config = ./config.dhall

let appLabel = { mapKey = "app", mapValue = config.appName }

let appMetadata =
      defaults.ObjectMeta // { name = config.appName, labels = [ appLabel ] }

let container =
          defaults.Container
      //  { name =
              config.appName
          , image =
              Some
              "earnestresearch/k8s-volume-discovery:0.2.1"
          , command =
              [ "./k8s-volume-discovery-exe"
              , "--masterURI"
              , "https://kubernetes.default.svc"
              , "--discovery-tag-key"
              , "kubernetes.io/cluster/${env:AWS_CLUSTER_NAME as Text}"
              , "-v"
              ]
          , resources =
            Some
             { requests =
              [
                {  mapKey = "cpu"
                ,  mapValue = "200m"
                }
              ]
              , limits =
              [
                { mapKey = "cpu"
                , mapValue  = "300m"
                }
              ]
            }
          }

let serviceAccount = ./serviceaccount.dhall

in        defaults.Deployment
      //  { metadata =
              appMetadata
          , spec =
              Some
              (     defaults.DeploymentSpec
                //  { template =
                            defaults.PodTemplateSpec
                        //  { metadata =
                                    appMetadata
                                //  { annotations =
                                        [ { mapKey =
                                              "iam.amazonaws.com/role"
                                          , mapValue =
                                              config.role
                                          }
                                        ]
                                    }
                            , spec =
                                Some
                                (     defaults.PodSpec
                                  //  { containers =
                                          [ container ]
                                      , serviceAccountName =
                                          Some serviceAccount.metadata.name
                                      }
                                )
                            }
                    , selector =
                        defaults.LabelSelector // { matchLabels = [ appLabel ] }
                    }
              )
          }
    : types.Deployment
