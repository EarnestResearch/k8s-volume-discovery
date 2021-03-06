let defaults =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/4ad58156b7fdbbb6da0543d8b314df899feca077/defaults.dhall sha256:4450e23dc81975d111650e06c0238862944bf699537af6cbacac9c7e471dfabe

let types =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/4ad58156b7fdbbb6da0543d8b314df899feca077/types.dhall sha256:e48e21b807dad217a6c3e631fcaf3e950062310bfb4a8bbcecc330eb7b2f60ed

let config = ./config.dhall

in        defaults.ClusterRole
      //  { metadata =
              defaults.ObjectMeta // { name = config.appName }
          , rules =
              [     defaults.PolicyRule
                //  { apiGroups =
                        [ "" ]
                    , resources =
                        [ "persistentvolumes", "persistentvolumeclaims" ]
                    , verbs =
                        [ "get", "list", "create" ]
                    }
              ]
          }
    : types.ClusterRole
