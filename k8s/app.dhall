let k8s =
      https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/4ad58156b7fdbbb6da0543d8b314df899feca077/typesUnion.dhall sha256:8e8db456b218b93f8241d497e54d07214b132523fe84263e6c03496c141a8b18

in  { apiVersion =
        "v1"
    , kind =
        "List"
    , items =
        [ k8s.Deployment ./deployment.dhall
        , k8s.ServiceAccount ./serviceaccount.dhall
        , k8s.ClusterRole ./clusterrole.dhall
        , k8s.RoleBinding ./clusterrolebinding.dhall
        ]
    }
