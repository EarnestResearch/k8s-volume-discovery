DOCKER_BASE_IMAGE?=earnestresearch/k8s-volume-discovery
VERSION=0.1.0
AWS_CLUSTER_NAME?=example
DHALL_TO_YAML = @AWS_CLUSTER_NAME=$(AWS_CLUSTER_NAME) dhall-to-yaml --omitEmpty <<< ./k8s/app.dhall

build:
	stack build --fast

test:
	stack test --fast

build-docker-image:
	docker pull ${DOCKER_BASE_IMAGE}-dependencies:$(VERSION) || true
	docker build --target dependencies --cache-from ${DOCKER_BASE_IMAGE}-dependencies:$(VERSION) -t ${DOCKER_BASE_IMAGE}-dependencies:$(VERSION) .
	docker build --target app --cache-from ${DOCKER_BASE_IMAGE}-dependencies:$(VERSION) -t ${DOCKER_BASE_IMAGE}:$(VERSION) .
	docker push ${DOCKER_BASE_IMAGE}-dependencies:$(VERSION)
	docker tag ${DOCKER_BASE_IMAGE}-dependencies:$(VERSION) ${DOCKER_BASE_IMAGE}-dependencies:latest
	docker push ${DOCKER_BASE_IMAGE}-dependencies:latest

push-docker-image: build-docker-image
	docker push ${DOCKER_BASE_IMAGE}:$(VERSION)
	docker tag ${DOCKER_BASE_IMAGE}:$(VERSION) ${DOCKER_BASE_IMAGE}:latest
	docker push ${DOCKER_BASE_IMAGE}:latest

dhall-format:
	find k8s -name '*.dhall' | xargs -n 1 dhall --ascii lint --inplace

dhall-to-yaml:
	$(DHALL_TO_YAML)

deploy: 
	$(DHALL_TO_YAML) | kubectl apply -n kube-system -f -

.PHONY: build test build-docker-image push-docker-image dhall-format dhall-to-yaml deploy
