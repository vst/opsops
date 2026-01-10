#!/usr/bin/env bash

## NOTE: Things would be much easier if we could use Nix, but we can
## not (or I find it rather tedious). So, we have to use Docker and
## Haskell Stack to build our static binary.

## Executable name:
EXECUTABLE_NAME="$(yq ".executables | keys | .[0]" package.yaml)"

## Stackage resolver:
STACKAGE_RESOLVER="$(yq ".resolver" stack.yaml)"

## GHC version:
GHC_VERSION="$(curl -s "https://www.stackage.org/${STACKAGE_RESOLVER}" | grep -oP 'ghc-\K[0-9.]+' | head -n1)"

## Docker image:
DOCKER_IMAGE="quay.io/benz0li/ghc-musl:${GHC_VERSION}"

## Final executable name:
FINAL_EXECUTABLE_NAME="${EXECUTABLE_NAME}-static-$(uname --kernel-name | tr '[:upper:]' '[:lower:]')-$(uname --machine)"

## Final executable path:
FINAL_EXECUTABLE_PATH="/tmp/${FINAL_EXECUTABLE_NAME}"

## Docker container name:
CONTAINER_NAME="static-builder-for-${EXECUTABLE_NAME}"

## Run the Docker container:
docker run -i --detach -v "$(pwd):/app" --name "${CONTAINER_NAME}" "${DOCKER_IMAGE}" /bin/bash

## Whitelist codebase directory for Git queries:
docker exec "${CONTAINER_NAME}" git config --global --add safe.directory /app

## Cleanup inside the container:
docker exec -w "/app" "${CONTAINER_NAME}" cabal clean
docker exec -w "/app" "${CONTAINER_NAME}" cabal v1-clean
docker exec -w "/app" "${CONTAINER_NAME}" stack clean --full

## Build the static binary:
docker exec -w "/app" "${CONTAINER_NAME}" stack build

## Install the static binary to our local-bin-path (/tmp):
docker exec -w "/app" "${CONTAINER_NAME}" stack install

## Install upx:
docker exec -w "/app" "${CONTAINER_NAME}" apk add upx

## Compress the executable:
docker exec -w "/app" "${CONTAINER_NAME}" upx "/tmp/${EXECUTABLE_NAME}"

## Copy the binary to the host:
docker cp "${CONTAINER_NAME}:/tmp/${EXECUTABLE_NAME}" "${FINAL_EXECUTABLE_PATH}"

## Cleanup:
docker exec -w "/app" "${CONTAINER_NAME}" stack clean --full
docker rm -f "${CONTAINER_NAME}"
file "${FINAL_EXECUTABLE_PATH}"
find "${FINAL_EXECUTABLE_PATH}"
