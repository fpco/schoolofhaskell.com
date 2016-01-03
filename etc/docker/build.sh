#!/usr/bin/env bash
set -xe
cd "$(dirname "$0")/../.."
docker build -t fpco/soh-site-base etc/docker/
stack --docker --docker-auto-pull image container
[[ "$1" == "--push" ]] && docker push fpco/soh-site
