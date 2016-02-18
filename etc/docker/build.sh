#!/usr/bin/env bash
set -xe
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
docker build -t fpco/soh-site-base etc/docker/soh-site-base
stack image container "$@"
