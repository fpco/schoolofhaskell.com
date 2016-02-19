#!/usr/bin/env bash
# See description at https://github.com/fpco/devops-helpers#wrappers
set -xe
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
docker build -t fpco/soh-site-base etc/docker/soh-site-base
stack image container "$@"
