#!/usr/bin/env bash
# See description at https://github.com/fpco/devops-helpers#wrappers
set -xe
exec "$(dirname ${BASH_SOURCE[0]})/../common/devops-helpers/kubernetes/deploy_rc_helper.sh" \
     --app "soh-site" \
     --repo fpco/soh-site \
     --specdir "$(dirname "${BASH_SOURCE[0]}")" \
     "$@"
