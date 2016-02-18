#!/usr/bin/env bash
exec "$(dirname ${BASH_SOURCE[0]})/../common/devops-helpers/kubernetes/deploy_rc_helper.sh" \
     --app "soh-site" \
     --repo fpco/soh-site \
     --specdir "$(dirname "${BASH_SOURCE[0]}")" \
     --clusters ~/.kube/clusters/fpco-prod-us-east-1?/kubeconfig \
     "$@"
