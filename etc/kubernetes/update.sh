#!/usr/bin/env bash
set -xe
TEMPSPEC="$(mktemp /tmp/spec.yaml.XXXXXX)"
sed -E \
    -e "s@^( *name: )soh-site-prod-v[0-9]*\$@\1soh-site-prod-v$(date +%Y%m%d%H%M%S)@" \
    -e "s@^( *version: )v[0-9]*\$@\1v$(date +%Y%m%d%H%M%S)@" \
    "etc/kubernetes/soh-site-prod-rc.yaml" \
    | tee "$TEMPSPEC"
OLDRC="$(kubectl get replicationcontrollers -o name -l "app=soh-site-prod" | sed 's/.*\///')"
kubectl rolling-update "$OLDRC" -f "$TEMPSPEC"
rm -f "$TEMPSPEC"
