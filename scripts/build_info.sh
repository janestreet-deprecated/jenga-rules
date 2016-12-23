#!/usr/bin/env bash

set -e -o pipefail


username=$(whoami 2>/dev/null || echo "user-id-$(id -u)")

echo -n "((username $username) (hostname $(hostname)) (kernel $(uname -r)) (build_date $(date -u +%Y-%m-%d)) (build_time $(date -u +%H:%M:%S)) $1)"
