#!/usr/bin/env bash

set -e -o pipefail


username=$(whoami 2>/dev/null || echo "user-id-$(id -u)")

echo -n "((username $username) (hostname $(hostname)) (kernel $(uname -r)) (build_time \"$(date -u "+%Y-%m-%d %H:%M:%S")Z\") $1)"
