#!/bin/sh
# Computes a deterministic 12-char SHA1 fingerprint of the codebase
cd "${1:-.}" || exit 1
if   command -v sha1sum >/dev/null 2>&1; then H=sha1sum
elif command -v shasum  >/dev/null 2>&1; then H=shasum
elif command -v openssl >/dev/null 2>&1; then H="openssl dgst -sha1 -r"
else echo "NO-HASH-TOOL"; exit 0; fi
id=$(cat entrypoint.sh Dockerfile config/knobs.json R/*.R tools/*.R 2>/dev/null | $H | cut -c1-12)
case "$id" in
  [0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]) echo "$id" ;;
  *) echo "UNHASHABLE" ;;
esac
