#!/bin/bash

VM_INSTALL_URL="http://files.pharo.org/script/ciNBCogVM.sh"
IMAGE_URL="https://ci.inria.fr/pharo-contribution/job/Pier3BookOnPharo20/lastSuccessfulBuild/artifact/Pier3BookOnPharo20.zip"

# stop the script if a single command fails
set -e

tempzip=$(mktemp imageXXXXX.zip)

# on mac os wget can be quite old and not recognizing --no-check-certificate
CERTCHECK="--no-check-certificate"
wget --help | grep -- "$CERTCHECK" 2>&1 > /dev/null || CERTCHECK=''

wget ${CERTCHECK} --output-document - "$VM_INSTALL_URL" | bash
wget ${CERTCHECK} --progress=bar:force --output-document="$tempzip" "$IMAGE_URL"

trap "rm '$tempzip'" EXIT

for f in $(zipinfo -1 "$tempzip"); do
    ext="${f##*.}"
    if [ "$ext" == image -o "$ext" == changes ]; then
        echo "Pharo.$ext"
        unzip -qp  "$tempzip" "$f" > "Pharo.$ext"
    fi
done
