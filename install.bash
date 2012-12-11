#!/bin/bash

IMAGE_URL="https://ci.inria.fr/rmod/job/Pier3BookOnPharo20/lastSuccessfulBuild/artifact/Pier3BookOnPharo20.zip"


# stop the script if a single command fails
set -e

wget --quiet -qO - http://pharo.gforge.inria.fr/ci/ciNBCog.sh | bash
wget --progress=bar:force --output-document=image.zip $IMAGE_URL


IMAGE_DIR="image"
mkdir $IMAGE_DIR

unzip -q -d $IMAGE_DIR image.zip

# find the image name
PHARO_IMAGE=`find $IMAGE_DIR -name *.image`
PHARO_CHANGES=`find $IMAGE_DIR -name *.changes`

# rename
mv "$PHARO_IMAGE" Pharo.image
mv "$PHARO_CHANGES" Pharo.changes

rm -rf image image.zip

echo Pharo.image
echo Pharo.changes
