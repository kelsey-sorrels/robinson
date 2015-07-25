#!/bin/bash

LEINUBERJAR=0
UPLOAD=0
while getopts "lu" opt;
do
    echo "Got opt $opt"
    case "$opt" in
    l) LEINUBERJAR=1;;
    u) UPLOAD=1;;
    esac
done

# keep track of the build version
VERSION=`date +'%Y.%m.%d-%H.%M.%S'`'-'`git rev-parse --short HEAD`

#clean
rm target/robinson-*.zip
#rm -rf target/robinson-*

if [[ $LEINUBERJAR == 1 ]]; then
  lein uberjar
fi

#setup target dir
mkdir -p target/robinson-$VERSION

#copy jar
cp target/robinson-0.0.1-SNAPSHOT-standalone.jar target/robinson-$VERSION/robinson.jar
chmod +x target/robinson-$VERSION/robinson.jar

#copy resources
echo $VERSION >> target/robinson-$VERSION/VERSION
cp -R config target/robinson-$VERSION/
cp -R data   target/robinson-$VERSION/
cp -R images target/robinson-$VERSION/
cp -R quests target/robinson-$VERSION/
mkdir     target/robinson-$VERSION/log
mkdir     target/robinson-$VERSION/save

# set configuration to upload saves
touch target/robinson-$VERSION/config/.feedbackparticipant

# make zip
cd target
zip -r robinson-$VERSION.zip robinson-$VERSION
cd -

echo "Wrote target/robinson-$VERSION.zip"

if [[ $UPLOAD == 1 ]] ; then
  s3cmd put target/robinson-$VERSION.zip s3://aaron-santos.com.robinson/download/zip/robinson-$VERSION.zip
fi


