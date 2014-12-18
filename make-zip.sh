#!/bin/sh

#clean
rm target/robinson-win.zip
rm -rf target/robinson-win
mkdir -p target/robinson-win

# keep track of the build version
git rev-parse HEAD >> target/robinson-win/VERSION

#copy jar
cp target/robinson-0.0.1-SNAPSHOT-standalone.jar target/robinson-win/robinson.jar
chmod +x target/robinson-win/robinson.jar

#copy resources
cp -R config target/robinson-win/
cp -R data   target/robinson-win/
cp -R images target/robinson-win/
cp -R quests target/robinson-win/
mkdir     target/robinson-win/log
mkdir     target/robinson-win/save

# set configuration to upload saves
touch target/robinson-win/config/.feedbackparticipant

# make zip
cd target
zip -r robinson-win robinson-win
cd -

