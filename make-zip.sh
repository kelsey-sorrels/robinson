#!/bin/sh

rm target/robinson-win.zip
rm -rf target/robinson-win
mkdir -p target/robinson-win

cp target/robinson-0.0.1-SNAPSHOT-standalone.jar target/robinson-win/robinson.jar
chmod +x target/robinson-win/robinson.jar

cp -R config target/robinson-win/
cp -R data   target/robinson-win/
cp -R images target/robinson-win/
cp -R quests target/robinson-win/
mkdir     target/robinson-win/log
mkdir     target/robinson-win/save

cd target
zip -r robinson-win robinson-win
cd -

