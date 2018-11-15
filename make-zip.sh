#!/bin/bash

LEINUBERJAR=0
UPLOAD=0
# keep track of the build version
VERSION=`date +'%Y.%m.%d-%H.%M.%S'`'-'`git rev-parse --short HEAD`

while getopts "luv:" opt;
do
    echo "Got opt $opt"
    case "$opt" in
    l) LEINUBERJAR=1;;
    u) UPLOAD=1;;
    v) VERSION=$OPTARG'-'`git rev-parse --short HEAD`;;
    esac
done

declare -A PLATFORMS
#PLATFORMS[linux-x64]='openjdk-11.0.1_linux-x64_bin.tar.gz'
PLATFORMS[osx-x64]='openjdk-11.0.1_osx-x64_bin.tar.gz'
#PLATFORMS[windows-x64]='openjdk-11.0.1_windows-x64_bin.zip'

declare -A JAVA_HOMES
JAVA_HOMES[linux-x64]='jvms/linux-x64/jdk-11.0.1/'
JAVA_HOMES[osx-x64]='jvms/osx-x64/jdk-11.0.1.jdk/Contents/Home/'
JAVA_HOMES[windows-x64]='jvms/windows-x64/jdk-11.0.1/'

#clean
rm target/robinson-*.zip
#rm -rf target/robinson-*

function upload() {
    local file_path=$1
    local file_name=$(basename $file_path)
    if [[ $UPLOAD == 1 ]] ; then
      S3_URL=s3://aaron-santos.com.robinson/download/$file_name
      s3cmd put -P $file_path $S3_URL
      echo "Wrote $file_name"
    fi
}
function osx-package-extra() {
    local target_path=$1
    local app_root=$target_path/../dmg/Robinson.app

    mkdir -p $app_root/Contents/{MacOS,Resources}
    cp -r $target_path/* $app_root/Contents/Resources
    # copy libjli.dylib
    cp  "${JAVA_HOMES[osx-x64]}../MacOS/libjli.dylib" $app_root/Contents/MacOS/
    # copy Info.plist
    cp dev-resources/Info.plist $app_root/Contents/
    # copy .icns to Resources...
    cp images/AppIcon $app_root/Contents/Resources/
    # Create dmg
    #genisoimage -V robinson -D -R -apple -no-pad -o target/robinson-$VERSION.dmg $target_path/../dmg 

    upload target/robinson-$VERSION.dmg
}

function windows-package-extra() {
    local target_path=$1

    # make exe launcher
    lein launch4j

    mv dev-resources/robinson.exe $target_path

    # make zip
    cd $target_path/..
    mv jlink robinson-$platform-$VERSION
    zip -r robinson-$platform-$VERSION.zip robinson-$platform-$VERSION
    cd -
    upload target/$platform/robinson-$platform-$VERSION.zip
}

function linux-package-extra() {
    # make zip
    cd $target_path/..
    mv jlink robinson-$platform-$VERSION
    zip -r robinson-$platform-$VERSION.zip robinson-$platform-$VERSION
    cd -
    upload target/$platform/robinson-$platform-$VERSION.zip
}

function package()
{
    local target_path=$1
    local platform=$2

    chmod +x $target_path/robinson.jar

    #copy resources
    echo $VERSION >> $target_path/VERSION
    cp LICENSE $target_path
    cp README.md $target_path
    cp PRIVACY.md $target_path
    cp -R config $target_path
    cp -R data   $target_path
    cp -R fonts  $target_path
    cp -R images $target_path
    cp -R quests $target_path
    mkdir     $target_path/log
    mkdir     $target_path/save

    # Move over production config settings
    rm $target_path/config/settings.edn
    mv $target_path/config/production-settings.edn $target_path/config/settings.edn

    # Move over fresh(empty) scores file
    rm $target_path/scores/scores.edn
    mv $target_path/scores/production-scores.edn $target_path/scores/scores.edn

    # Remove and userid file
    rm $target_path/config/.userid

    # set configuration to upload saves
    touch $target_path/config/.feedbackparticipant

    if [[ "$platform" == "osx-x64" ]]; then
        osx-package-extra $target_path
    elif [[ "$platform" == "windows-x64" ]]; then
        windows-package-extra $target_path
    elif [[ "$platform" == "linux-x64" ]]; then
        linux-package-extra $target_path
    else
        echo "Platform not recognized $platform"
    fi
}


PACKR_URL='https://oss.sonatype.org/content/repositories/snapshots/com/badlogicgames/packr/packr/2.1-SNAPSHOT/packr-2.1-20180720.193219-11-jar-with-dependencies.jar'

if [[ $LEINUBERJAR == 1 ]]; then


    mkdir "jvms"
    # Download Packr
    wget $PACKR_URL -nc -O "jvms/packr-2.1.jar"
    # Download jdks
    for PLATFORM in "${!PLATFORMS[@]}"; do
        echo $PLATFORM
        FILENAME=${PLATFORMS[$PLATFORM]}
        mkdir -p "jvms/$PLATFORM"
        URL="https://download.java.net/java/GA/jdk11/13/GPL/$FILENAME"
        # Download jdk
        wget $URL -nc -O "jvms/$PLATFORM/$FILENAME"
        # Decompress jdk
        if [[ -d "jvms/$PLATFORM" ]];
        then
            echo "jvm: $PLATFORM already exists. Skipping decompress."
        else
            if [[ "$FILENAME" == *"tar"* ]];
            then
                echo "Untar-ing $FILENAME"
                tar xvf "jvms/$PLATFORM/$FILENAME" --directory "jvms/$PLATFORM"
            else
                echo "Unzipping $FILENAME"
                unzip -o "jvms/$PLATFORM/$FILENAME" -d "jvms/$PLATFORM"
            fi
        fi
        
        if [[ ! -f "target/robinson-0.0.1-SNAPSHOT" ]]; then
            # Build standalone binary
            lein with-profile bin-$PLATFORM bin
        fi

        # build jlink image
        echo "lein with-profile $PLATFORM jlink init"
        export JAVA_HOME=${JAVA_HOMES[$PLATFORM]}
        echo "jlink JAVA_HOME=$JAVA_HOME"
        lein with-profile jlink-$PLATFORM jlink init

        # Copy binary into image
        cp "target/robinson-0.0.1-SNAPSHOT" target/$PLATFORM/jlink/robinson.jar
        
        #copy assets/conf to target dir
        package target/$PLATFORM/jlink $PLATFORM


    done
fi

exit 1



