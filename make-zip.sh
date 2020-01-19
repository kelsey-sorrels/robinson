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
PLATFORMS[linux-x64]='openjdk-14-ea+26_linux-x64_bin.tar.gz'
PLATFORMS[osx-x64]='openjdk-14-ea+26_osx-x64_bin.tar.gz'
PLATFORMS[windows-x64]='openjdk-14-ea+26_windows-x64_bin.zip'

declare -A JAVA_HOMES
JAVA_HOMES[linux-x64]='jvms/linux-x64/jdk-14/'
JAVA_HOMES[osx-x64]='jvms/osx-x64/jdk-14.jdk/Contents/Home/'
JAVA_HOMES[windows-x64]='jvms/windows-x64/jdk-14/'

#clean
find target -name 'robinson-*.zip' -delete
rm -rf target/robinson-*

function upload() {
    local file_path=$1
    local file_name=$(basename $file_path)
    if [[ $UPLOAD == 1 ]] ; then
      S3_URL=s3://aaron-santos.com.robinson/download/$file_name
      s3cmd put -P $file_path $S3_URL
      echo "Wrote $file_name"
    fi
}

MACOS_STUB_URL='https://raw.githubusercontent.com/tofi86/universalJavaApplicationStub/v3.0.4/src/universalJavaApplicationStub'
function osx-package-extra() {
    local target_path=$1
    local platform=$2
    local app_root=$target_path/../dmg/Robinson.app

    # Download universalJavaApplicationStub
    wget $MACOS_STUB_URL -nc -O "jvms/universalJavaApplicationStub"

    mkdir -p $app_root/Contents/{MacOS,Resources}
    cp -r $target_path/* $app_root/Contents/Resources

    # copy jlink image into Robinson.app/Contents/Java
    cp -r $target_path/../jlink $app_root/Contents/Java

    # copy univeralJavaApplicationStub into Robinson.app/Contents/MacOS
    cp jvms/universalJavaApplicationStub $app_root/Contents/MacOS/
    chmod +x $app_root/Contents/MacOS/universalJavaApplicationStub

    # copy Info.plist
    cp dev-resources/Info.plist $app_root/Contents/
    # create PkgInfo
    echo "APPL????" > $app_root/Contents/MacOS/PkgInfo
    # copy .icns to Resources...
    cp images/AppIcon.icns $app_root/Contents/Resources/
    # Create dmg
    genisoimage -V robinson -D -R -apple -no-pad -o target/robinson-$VERSION.dmg $target_path/../dmg 

    upload target/robinson-$VERSION.dmg
}

function windows-package-extra() {
    local target_path=$1
    local platform=$2

    # copy jvm image into target_path
    cp -r $target_path/../jlink/* $target_path

    # make exe launcher
    lein launch4j

    mv dev-resources/robinson.exe $target_path

    # make zip
    cd $target_path/..
    mv $(basename $target_path) robinson-$platform-$VERSION
    zip -r robinson-$platform-$VERSION.zip robinson-$platform-$VERSION
    cd -
    upload target/$platform/robinson-$platform-$VERSION.zip
}

function linux-package-extra() {
    local target_path=$1
    local platform=$2

    # copy jvm image into target_path
    cp -r $target_path/../jlink/* $target_path

    # cp run.sh
    cp dev-resources/run.sh $target_path

    # make tar.gz
    cd $target_path/..
    mv $(basename $target_path) robinson-$platform-$VERSION
    tar -zcvf robinson-$platform-$VERSION.tar.gz robinson-$platform-$VERSION
    cd -
    upload target/$platform/robinson-$platform-$VERSION.tar.gz
}

# Moves resouces into $target_path and then calls platform-specific bundling function
function package()
{
    local target_path=$1
    local platform=$2

    # Copy binary into dist
    cp "target/$platform-robinson.jar" $target_path/robinson.jar
        
    chmod +x $target_path/robinson.jar

    #copy resources
    echo $VERSION >> $target_path/VERSION
    cp LICENSE $target_path
    cp README.md $target_path
    cp PRIVACY.md $target_path
    mkdir -p $target_path/config/fonts
    cp config/*.edn $target_path/config/
    cp config/*.clj $target_path/config/
    cp config/fonts/boxy-16.edn $target_path/config/fonts/
    cp config/fonts/boxy-24.edn $target_path/config/fonts/
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
        osx-package-extra $target_path $platform
    elif [[ "$platform" == "windows-x64" ]]; then
        windows-package-extra $target_path $platform
    elif [[ "$platform" == "linux-x64" ]]; then
        linux-package-extra $target_path $platform
    else
        echo "Platform not recognized $platform"
    fi
}


if [[ $LEINUBERJAR == 1 ]]; then


    mkdir -p "jvms"
    # Download jdks
    for PLATFORM in "${!PLATFORMS[@]}"; do
        echo $PLATFORM
        FILENAME=${PLATFORMS[$PLATFORM]}
        URL="https://download.java.net/java/early_access/jdk14/26/GPL/$FILENAME"
        # Download jdk
        wget $URL -nc -O "jvms/$FILENAME"
        # Decompress jdk
        if [[ -d "jvms/$PLATFORM" ]];
        then
            echo "jvm: $PLATFORM already exists. Skipping decompress."
        else
            mkdir -p "jvms/$PLATFORM"
            if [[ "$FILENAME" == *"tar"* ]];
            then
                echo "Untar-ing $FILENAME"
                tar xvf "jvms/$FILENAME" --directory "jvms/$PLATFORM"
            else
                echo "Unzipping $FILENAME"
                unzip -o "jvms/$FILENAME" -d "jvms/$PLATFORM"
            fi
        fi
        
        if [[ ! -f "target/$PLATFORM-robinson.jar" ]]; then
            # Build standalone binary
            lein with-profile bin-$PLATFORM bin
        fi

        # build jlink image
        echo "lein with-profile $PLATFORM jlink init"
        export JAVA_HOME=${JAVA_HOMES[$PLATFORM]}
        echo "jlink JAVA_HOME=$JAVA_HOME"
        lein with-profile jlink-$PLATFORM jlink init

        mkdir -p target/$PLATFORM/dist
        #copy assets/conf to target dir
        package target/$PLATFORM/dist $PLATFORM


    done
fi

exit 1



