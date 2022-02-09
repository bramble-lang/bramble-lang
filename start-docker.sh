#!/bin/sh

image=fedora

while getopts i: flag
do
    case "${flag}" in
        i) image=${OPTARG};;
    esac
done

# check for the existence of the fedora image
echo "Check if bramble-${image} exists"
not_exist=$(docker image inspect bramble-${image}:latest)
result=$?
if [ $result -ne 0 ]; then
    echo "Build Image for bramble-${image}"
    docker build -t bramble-${image} -f ./docker/linux/${image}/Dockerfile .
fi

# Start an interactive shell in Fedora and mount the local bramble repo
docker run -it -v `pwd`:/work/bramble-lang bramble-${image} sh