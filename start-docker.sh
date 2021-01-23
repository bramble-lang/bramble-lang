#!/bin/sh

image=fedora

while getopts i: flag
do
    case "${flag}" in
        i) image=${OPTARG};;
    esac
done

# check for the existence of the fedora image
echo "Check if braid-${image} exists"
not_exist=$(docker image inspect braid-${image}:latest)
result=$?
if [ $result -ne 0 ]; then
    echo "Build Image for braid-${image}"
    docker build -t braid-${image} -f ./docker/linux/${image}/Dockerfile .
fi

# Start an interactive shell in Fedora and mount the local braid repo
docker run -it -v `pwd`:/work/braid-lang braid-${image} sh