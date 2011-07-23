#!/bin/sh

NAME=`oasis query Name 2> /dev/null`
VERSION=`oasis query Version 2> /dev/null`
DARCS_REPO=`pwd`
export DARCS_REPO

exec darcs dist --dist-name $NAME-$VERSION
