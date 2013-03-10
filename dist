#!/bin/bash
#
# dist
# ----
# Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# Script to build the release

set -e

# Extract project parameters from _oasis
NAME=`oasis query Name 2> /dev/null`
VERSION=`oasis query Version 2> /dev/null`
PREFIX=$NAME-$VERSION
ARCHIVE=$(pwd)/$PREFIX.tar.gz

# Create a branch for the release
git checkout -b release-$VERSION

# Generate files
oasis setup

# Set release mode in the Makefile
sed 's/^SETUP := setup-dev.exe.*/SETUP := setup.exe/' Makefile > Makefile.new
mv Makefile.new Makefile

# Remove this script
rm -f dist

# Commit
git add --all --force
git commit -m "prepare release"
git tag $VERSION

git checkout master
