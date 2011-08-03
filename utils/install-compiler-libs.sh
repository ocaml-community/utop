#!/bin/sh
#
# install-compiler-libs.sh
# ------------------------
# Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of utop.

set -e

if [ $# -ne 1 ]; then
    echo "usage: $0 <ocaml source path>"
    exit 2
fi

SOURCEPATH="$1"

if [ ! -d "$SOURCEPATH/typing" -o ! -d "$SOURCEPATH/parsing" -o ! -d "$SOURCEPATH/utils" ]; then
    echo "'$1' does not contain ocaml sources."
    exit 1
fi

if [ ! -f "$SOURCEPATH/typing/types.cmi" -o ! -f "$SOURCEPATH/parsing/longident.cmi" ]; then
    echo "ocaml sources in '$1' are not compiled."
    exit 1
fi

STDLIBPATH=`ocamlc -where`
INSTALLPATH="$STDLIBPATH/compiler-libs"

mkdir -p "$INSTALLPATH"

for dir in typing parsing utils; do
    echo "copying cmi files from '$SOURCEPATH/$dir' to '$INSTALLPATH/$dir'"
    mkdir -p "$INSTALLPATH"/$dir
    for file in "$SOURCEPATH"/$dir/*.cmi; do
        cp "$file" "$INSTALLPATH"/$dir
    done
done
