#!/bin/bash

pushd "$(dirname "$0")" > /dev/null

mkdir -p bin
mkdir -p lib

fpc @fp.cfg $@
ERRCOMP=$?

popd > /dev/null

exit $ERRCOMP
