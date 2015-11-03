#!/usr/bin/env bash
set -e

DIRNAME=`dirname $0`

cd $DIRNAME/..

# install necessary prequisites
bash script/bootstrap.sh

# run the install script itself
runhaskell script/Main.hs -- install tools.yaml >> tools.adoc
