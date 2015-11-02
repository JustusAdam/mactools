#!/usr/bin/env bash
set -e

# install necessary prequisites
bash script/bootstrap.sh

# run the install script itself
runhaskell script/Main.hs -- install tools.yaml >> tools.adoc
