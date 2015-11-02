#!/usr/bin/env bash
set -e
bash script/bootstrap.sh
runhaskell script/Main.hs
