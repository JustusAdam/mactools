#!/usr/bin/env bash
set -e

cd `dirname $0`

# installing homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# installing stack
brew install stack

# build the setup script
stack build

stack exec -- mactools install tools.yaml >> tools.adoc
