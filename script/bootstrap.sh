#!/usr/bin/env bash
set -e
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install ghc cabal
cabal install process aeson
