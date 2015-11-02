#!/usr/bin/env bash
set -e

# installing homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# installing haskell and cabal
brew install ghc cabal

# install packages required to run the setup script
cabal install process yaml
