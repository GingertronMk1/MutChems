#!/bin/sh

packages="./data/packages.txt"

while IFS="" read -r p || [ -n "$p" ]
do
  echo "cabal installing $p"
  cabal install $p
done < $packages