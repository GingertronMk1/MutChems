#!/bin/sh

echo "Formatting files with stylish-haskell..."
stylish-haskell -i -v -c .stylish-haskell.yaml ./**/*.hs
echo "Formatting complete"
echo "Linting files with hlint"
hlint ./**/*.hs
echo "Linting complete"