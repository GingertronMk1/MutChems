#!/bin/sh

echo "Formatting files with stylish-haskell..."
stylish-haskell -i -v -c .stylish-haskell.yaml src/**/*.hs
echo "Formatting complete"
echo "Linting files with hlint"
hlint src/**/*.hs
echo "Linting complete"
