#!/bin/sh

echo "Checking for existence of $1..."

ORIGIN_BRANCH="origin/$1"

if [ -z "$(git branch -r | grep "$ORIGIN_BRANCH")" ]
then
  CURRENT_BRANCH="$(git branch --show-current)"
  echo "No branch called $1, creating..."
  git checkout -b $1
  git push -u origin $1
  echo "Checking back out to $CURRENT_BRANCH"
  git checkout $CURRENT_BRANCH
else
  echo "Branch $1 exists, doing nothing..."
fi
