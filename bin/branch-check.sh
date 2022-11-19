#!/bin/sh

git fetch origin

if [ $(git branch | grep "$@") ]
then
  echo "Branch $@ exists, doing nothing..."
else
  CURRENT_BRANCH="$(git branch --show-current)"
  echo "No branch called $@, creating..."
  git checkout -b $@
  git push -u origin $@
  echo "Checking back out to $CURRENT_BRANCH"
  git checkout $CURRENT_BRANCH
fi
