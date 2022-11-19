#!/bin/sh

git fetch origin

git branch

if [ -z $(git branch | grep $@) ]
then
  CURRENT_BRANCH="$(git branch --show-current)"
  echo "No branch called $@, creating..."
  git checkout -b $@
  git push -u origin $@
  echo "Checking back out to $CURRENT_BRANCH"
  git checkout $CURRENT_BRANCH
else
  echo "Branch $@ exists, doing nothing..."
fi
