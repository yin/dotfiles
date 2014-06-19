#!/bin/bash

autosave_dir=../${PWD##*/}.auto-save
worktree_dir=$PWD
export GIT_DIR=$autosave_dir
export GIT_WORK_TREE=$worktree_dir
echo GIT_DIR=$GIT_DIR
echo GIT_WORK_TREE=$GIT_WORK_TREE

if [ ! -e $autosave_dir ]; then
    mkdir -p $autosave_dir
    git init
elif [ ! -d $autosave_dir ]; then
    echo "Path $autosave_dir must be a a folder (git repository)." >&2
    exit 1
fi

while true; do
  git add -A
  changed=$(git status -s)
  git commit -m "auto-save: $(date)
$changed"
  sleep 60
done
