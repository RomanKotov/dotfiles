#!/usr/bin/env bash

DOT_FILES=`git ls-files | grep -E "^\."`

FOLDERS=(~/.config/nvim ~/.config/nvim/compiler)

for folder in ${FOLDERS[@]}; do
  echo "Creating $folder"
  mkdir -p $folder
done

for file in $DOT_FILES; do
  echo "Processing $file"

  destination="$HOME/$file"

  if [ -f $destination ]
  then
    backup="$destination.backup"
    cp $destination $backup
    echo "$backup created"
  fi

  ln -sf `realpath $file` $destination
done
