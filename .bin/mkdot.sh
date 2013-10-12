#!/bin/bash

dotfile=$1
dotfiles_dir=~/.dotfiles.git

cmd() {
	execcmd=($@)
	echo "  $ " ${execcmd[@]}
	${execcmd[@]}
}

if [ -e ~/$dotfile ] && [ ! -L ~/$dotfile ]; then
	export GIT_DIR=$dotfiles_dir/.git
	cmd git stash
	
	cmd mv ~/$dotfile $dotfiles_dir/$dotfile \
	  && cmd ln -sb $dotfiles_dir/$dotfile ~/$dotfile \
	  && cmd git add $dotfiles_dir/$dotfile
	git status  
	# Commit, Push, Unstash
	echo git commit -m "Added file using #mkdot: $dotfile"
	git commit -m "Added file using #mkdot: $dotfile" \
	  && cmd git push origin
	
	echo "Your stashes:"
	cmd git stash list
	echo "... please unstash your previous changes with:"
	echo "      $ git stash pop"
else
	if  [ ! -e ~/$dotfile ]; then
		echo "File ~/$dotfile does not exists."
	elif [ -L ~/$dotfile ]; then
		echo "File ~/$dotfile is already a symlink."
	fi
fi

