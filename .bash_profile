#!/bin/bash

echo ".bash_profile: starting login shell"

rcfile=~/.bashrc
if [ -r $rcfile ]; then
	. $rcfile
fi

export PATH=$HOME/bin:$PATH

#TODO: setup.sh#nvm adds a line here, make sure it can't do that.
[ -s $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh # This loads NVM

