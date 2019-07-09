#!/bin/zsh

CHOME=`pwd`

echo "Running emacs packages install script..."
HOME=$CHOME emacs --script ./emacs-install-packages.el
echo "All emacs packages installed"
