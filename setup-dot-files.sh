#!/bin/zsh

pushd ~

echo "Deleting old symbolic links..."
rm -rf .emacs.d
rm .fzf.zsh
rm -rf .vim
rm .vimrc
rm .zshrc
echo "Old symbolic links removed"

echo "Creating new symbolic links..."
ln -s dotfiles/.emacs.d .emacs.d
ln -s dotfiles/.vim .vim
ln -s dotfiles/.vimrc .vimrc
ln -s dotfiles/.zshrc .zshrc
echo "Symbolic links created"

echo "Cloning Vundle for vim..."
git clone --depth 1 https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
echo "Vundle installed"

echo "Installing vim packages..."
vim +PluginInstall +qall
echo "All vim plugins installed"

popd

echo "Cloning oh-my-zsh..."
git clone --depth 1 https://github.com/robbyrussell/oh-my-zsh.git .oh-my-zsh
echo "oh-my-zsh installed"

echo "Cloning and installing fzf..."
git clone --depth 1 https://github.com/junegunn/fzf.git .fzf
printf 'y\ny\ny\n' | ./.fzf/install
pushd .oh-my-zsh/themes/
echo "fzf installed"

echo "Cloning and installing oh-my-zsh theme powerlevel9k..."
git clone --depth 1 https://github.com/bhilburn/powerlevel9k.git
echo "powerlevel installed"

popd

echo "Running emacs packages install script..."
emacs --script ./emacs-install-packages.el
echo "All emacs packages installed"

echo "Installing jedi for python mode"
pip install jedi
echo "If python is installed and pip has worked then Jedi is installed, otherwise install manually"
