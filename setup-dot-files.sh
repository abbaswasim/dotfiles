#!/bin/zsh

pushd ~

echo "Deleting old symbolic links..."
rm -rf .emacs.d
rm .fzf.zsh
rm -rf .vim
rm .vimrc
rm .zshrc

echo "Creating new symbolic links..."
ln -s dotfiles/.emacs.d .emacs.d
ln -s dotfiles/.vim .vim
ln -s dotfiles/.vimrc .vimrc
ln -s dotfiles/.zshrc .zshrc

echo "Cloning Vundle for vim..."
git clone --depth 1 https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

echo "Installing vim packages..."
vim +PluginInstall +qall

popd

echo "Cloning oh-my-zsh..."
git clone --depth 1 https://github.com/robbyrussell/oh-my-zsh.git .oh-my-zsh

echo "Cloning and installing fzf..."
git clone --depth 1 https://github.com/junegunn/fzf.git .fzf
printf 'y\ny\ny\n' | ./.fzf/install
pushd .oh-my-zsh/themes/

echo "Cloning and installing oh-my-zsh theme powerlevel9k..."
git clone --depth 1 https://github.com/bhilburn/powerlevel9k.git

echo "Running emacs packages install script..."
emacs --script ./emacs-install-packages.el

popd
