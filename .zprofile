# User configuration
export PATH=~/bin:$PATH
export PATH=$PATH:$JAVA_HOME

# source $HOME/dotfiles/setup-clang.sh # Not required anymore Apple's LLVM is good enough at this point

eval "$(/usr/local/bin/brew shellenv)"
# source $HOME/dotfiles/setup-clang.sh # Not required anymore Apple's LLVM is good enough at this point
if [[ "$OSTYPE" != "linux-gnu" ]]; then
    if [[ $(uname -m) == 'arm64' ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    else
        eval "$(/usr/local/bin/brew shellenv)"
    fi
fi
