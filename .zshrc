# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
#
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export TERM="xterm-256color"
# Path to your oh-my-zsh installation.
export ZSH=$HOME/dotfiles/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="powerlevel10k/powerlevel10k"
# ZSH_THEME="steeef"

# The following will convert all .txt files into .html files
# zmv '(*).txt' '$1.html'

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git macos emacs gitignore zsh-syntax-highlighting)

# User configuration
# export PATH=~/bin:$PATH

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# 10 second wait if you do something that will delete everything. Dummy proof
setopt RM_STAR_WAIT

# Spell check commands!  (Sometimes annoying)
# setopt CORRECT

# don't just 'cd dir' but just type 'dir'
setopt AUTO_CD

# don't save duplicates in history
setopt HIST_IGNORE_DUPS

# Even if there are commands inbetween commands that are the same, still only save the last one
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS

# If a line starts with a space, don't save it.
setopt HIST_IGNORE_SPACE
setopt HIST_NO_STORE

# this one lets zsh stop expanding globs like * before sent to find etc
setopt NO_NOMATCH

# Where it gets saved
HISTFILE=~/.omz_history
NUMCPUS=8 # Default number looks good

# incase running on Arch within a debian docker container, seperate history
if [[ "$OSTYPE" == "linux-gnu" ]]; then
	if [ -f /etc/os-release ]; then
		. /etc/os-release
		ID_LIKE=$ID_LIKE # will be arch on ArchLinux
		if [[ "$ID_LIKE" == "debian" ]]; then
			HISTFILE=~/.omz_history_docker_ubuntu
		fi
	else
		echo "Os unknown! something went wrong your OS is undefined, fix .zshrc"
	fi	# ...
	NUMCPUS=$(awk '/^processor/ {++n} END {print n+1}' /proc/cpuinfo)
fi

# Remember about a years worth of history (AWESOME)
SAVEHIST=10000
HISTSIZE=10000

# Don't overwrite, append!
setopt APPEND_HISTORY

# Killer: share history between multiple shells
setopt SHARE_HISTORY

if [[ "$OSTYPE" == "darwin"* ]]; then
	alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
	alias sublime="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
	alias s="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
	alias metal_validation="export METAL_DEVICE_WRAPPER_TYPE=1;export METAL_ERROR_MODE=5;export METAL_DEBUG_ERROR_MODE=5"
	alias OA="r"
	NUMCPUS=`sysctl -n hw.ncpu`
fi

alias what_can_i_delete='du -sh * | gsort -hr'
alias rf='rm -rf'
alias create_patch='diff -rupN'
alias less='less -r'								# raw control characters
alias gcleanco='git clean -fdx;git checkout -- .'         # Full clean of git repository
alias who-is-holding-reference='lsof | grep '         # Find out who is grabing onto files
alias all-predefined-macros='clang -dM -E -x c /dev/null'

export ANDROID_NDK_ROOT=/development/android/sdk/ndk/23.1.7779620
export ANDROID_SDK_ROOT=/development/android/sdk
export ANDROID_HOME=$ANDROID_SDK_ROOT
export ANDROID_NDK_HOME=$ANDROID_NDK_ROOT
export ANDROID_MAKE_CCACHE=ccache
export ANDROID_ROOT=$ANDROID_SDK_ROOT

# setup python environment, start with lldb python package path
export PYTHONPATH=`lldb -P`

# alias gk='gitk --all &'
alias gs='git status'
# alias gd='git diff --color'
alias gds='git diff --color --staged'
alias gdws='git diff --color --ignore-space-at-eol -b -w'
# alias ga='git add'
# alias gco='git checkout'
alias gaup='git add -up'
alias gpom='git push origin master'
alias gcm='git commit -m'
alias ganw="git diff -U0 -w --no-color | git apply --cached --ignore-whitespace --unidiff-zero" # Git add ignoring whitespace
alias gsquash_last_commits='git rebase -i HEAD~2'
alias git-create-patch='git format-patch master --stdout' # // Just pipe it into a .patch
alias git-show-files-changed-in-commit="git diff-tree --no-commit-id --name-only -r"

alias lt='ls -alstr'
alias grep='grep -nI'
alias search_in_all_c_type_sources="find . -name '*.h' -or -name '*.c' -or -name '*.cpp' -or -name '*.cc' | xargs grep"
alias find_executables="find . -perm +0111 -type f"

alias rgd='rg --colors line:fg:yellow --colors line:style:bold --colors path:fg:green --colors path:style:bold --colors match:fg:black --colors match:bg:yellow --colors match:style:nobold'

alias adb="$ANDROID_SDK_ROOT/platform-tools/adb"
alias fastboot="$ANDROID_SDK_ROOT/platform-tools/fastboot"
alias ndk-build="$ANDROID_NDK_ROOT/ndk-build"
alias android="$ANDROID_SDK_ROOT/tools/android"
alias which_arch='lipo -info'
alias mount_to_dev='hdiutil attach -imagekey diskimage-class=CRawDiskImage -nomount'
alias android_print_screen='adb shell screencap -p | perl -pe 's/\x0D\x0A/\x0A/g' > screen.png'
alias android_print_screen_adb='adb shell screencap -p /sdcard/screen.png; adb pull /sdcard/screen.png; adb shell rm /sdcard/screen.png'
alias apktool="$ANDROID_SDK_ROOT/apktool/apktool"
alias open_gnome_terminal_on_server='ssh -f -Y $USERNAME@server gnome-terminal'
alias flush_dns_cache='sudo dscacheutil -flushcache;sudo killall -HUP mDNSResponder;say cache flushed'
alias vim_without_vimrc='vim -u NONE -N'
alias find_everything='locate / | fzf'
alias find_and_delete_files='#find . -type f -name ".non-existent-file" -exec rm -f {} \;'
alias rsync_from_to='rsync -a from to'

# Emacs specific setup, copied from .oh-my-zsh
unalias emacs
unalias e
export EMACS_PLUGIN_LAUNCHER="~/dotfiles/misc/emacsclient.sh"
# set EDITOR if not already defined.
export EDITOR="${EDITOR:-${EMACS_PLUGIN_LAUNCHER}}"
alias emacs="$EMACS_PLUGIN_LAUNCHER"
alias e=emacs
alias ee='e $($(fc -ln -1))'
alias evim='te'

alias o='open'
alias oo='open .'

# this way just use G to grep for stuff
alias -g G="| grep"
alias -g L="| less"

alias update_emacs_packages='emacs -batch -u "$USER" -f package-utils-upgrade-all -kill'
alias start_synergy_server='cd /software/synergy/;./synergy-core --server -c synergy.conf; cd -'
alias kill_synergy_server='killall -9 synergy-core'
alias synergy_key_reset='sudo /usr/bin/xset r on'
alias cpp_to_template_proccessed='clang++ -Xclang -ast-print -fsyntax-only'

alias start_barrier_server="/Applications/Barrier.app/Contents/MacOS/barriers --no-tray --debug INFO --enable-drag-drop --enable-crypto -c /software/barrier/barrier-server.config.sgc --address :24800"
alias kill_barrier_server='killall -9 barriers'

alias mk='make -j$NUMCPUS'
alias emasc=emacs
alias c=bat
alias lc=bat
alias cl=bat

# Undo a `git push`
alias undo_git_push="git push -f origin HEAD^:master"
alias cleanup_dsstore_files="find . -name '*.DS_Store' -type f -ls -delete"
alias mount_bsg_jira="sudo mount -o resvport server:/home/$USERNAME /development/remote-dir"
alias reset_audio="sudo killall coreaudiod"

alias list_my_snippets="cd ~/.emacs.d/snippets/; cat */* G key; cd -"

alias k9="kill -9"

# Android adb specific aliases
#Set aliases for most common keys
alias adbk_back="adb shell input keyevent 4"
alias adbk_home="adb shell input keyevent 3"
alias adbk_up="adb shell input keyevent 19"
alias adbk_down="adb shell input keyevent 20"
alias adbk_left="adb shell input keyevent 21"
alias adbk_right="adb shell input keyevent 22"
alias adbk_center="adb shell input keyevent 23"
alias adbk_vol_up="adb shell input keyevent 24"
alias adbk_vol_down="adb shell input keyevent 25"
alias adbk_power="adb shell input keyevent 26"
alias adbk_camera="adb shell input keyevent 27"
alias adbk_clear="adb shell input keyevent 28"
alias adbk_search="adb shell input keyevent 84"
alias adbk_enter="adb shell input keyevent 66"
alias adbk_call="adb shell input keyevent 5"
alias adbk_menu="adb shell input keyevent 82"

# cd to selected file folder
cdf()
{
   local file
   local dir
   file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

# User defined functions
function print_screen_android()
{
	adb shell screencap -p | perl -pe 's/\x0D\x0A/\x0A/g' > $1
}

function dga()
{
	ag --hidden --all-text "$*"
}

function dg()
{
	rg --hidden --colors line:fg:yellow --colors line:style:bold --colors path:fg:green --colors path:style:bold --colors match:fg:black --colors match:bg:yellow --colors match:style:nobold "$*"
}

function ag_without_semicolon()
{
	ag "$*(?!.*;)"
}

function rg_without_semicolon()
{
	rg --colors line:fg:yellow --colors line:style:bold --colors path:fg:green --colors path:style:bold --colors match:fg:black --colors match:bg:yellow --colors match:style:nobold "$*(?!.*;)"
}


function dg_without_semicolon()
{
	ag --hidden --all-text "$*(?!.*;)"
}

function dgr()
{
	grep -nI "$*" . --exclude=.svn --exclude=.git
}

# grep count lines before and after
function gx()
{
	count="$1"
	grep -C $count -nrI "$2" . --exclude=.svn --exclude=.git
}

function dgf()
{
	grep -nIl "$*" .
}

function f()
{
  find . -name $1
}

function find_something_in_something()
{
	toFind="$1"
	if [ ! $2 ] ; then file="*" ; else file="$2"; fi
	find . -name "$file" -print0 | xargs -0 grep --color -nI "$toFind"
}
# quickly compile shader to spir-v and run spirv-cross on the spv to covert to msl
function compile_to_msl()
{
	glslangValidator -V $1 -o $1.spv && spirv-cross --msl-decoration-binding $2 $1.spv --msl-version 30000 --msl --output $1.spv.msl
}

# checkout a branch with fzf easily
gcob()
{
  local branches branch
  branches=$(git branch -a) &&
  branch=$(echo "$branches" | fzf +m) &&
  git checkout $(echo "$branch" | awk '{print $1}' | sed "s/.* //")
}

# gcshow - git commit browser, look for commits by fuzzy searching
gcshow()
{
  git log --graph --color=always \
	  --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
	  --bind "ctrl-m:execute:
				(grep -o '[a-f0-9]\{7\}' | head -1 |
				xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
				{}
FZF-EOF"
}

# fuzzy search a commit and check it out
gcoc() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}

# Make, install, run and collect android apk error logs
android_make_deploy_collect()
{
	APK_PATH=
	PACKAGE_NAME=
	ACTIVITY_NAME=
	FILTER="E"

	if [ "$#" -lt 3 ]; then
		echo "To use this function provide apk to install, package name, activity name and optional filter\n\tandroid_make_deploy_collect bla.apk com.bla.something MyActivity {filter, default E}\t where filter is {E|V|I}"
	else
		APK_PATH=$1
		PACKAGE_NAME=$2
		ACTIVITY_NAME=$3

		if [ "$#" -eq 4 ]; then
			FILTER=$4
		fi

		make -j8 && adb install -r $APK_PATH && adb shell am start -n $PACKAGE_NAME/$PACKAGE_NAME.$ACTIVITY_NAME && sleep 2 && adb logcat --pid=`adb shell pidof -s $PACKAGE_NAME` *:$FILTER
	fi
}

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
