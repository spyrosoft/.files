#Applicable only if using X:
#[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort access
zstyle ':completion:*' ignore-parents pwd
zstyle ':completion:*' menu select=long
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl true
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/home/username/.zshrc'

autoload -Uz compinit
compinit
autoload -U colors
colors
# End of lines added by compinstall

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=50000
SAVEHIST=50000
setopt appendhistory autocd beep nomatch notify
# End of lines configured by zsh-newuser-install

bindkey '^[[Z' reverse-menu-complete 


# User Customization

alias hosts="e /etc/hosts"
alias thunderbird-profiles="thunderbird -profilemanager &"
alias inkscaperc="e /usr/share/inkscape/keys/default.xml"


# General Customization

alias zshrc="e ~/.zshrc"
alias rezshrc="source ~/.zshrc"
alias emacsrc="e ~/.emacs"
alias vimrc="vi ~/.vimrc"

alias e="emacs"
export EDITOR="/usr/bin/emacs"
alias vi="vim"

alias ls="ls -t --color"
alias su="su -"
alias mkdir="mkdir -p"
alias grep="grep --color"
alias od="od -a"
alias du="du -h"

alias status="git status"
alias add="git add"
alias commit="git commit"
alias push="git push"
alias git-sync="git pull && git push"

# Mac only
if [ "$(uname)" == "Darwin" ]; then
	unalias ls
	alias ls="ls -t -G"
	# Mac does not ship with wget; `curl -O' is equivalent
	alias wget="curl -O"
fi

# If xdg-open is a command, alias it to `open'
if hash xdg-open 2>/dev/null; then
	alias open="xdg-open"
fi

# Often I need to `find' all non-hidden files recursively
# in the current directory and grep over them.
# As opposed to `grep -r'.
# Useful in git repositories.
function find-grep() {
	find . -type f -name "$1" -exec grep -Hn "$2" {} +
}

function download-website() {
	wget $1 \
		--tries 3 \
		--recursive \
		--level=99 \
		--convert-links \
		--page-requisites \
		--show-progress
}

# Equivalent to `mkdir NEW-DIRECTORY; cd NEW-DIRECTORY'
function mkcd() {
	if [[ $# -eq 0 ]]; then
		echo "Please supply a directory to create and cd into."
	fi
	if [[ $# -eq 1 ]]; then
		mkdir $1
		cd $1
	fi
	if [[ $# -gt 1 ]]; then
		echo "Only one argument is allowed."
	fi
}

# Equivalent to `mv OLD-DIRECTORY NEW-DIRECTORY; cd NEW-DIRECTORY'
function mvcd() {
	if [[ $# -lt 2 ]]; then
		echo "Please supply the existing directory, and new directory name."
	fi
	if [[ $# -eq 2 ]]; then
		mv $1 $2
		cd $2
	fi
	if [[ $# -gt 2 ]]; then
		echo "Only two arguments are allowed."
	fi
}

# Mount a remote filesystem via sshfs to /tmp/FIRST_ARGUMENT and cd there
function mount-remote() {
	if [[ $# -lt 2 || $# -gt 2 ]]; then
		echo "Usage: mount-remote user@hostname local-directory-name"
	fi
	if [[ $# -eq 2 ]]; then
		mkdir /tmp/$2 && sshfs $1: /tmp/$2
		cd /tmp/$2
	fi
}

# Unmount a previously initiated `mount-remote'
function unmount-remote() {
	cd ~
	fusermount -u /tmp/$1 && rmdir /tmp/$1
}

# Three common `sass --watch' idioms for easy use
function sass-watch() {
	if [[ $# -eq 0 ]]; then
		echo "sass --watch sass/styles.sass:css/styles.css &"
		sass --watch sass/styles.sass:css/styles.css &
		return
	fi
	if [[ $# -eq 1 ]]; then
		echo "sass --watch sass/$1.sass:css/$1.css &"
		eval "sass --watch sass/$1.sass:css/$1.css &"
		return
	fi
	if [[ $# -eq 2 ]]; then
		echo "sass --watch $1:$2 &"
		eval "sass --watch $1:$2 &"
		return
	fi
	echo "Usage: sass-watch [file name without extension, or both file paths with extension]"
}

function scss-watch() {
	if [[ $# -eq 0 ]]; then
		echo "sass --watch scss/styles.scss:css/styles.css &"
		sass --watch scss/styles.scss:css/styles.css &
		return
	fi
	if [[ $# -eq 1 ]]; then
		echo "sass --watch scss/$1.scss:css/$1.css &"
		eval "sass --watch scss/$1.scss:css/$1.css &"
		return
	fi
	if [[ $# -eq 2 ]]; then
		echo "sass --watch $1:$2 &"
		eval "sass --watch $1:$2 &"
		return
	fi
	echo "Usage: scss-watch [file name without extension, or both file paths with extension]"
}

# Why doesn't the zip utility behave this way by default?
# When supplied with one argument,
# create a zip file of the same name right there.
function zip() {
	if [[ $# -eq 1 ]]; then
		/usr/bin/env zip $1.zip $1
	fi
	if [[ $# -gt 1 ]]; then
		/usr/bin/env zip $@
	fi
}

# Commonly, I will need to create a zip which only contains the
# contents of a directory, and not the directory.
# Without this command, it would take a full four (annoying) commands.
function zip-contents {
	if [[ $# -ne 1 ]]; then
		echo "Usage: zip-contents path/to/directory"
		return
	fi
	if [ ! -d $1 ]; then
		echo "Directory does not exist: $1"
		return
	fi
	cd $1
	/usr/bin/env zip $1.zip *
	mv $1.zip ..
	cd ..
}

# When permissions are weird or wrong, run this command.
# Note: these are very strict permissions.
# Defaults to the current directory, or accepts one directory argument.
# Files: User RW, Group R, Other none
# Directories: User RWX, Group RX, Other none
function set-standard-permissions() {
	if [[ $# -eq 0 ]]; then
		find . -type f -exec chmod 640 {} +
		find . -type d -exec chmod 750 {} +
	fi
	if [[ $# -eq 1 ]]; then
		find $1 -type f -exec chmod 640 {} +
		find $1 -type d -exec chmod 750 {} +
	fi
	if [[ $# -gt 2 ]]; then
		echo "Usage: set-standard-permissions [optional directory/file - defaults to current directory]"
	fi
}

# The diff utility requires two arguments.
# The git diff utility requires one argument.
# Based how many arguments are passed, choose the correct context.
function diff() {
	if [[ $# -eq 0 ]]; then
		git diff .
	fi
	if [[ $# -eq 1 ]]; then
		git diff $@
	fi
	if [[ $# -gt 1 ]]; then
		/usr/bin/env diff $@
	fi
}

PROMPT="%{$fg_bold[cyan]%}%C~%{$reset_color%} "