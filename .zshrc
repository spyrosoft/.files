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

alias zshrc="e ~/.zshrc"
alias rezshrc="source ~/.zshrc"
alias emacsrc="e ~/.emacs"
alias vimrc="vi ~/.vimrc"
alias inkscaperc="e /usr/share/inkscape/keys/default.xml"

alias e="emacs"
alias emacs="emacs -nw"
export EDITOR="/usr/bin/emacs -nw"
alias vi="vim"

alias thunderbird-profiles="thunderbird -profilemanager &"

alias su="su -"
alias mkdir="mkdir -p"
alias od="od -a"
alias du="du -h"
alias ls="ls -t --color"
alias grep="grep --color"

# If xdg-open is a command, alias it to `open'
if hash xdg-open 2>/dev/null; then
	alias open="xdg-open"
fi

alias git-sync="git pull && git push"

alias hosts="e /etc/hosts"

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

function mount-remote() {
	if [[ $# -lt 2 || $# -gt 2 ]]; then
		echo "Usage: mount-remote user@hostname local-directory-name"
	fi
	if [[ $# -eq 2 ]]; then
		mkdir /tmp/$2 && sshfs $1: /tmp/$2
		cd /tmp/$2
	fi
}

function unmount-remote() {
	cd ~
	fusermount -u /tmp/$1 && rmdir /tmp/$1
}

function sass-watch() {
	if [[ $# -eq 0 ]]; then
		sass --watch sass/styles.sass:css/styles.css &
		return
	fi
	if [[ $# -eq 1 ]]; then
		sass --watch sass/$1.sass:css/$1.css &
		return
	fi
	if [[ $# -eq 2 ]]; then
		sass --watch $1:$2 &
	fi
	echo "Usage: sass-watch [file name without extension]"
}

function scss-watch() {
	if [[ $# -eq 0 ]]; then
		sass --watch scss/styles.scss:css/styles.css &
		return
	fi
	if [[ $# -eq 1 ]]; then
		sass --watch scss/$1.scss:css/$1.css &
		return
	fi
	if [[ $# -eq 2 ]]; then
		sass --watch $1:$2 &
	fi
	echo "Usage: scss-watch [file name without extension]"
}

function zip() {
	if [[ $# -eq 1 ]]; then
		/usr/bin/zip $1.zip $1
	fi
	if [[ $# -gt 1 ]]; then
		/usr/bin/zip $@
	fi
}

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
	/usr/bin/zip $1.zip *
	mv $1.zip ..
	cd ..
}

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

PROMPT="%{$fg_bold[cyan]%}%C~%{$reset_color%} "