# General Customization

alias zshrc="e ~/.zsh-custom"
alias rezshrc="source ~/.zshrc"
alias emacsrc="e ~/.emacs.d/init.el"
alias vimrc="vi ~/.vimrc"

alias e="emacs -mm"
export EDITOR="/usr/bin/env emacs -mm"
alias vi="vim"

alias ls="ls -t --color"
alias su="su -"
alias root="su"
alias mkdir="mkdir -p"
alias grep="grep --color"
alias od="od -a"
alias du="du -h"
alias tail="tail -f"

alias boom="rm -rfI"

alias status="git status"
alias add="git add"
alias commit="git commit"
alias push="git push"
alias pull="git pull"
alias git-sync="git pull && git push"
alias log="git log" # Note that this overrides the bash math log() function
alias init="git init" # Note that this may override the init binary

if ! hash wget 2>/dev/null; then
	alias wget="curl -O"
fi

# Mac only
if [[ "$(uname)" == "Darwin" ]]; then
	alias ls="ls -t -G"
fi

# If xdg-open is a command, alias it to `open'
if hash xdg-open 2>/dev/null; then
	alias open="xdg-open"
fi

# Disable Ctrl-S flow control stop
stty -ixon

# Git it?
function git() {
	if [[ "$1" == "ready" ]]; then
		git status
	elif [[ "$1" == "set" ]]; then
		if [[ $# -eq 1 ]]; then
			git add .
		else
			git add $@
		fi
		git status
	elif [[ "$1" == "goin" ]]; then
		git commit && git push
	elif [[ "$1" == "back" ]]; then
		git diff
	else
		/usr/bin/env git $@
	fi
}

# The diff utility requires two arguments.
# The git diff utility requires one argument.
# Based how many arguments are passed, choose the correct context.
function diff() {
	if [[ $# -eq 0 ]]; then
		git diff .
	elif [[ $# -eq 1 ]]; then
		git diff $@
	else
		/usr/bin/env diff $@
	fi
}

# Why doesn't the zip utility behave this way by default?
# When supplied with one argument,
# create a zip file of the same name right there.
function zip() {
	if [[ $# -eq 1 ]]; then
		/usr/bin/env zip -r $1.zip $1
	else
		/usr/bin/env zip $@
	fi
}
# Commonly, I will need to create a zip which only contains the
# contents of a directory, and not the directory.
# Without this command, it would take a full four (annoying) commands.
function zip-contents {
	if [[ $# -ne 1 ]]; then
		echo "Usage: $0 path/to/directory"
		return
	elif [ ! -d $1 ]; then
		echo "Directory does not exist: $1"
	else
		cd $1
		/usr/bin/env zip -r $1.zip *
		mv $1.zip ..
		cd ..
	fi
}

# Often I need to `find' all non-hidden files recursively
# in the current directory and grep over them.
# As opposed to `grep -r'.
# Useful in git repositories.
function find-grep() {
	if [[ $# -eq 1 ]]; then
		find . -type f -not -path '*/\.*' -exec grep -Hn "$1" {} + | grep "$1"
	elif [[ $# -eq 2 ]]; then
		find . -type f -not -path '*/\.*' -name "$1" -exec grep -Hn "$2" {} + | grep "$2"
	else
		echo "Usage: $0 [optional file name pattern] [search pattern]"
	fi
}

# Easier than `find . -type f'
function find-file() {
	if [[ $# -eq 1 ]]; then
		eval "find . -type f -name '$1'"
	elif [[ $# -eq 2 ]]; then
		eval "find $2 -type f -name '$1'"
	else
		echo "Usage: $0 [file name pattern] [optional initial directory]"
	fi
}

# Easier than `find . -type d'
function find-directory() {
	if [[ $# -eq 1 ]]; then
		eval "find . -type d -name '$1'"
	elif [[ $# -eq 2 ]]; then
		eval "find $2 -type f -name '$1'"
	else
		echo "Usage: $0 [directory name pattern] [optional initial directory]"
	fi
}

# Often I need to search and replace over all files the current a directory recursively.
function search-replace() {
	if [[ $# -lt 2 || $# -gt 3 ]]; then
		echo "Usage: $0 'search-pattern' 'replace-pattern' ['file-pattern']"
		return
	fi
	
	# when a file pattern is passed as an argument use find-grep otherwise use grep
	if [[ $# -eq 3 ]]; then
		# Demonstrate what changes will be made
		find-grep $3 $1
		grep_results=`find . -type f -name "$3" -not -path '*/\.*' -exec grep -l "$1" {} + | grep "$1"`
	else
		find-grep $1
		# `grep -r` searches through dotfiles which is bad for git repositories
		# Instead using find + grep -l
		grep_results=`find . -type f -not -path '*/\.*' -exec grep -l "$1" {} +`
	fi
    
	if [[ "$grep_results" == "" ]]; then
		echo "Grep didn't find anything that matched your search"
		return
	fi
	
	echo "Proceed? (Y/n)"
	read proceed
	if [[ "$proceed" == "y" || "$proceed" == "Y" || "$proceed" == "" ]]; then
		echo $grep_results | xargs sed -i s@$1@$2@g
	fi
	
	unset grep_command grep_results proceed
}

# Combine all variations of package manager update commands into one
function update() {
	if hash pacman 2>/dev/null; then
		pacman -Syu
	elif hash yum 2>/dev/null; then
		yum -y update
	elif hash aptitude 2>/dev/null; then
		aptitude update && aptitude upgrade
	elif hash apt-get 2>/dev/null; then
		apt-get update && apt-get upgrade
	elif hash brew 2>/dev/null; then
		brew update
	fi
}

# Equivalent to `mkdir NEW-DIRECTORY; cd NEW-DIRECTORY'
function mkcd() {
	if [[ $# -eq 1 ]]; then
		mkdir $1
		cd $1
	else
		echo "Usage: $0 [directory]"
	fi
}

# Equivalent to `mv OLD-DIRECTORY NEW-DIRECTORY; cd NEW-DIRECTORY'
function mvcd() {
	if [[ $# -eq 2 ]]; then
		mv $1 $2
		if [ -d $2 ]; then
			cd $2
		# If the second argument is a file,
		# cd into its containing directory
		else
			cd $(dirname "$2")
		fi
	else
		echo "Usage: $0 [existing directory] [new directory name]"
	fi
}

# Mount a remote filesystem via sshfs to /tmp/FIRST_ARGUMENT and cd there
function mount-remote() {
	if [[ $# -eq 2 ]]; then
		mkdir /tmp/$2 && sshfs $1: /tmp/$2
		cd /tmp/$2
	else
		echo "Usage: $0 user@hostname local-directory-name"
	fi
}

# Unmount a previously initiated `mount-remote'
function unmount-remote() {
	cd ~
	fusermount -u /tmp/$1 && rmdir /tmp/$1
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
	elif [[ $# -eq 1 ]]; then
		find $1 -type f -exec chmod 640 {} +
		find $1 -type d -exec chmod 750 {} +
	else
		echo "Usage: $0 [optional directory/file - defaults to current directory]"
	fi
}

function find-broken-symlinks() {
	if [[ $# == 1 ]]; then
		find $1 -type l -exec file {} + | grep broken
	else
		find . -type l -exec file {} + | grep broken
	fi
}

# Quit all running jobs
function quit-jobs() {
    kill $(jobs -p)
}

# Analyze your most frequent commands
function most-frequent-commands() {
	if [[ "$1" == "" ]]; then
		history_file=~/.histfile
	else
		history_file=$1
	fi
	awk '{print $1}' $history_file | sort | uniq -c | sort -n
	unset history_file
}

# Download the latest .zshrc - there are frequently new improvements
function latest-zshrc {
	cd /tmp
	# Delete /tmp/.zshrc if it exists
	if [ -f .zshrc ]; then rm .zshrc; fi
	wget https://raw.githubusercontent.com/spyrosoft/dotfiles/master/.zshrc
	cd -
	diff_results=`diff /tmp/.zshrc ~/.zshrc`
	if [[ "$diff_results" == "" ]]; then
		rm /tmp/.zshrc
		echo "Nothing to do."
		return
	fi
	echo $diff_results
	echo "Proceed? (Y/n)"
	read proceed
	if [[ "$proceed" == "y" || "$proceed" == "Y" || "$proceed" == "" ]]; then
		mv /tmp/.zshrc ~/.zshrc
		rezshrc
	fi
	unset proceed
}

# Three common `sass --watch' idioms for easy use
# Default:
# sass --watch 
function sass-watch() {
	if [[ $# -gt 2 ]]; then
		echo "Usage: $0 [file name without extension, or both file paths with extension]";
		return 1;
	fi
	sass-watch-command "sass" $@
}

# Equivalents to the sass-watch command using scss
function scss-watch() {
	if [[ $# -gt 2 ]]; then
		echo "Usage: $0 [file name without extension, or both file paths with extension]";
		return 1;
	fi
	sass-watch-command "scss" $@
}

function sass-watch-command() {
	css_directory="css"
	sass_file_name="app"
	if [[ $# -eq 2 ]]; then
		sass_file_name="$2"
	elif [[ $# -eq 3 ]]; then
		sass_file_name="$2"
		css_directory="$3"
	fi
	
	sass_watch_command="sass --watch $1/$sass_file_name.${1}:$css_directory/$sass_file_name.css &"
	echo $sass_watch_command
	eval $sass_watch_command
	unset css_directory sass_file_name sass_watch_command
}

# For when your command only accepts one argument and you want to expand a bash wildcard file pattern
function for-each() {
	if [[ $# -lt 2 ]]; then echo "Usage: $0 \"command --example\" value [value ...]"; fi
	first_argument=true
	for argument in "$@"; do
		# The first argument is the command - skipping
		if [[ "$first_argument" == true ]]; then first_argument=false; continue; fi
		eval "$1 $argument"
	done
	unset first_argument
}

# Run a command on each line of a file
function for-each-line() {
	if [[ $# -ne 2 ]]; then echo "Usage: $0 \"command --example\" file"; fi
	while IFS='' read -r line || [[ -n "$line" ]]; do
		eval "$1 $line"
	done < $2
}

# Resize images to be square with padding
function square-images() {
	if [[ $# -eq 0 ]]; then
		square-images-loop *.jpg
	elif [[ $# -gt 0 ]]; then
		square-images-loop $@
	fi
}
function square-images-loop() {
	if [[ ! -d "resized" ]]; then
	   mkdir "resized"
	fi
	echo $@
	for image in $@
	do
		convert $image -virtual-pixel white -set option:distort:viewport "%[fx:max(w,h)]x%[fx:max(w,h)]-%[fx:max((h-w)/2,0)]-%[fx:max((w-h)/2,0)]" -filter point -distort SRT 0 +repage "resized/$image"
	done
}

function download-website() {
	if ! hash wget 2>/dev/null; then
		echo "The wget command could not be found."
		return
	fi
	wget $1 \
		--tries 3 \
		--recursive \
		--level=99 \
		--convert-links \
		--page-requisites \
		--show-progress
}


# ZSH Configuration

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
zstyle :compinstall filename '~/.zshrc'

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

#Applicable only if using X:
#[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

source ~/.zsh-custom

if [[ "$prompt_color" == "" ]]; then prompt_color="cyan"; fi
PROMPT="%{$fg_bold[$prompt_color]%}$prompt_prefix%C~%{$reset_color%} "