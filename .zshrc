# General Customization

alias zshrc="e ~/.zsh-custom"
alias rezshrc="source ~/.zshrc"
alias emacsrc="e ~/.emacs.d/init.el"
alias vimrc="vi ~/.vimrc"

alias e="emacs -mm"
export EDITOR="/usr/bin/env emacs -mm"
alias vi="vim"

# Sort by modification timestamp (-t)
alias ls="ls -t --color"
alias ll="ls -lh"
alias l.="ls -a"
alias ll.="ls -lha"
# The - inherits the environment of the specified user (root if none specified)
alias su="su -"
alias root="su"
# If a parent directory doesn't exist, create it as well (-p)
alias mkdir="mkdir -p"
alias grep="grep --color"
# Make the output human readable (-a)
alias od="od -a"
# Make the file sizes human readable (-h)
alias du="du -h"
# I use tail exclusively for logs, hence the continued output flag (-f)
alias tail="tail -f"
# Display all dig records by default
alias dig="dig any"

alias status="git status"
alias add="git add"
alias commit="git commit"
alias push="git push"
alias pull="git pull"
alias fetch="git fetch"
alias rebase="git rebase"
alias git-sync="git fetch && git push"
alias log="git log" # Note that this overrides the bash math log() function
alias init="git init" # Note that this may override the init binary

alias half-image="mogrify -resize 50%"
alias convert-video="ffmpeg -i"

# Distros have wget, curl, or both - this unifies them under wget (I chose one at random)
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
			# Add an alias to the global .gitconfig: set = add
			/usr/bin/env git $@
		fi
		git status
	elif [[ "$1" == "goin" ]]; then
		git commit && git push
	elif [[ "$1" == "back" ]]; then
		git diff
	# Automatically cd into the cloned directory
	elif [[ "$1" == "clone" ]]; then
		/usr/bin/env git $@
		# Return if the git clone command fails
		if [[ $? -gt 0 ]]; then return; fi
		if [[ $# -eq 2 ]]; then
			git_clone_directory=`echo "$0 $@" | sed -n 's/.*\/\(.\+\)$/\1/p'`
			cd $git_clone_directory
			unset git_clone_directory
		elif [[ $# -eq 3 ]]; then
			cd $2
		fi
	elif [[ "$1" == "it" ]]; then
		git set && git goin
	else
		/usr/bin/env git $@
	fi
}

function cp() {
	if [[ ! -f $2 ]]; then mkdir `dirname $2`; fi
	/usr/bin/env cp $@
}

function mv() {
	if [[ $# -eq 1 ]]; then echo "Must have more than one argument."; return; fi
	if [[ ! -f $2 ]]; then mkdir `dirname $2`; fi
	/usr/bin/env mv $@
}

function touch() {
	if [[ ! -f $2 ]]; then mkdir `dirname $1`; fi
	/usr/bin/env touch $@
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
	if [[ $# -ne 1 ]]; then echo "Usage: $0 path/to/directory"; return; fi
	if [ ! -d $1 ]; then
		echo "Directory does not exist: $1"
	else
		cd $1
		/usr/bin/env zip -r $1.zip *
		mv $1.zip ..
		cd ..
	fi
}

# Identify which type of extraction a file needs and do so
function extract {
	if [[ $# -ne 1 ]]; then echo "Usage: $0 path/to/compressed/file"; return; fi
	file_results=`file $1`
	if [[ $file_results =~ Zip ]]; then
		unzip $1
	elif [[ $file_results =~ gzip ]]; then
		tar xvfz $1
	elif [[ $file_results =~ bzip ]]; then
		tar xvf $1
	else
		echo "Unknown compression type."
	fi
	unset file_results
}

# Often I need to `find' all non-hidden files recursively
# in the current directory and grep over them.
# As opposed to `grep -r'.
# Useful in git repositories.
function find-grep() {
	if [[ $# -eq 1 ]]; then
		find . -type f -not -path '*/\.*' -exec grep -Hn "$1" {} + | grep "$1"
	elif [[ $# -eq 2 ]]; then
		find . -type f -not -path '*/\.*' -name "*.$1" -exec grep -Hn "$2" {} + | grep "$2"
	else
		echo "Usage: $0 [optional file extension] [search pattern]"
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
	if [[ $# -lt 2 || $# -gt 3 ]]; then echo "Usage: $0 'search-pattern' 'replace-pattern' ['file-pattern']"; return; fi
	
	# when a file pattern is passed as an argument use find-grep otherwise use grep
	if [[ $# -eq 3 ]]; then
		# Demonstrate what changes will be made
		find-grep $3 $1
		grep_results=`find . -type f -name "$3" -not -path '*/\.*' -exec grep -l "$1" {} +`
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
	else
		echo "Unknown package manager."
	fi
}

# Combine all variations of package manager installation commands
function install() {
	if hash pacman 2>/dev/null; then
		pacman -S $@
	elif hash yum 2>/dev/null; then
		yum install $@
	elif hash aptitude 2>/dev/null; then
		aptitude install $@
	elif hash apt-get 2>/dev/null; then
		apt-get install $@
	elif hash brew 2>/dev/null; then
		brew install $@
	else
		echo "Unknown package manager."
	fi
}

# Combine all variations of package manager uninstallation commands
function uninstall() {
	if hash pacman 2>/dev/null; then
		pacman -Rns $@
	elif hash yum 2>/dev/null; then
		yum remove $@
	elif hash aptitude 2>/dev/null; then
		aptitude remove $@
	elif hash apt-get 2>/dev/null; then
		apt-get remove $@
	elif hash brew 2>/dev/null; then
		brew uninstall $@
	else
		echo "Unknown package manager."
	fi
}

# Equivalent to `mkdir NEW-DIRECTORY; cd NEW-DIRECTORY'
function mkcd() {
	if [[ $# -ne 1 ]]; then echo "Usage: $0 [directory]"; return; fi
	mkdir $1
	cd $1
}

# Equivalent to `mv OLD-DIRECTORY NEW-DIRECTORY; cd NEW-DIRECTORY'
function mvcd() {
	if [[ $# -ne 2 ]]; then echo "Usage: $0 [existing directory] [new directory name]"; return; fi
	mv $1 $2
	if [ -d $2 ]; then
		cd $2
		# If the second argument is a file,
		# cd into its containing directory
	else
		cd $(dirname "$2")
	fi
}

# Permanently remove something recursively
# Get out of the habbit of typing rm -rf
# Sometimes muscle memory takes over and you rm -rf something you didn't mean to
function boom() {
	if [[ $# -eq 0 ]]; then echo "Useage: $0 ...files/directories..."; return; fi
	echo "The following will be demolished:"
	echo $@
	echo "Proceed? (Y/n)"
	read proceed
	if [[ "$proceed" == "y" || "$proceed" == "Y" || "$proceed" == "" ]]; then
		rm -rf $@
	fi
	unset proceed
}

# Mount a remote filesystem via sshfs to /tmp/FIRST_ARGUMENT and cd there
function mount-remote() {
	if [[ $# -ne 2 ]]; then echo "Usage: $0 user@hostname local-directory-name"; return; fi
	mkdir /tmp/$2 && sshfs $1: /tmp/$2
	cd /tmp/$2
}

# Unmount a previously initiated `mount-remote'
function unmount-remote() {
	if [[ $# -ne 1 ]]; then echo "Usage: $0 local-directory-name"; return; fi
	cd ~
	fusermount -u /tmp/$1 && rmdir /tmp/$1
}

# Manage ssh tunnels for http proxys, etc.
function tunnel() {
	if [[ $# -lt 1 || $# -gt 2 ]]; then echo "Usage: $0 user@domain.com [port number]"; return; fi
	tunnel_port=9001
	if [[ $# -eq 2 ]]; then tunnel_port=$2; fi
	ssh -D $tunnel_port -f -C -q -N $1
	unset tunnel_port
}

function tunnels() {
	ps xo pid,command | grep 'ssh \-D [0-9]\+ \-f \-C \-q \-N'
}

function close-tunnel() {
	if [[ $# -ne 1 ]]; then echo "Usage: $0 tunnel-command-pattern"; return; fi
	tunnel_process_details=`tunnels | grep $1`
	if [[ $tunnel_process_details == "" ]]; then echo "No tunnel exists for this pattern: $1"; return; fi
	echo $tunnel_process_details
	echo "Proceed? (Y/n)"
	read proceed
	if [[ "$proceed" == "y" || "$proceed" == "Y" || "$proceed" == "" ]]; then
		tunnel_ssh_pid=`echo "$tunnel_process_details" | awk '{ print \$1 }'`
		kill $tunnel_ssh_pid
		echo "Tunnel closed"
		unset tunnel_ssh_pid
	else
		echo "Exiting"
	fi
	unset proceed tunnel_process_details
}

function close-tunnels() {
	tunnel_process_details=`tunnels`
	if [[ $tunnel_process_details == "" ]]; then echo "No tunnels are currently open."; return; fi
	echo $tunnel_process_details
	echo "Proceed? (Y/n)"
	read proceed
	if [[ "$proceed" == "y" || "$proceed" == "Y" || "$proceed" == "" ]]; then
		pkill -f "ssh -D"
	else
		echo "Exiting"
	fi
	unset tunnel_process_details proceed
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

function order-files-by-size {
	find . -type f -ls | sort -n -k7
}

# Upload files to your server for easy download elsewhere
# Note: set upload_user, upload_domain, and upload_directory in .zsh-custom
function upload() {
	if [[ ! -f $1 && ! -d $1 ]]; then echo "The file $1 does not exist."; return; fi
	file_to_upload=$1
	remove_file_to_upload="no"
	if [ -d $1 ]; then
		if [[ -f "$1.zip" ]]; then
			echo "I don't want to overwrite the existing $1.zip file. Maybe you would like to upload it instead?"
			return
		fi
		zip $1
		file_to_upload="$1.zip"
		remove_file_to_upload="yes"
	fi
	scp $file_to_upload $upload_user@$upload_domain:$upload_directory
	if [[ "$remove_file_to_upload" == "yes" ]]; then
		rm $file_to_upload
	fi
	unset file_to_upload remove_file_to_upload
}

function download() {
	if ! scp $upload_user@$upload_domain:$upload_directory/$1 . 2> /tmp/download-error; then
		if [[ ! "$1" =~ .zip$ ]]; then
			scp $upload_user@$upload_domain:$upload_directory/$1.zip .
		else
			cat /tmp/download-error
		fi
	fi
	rm /tmp/download-error 2> /dev/null
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
	if [[ $# -gt 2 ]]; then echo "Usage: $0 [file name without extension] [file path]"; return; fi
	sass-watch-command "sass" $@
}

# Equivalents to the sass-watch command using scss
function scss-watch() {
	if [[ $# -gt 2 ]]; then echo "Usage: $0 [file name without extension] [file path]"; return; fi
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

# Locate all git repositories under the home directory and add all repos to a list which have changes that need to be pushed
function git-find-repos-with-changes() {
	repos_file_name="/tmp/my-git-repos"
	repos_to_push_file_name="/tmp/my-git-repos-to-push"
	repo_has_changes_string="staged for commit"
	repo_branch_ahead_string="Your branch is ahead"

	find ~ -type d -name ".git" > $repos_file_name

	if [ -f $repos_to_push_file_name ]; then
		rm $repos_to_push_file_name
		touch $repos_to_push_file_name
	fi

	while IFS='' read -r line || [[ -n "$line" ]]; do
		cd "$line/.."
		repo_has_changes=`git status | grep $repo_has_changes_string`
		repo_branch_ahead=`git status | grep $repo_branch_ahead_string`
		echo; echo `pwd`; echo
		if [[ -n $repo_has_changes || -n $repo_branch_ahead ]]; then
			git status
			read user_input </dev/tty
			echo "Add to the recently changed list?"
			if [ "$user_input" = "y" ]; then
				echo $line >> $repos_to_push_file_name
			fi
		fi
		unset repo_has_changes repo_branch_ahead
	done < $repos_file_name

	rm $repos_file_name
	
	unset repos_file_name repos_to_push_file_name repo_has_changes repo_branch_ahead_string
}

function git-next-repo-with-changes() {
	repos_to_push_file_name="/tmp/my-git-repos-to-push"
	new_repos_to_push_file_name="/tmp/my-git-repos-to-push-new"
	
	no_more_repos_message="There are currently no repos marked with changes. To repopulate the list, run git-find-repos-with-changes."
	
	if [[ ! -f $repos_to_push_file_name ]]; then
		echo $no_more_repos_message
		return
	fi
	
	repo_with_changes=`head -n 1 $repos_to_push_file_name`
	
	how_many_more_repos=`wc -l $repos_to_push_file_name | awk '{print $1}'`
	if [[ $how_many_more_repos -eq 1 ]]
		then
			rm $repos_to_push_file_name
		else
			/usr/bin/env tail -n +2 $repos_to_push_file_name > $new_repos_to_push_file_name
			mv $new_repos_to_push_file_name $repos_to_push_file_name
	fi
	if [[ $how_many_more_repos -eq 0 ]]; then
		echo $no_more_repos_message
		return
	fi
	
	cd "$repo_with_changes/.."
	pwd
	git status
	
	unset repos_to_push_file_name new_repos_to_push_file_name no_more_repos_message repo_with_changes how_many_more_repos
}

# For when your command only accepts one argument and you want to expand a bash wildcard file pattern
function for-each() {
	if [[ $# -lt 2 ]]; then echo "Usage: $0 \"command --example\" value [value ...]"; return; fi
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
	if [[ $# -ne 2 ]]; then echo "Usage: $0 \"command --example\" file"; return; fi
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

function ftp-download() {
	if [[ $# -ne 2 ]]; then echo "Usage: $0 username ftp.example.com"; return; fi
	wget -m --ask-password --user="$1" "ftp://$2"
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