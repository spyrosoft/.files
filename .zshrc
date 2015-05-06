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

alias vi="vim"
alias e="emacs"
export EDITOR="/usr/bin/emacs"

alias su="su -"
alias mkdir="mkdir -p"
alias od="od -a"
alias du="du -h"
alias man="man -a"
alias ls="ls -t --color"

function find-grep() {
	find . -type f -name "$1" -exec grep -Hn "$2" {} +
}

function port-knock() {
	if [[ $# -eq 0 ]]
		then
			echo "Error: Need the host as an argument. :D"
		else
			echo "( ( knock ) )"
			nmap -Pn --host_timeout 201 --max-retries 0 -p $2 $1 > /dev/null
			sleep 0.4
			echo "( ( knock ) )"
			nmap -Pn --host_timeout 201 --max-retries 0 -p $3 $1 > /dev/null
			sleep 0.4
			echo "( ( knock ) )"
			nmap -Pn --host_timeout 201 --max-retries 0 -p $4 $1 > /dev/null
			sleep 0.4
	fi
}

PROMPT="%{$fg_bold[cyan]%}%C~%{$reset_color%} "