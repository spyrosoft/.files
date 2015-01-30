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

alias zshrc="vi ~/.zshrc"
alias rezshrc="source ~/.zshrc"
alias vimrc="vi ~/.vimrc"

alias vi="vim"
alias emacs="emacs -nw"
alias e="emacs"

alias su="su -"
alias mkdir="mkdir -p"
alias od="od -a"
alias man="man -a"

function find-grep() {
	find . -type f -name "$1" -exec grep -Hn "$2" {} +
}

PROMPT="%{$fg_bold[cyan]%}%C~%{$reset_color%} "
