#set tab completion
autoload -U compinit
compinit

#set auto correction
setopt correctall

#set prompt text 
export PS1=$'\e[0;34m$ %d \e[0m'

#improve color support
export TERM=xterm-256color

#add color to ls
LS_COLORS=$LS_COLORS:'di=1;37:' ; export LS_COLORS

#enable name generation for simpler grep 
setopt extendedglob

#display red dots while waiting for completion.
COMPLETION_WAITING_DOTS="true"

#do not record a command if in history
setopt histignoredups 

#show if dir or file on auto-completion with trailing /
setopt listtypes

#import aliases
source ~/.alias

