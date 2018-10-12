#set tab completion
autoload -U compinit
compinit

#set auto correction
setopt correctall

#set prompt text 
export PS1=$'\e[0;34m$ %d \e[0m'

#improve color support
export TERM=xterm-256color

#enable name generation for simpler grep 
setopt extendedglob

#display red dots while waiting for completion.
COMPLETION_WAITING_DOTS="true"

#do not record a command if in history
setopt histignoredups 

#show if dir or file on auto-completion with trailing /
setopt listtypes

#set alias
alias tick="ping www.google.de"
alias zshrc="vim ~/.zshrc"
alias vimrc="vim ~/.vimrc"
alias finder="open ~"
alias rasp="ping 192.168.2.106"
alias sshrasp="ssh pi@192.168.2.106"
alias reload=". ~/.zshrc"
alias enterp="cd ~/Projects/csharp/Lookup"
alias entersh="cd /Users/akira/Documents/shellscripts/"
alias paths="sudo vim /etc/paths" #Need to fix this
alias progs="cd /usr/local/"
alias emacs.d="cd ~/.emacs.d/"
