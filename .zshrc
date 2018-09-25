#set tab completion
autoload -U compinit
compinit

#set auto correction
setopt correctall

#set prompt text 
export PS1=$'\e[0;34m$ %d \e[0m'

#improve color support
export TERM=xterm-256color

#set alias
alias tick="ping www.google.de"
alias zshrc="vim ~/.zshrc"
alias finder="open ~"
alias rasp="ping 192.168.2.106"
alias sshrasp="ssh pi@192.168.2.106"
alias reload=". ~/.zshrc"
alias enterp="cd /Users/akira/Projects/c#/Lookup"
alias entersh="cd /Users/akira/Documents/shellscripts/"
alias paths="sudo vim /etc/paths"
