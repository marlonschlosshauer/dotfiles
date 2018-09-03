#set tab completion
autoload -U compinit
compinit

#set auto correction
setopt correctall

#set prompt text 
export PS1=$'\e[0;34m$ %d \e[0m'

#set alias
alias tick="ping www.google.de"
alias zshrc="vim ~/.zshrc"
alias finder="open ~"
alias firefox="/Applications/Firefox.app/Contents/MacOs/firefox"


