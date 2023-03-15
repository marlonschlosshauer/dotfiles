# Change prompt
PS1="%1d$ "

# Set region
export LC_ALL=en_US.UTF-8

# Import aliases
if [ -f ~/.alias ]; then
    . ~/.alias
fi

# Fix git bricking itself because GPG wants in terminal or TTY
export GPG_TTY=$(tty)
