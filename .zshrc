# Change prompt
PS1="%1d$ "

# Set region
export LC_ALL=en_US.UTF-8

# Import aliases
if [ -f ~/.alias ]; then
    . ~/.alias
fi

export PATH="/opt/homebrew/opt/node@16/bin:/opt/homebrew/lib/node_modules/typescript/lib:$PATH"

# Fix git bricking itself because GPG wants in terminal or TTY
export GPG_TTY=$(tty)
