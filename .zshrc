# Change prompt
PS1='%D{%H:%M:%S} %~ $ '

# Set region
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Import aliases
if [ -f ~/.alias ]; then
    . ~/.alias
fi

export PATH="/opt/homebrew/opt/node@16/bin:/opt/homebrew/lib/node_modules/typescript/lib:$PATH"

# Fix git bricking itself because GPG wants in terminal or TTY
export GPG_TTY=$(tty)

# bun completions
[ -s "/Users/msc/.bun/_bun" ] && source "/Users/msc/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
