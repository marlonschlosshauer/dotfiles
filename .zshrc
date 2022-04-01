# Change prompt
PS1="%1d$ "

# Set region
export LC_ALL=en_US.UTF-8

# Import aliases
if [ -f ~/.alias ]; then
    . ~/.alias
fi

# gitignore templating service
function gi() { curl -sLw n https://www.toptal.com/developers/gitignore/api/$@ ;}
export PATH="/opt/homebrew/opt/node@16/bin:/opt/homebrew/lib/node_modules/typescript/lib:$PATH"
