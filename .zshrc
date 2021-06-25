# Change prompt
PS1="%1d$ "

# Set region
export LC_ALL=en_US.UTF-8

# Import aliases
if [ -f ~/.alias ]; then
    . ~/.alias
fi

# Enable brew completion
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH

  autoload -Uz compinit
  compinit
fi

# gitignore templating service
function gi() { curl -sLw n https://www.toptal.com/developers/gitignore/api/$@ ;}
