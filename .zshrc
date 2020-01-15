export PATH=~/Applications/:/usr/local/bin:/bin:/usr/sbin:/sbin:/usr/local/share/dotnet:~/.dotnet/tools:/usr/bin:/Library/TeX/texbin/

# Change prompt
PS1="%1d$ "

# Import aliases
if [ -f ~/.alias ]; then
    . ~/.alias
fi

