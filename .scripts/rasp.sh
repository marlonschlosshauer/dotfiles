#Wrapper for using git commands specifically from own git server

#Manage variables
#Exit if below two
#Execute command

#Bind alias to rasp to rasp.sh for easy access

#Set up variable 
if [ $# -lt 2 ]
then
    echo "Not enough variables."
    exit 1;
else
    command=$1
    prefix=$2;
fi

git $1 git@192.168.2.106:/home/git/repos/$2
