#Set up variable
if[ $# == 0]
then 
	exit 0
else
	name=$1
fi

sudo $1 > /etc/paths


