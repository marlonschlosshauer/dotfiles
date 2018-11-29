#Set up variable
if [ $# == 0 ]
then 
	exit 0
else
	command=$1
fi

grep -rnw * -e command 
