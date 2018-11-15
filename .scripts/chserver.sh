#Checking for > 1 because grep is going to be listed as process

#Check if MySQL is running 
sql=$(ps aux | grep mysql | wc -l);

if [ $sql -gt "1" ]
then
    echo "MySQL is running.";
else
    echo "MySQL is not running.";
fi;

#Check Apache 
Apache=$(ps aux | grep mysql | wc -l);

if [ $Apache -gt "1" ]
then
    echo "Apacheis running.";
else
    echo "Apache is not running.";
fi;
