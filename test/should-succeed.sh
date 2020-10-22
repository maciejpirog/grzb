echo "** should succeed tests"
sum=0
for file in should-succeed/*.imp
do
    # $file
    .././grzb $file >/dev/null
    res=$?
    if [ $res -eq 0 ]
    then echo "grzb says ok"
    else
	echo ""
        echo "\033[0;31mgrzb says error:"
	echo $file
	echo "\033[0m"
    fi
    sum=$(( $sum + $res ))
done
if [ $sum -eq 0 ]
then
    echo "GOOD: should-succeed tests passed"
    exit 0
fi
echo "BAD: at least one should-succeed test failed"
exit 1
