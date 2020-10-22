echo "** examples"
sum=0
for file in ../examples/*.imp
do
    # echo $file
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
    echo "GOOD: examples passed"
    exit 0
fi
echo "BAD: at least one example failed"
exit 1
