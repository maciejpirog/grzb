echo "** should succeed tests"
sum=0
for file in should-succeed/*.while
do
    $file
    .././grzb $file >/dev/null
    if [ $? -eq 0 ]
    then echo "grzb says ok"
    else echo "grzb says error"
    fi
    sum=$(( $sum + $? ))
done
if [ $sum -eq 0 ]
then
    echo "GOOD: should-succeed tests passed"
    exit 0
fi
echo "BAD: at least one should-succeed test failed"
exit 1
