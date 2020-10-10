echo "** should fail tests"
sum=1
for file in should-fail/*.while
do
    echo $file
    .././grzb $file >/dev/null
    sum=$(( $sum * $? ))
done
if [ $sum -eq 0 ]
then
    echo "BAD: should fail test failed. At least one program succeeded!"
    exit 1
fi
echo "GOOD: every should-fail test failed"
exit 0
