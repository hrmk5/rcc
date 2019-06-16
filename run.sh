input="$1"

cargo run --quiet "$input" > tmp.s
if [ $? -ne 0 ]; then
    cat tmp.s
    exit 1
fi

gcc -o tmp tmp.s
./tmp
echo $?
