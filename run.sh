input="$1"

./target/debug/rcc.exe "$input" | tee tmp.s
gcc -o tmp tmp.s
./tmp
echo $?