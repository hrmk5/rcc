input="$1"

tmpfile=$(mktemp "/tmp/tmp.XXXXXX.s")
tmpbin=$(mktemp "/tmp/tmp.XXXXXX")

atexit() {
    rm "$tmpfile" "$tmpbin"
}

trap atexit EXIT

cargo run "$input" > "$tmpfile"
if [ $? -ne 0 ]; then
    cat "$tmpfile"
    exit 1
fi

gcc -o "$tmpbin" "$tmpfile"
$tmpbin
echo $?
