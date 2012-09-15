
convertfile() {
	awk '{printf "%s\r\n", $0}' $1 >$1.new
	if ! diff $1 $1.new >/dev/null; then \
	  mv -f $1.new $1
	  echo -n "unix2dos: $1 "
	  echo "changed."
	else
    echo -n "."
	  rm $1.new
	fi
}
while test ! -z "$1"; do
  convertfile $1
  shift
done


