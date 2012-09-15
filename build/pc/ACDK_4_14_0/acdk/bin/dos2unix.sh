

echo "Making dos2unix for all source"
SUFFIXES="bsd c cpp h html lsp linux sunos-gcc htxt txt nmake"
NAMES="README INSTALL LICENCE CHANGELOG"

convertfile() {
  cat $1 | tr -d '\r' >$1.new
	  if ! diff $1 $1.new >/dev/null; then
	    mv -f $1.new $1
	    echo -n "dos2unix: $1 "
	    echo "changed."
	  else
	    echo -n "."
	    rm $1.new
	  fi
}

for SUF in $SUFFIXES; do
  for FILE in `find . -name "*.$SUF" -type f -print`; do
	  convertfile $FILE
  done
done
echo "Readme files"
for NAM in $NAMES; do
  if test -f $NAM; then 
    convertfile $NAM
  fi
done
echo "Done."
