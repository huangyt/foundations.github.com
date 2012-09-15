echo "Making unix2dos for all source "
SUFFIXES="c cpp h html lsp htxt txt csf cfg"
NAMES="README INSTALL LICENCE CHANGELOG"

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


