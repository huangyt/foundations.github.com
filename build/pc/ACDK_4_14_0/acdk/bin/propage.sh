
# expect file subdir modules

f=$1
shift
subdir=$1
if test "x$1" = "x"; then
    echo "<file to copy> <subdir in the package> <packages..>"
    exit
fi
shift
while test "x$1" != "x"; do
    echo "cp $f $1/$subdir"
    cp $f $1/$subdir
    shift
done

