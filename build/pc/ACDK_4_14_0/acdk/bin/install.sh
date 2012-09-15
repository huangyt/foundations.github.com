
#
# installation script for ACDK
# 
# install.sh -d installdir | sourcefile destdir
#


create_dir=0
dirname=
source=
target=
withsrc=0
#echo "PWD: $PWD"
#echo `ls -l`
#echo "pwd: `pwd`"
#PWD=`pwd`

while test x"$1" != x; do
    case $1 in
    -acdk-home) shift
        ACDKHOME=$1
        shift
        continue
        ;;
     -web-home) shift
        WEBHOME=$1
        shift
        continue
        ;;
      -package-name) shift
        PACKAGENAME=$1
        shift
        continue
        ;;
     -inclsrc) shift
       withsrc=1
       continue
       ;;
     -pwd) shift
	PWD=$1
	shift
	continue
	;;
     *) source=$1
        shift
        target=$1
        shift
        continue
        ;;
    esac
done

if test z"$ACDKHOME" = z; then
    echo "ACDKHOME is not defined."
    echo "abort installation!"
    exit 1
fi

ensuredir() {
    dirname="$1"
    #echo "ensuredir: $dirname"
    if test x"$dirname" != x; then
        if test -d $dirname; then
            return 0
        fi;
        defaultIFS='
        '
        IFS="${IFS-${defaultIFS}}"
        oIFS="${IFS}"
        IFS='%'
        set - `echo $dirname | sed -e 's@/@%@g' -e 's@^%@/@'`
        IFS="${oIFS}"
        pathcomp=''
        while test $# -ne 0; do
            pathcomp="${pathcomp}${1}"
            #echo $pathcomp
            shift
            if test ! -d "${pathcomp}"; then
                echo "mkdir ${pathcomp}"
                mkdir "${pathcomp}"
            fi
            pathcomp="${pathcomp}/"
        done
    fi
}

mydirname() {
    echo `echo $1 | sed -e 's,[^/]*$,,;s,/$,,;s,^$,.,'`
}

mybasename() {
    pn=$1
    basename "$1"
}

mycp() {
    source=$1
    target=$2
    if test z"$BASH" != z; then
	    if test ! -f "$target" -o "$source" -nt "$target"; then
	      echo "mcp $source $target"
	      cp "$source" "$target"
	    fi
    else
	    echo "cp $source $target"
	    cp "$source" "$target"
    fi
}

installsources() {
    ACDKHOME=$1
    #set -x
    #echo $PWD
    cd $PWD
    if test ! -d src; then
	    echo `ls -l`
	    echo `ls -l src`
      return 0
    fi
    cd src
    for f in `find . -name '*.h'`; do
        spath=`mydirname $f`;
        fname=
        #echo "spath = $spath"
        ensuredir $ACDKHOME/include/$spath
        mycp $f $ACDKHOME/include/$spath/`mybasename $f`
    done
    if test withsrc = 1; then
	for f in `find . -name '*.h'`; do
	    spath=`mydirname $f`;
	    fname=
        #echo "spath = $spath"
	    ensuredir $ACDKHOME/include/$spath
	    mycp $f $ACDKHOME/include/$spath/`mybasename $f`
	done
    fi
    cd ..
    if test -d tests; then
      cd tests
      for f in `find . -name '*.h'`; do
        spath=`mydirname $f`;
        fname=
        #echo "spath = $spath"
        ensuredir $ACDKHOME/include/$spath
        mycp $f $ACDKHOME/include/$spath/`mybasename $f`
      done
      if test withsrc = 1; then
	for f in `find . -name '*.h'`; do
	    spath=`mydirname $f`;
	    fname=
        #echo "spath = $spath"
	    ensuredir $ACDKHOME/include/$spath
	    mycp $f $ACDKHOME/include/$spath/`mybasename $f`
	done
      fi
      cd ..
    else
      echo "package as no Tests"
    fi
}

installcfgs() {
    ACDKHOME=$1
    echo "installcfgs"
    if test ! -d cfg; then
	echo "target has no cfg"
        return 0
    fi
    cd cfg
    # doesnot work for solaris for f in `find . -type f \! -path '*/CVS/*'`; do \
    for f in `find . -type f`; do
	if echo $f | awk '{ if ($1 ~ /\/CVS\//) exit 1; else exit 0; }'; then
	    spath=`mydirname $f`;
	    ensuredir $ACDKHOME/cfg/$spath
	    mycp $f $ACDKHOME/cfg/$spath/`mybasename $f`
	fi
    done
    cd ..
}

installclasses() {
    ACDKHOME=$1
    cd src
    for f in `find . -name '*.class'`; do \
        spath=`mydirname $f`;
        ensuredir $ACDKHOME/bin/$spath
        mycp $f $ACDKHOME/bin/$spath/`mybasename $f`
    done
    cd ..
}

installbin () {
  ACDKHOME=$1
  if test ! -d bin; then
    return 0
  fi
  cd bin
  for f in `find . -type f`; do \
    spath=`mydirname $f`;
    ensuredir $ACDKHOME/bin/$spath
    mycp $f $ACDKHOME/bin/$spath/`mybasename $f`
  done
  cd ..
}

installdocs() {
    WEBHOME=$1
    PACKAGENAME=$2
    
    if test z"$WEBHOME" = "z"; then
        return 0
    fi
    if test ! -d "$WEBHOME"; then 
        #echo "$WEBHOME does not exists"
        return 0
    fi
    echo "install docs into $WEBHOME / $PACKAGENAME"
    
    ensuredir $WEBHOME/$PACKAGENAME
    
    for f in `find docs -type f -name '*.xml'` `find docs -type f -name '*.htxt'`; do 
        spath=`mydirname $f`
        ensuredir $WEBHOME/$PACKAGENAME/$spath
        mycp $f $WEBHOME/$PACKAGENAME/$spath/`mybasename $f`
    done
}

#echo "ACDKHOME = $ACDKHOME"
installsources "$ACDKHOME"
installcfgs "$ACDKHOME"
installbin "$ACDKHOME"
installclasses "$ACDKHOME"
installdocs "$WEBHOME" "$PACKAGENAME"
