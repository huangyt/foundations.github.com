
DISTRIBVERSION=4.02
PACKAGEVERSION=4.02.0
RPMHOME=/artefaktur/distribution/rpms
GENWEBHOME=../genweb
set -x
PERL=perl


## note PWD is==ACDKHOME

# need for second stage 
cp $GENWEBHOME/bin/backup.pl distrib


cd ..


if test 1 = 1; then
    ln -s ./acdk ./acdk-$DISTRIBVERSION
 $PERL ./genweb/bin/backup.pl \
  --exclbin --exclcvs --distignore \
  -ptsrc\.html -pids\.db -p\.\#.* \
  -etds -eilf -eils -eilc -eild -elimg -estackdump -eref -epft -esuo -epbi -epbt -edout -edb \
  -P/idl/.*\.cpp -P/idl/.*\.h -elog -e_ll -n_profile.txt -n_profile1.txt -nKopie \
  -dacdk_wx -dacdk_doc -dacdk_tk -dacdk_gtk -dacdk_tools_modtests -dacdk_tutorial \
  -daal -dacdkx_corba -dacdkx_sol -dacdk_dmi \
  -dCVS -dtobj \
  -P^include +Pinclude/note.txt \
  -P^cfg +P^cfg/general \
  -nimages +P^images \
  --name acdk-$PACKAGEVERSION-src acdk-$DISTRIBVERSION 

 rm ./acdk-$DISTRIBVERSION
 cp acdk-$PACKAGEVERSION-src.tgz $RPMHOME/SOURCES/acdk-$PACKAGEVERSION-src.tar.gz
fi

rpm -vv -ba acdk/distrib/acdk.spec
#rpm -vv -ba acdk/distrib/acdk.spec


