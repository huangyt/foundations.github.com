
use Cwd;
#use strict;

#IGNOREFILES='\*.csm \*.sym \*.bak \*.BAK \*.gid \*.mk1 \*.pdb \*.ph \*.rch \*.rws \*.sbr \*.sbt \*.phc \*.pch \*.bsc \*.ilk \*.trc \*.~* \*.lk1 \*.map \*.err \*.evt \*.ids \*.tr2 \*.idb \*.obj \*.o \*.res \*.ncb \*.clsdcl \*.ods \*.i \*.lst \*.exp'
my $exepath = getcwd();
my $SOURCE = '';
my $ARCHIV = '';
my $ASZIP = 0;
my $PREFIX = 'acdk';
my $PVERSION;
my $DVERSION;
my $NAME;
my $SNAP = 0;


#$DatenRoot='h:/d';
#$ArchivPath = 'm:\\backup';

if ($#ARGV != 1) {
  my $i = 0;
  while ($i <  $#ARGV ) {
    $_ = @ARGV[$i];
    if (/\-snap/) {
      $SNAP = 1;
    } elsif (/\-zip/) {
      $ASZIP = 1;
    } elsif (/\-prefix/) {
      $i++;
      $PREFIX = @ARGV[$i];
    } elsif (/\-src/) {
      $i++;
      $SOURCE = @ARGV[$i];
    } elsif (/\-name/) {
      $i++;
      $NAME = @ARGV[$i];
    } elsif (/\-pv/) {
      $i++;
      $PVERSION = @ARGV[$i];
    } elsif (/\-dv/) {
      $i++;
      $DVERSION = @ARGV[$i];
    }
    $i++;
  }
}

if ($SOURCE eq '' || $NAME eq '') {
  print("makedistr [-zip] -src <sourcedir> -archiv <archivfile>\n");
  exit 1;
}



$c=0;


print("make distribution with SOURCE=[$SOURCE] ARCHIV=[$ARCHIV]\n");


@_IgnoreFiles =
  ( ".*bak", "*.o", "*.csm", "*.sym", "*.gid", "*.mk1", "*.pdb",
    "*.ph", "*.rch", "*.rws", "*.sbr", "*.sbt", "*.phc", "*.pch",
    "*.bsc", "*.ilk", "*.trc", "*.lk1", "*.map", "*.ids", "*.tr2",
    "*.idb", "*.obj", "*.o", "*.res", "*.ncb", "*.clsdcl", "*.ods",
    "*.i", "*.lst", "*.exp", "*.plg");

@IgnoreFiles =
  ( "\\.bak\$", "\\.o\$", "\\.csm\$", "\\.sym\$", "\\.gid\$", "\\.mk1\$", "\\.pdb\$",
    "\\.ph\$", "\\.rch\$", "\\.rws\$", "\\.sbr\$", "\\.sbt\$", "\\.phc\$", "\\.pch\$",
    "\\.bsc\$", "\\.ilk\$", "\\.trc\$", "\\.lk1\$", "\\.map\$", "\\.ids\$", "\\.tr2\$",
    "\\.idb\$", "\\.obj\$", "\\.o\$", "\\.res\$", "\\.ncb\$", "\\.clsdcl\$", "\\.ods\$",
    "\\.i\$", "\\.lst\$", "\\.exp\$", "\\.plg\$", "\\.deps\$", "\\#\$", "\\~\$", 
    "^CVS\$"
    );

#$zip = "g:\\cygwin\\contrib\\bin\\zip.exe";
#$zip = "f:\\bin\\doszip.exe";
#$cat = "g:/cygwin/bin/cat.exe";
#$cat = "type";

@FilesToBackup = ();

sub getFileContent($) {
  return "" unless open(DECF, "$_[0]");
  my @all = <DECF>;
  close(DECF);
  #my $t = scalar @all;
  if (wantarray) {
    return @all;
  } else {
    return "@all";
  }
}



sub processSubDir($$) {
	my ($sdir, @cvsignore) = @_;
	print("makedistr: $sdir\n");
	my @allfiles = glob('*');
	if (-f '.distignore') {
		@cvsignore = getFileContent('.distignore');
		#print("DISTIGNORE: @cvsignore\n");
	}
	my @restfiles = ();
	my @allignore = ();
	for (@IgnoreFiles) { unshift(@allignore, $_); }
	for my $i (@cvsignore) {
	  chomp($i);
	  unshift(@allignore, $i);
	  #print("IGNORE ADD bin: [$i]\n");

	}
	#print("ALLIGNORE: @allignore\n");
	foreach my $f (@allfiles) {
	  my $ign = 0;
	  foreach my $p (@allignore) {
	    if ($ign == 1) {
	    } else {
	      $_ = $f;
	      if ($p ne '' && /$p/) {
	        print("FILE=[$f]; $PATTERN=[$p]; IGNORE\n");
	        $ign = 1;
	      } else {
  	      #print("FILE=[$f]; PATTERN=[$p]; NOT IGNORE\n");

	      }
	    }
	  }
	  if ($ign == 0) {

	    push (@restfiles, $f);
	  }
	}
	foreach my $f (@restfiles) {
		if (-d $f) {
			chdir $f;
			processSubDir(($sdir eq "" ? $f : "$sdir/$f"), @cvsignore);
			chdir '..';
		} else {
      #print("Backup File: " . "$sdir/$f" . "\n");
			push(@FilesToBackup, ($sdir eq "" ? $f : "$sdir/$f"));
		}
	}
}

my $curdir = `pwd`;
if ($SNAP) {
  $Module = '.';
} else {
  $Module = substr($curdir, rindex($curdir, '/') + 1);
  chomp($Module);
}


chdir $SOURCE;


$Flistcorrect = $SOURCE eq ".." ? '' : '../';
$Modulecorrect = $SOURCE eq ".." ? '' : "$Module/";


if ($SNAP) {
  $ToRootDir = "..";
  $Flistcorrect = '..';
} else {
  $ToRootDir = $SOURCE eq ".." ? '..' : '../..';
}

processSubDir('', ());

my @corefiles = (
  'acdk_all.linux',
  'acdk_all.nmake',
  'acdk_all.sunos-gcc',
  'acdk_globals.linux',
  'acdk_globals.sunos-gcc',
  'Makefile',
  'acdk_all.dsw',
  'make_mod.cmd',
  'bin/acdklisp.cmd',
  'bin/acdkmc.cmd',
  'bin/install.cmd',
  'bin/install_cmd.exl',
  'bin/install.sh', 
  'cfg/general/version.txt'
                 
);

print("PWD: " . `pwd`);

if ($SNAP) {
  $flistname = "../filelist.lst";
} else {
  $flistname="$Flistcorrect$Module/filelist.lst";
}
my $ddir = $PREFIX;
$ddir = "$ddir-$DVERSION" if $DVERSION;

unlink $flistname if (-f $flistname);
open(FLIST, "> $flistname") or die "Cannot open $flistname";
foreach $f (@FilesToBackup) {
  print(FLIST "$ddir/$Modulecorrect$f\n");
}

if ($Module eq 'acdk_core' || $SNAP) {
  for my $f (@corefiles) {
    print(FLIST "$ddir/$f\n");
  }    
}

close(FLIST);

my $zipfilename = $NAME;
$zipfilename = "$zipfilename-$PVERSION" if $PVERSION;

my $fullflistname = "$ddir/$Module/filelist.lst";

$fullflistname = 'filelist.lst' if ($SNAP);

if ($ASZIP) {
  $cmd = "cat $ddir/$Module/filelist.lst | zip -9 distribution/$zipfilename.zip" . ' -@';
} else {
  $cmd = "/bin/tar zcvf distribution/$zipfilename.tar.gz -T $fullflistname";
}

chdir $ToRootDir;

print("Packing-Dir: " . `pwd`);
print("EXEC: $cmd\n");

print(`$cmd`);
unlink($flistname);

exit 0;






