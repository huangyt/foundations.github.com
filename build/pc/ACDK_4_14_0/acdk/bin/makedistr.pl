
use Cwd;
#use strict;

#IGNOREFILES='\*.csm \*.sym \*.bak \*.BAK \*.gid \*.mk1 \*.pdb \*.ph \*.rch \*.rws \*.sbr \*.sbt \*.phc \*.pch \*.bsc \*.ilk \*.trc \*.~* \*.lk1 \*.map \*.err \*.evt \*.ids \*.tr2 \*.idb \*.obj \*.o \*.res \*.ncb \*.clsdcl \*.ods \*.i \*.lst \*.exp'
my $exepath = getcwd();
my $SOURCE = '';
my $ARCHIV = '';
my $ASZIP = 0;
my $Suffix = 'acdk';
my $Packageversion;
my $Distribversion;
#$DatenRoot='h:/d';
#$ArchivPath = 'm:\\backup';

if ($#ARGV != 1) {
  my $i = 0;
  while ($i <  $#ARGV ) {
    $_ = @ARGV[$i];
    if (/\-zip/) {
      $ASZIP = 1;
    } elsif (/\-noacdk/) {
      $Suffix = '';
    } elsif (/\-suffix/) {
      $i++;
      $Suffix = @ARGV[$i];
    } elsif (/\-src/) {
      $i++;
      $SOURCE = @ARGV[$i];
    } elsif (/\-archiv/) {
      $i++;
      $ARCHIV = @ARGV[$i];
    } elsif (/\-pv/) {
      $i++;
      $Packageversion = @ARGV[$i];
    } elsif (/\-dv/) {
      $i++;
      $Distribversion = @ARGV[$i];
    }
    $i++;
  }
}

if ($SOURCE eq '' || $ARCHIV eq '') {
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
    "\\.i\$", "\\.lst\$", "\\.exp\$", "\\.plg\$", 
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
$Module = substr($curdir, rindex($curdir, '/') + 1);
chomp($Module);

chdir $SOURCE;
$Flistcorrect = $SOURCE eq ".." ? '' : '../';
$Modulecorrect = $SOURCE eq ".." ? '' : "$Module/";
$ToRootDir = $SOURCE eq ".." ? '..' : '../..';

my $distribdir = $Suffix;
$distribdir = "acdk-$Distribversion" if $Distribversion;

print("Module=[$Module]\n");
print("Suffix=[$Suffix]\n");

processSubDir('', ());

print("PWD: " . `pwd`);

$flistname="$Flistcorrect$Module/filelist.lst";
unlink $flistname if (-f $flistname);
open(FLIST, "> $flistname") or die "Cannot open $flistname";
foreach $f (@FilesToBackup) {
  print(FLIST "$Suffix/$Modulecorrect$f\n");
}
close(FLIST);

if ($ASZIP) {
  $cmd = "cat $Suffix/$Module/filelist.lst | zip -9 $ARCHIV" . ' -@';
} else {
  $cmd = "/bin/tar zcvf $ARCHIV -T $Suffix/$Module/filelist.lst";
}

chdir $ToRootDir;
print("Packing-Dir: " . `pwd`);
print("EXEC: $cmd\n");

print(`$cmd`);
unlink($flistname);

exit 0;






