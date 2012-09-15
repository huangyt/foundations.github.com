
# 
# To implement Features 
# -exclude <pattern>
# -include <pattern>
# -exclude-dir <pattern>
#
# Nice to have 
# -split <size>
# -cvsignore
# -distignore


use Cwd;
use POSIX;
#use strict;


my $exepath = getcwd();
my $SOURCE = '';
my $ARCHIV = '';
my $ASZIP = 0;
my $PREFIX = '';
my $PVERSION;
my $DVERSION;
my $NAME = '';
my $SNAP = 0;
my $SOLARISTAR = 0;
my $SPLIT = 0;
my $FILELIST = 0;

my @PATTERN = ();


my $READDISTIGNORE = 0;
my $READCVSIGNORE = 0;


my @StandardPattern =
  ( "+p.", "-p\\.bak\$", "-p\\.o\$", "-p\\.csm\$", "-p\\.sym\$", "-p\\.gid\$", "-p\\.mk1\$", "-p\\.pdb\$",
    "-p\\.ph\$", "-p\\.rch\$", "-p\\.rws\$", "-p\\.sbr\$", "-p\\.sbt\$", "-p\\.phc\$", "-p\\.pch\$",
    "-p\\.bsc\$", "-p\\.ilk\$", "-p\\.trc\$", "-p\\.lk1\$", "-p\\.map\$", "-p\\.ids\$", "-p\\.tr2\$",
    "-p\\.idb\$", "-p\\.obj\$", "-p\\.o\$", "-p\\.res\$", "-p\\.ncb\$", "-p\\.clsdcl\$", "-p\\.ods\$",
    "-p\\.i\$", "-p\\.lst\$", "-p\\.exp\$", "-p\\.plg\$", "-p\\.deps\$", 
    "-dSunWS_cache", "-p^core\$", "-epurify", "-dpurify_cache", 
    "-p\\#\$", "-p\\~\$", 
    "-p\\.opt\$",
    "-p^\\.\$", "-p^\\.\\.\$"
    );

for (@StandardPattern) { push(@PATTERN, $_); }

my @NoExecutables = 
( "-p\\.lib\$", "-p\\.dll\$", "-p\\.exe\$", 
  "-eso", "-ea"
  ) ;



push(@NoExecutables, "-fELF");# if ("$OSNAME" != "MSWin32");


my @NoCvs = 
( "-d^CVS\$", "-d^\.cvsignore\$" );
 


sub help()
{
  print <<EOF;
  
  Backup script (mailto:kommer\@artefaktur.com)
  
  backup.pl [options] directory
  
  where options are:
  
File/dir matching: 
  [+-]ppattern  in/exclude pattern to file
  [+-]Ppattern  in/exclude pattern to path
  [+-]eext      in/exclude file.ext
  [+-]dpath     in/exclude directory path
  [+-]nfilename in/exclude file name
  [+-]fpattern  in/exclude using unix file match against pattern
  [+-]mminsize  in/exclude files with min size in byte
  [+-]Mminsize  in/exclude files with maximum size in byte
  --nostdp      no standard file pattern
  --exclbin     exclude binary executable files
  --exclcvs     exclude CVS related files
  --cvsignore   read .cvsignore in dir and ignore files/dir
  --distignore  read .distignore in dir and ignore files/dir
    
Archive options:
  --slit size   make more archives at KB uncompressed files
  --zip         using zip to make archive
  --soltar      using solaris tar and gzip
  --filelist    create only file list, no archive
  --name        name of archive. If no name is given the first argument
                is used.
  --help        print this help
  
  Each pattern will be applied to file in the order they are declared. The
  last pattern matches to the file determine, if the file will be included or not.
  In normal case the patterns begin with +p. to include all files.
  
EOF
}


if ($#ARGV != -1) {
  my $i = 0;
  while ($i <=  $#ARGV ) {
    $_ = @ARGV[$i];
    if (/\-snap/) {
      $SNAP = 1;
    } elsif (/\-zip/) {
      $ASZIP = 1;
    } elsif (/\-prefix/) {
      $i++;
      $PREFIX = @ARGV[$i];
    } elsif (/^\-\-exclbin$/) {
      for (@NoExecutables) { push(@PATTERN, $_); }
    } elsif (/\-\-cvsignore/) {
      $READCVSIGNORE = 1;
    } elsif (/\-\-distignore/) {
      $READDISTIGNORE = 1;
    } elsif (/\-\-nostdp/) {
      @PATTERN  = ();
    } elsif (/\-\-exclcvs/) {
      for (@NoCvs) { push(@PATTERN, $_); }
    } elsif (/\-\-split/) {
      $i++;
      $SPLIT = $ARGV[$i];
    } elsif (/\-\-soltar/) {
      $SOLARISTAR = 1;
    } elsif (/\-\-filelist/) {
      $FILELIST = 1;
    } elsif (/\-\-help/) {
      help();
      return 0;
      
    } elsif (/\-src/) {
      $i++;
      $SOURCE = $ARGV[$i];
    } elsif (/\-\-name/) {
      $i++;
      $NAME = @ARGV[$i];
    }  elsif (/[\-\+]d(.*)/ || /[\-\+]e(.*)/ ||
              /[\-\+]p(.*)/ || /[\-\+]f(.*)/ ||
              /[\-\+]n(.*)/ || /[\-\+]P(.*)/ ||
              /[\-\+]M(.*)/)
    {
      push(@PATTERN, @ARGV[$i]);
    } else {
      $SOURCE = @ARGV[$i];
    }
    $i++;
  }
}


if ($NAME eq '' && $SOURCE ne '')
{
  $NAME = $SOURCE . '-' . `date '+%Y-%m-%d_%H-%M-%S'`;
  chomp($NAME);
}

LOGT("NAME= $NAME SOURCE = $SOURCE");
if ($SOURCE eq '' || $NAME eq '') {
  help();
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
    "^CVS\$",
    "\\.lib\$", "\\.dll\$", "\\.exe\$", "\\.opt\$",
    "^\\.\$", "^\\.\\.\$"
    
    );




@FilesToBackup = ();

sub getFileContent($) {
  return "" unless open(DECF, "$_[0]");
  my @all = <DECF>;
  close(DECF);
  if (wantarray) {
    return @all;
  } else {
    return "@all";
  }
}

sub getFiles($)
{
  my ($dir) = @_;
  my $dd = POSIX::opendir($dir);
  my @files = POSIX::readdir($dd);
  POSIX::closedir($dd);
  return @files;
}

sub LOGT($)
{
  my ($msg) = @_;
  print("    $msg\n");
}

sub LOGD($)
{
  my ($msg) = @_;
  print("D   $msg\n");
}

sub LOGW($)
{
  my ($msg) = @_;
  print("**  $msg\n");
}

sub LOGE($)
{
  my ($msg) = @_;
  print("*** $msg\n");
}


sub ingoreFile($$)
{
  my ($f, $path) = @_;
  my $ignore = 0;
  for my $p (@PATTERN)
  {
    if ($p =~ /\+d(.*)/) {
      if ("$path/$f" =~ /(.*?)$1(.*)/)
      {
        LOGT("   $p [$path/$f] in $path");
        $ignore = 0;
      }
    } 
    elsif ($p =~ /([\+\-])p(.*)/) 
    {
      my $sign = $1;
      my $pattern = $2;
      if ($f =~ /$pattern/)
      {
        LOGT("   $p [$path/$f]");
        if ($sign eq '-')
        {
          $ignore = 1;
        } else {
          $ignore = 0;
        }
       }
    } 
    elsif ($p =~ /([\+\-])P(.*)/) 
    {
        my $sign = $1;
        my $ext = $2;
        if ("$path/$f" =~ /$ext/)
        {
          LOGT("   $p [$path/$f]");
          if ($sign eq '-')
          {
            $ignore = 1;
          } else {
            $ignore = 0;
          }
        }
    } elsif ($p =~ /([\-\+])e(.*)/) 
    {
      my $sign = $1;
      my $ext = $2;
      if ($f =~ /.*\.$ext$/)
      {
        LOGT("   $p [$f] in $path");
        if ($sign eq '-')
        {
          $ignore = 1;
        } else {
          $ignore = 0;
        }
      }
    } 
    elsif ($p =~ /([\+\-])f(.*)/) 
    {
      my $sign = $1;
      my $pattern = $2;
      my $fileout = `file $f`;
      #print "filetype:$fileout\n";
      #LOGD("   file [$f]: $fileout");
      if ($fileout =~ /$pattern/)
      {
        LOGT("   $p [$f] in $path");
        if ($sign eq '-')
        {
          $ignore = 1;
        } else {
          $ignore = 0;
        }
      }
    } 
    elsif ($p =~ /([\+\-])M(.*)/) 
    {
      my $sign = $1;
      my $maxsize = $2;
      my @sa = stat($f);
      if ($sa[7] > $maxsize)
      {
        LOGT("   $p [$f] in $path");
        if ($sign eq '-')
        {
          $ignore = 1;
        } else {
          $ignore = 0;
        }
      }
    } 
    elsif ($p =~ /([\+\-])\m(.*)/) 
    {
      my $sign = $1;
      my $minsize = $2;
      my @sa = stat($f);
      if ($sa[7] < $minsize)
      {
        LOGT("   $p [$f] in $path");
        if ($sign eq '-')
        {
          $ignore = 1;
        } else {
          $ignore = 0;
        }
      }
    } 
    elsif ($p =~ /([\+\-])n(.*)/) 
    {
      my $sign = $1;
      my $name = $2;
      if ($f eq $name)
      {
        LOGT("   $p [$f] in $path");
        if ($sign eq '-')
        {
          $ignore = 1;
        } else {
          $ignore = 0;
        }
      }
    } 
  }
  #LOGT("[$path/$f] $ignore");
  return $ignore;
}

sub ignoreDir($)
{
  my ($f) = @_;
  my $ignore = 0;
  #LOGT("    IG $f");
  for my $p (@PATTERN)
  {
    if ($p =~ /\+d(.*)/) {
      my $d = $1;
      if ($f =~ /(.*?)$d(.*)/)
      {
        LOGT("   $p [$f]");
        $ignore = 0;
      }
    } elsif ($p =~ /\-d(.*)/) {
      my $d = $1;
      if ($f =~ /(.*?)$d(.*)/)
      {
        LOGT("   $p [$f]");
        $ignore = 1;
      }
    }
  }
  return $ignore;
}

sub processSubDir($$) {
   my ($sdir, @cvsignore) = @_;
   my @allfiles = getFiles('.');
   
   if ($READCVSIGNORE && -f '.cvsignore') {
     @cvsignore = getFileContent('.distignore');
   }
   if ($READDISTIGNORE  && -f '.distignore') {
      @cvsignore = getFileContent('.distignore');
   }
   my @restfiles = ();
   my @allignore = ();
   #for (@IgnoreFiles) { unshift(@allignore, $_); }
   
   for my $i (@cvsignore) 
   {
     chomp($i);
     unshift(@allignore, $i);
   }
   foreach my $f (@allfiles) 
   {
     my $ign = 0;
     foreach my $p (@allignore) 
     {
       if ($ign == 1) 
       {
             
       } else {
         $_ = $f;
         if ($p ne '' && /$p/) 
         {
           LOGT("FILE=[$f]; $PATTERN=[$p]; IGNORE");
           $ign = 1;
         } 
       }
     }
     if ($ign == 0) 
     {
      if (-d $f || ingoreFile($f, $sdir) == 0)
      {
         push (@restfiles, $f);
       }
     }
   }
   foreach my $f (@restfiles) 
   {
     if (!-r $f) {
       LOGW("Cannot read file [$sdir/$f]");
     } elsif ($f eq '.' || $f eq '..') {
       # nothing
     } elsif (-l $f) {
       LOGT("Not following link [$sdir/$f]");
     } elsif (-d $f && !(-x $f)) {
       LOGW("Cannot execute directory [$sdir/$f]");
       # nothing
      } elsif (-d $f) {
        if (ignoreDir("$sdir/$f") == 0)
        {
           #LOGD("Before $sdir " . `pwd`);
           chdir $f;
           processSubDir(($sdir eq "" ? $f : "$sdir/$f"), @cvsignore);
           chdir '..';
           #LOGD("After $sdir " . `pwd`);
         }
      } else {
      #print("Backup File: " . "$sdir/$f" . "\n");
         push(@FilesToBackup, ($sdir eq "" ? $f : "$sdir/$f"));
      }
   }
}

my $curdir = `pwd`;
$Module = $SOURCE;
#if ($SNAP) {
#  
#} else {
  #$Module = substr($curdir, rindex($curdir, '/') + 1);
  #chomp($Module);
#}




LOGD("Pattern: @PATTERN\n");

$Flistcorrect = $SOURCE eq ".." ? '' : '../';
$Modulecorrect = $SOURCE eq ".." ? '' : "$Module/";


#if ($SNAP) {
  #$ToRootDir = "..";
  #$Flistcorrect = '..';
#} else {
  #$ToRootDir = $SOURCE eq ".." ? '..' : '../..';
#}

my $sicpwd = `pwd`;
chomp($sicpwd);

chdir $SOURCE || die "Cannot chdir to $SOURCE pwd=" . `pwd`;

processSubDir('', ());
chdir '..' if ($SOURCE ne '.');
my $newpwd = `pwd`;
chomp($newpwd);
die "original PWD is not equal. OLD PWD=[$sicpwd], now [$newpwd]" if ($newpwd ne $sicpwd);
print("PWD: " . `pwd`);


$flistname = "filelist.lst";


unlink $flistname if (-f $flistname);

my @filelistnames = ();
my $archiveVolumeId = '';

if ($SPLIT > 0)
{
  $SPLIT = $SPLIT * 1000;
  my $cursize = 0;
  my $curvol = 0;
  open(FLIST, "> $flistname$curvol") or die "Cannot open $flistname$curvol";
  foreach $f (@FilesToBackup) 
  {
    my @sa = stat("$Modulecorrect$f");
    my $nfs = $sa[7];
    $nfs = $nfs * 1000 if ($f =~ /.*\.zip/ || $f =~ /.*\.gz/ || $f =~ /.*\.gz/);
    my $filesize = $nfs;
    
    #LOGT("file=[$f] cursize=[$cursize]; filesize=[$filesize]; SPLIT=[$SPLIT]");
    if ($cursize + $filesize > $SPLIT)
    {
      close(FLIST);
      push(@filelistnames, "$flistname$curvol");
      ++$curvol;
      $cursize = 0;
      open(FLIST, "> $flistname$curvol") or die "Cannot open $flistname$curvol";
    }
    $cursize += $filesize;
    print(FLIST "$Modulecorrect$f\n");
  }
  close(FLIST);
  push(@filelistnames, "$flistname$curvol");
  $archiveVolumeId = '1';
} else {
  push(@filelistnames, $flistname);
  open(FLIST, "> $flistname") or die "Cannot open $flistname";
  foreach $f (@FilesToBackup) {
    print(FLIST "$Modulecorrect$f\n");
  }
  close(FLIST);
}

for my $flistname (@filelistnames)
{
  my $zipfilename = $NAME;
  $zipfilename .= "_$archiveVolumeId" if ($archiveVolumeId != 0);
  if ($ASZIP) 
  {
    $cmd = "cat $flistname | zip -9 $zipfilename.zip" . ' -@';
  } 
  elsif ($FILELIST)
  {
    `cp $flistname $zipfilename.lst`;
  }
  else 
  {
    if ($SOLARISTAR)
    {
      $cmd = "/bin/tar cvf $zipfilename.tar" . " -I $flistname; gzip $zipfilename.tar";
    } else {
      $cmd = "/bin/tar zcvf $zipfilename.tgz" . " -T $flistname";
    }
  }
  print("EXEC: $cmd\n");
  print(`$cmd`);
  unlink($flistname);
  $archiveVolumeId += 1;
}

