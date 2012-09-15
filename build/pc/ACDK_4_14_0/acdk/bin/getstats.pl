
my $headercount = 0;
my $headerlinecount = 0;
my $headerbytes = 0;
my $sourcecount = 0;
my $sourceline = 0;
my $classescount = 0;

sub getFileContent($) 
{
  return "" unless open(DECF, "$_[0]");
  my @all = <DECF>;
  close(DECF);
  my $t = scalar @all;
  if (wantarray) {
    return @all;
  } else {
    return "@all"; 
  }
}


sub processHeader($)
{
  my ($file) = @_;
  ++$headercount;
  for my $l (getFileContent($file)) {
    $headerbytes += length($l);
    chomp($l);
    ++$headerlinecount;
    
    #if ($l =~ /[^\<t]class / && !($l =~ /\, class /) && !($l =~ /\/\/ class/) ) 
    if (($l =~ /^class / || $l =~ /^struct /) && !($l =~ /\;/) ) 
    {
      print("Class = [$l] [$file]\n");
      ++$classescount;
    }
  }
}

sub processSource($)
{
  my ($file) = @_;
  return processHeader($file);
}

sub processSubDir($) 
{
	my ($sdir) = @_;
	my @allfiles = glob('*');
	foreach my $f (@allfiles) {
    if (-d $f) {
      next if (($sdir eq '.' && $f eq 'include') || $f eq 'acdk_doc');
  		chdir $f;
	  	processSubDir("$sdir/$f");
		  chdir '..';
    } else {
      $_ = $f;
      if (/.+\.h$/) {
        processHeader($f);
      } elsif ( /.+\.cpp$/) { # || /.+\.c/
        processSource($f);
      }
    }
  }
}

processSubDir ".";
print("FILE=[$headercount]; LINES=[$headerlinecount]; BYTES=[$headerbytes]; CLASSES=[$classescount]\n");
