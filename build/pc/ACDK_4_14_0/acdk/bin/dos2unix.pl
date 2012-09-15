

#while (<>) {
#	if ($ARGV ne $oldargv) 
#	{
		#($dev,$ino,$mode,$nlink,$uid,$gid) = stat($ARGV);
#		$backup = $ARGV . '.bak';
#		rename($ARGV, $backup);
#		open (ARGVOUT, ">$ARGV");
#		chmod $mode, $ARGV;
#		select(ARGVOUT);
#		$oldargv = $ARGV;
	#}
	#s/$find/$sub/;
#} continue {
#	print;
#	if (eof) {
		#print STDOUT "Converted: $oldargv\n";
	#	unlink $backup if $rm_bak;
	#}
#}
#select(STDOUT);

sub processSource($$)
{
  my ($file, $dir) = @_;
  my @SUFFIXES = qw(bsd c cpp h html lsp linux sunos-gcc htxt txt nmake csf cfg sh pl pm bpgr cbx);
  my @FILES = qw(Makefile README INSTALL LICENCE CHANGELOG Root Entries Repository);
  my $found = 0;
  for my $suffix (@SUFFIXES)
  {
    if ($file =~ /.*\.$suffix$/)
    {
      #print "found Extension: $file = $suffix\n";
      $found = 1;
      last;
    }
  }
  unless ($found)
  {
    for my $fq (@FILES)
    {
      if ($file eq $fq)
      {
        $found = 1;
        last;
      }
    }
  }
  return unless $found;
  open INFILE, "<$file";
  binmode INFILE;
  my $cont = join("", <INFILE>);
  #print "$file = [$cont]\n";
  close INFILE;
   
  my $contsic = $cont;
  my $find = "\r";	# find this
  my $sub = undef;	# substitute with this

  $cont =~ s/$find/$sub/g;
  return if ($cont eq $contsic);
  print "dos2unix: $dir/$file\n";
  #return;
  
  open OUTFILE, ">$file";
  binmode OUTFILE;
  print OUTFILE $cont;
  close OUTFILE;
}

sub processSubDir($) 
{
	my ($sdir) = @_;
	my @allfiles = glob('*');
	foreach my $f (@allfiles) {
    if (-d $f) {
      next if (($sdir eq '.' && $f eq 'include') || $f eq 'acdk_doc' || $f eq 'tobj');
  		chdir $f;
	  	processSubDir("$sdir/$f");
		  chdir '..';
    } else {
      processSource($f, $sdir);
    }
  }
}

processSubDir ".";
