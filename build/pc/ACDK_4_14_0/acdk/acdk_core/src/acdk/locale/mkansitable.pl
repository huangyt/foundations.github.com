
# generates the UniCode table using official Unicode spec

my $filename = $ARGV[0];
my @table = ();
open FILE, $filename or die "cannot open file " . $filename;
$filename =~ /(.*)\./;
my $outfile = $1 . "_mapping.h";
my $structname = $1;
$structname =~ s/\-/_/g;
$structname = "codepage_" . $structname . "_mapping";


my $counter = -1;
my $linecount = 0;
my $RTABLE;
while  (<FILE>)
{ 
  ++$linecount;
  my $l = $_;
  chomp($l);
  next if ($l =~ /^\#.*/);
  my $comment = '';
  if ($l =~ /(.*?)\#(.*)/)
  {
    $l = $1;
    $comment = $2;
  }
  next if ($l eq '');
  ++$counter;
  
  my $e;
  unless ($l =~ /(0x.+?)\s(0x.+)(\s.*)/)
  {
    if ($l =~ /(0x.+?)\s/)
    {
      $e->{'a'} = hex($1);
      $e->{'u'} = hex(0xFFFF);
    }
    else
    {
      print "$filename, $linecount:  Incompatible line: [$l]\n";
      next;
    }
  }
  else
  {
    
    $e->{'a'} = hex($1);
    $e->{'u'} = hex($2);
  }
  
  
  $e->{'comment'} = $comment;
  if ($e->{'a'} != $counter)
  {
    #print "not continued index\n";
  }
  $RTABLE->{$e->{'u'}} = $e->{'a'};
  print "[" . $e->{'u'} . "] [" . $e->{'a'} . "]\n";
  push(@table, $e);
}

sub orconcat($$)
{
  my ($l, $r) = @_;
  return $r if (length($l) == 0);
  return "$r | $l";
}

sub getHexVal($)
{
  my ($v) = @_;
  return '0' if ($v eq '');
  return $v;
}
sub getNumVal($)
{
  my ($v) = @_;
  return '-1' if ($v eq '');
  return $v;
}

#print "#include \"UnicodeTable.h\"\n\n";
#print "UnicodeInfo UnicodeTable[] = {\n";

if (1) {
open OUTF, ">$outfile";
print OUTF "\nACDK_NO_METAINFO_HEADER\n\n";
print OUTF "\nunsigned short " .  $structname . "[" . ($counter + 1) . "] =\n";
print OUTF  "{\n";
foreach $e (@table)
{
  
  #print "{ " . $e->{'a'} . ", " . $e->{'u'} . " },\n";
  print OUTF  "  ";
  my $t = $e->{'u'};
  if ($t > 0xFFFF)
  {
    print OUTF "0xFFFF, ";
  } 
  else 
  {
    printf OUTF "0x%04x, ", $e->{'u'};
  }
  print OUTF " // ";
  printf OUTF "0x%04x, ", $e->{'a'};

  if ($e->{'comment'} ne '')
  {
    print OUTF " // " . $e->{'comment'};
  }
  print OUTF "\n";
}
print OUTF "};\n";
} # 0

my $curtablenum = 0;
my @firstmaptable = ();
my $UNMAPPABLE = 0xFFFF;
for (my $i = 0; $i < 255; ++$i)
{
  my @secondmaptable = ();
  my $hasmapping = 0;
  for (my $j = 0; $j < 255; ++$j)
  {
    my $ucode = ($i << 8) + $j;
    if (defined($RTABLE->{$ucode}))
    {
      $hasmapping = 1;
      push(@secondmaptable, $RTABLE->{$ucode});
    }
    else
    {
      push(@secondmaptable, $UNMAPPABLE);
    }
  }
  if ($hasmapping == 1) 
  {
    my $mapname = $structname . "_cp2uc_" . $curtablenum;
    print OUTF "unsigned short " . $mapname . "[256] = {\n";
    my $breaknum = 0;
    foreach my $e (@secondmaptable)
    {
      if ($e > 0xFF)
      {
        #print "\n\n$outfile: value greater then unsigned char: $e\n\n";
      }
      printf OUTF "0x%04x, ", $e;
      ++$breaknum;
      if ($breaknum > 7) { print OUTF "\n"; $breaknum = 0; }
    }
    print OUTF "};\n\n";
    push(@firstmaptable, $mapname);
     
    ++$curtablenum;
  }
  else
  {
    push(@firstmaptable, "cp2uc_unmappable");
  }
}
print OUTF "unsigned short* " . $structname . "_cp2uc[256] = {\n";
my $breaknum = 0;
for my $e (@firstmaptable)
{
  print OUTF $e . ", ";
   ++$breaknum;
   if ($breaknum > 7) { print OUTF "\n"; $breaknum = 0; }
}
print OUTF "};\n\n";


close OUTF;

close FILE;
