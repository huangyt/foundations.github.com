
# generates the UniCode table using official Unicode spec

my @table = ();
open FILE, "<UnicodeData.txt";

while  (<FILE>)
{ 
  my $l = $_;
  #              val    nam    cat     comb  bidir  decop  decdig  digit  num   mirr   oldn   comm   upc   lowc   titc
  unless ($l =~ /(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*?)\;(.*)/)
  {
    print "Incompatible line: $l\n";
    next;
  }
  my $e;
  $e->{'value'} = $1;
  $e->{'name'} = $2;
  $e->{'category'} = $3;
  $e->{'combine'} = $4;
  $e->{'bidirect'} = $5;
  $e->{'decomposit'} = $6;
  $e->{'decdig'} = $7;
  $e->{'digit'} = $8;
  $e->{'num'} = $9;
  $e->{'mirror'} = $10;
  $e->{'oldname'} = $11;
  $e->{'comment'} = $12;
  $e->{'upcase'} = $13;
  $e->{'lowcase'} = $14;
  $e->{'titcase'} = $15;
  #print $e->{'value'} . ": " . $e->{'name'} . "\n";
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

print "#include \"UnicodeTable.h\"\n\n";
print "UnicodeInfo UnicodeTable[] = {\n";
  
my $c = 0;
my $fixupTable = {};
my $absFixup = 0;
my $recordCount = 0;
foreach $e (@table)
{
	my $realnum = hex($e->{'value'});
  if ($c != $realnum)
  {
  	#last if ($realnum > 0xFFFF);
  		
  	my $dif = $realnum - $c;
  	if ($dif > 50)
  	{
  		printf "/*\n  not set from 0x%04x to 0x%04x ($dif entries) \n*/\n", $c, $realnum;
  		$absFixup += $dif;
  		$fixupTable->{$c} = {};
  		$fixupTable->{$c}->{'lower'} = $c;
  		$fixupTable->{$c}->{'upper'} = $realnum - 1;
  		$fixupTable->{$c}->{'fixup'} = $absFixup;
  		$c = $realnum;
  	}
  	else
  	{
  		#print("index position not equal value: index: $c value: $realnum (" . $e->{'value'} . ")\n");
  		for (; $c < $realnum; ++$c)
  		{
  			printf "{ 0x%04x, \"<unspec>\", Unpecified, -1, 0x0, 0x0, 0x0 }, // manually inserted\n", $c;
  			++$recordCount;
  		}
  	}
  }
  
  print "{ 0x" . $e->{'value'} .
        ", \"" . $e->{'name'} . "\", ";
  ++$c;
  ++$recordCount;
  my $erg = '';
  my $cat = $e->{'category'};
  if ($cat =~ /Lu/)
  {
    $erg = orconcat($erg, "Letter | UpCase");
  } 
  elsif ($cat =~ /Ll/)
  {
    $erg = orconcat($erg, "Letter | DownCase");
  }
  elsif ($cat =~ /Lt/)
  {
    $erg = orconcat($erg, "Letter | TitleCase");
  }
  elsif ($cat =~ /Lo/)
  {
    $erg = orconcat($erg, "Letter");
  }
  elsif ($cat =~ /Lm/)
  {
    $erg = orconcat($erg, "Letter | Modifier");
  }
  elsif ($cat =~ /Mn/)
  {
    $erg = orconcat($erg, "Mark | NonSpacing");
  }
  elsif ($cat =~ /Mc/)
  {
    $erg = orconcat($erg, "Mark | SpacingCombined");
  }
  elsif ($cat =~ /Me/)
  {
    $erg = orconcat($erg, "Mark | Enclosing");
  }
  elsif ($cat =~ /Nd/)
  {
    $erg = orconcat($erg, "Number | Decimal");
  }
  elsif ($cat =~ /Nl/)
  {
    $erg = orconcat($erg, "Number | Letter");
  }
  elsif ($cat =~ /No/)
  {
    $erg = orconcat($erg, "Number");
  }
  elsif ($cat =~ /Zs/)
  {
    $erg = orconcat($erg, "Seperator | Space");
  }
  elsif ($cat =~ /Zl/)
  {
    $erg = orconcat($erg, "Seperator | Line");
  }
  elsif ($cat =~ /Zp/)
  {
    $erg = orconcat($erg, "Seperator | Paragraph");
  }
  elsif ($cat =~ /Cc/)
  {
    $erg = orconcat($erg, "Control");
  }
  elsif ($cat =~ /Cf/)
  {
    $erg = orconcat($erg, "Format");
  }
  elsif ($cat =~ /Cs/)
  {
    $erg = orconcat($erg, "Surogat");
  }
  elsif ($cat =~ /Co/)
  {
    $erg = orconcat($erg, "Other");
  }
  elsif ($cat =~ /P./)
  {
    $erg = orconcat($erg, "Punctation");
  }
  elsif ($cat =~ /S./)
  {
    $erg = orconcat($erg, "Symbol");
  }
  else
  {
    print "\n\nUnknown category [$cat]: " . $e->{'value'} . ": "  . $e->{'name'} . "\n\n";
  }
  $cat  = $e->{'bidirect'};
  if ($cat =~ /WS/)
  {
    $erg = orconcat($erg, "Whitespace");
  }
  print $erg;
  #print ", " . $e->{'decdig'};
  #print ", " . $e->{'digit'};
  print ", " . getNumVal($e->{'num'});
  print ", 0x" . getHexVal($e->{'upcase'});
  print ", 0x" . getHexVal($e->{'lowcase'});
  print ", 0x" . getHexVal($e->{'titcase'});
  
  print " },\n";
  
}
print "};\n";
my $fixupCount = 0;
print "int UnicodeTable_Fixups[][] = {\n";
foreach my $c (sort { $a <=> $b } keys(%{$fixupTable}))
{
	#my $table = $fixupTable->{$start};
	
	#$fixupTable->{$c}->{'lower'} = $c;
  #		$fixupTable->{$c}->{'upper'} = $realnum - 1;
  		
	printf "{ 0x%04x, 0x%04x, 0x%04x },\n", $fixupTable->{$c}->{'lower'}, $fixupTable->{$c}->{'upper'}, $fixupTable->{$c}->{'fixup'};
	++$fixupCount;
}
print "};\n";

print "int fixupCount = $fixupCount;\n";
print "int recordCount = $recordCount;\n";
