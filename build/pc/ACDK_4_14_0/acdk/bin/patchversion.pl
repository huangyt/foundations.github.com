
use strict;

my $PV = '4.00.1';
my $DV = '4.00';

sub getFileContent($) 
{
  return unless open(DECF, "<$_[0]");
  #binmode DECF;
  my @all = (); # = <DECF>;
  for(<DECF>) {
    chomp($_);
    push(@all, $_);
  }
  close(DECF);
  if (wantarray) {
    return @all;
  } else {
    return join("\n", @all); 
  }
}

sub writeFile($$)
{
  my ($fname, $cont) = @_;
  #return;
  open(OF, ">$fname");
  binmode OF;
  print(OF $cont);
  close(OF);
}

sub patchver($)
{
  my ($file) = @_;
  my @cont = getFileContent($file);
  my @erg = ();
  my $changed = 0;
  my $line = 0;
  for my $l (@cont) {
    #print "$l\n";
    $line += 1;
    if ($l =~ /(.*?)\(setg\ acdk\-project\-distribution\-version\ \"(.*?)\"\)(.*)/) {
      unless ($DV eq $2) {
        $changed = 1;
        my $nl = "$1(setg acdk-project-distribution-version\ \"$DV\")$3";
        print ("CHANGED [$file,$line]: $l -> $nl\n");
        push(@erg, $nl);
      } else {
        #print ("UNCHANGED [$file,$line]: $l\n");
        push(@erg, $l);
      }
    } elsif ($l =~ /(.*?)\(setg\ acdk\-project\-package\-version\ \"(.*?)\"\)(.*)/) {
      unless ($PV eq $2) {
        $changed = 1;
        my $nl = "$1(setg acdk-project-package-version\ \"$PV\")$3";
        print ("CHANGED [$file,$line]: $l -> $nl\n");
        push(@erg, $nl);
      } else {
        #print ("UNCHANGED [$file,$line]: $l\n");
        push(@erg, $l);
      }  
      
    } elsif ($l =~ /(.*?)distribution-version\>(.*?)\<(.*)/) {
      unless ($DV eq $2) {
        $changed = 1;
        my $nl = "$1distribution-version\>$DV\<$3";
        print ("CHANGED [$file,$line]: $l -> $nl\n");
        push(@erg, $nl);
      } else {
        push(@erg, $l);
      }  
    } elsif ($l =~ /(.*?)package-version\>(.*?)\<(.*)/) {
      unless ($PV eq $2) {
        $changed = 1;
        my $nl = "$1package-version\>$PV\<$3";
        print ("CHANGED [$file,$line]: $l -> $nl\n");
        push(@erg, $nl);

      } else {
        push(@erg, $l);
      }  
    } else {
        push(@erg, $l);
    }
  }
  return unless ($changed);
  print "write file: $file\n";
  writeFile($file, join("\n", @erg));
}


my $files = `find . -name '*.lsp' -or -name '*.xml'`;

my @files = split('\n', $files);
for my $f (@files) {
  patchver($f);
}

