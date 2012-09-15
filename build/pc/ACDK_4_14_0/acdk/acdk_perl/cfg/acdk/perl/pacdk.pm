
package pacdk;

sub new
{
  my $self = {};
  #print "pacdk::new(" . join(", ", @_) . ")\n";
  $self->{'obj'} = acdk::new(@_);
  return bless($self);
}
sub wrap_acdk_object($)
{
    my ($obj) = @_;
    my $self = {};
    $self->{'obj'} = $obj;
    return bless($self);
}

sub wao($)
{
    my ($obj) = @_;
    if (UNIVERSAL::isa($obj, 'acdk'))
    {
	return wrap_acdk_object($obj);
    }
    return $obj;
}

#use Devel::Peek;

sub dumpvar($)
{
    my ($var) = @_;
    #Devel::Peek::Dump($var);
}

sub AUTOLOAD
{
  my ($self) = @_;
  

  my $n = $pacdk::AUTOLOAD;
  $acdk::AUTOLOAD = $n;
  my @sargs = @_;
  shift(@sargs);
  #print "pacdk::AUTOLOAD(" . join(", ", @sargs) . ")\n";
  return wao($self->{'obj'}->AUTOLOAD(@sargs));
}

sub DESTROY
{
  # empty, just avoid to call AUTOLOAD 
  # underlying Object* will be destroyed automatically
}


sub invoke
{
  my ($self) = @_;
  #print "\\&acdk::AUTOLOAD:\n";
  #Devel::Peek::Dump(\&acdk::AUTOLOAD);
  #print "\\&pacdk::AUTOLOAD:\n";
  #Devel::Peek::Dump(\&pacdk::AUTOLOAD);

  #print "\\&acdk::invoke\n";
  #Devel::Peek::Dump(\&acdk::invoke);
  shift(@_);
  return wao($self->{'obj'}->invoke(@_));
}
sub invoke_static
{
  return wao(acdk::invoke_static(@_));
}
sub peek
{
  my ($self) = @_;
  shift(@_);
  return wao($self->{'obj'}->peek(@_));
}
sub poke
{
  my ($self) = @_;
  shift(@_);
  return wao($self->{'obj'}->poke(@_));
}
sub peek_static
{
  return wao(acdk::peek_static(@_));
}
sub poke_static
{
  return wao(acdk::poke_static(@_));
}

sub perl2acdk($)
{
  return acdk::perl2acdk(@_);
}

sub acdk2perl($)
{
  return wao(acdk::acdk2perl(@_));
}


return 1;


