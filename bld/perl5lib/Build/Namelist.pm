package Build::Namelist;
my $pkg_nm = 'Build::Namelist';
#-----------------------------------------------------------------------------------------------
#
# SYNOPSIS
# 
#   use Build::Namelist;
# 
#   # create empty object
#   my $nl = Build::Namelist->new();
#
#   # initialize object with namelist read from a file
#   my $nl = Build::Namelist->new($namelist_file_in);
#
#   # initialize object with a namelist formatted string
#   my $nl = Build::Namelist->new("&nl1 var1=val1 var2=val2 /");
# 
#   # get array of namelist group names defined in $nl
#   my @groups = $nl->get_group_names();
#
#   # get array of variable names for a specified namelist group
#   my @vars = $nl->get_variable_names('group_name');
#
#   # get value of a group variable
#   my $val = $nl->get_variable_value('group_name', 'variable_name');
#
#   # set value of a group variable
#   $nl->set_variable_value('group_name', 'variable_name', $val);
#
#   # write the namelist group(s) to an output file
#   $nl->write($namelist_file_out);
#
#   # append the namelist group(s) to an existing output file
#   $nl->write($namelist_file_out, 'append'=>1);
#
#   # write a subset of the groups to the output file
#   $nl->write($namelist_file_out, 'groups'=>['name1','name2']) 
#
# 
# DESCRIPTION
# 
# Build::Namelist objects are used to:
# 1. Parse Fortran namelist input (can handle multiple namelist groups).
# 2. Get group name, variable names, variable values.
# 3. Set new groups, name/value pairs.
# 4. Write output namelist.
#
# N.B.: FORTRAN is case insensitive.  So all variable names and group names in the
#       namelist object are converted to lower case.
#       Method arguments for variable and group names are not assumed to be lower case.
#       These names are converted to lower case internally.
#
# The simple parser employed in this module treats all values as scalar strings.  It
# doesn't keep track of whether the values are string or numeric, nor whether the values
# are scalar or arrays.  So it is up to the user to explicitly provide the quotes that
# a string (charecter(len=*)) in a Fortran namelist requires.  When reading values from
# a user supplied namelist, these quotes are preserved by the parser.
#
# new()
#     Create a new Build::Namelist object.  Can initialize the object with data
#     from a namelist formatted string or by reading a namelist(s) from a file.
#
# get_group_names()
#     Returns an array containing the namelist group name(s) defined in the object.
#
# get_variable_names($group_name)
#     Returns an array containing the variable names in the specified namelist group.
#
# get_variable_value($group_name, $variable_name)
#     Returns a scalar containing the value of variable in the specified namelist group.
#
# set_variable_value($group_name, $variable_name, $value)
#     Set the value of the specified variable in the specified group.
#
# write($filename, ['append'])
#     Write the namelist group(s) to $filename.  Default is to overwrite an existing file.
#     Append to an existing file by supplying the optional argument 'append'.
#


# COLLABORATORS
# 
#-----------------------------------------------------------------------------------------------
#
# Date        Author                  Modification
#-----------------------------------------------------------------------------------------------
#  2001       Erik Kluzek             Original version of namelist parsing code.
#
#  2007-May   Brian Eaton             Took the namelist parsing code from the original and used
#                                     it in a completely redesigned module.
#-----------------------------------------------------------------------------------------------

use strict;
#use warnings;
#use diagnostics;

use IO::File;
#use Data::Dumper;

# package variables

# As the parser recognizes pieces of the input namelist, it uses these variables
# to keep track of the current namelist entry.
my $group_name;       # namelist group for current parser state
my $variable_name;    # variable name for current parser state
my $variable_value;   # variable value for current parser state

sub new 
{
    my $class = shift;
    my $in    = shift;   # input namelist as either a string or the name of a file

    my $nm   = "$class\:\:new";
    my $self = {};

    # if new is invoked with no argument, create empty namelist object
    if ( ! defined($in) ) { return bless($self, $class); }

    # Determine which form of input has been used.  If a filename has been given then 
    # read the namelist file before invoking the parser.

    # Pass the parser an array of lines.  If the namelist input has been provided in 
    # a string, then put it into $line[0].  It the namelist has been provided by a
    # file, then read the file into @lines.

    my @lines;

    if ($in =~ m/^\s*&/) {   # if the input is a formatted namelist string, the first
                             # character must be an '&' optionally preceeded by blanks
	$lines[0] = $in;
    }
    else {                   # a filename was input

	# check that the file exists
	(-f $in)  or  die "$nm: failed to find namelist file $in";

	# read the file
	my $fh = IO::File->new($in, '<') or die "$nm: can't open file: $in\n";
	@lines = <$fh>;
	$fh->close;
    }

    _parse($self, \@lines);
    
    return bless($self, $class);
}

#-----------------------------------------------------------------------------------------------

sub get_group_names {

# Return an array of namelist group names

  my $self = shift;

  return( keys %$self );
}

#-----------------------------------------------------------------------------------------------

sub get_variable_names {

# Return an array of variable names for the specified namelist group

  my $self       = shift;
  my $group_name = shift;

  my $lc_group = lc($group_name);

  return( keys %{$self->{$lc_group}} );
}

#-----------------------------------------------------------------------------------------------

sub get_variable_value {

# Return variable value for specified namelist group and variable name

  my $self          = shift;
  my $group_name    = shift;
  my $variable_name = shift;

  my $lc_group = lc($group_name);
  my $lc_var   = lc($variable_name);

  return( $self->{$lc_group}->{$lc_var} );
}

#-----------------------------------------------------------------------------------------------

sub get_value {

# This is a convenience function which assumes that the requested variable only
# exists in one group in the namelist object.  So search through the groups until
# the variable name is found, and return the value for that occurance of the name.

  my $self          = shift;
  my $variable_name = shift;

  my $lc_var = lc($variable_name);

  my @groups = $self->get_group_names();
  foreach my $group (@groups) {
      if (defined $self->{$group}->{$lc_var}) {
	  return $self->{$group}->{$lc_var};
      }
  }

  # Variable name not found.
  return undef;
}

#-----------------------------------------------------------------------------------------------

sub set_variable_value {

# Set variable value for specified namelist group and variable name

# N.B. This routine does no validity checking.  New group names and new
#      variable names can be added, as well as overwriting the values of
#      existing variables.

  my $self           = shift;
  my $group_name     = shift;
  my $variable_name  = shift;
  my $variable_value = shift;

  my $lc_group = lc($group_name);
  my $lc_var   = lc($variable_name);

  $self->{$lc_group}->{$lc_var} = $variable_value;

  return 1;
}

#-----------------------------------------------------------------------------------------------

sub merge_nl {

# Merge the contents of the namelist object argument into the object invoking the method.
# The variables in the invoking object have higher precedence and are not overwritten.

    my $self = shift;    # namelist object to merge into
    my $nl   = shift;    # namelist object to merge from

    # loop over groups in namelist argument
    my @groups = $nl->get_group_names();
    foreach my $group (@groups) {

	# loop over variables in each group
	my @vars = $nl->get_variable_names($group);
	foreach my $var (@vars) {

	    # check whether variable is defined in the invoking object
	    unless (defined $self->get_variable_value($group, $var)) {

		# add the variables to the invoking namelist without overwriting
		# existing values
		my $val = $nl->get_variable_value($group, $var);
		$self->set_variable_value($group, $var, $val);
	    }
	}
    }
}

#-----------------------------------------------------------------------------------------------

sub write {

# Write namelist to file.
#
# The default is to overwrite an existing file.  To append
# to existing file set the optional argument 'append' to true, e.g.,
#
# $nl->write('filepath', 'append'=>1) 
#
# The default if to write all the groups in the namelist to the 
# specified output file.  To only write a subset of the groups
# supply the optional argument 'groups'=>['name1','name2',...], i.e.,
#
# $nl->write('filepath', 'groups'=>['name1','name2']) 

    my $self = shift;
    my $file = shift;   # filepath for output namelist
    my %opts = @_;      # options

    my $class = ref($self);
    my $nm = "$class\:\:Write";


    my $fh;
    if (defined($opts{'append'}) && $opts{'append'}) {
	$fh = IO::File->new($file, '>>' ) or die "$nm: can't open namelist file: $file\n";
    } else {
	if ( -f $file ) { unlink( $file ); }
	$fh = IO::File->new($file, '>' ) or die "$nm: can't open namelist file: $file\n";
    }

    # determine which groups to write
    my @groups;
    if (defined($opts{'groups'})) {
	@groups = @{$opts{'groups'}};
    }
    else {
	@groups = sort(keys(%$self));
    }

    # loop over namelist groups
    for my $name (@groups) {

	# $self->{$name} is a reference to the hash containing namelist group $name
	my $nl_ref = $self->{$name};

	print $fh "&$name\n";

	for my $key ( sort( keys(%$nl_ref) ) ) {
	    if ( defined($nl_ref->{$key}) ) {
		my $value = _split_namelist_value($self, $nl_ref->{$key});
		print $fh " $key\t\t= $value\n";
	    }
	}

	print $fh "/\n";
    }

    $fh->close;
}

#-----------------------------------------------------------------------------------------------

#sub print_internal_data {
#
#  my $self = shift;
#
#  print Dumper($self);
#
#  return();
#}

#-----------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------
# Perl regular expressions to match Fortran namelist tokens.

# Variable names.
# % for derived types, () for arrays
my $varmatch = "[A-Za-z_]{1,31}[A-Za-z0-9_]{0,30}[(0-9)%a-zA-Z_]*";

# Integer data.
my $valint = "[+-]?[0-9]+";
my $valint_repeat = "${valint}\\*$valint";

# Logical data.
my $vallogical1 = "\\.[Tt][Rr][Uu][Ee]\\.";
my $vallogical2 = "\\.[Ff][Aa][Ll][Ss][Ee]\\.";
my $vallogical = "$vallogical1|$vallogical2";
my $vallogical_repeat = "${valint}\\*$vallogical1|${valint}\\*$vallogical2";

# Real data.
# "_" are for f90 precision specification
my $valreal1 = "[+-]?[0-9]*\\.[0-9]+[EedDqQ]?[0-9+-]*";
my $valreal2 = "[+-]?[0-9]+\\.?[EedDqQ]?[0-9+-]*";
my $valreal = "$valreal1|$valreal2";
my $valreal_repeat = "${valint}\\*$valreal1|${valint}\\*$valreal2";

# String data.
# One problem with below is strings that have \" or \' in them
my $valstring1 = '\'[^\']+\'';
my $valstring2 = '"[^"]+"';
my $valstring = "$valstring1|$valstring2";
my $valstring_repeat = "${valint}\\*$valstring1|${valint}\\*$valstring2";

# Complex data.
my $valcomplex1 = "\\(\\s*$valreal1\\s*,\\s*$valreal1\\s*\\)";
my $valcomplex2 = "\\(\\s*$valreal1\\s*,\\s*$valreal2\\s*\\)";
my $valcomplex3 = "\\(\\s*$valreal2\\s*,\\s*$valreal1\\s*\\)";
my $valcomplex4 = "\\(\\s*$valreal2\\s*,\\s*$valreal2\\s*\\)";
my $valcomplex = "$valcomplex1|$valcomplex2|$valcomplex3|$valcomplex4";
my $valcomplex_repeat = "${valint}\\*$valcomplex1|${valint}\\*$valcomplex2|${valint}\\*$valcomplex3|${valint}\\*$valcomplex4";

# Match for all valid data-types: integer, real, complex, logical, or string data
# note: valreal MUST come before valint in this string to prevent integer portion of real 
#       being separated from decimal portion
my $valall = "$vallogical|$valstring|$valreal|$valint|$valcomplex";

# Match for all valid data-types with repeater: integer, real, complex, logical, or string data
# note: valrepeat MUST come before valall in this string to prevent integer repeat specifier 
#       being accepted alone as a value
my $valrepeat = "$vallogical_repeat|$valstring_repeat|$valreal_repeat|$valint_repeat|$valcomplex_repeat";

# Match for all valid data-types with or without numberic repeater at the lead
my $valmatch = "$valrepeat|$valall";

# Same as above when a match isn't required
my $nrvalmatch = $valmatch. "||";

#-----------------------------------------------------------------------------------------------

sub _parse {

# Parse a namelist formatted string.  The string may contain more than one
# namelist group.

    my ($self, $lines_ref) = @_;

    my $nm = "$pkg_nm\:\:_parse";

    # The control structure is set up to loop over lines read in from a file.
    my @lines = @$lines_ref;

    # First item expected is the namelist group name.  The parser will process
    # input lines until it finds one that starts with a group name.

    my $expect = "group_name";

    # Loop over each line in the namelist.
    while ( defined(my $line = shift @lines) ) {

	# The local variable $line is modified by the parser.  It removes tokens 
	# from $line as they are recognized.  Loop over line until all recognized
	# tokens have been removed.
	while ( defined($line) ) {

            my $err = _parse_next($self, \$line, \$expect);

	    if ( $expect eq "error" ) {
		die "$nm::ERROR::$err";
	    }

	}

    }

}


#-----------------------------------------------------------------------------------------------

sub _parse_next {
#
# _parse_next($self, \$line, \$expect )
#
# Parse the input $line for the next item of type $expect.
#
# Loads variable names and values into the package variables $variable_name
# and $variable_value.  When a name, value pair have been identified, a
# call to the internal method _setkeypair puts the values from the package
# variables into the object's internal hash.

# This parser stores all input values as strings.  It doesn't keep track of
# the data types of the parsed values, nor does it keep track of whether
# the input was scalar or an array.  Array input is maintained as a string
# of comma separated values.  The quotes from the input file are maintained
# as part of the value.


#
# Returns information on an error, if $expect changes to "error"
# otherwise returns "success".
#
    my $self   = shift;
    my $line_ref   = shift;      # Current state of a line read from file
    my $expect_ref = shift;      # Type of item you expect next 
                                 # ("group", "variable", "varorvalue", "=", or "value")

    my $line = $$line_ref;
    $_ = $$line_ref;

    my $expect = $$expect_ref;

    my $nm = "$pkg_nm\:\:_parse_next";

    my $err;


    # Blank line, return and continue
    if ( /^\s*$/ ) {
	$$line_ref = undef;
	return( "success" );
    }
    #
    # Switch based on what type of item you expect
    #
  SWITCH: {
      # Expect a "group"
      ( $expect eq "group_name" ) && do {
	  if ( $line =~ /^\s*[\$\&](\w+)(.*)/i ) {
	      $group_name = lc($1);
	      $$line_ref = $2;
	      $$expect_ref = "variable";
	  }
	  else {
	      # if the line doesn't start with a group name, throw it out and continue
	      $$line_ref = undef;
	      return( "success" );
	  }
	  last SWITCH;
      };

      # Expect a variable
      ( ($expect eq "variable") || ($expect eq "varorvalue") ) && do {

	  # End-designator (F90 form "/" and non-standard F77 forms (&end) )
	  if ( $line =~ /^\s*\/(.*)/ || $line =~ /^\s*[\$\&]end(.*)/i ) {
	      $$line_ref = $1;
	      _setkeypair($self);
	      # After finding the end of a namelist group, start searching for the next group.
	      $$expect_ref = "group_name";
	      return( "success" );
	  }

	  # variable
	  if ( /^\s*,?\s*($varmatch)(.*?)$/ ) {
	      $$line_ref = $2;
	      $$expect_ref = "=";
	      _setkeypair($self);
	      $variable_name = lc($1);
	  }
	  elsif ( $expect ne "varorvalue" ) {
	      $err = "ERROR($nm): expect a variable instead got: $_\n";
	      $$expect_ref = "error";
	      return( $err );
	  }
	  # value
	  elsif ( $expect eq "varorvalue"
		  &&   /^\s*([\s,]*)($nrvalmatch)([\s,]*)(.*?)$/ ) {
	      $$line_ref = $4;
	      $$expect_ref = "varorvalue";
	      my $val = $2;
	      my $repeat = undef;
	      if ( $val =~ /($valint)\*($valall)/ ) {
		  $repeat = $1;
		  $val = $2;
	      }
	      $variable_value = $variable_value . ",$val";
	      if ( defined($repeat) ) {
		  for( my $i = 0; $i < ($repeat-1); $i++ ) {
		      $variable_value = $variable_value . ",$val";
		  }
	      }
	      # Comments, only can follow a value
	      if ( $$line_ref =~ /^([\s,])*![^!]*$/ ) {
		  $$line_ref = undef;
	      }
	  }
	  else {
	      $err = "ERROR($nm): expect a F90 namelist constant or variable instead got: $_\n";
	      $$expect_ref = "error";
	      return( $err );
	  }
	  last SWITCH;
      };

      # Expect a "="
      ($expect eq "=") && do {
	  if ( /^\s*=(.*?)$/ ) {
	      $$line_ref = $1;
	      $$expect_ref = "value";
	  }
	  else {
	      $err = "ERROR($nm): expect a equal \'=\' sign instead got: $_\n";
	      $$expect_ref = "error";
	      return( $err );
	  }
	  last SWITCH;
      };

      # Expect a value
      ($expect eq "value") && do {
	  # value
	  if ( /^\s*(${valmatch})([\s,]*)(.*?)$/ ) {
	      $$line_ref = $3;
	      $$expect_ref = "varorvalue";
	      my $val = $1;
	      my $repeat = undef;
	      if ( $val =~ /(${valint})\*($valall)/ ) {
		  $repeat = $1;
		  $val = $2;
	      }
	      $variable_value = "$val";
	      if ( defined($repeat) ) {
		  for( my $i = 0; $i < ($repeat-1); $i++ ) {
		      $variable_value = $variable_value . ",$val";
		  }
	      }
	      # FORTRAN only allows comments after values
	      if ( $$line_ref =~ /^\s*![^!]*$/ ) {
		  $$line_ref = undef;
	      }
	  }
	  else {
	      $err = "ERROR($nm): expect a F90 constant for a namelist instead got: $_\n";
	      $$expect_ref = "error";
	      return( $err );
	  }
	  last SWITCH;
      };

      # default
      $err = "ERROR($nm): Bad type to expect: $$expect\n";
      $$expect_ref = "error";
      return( $err );
  }
}

#-----------------------------------------------------------------------------------------------

sub _setkeypair {

# Set the keyword pair into the hash for the current namelist group
# Called from _parse_next when a variable assignment is complete.

    my $self = shift;

    my $nm = "$pkg_nm\:\:_setkeypair";

#    print "set: group=$group_name var=$variable_name val=$variable_value\n";

    if ( defined( $variable_name ) ) {
	if ( ! defined($variable_value) ) {
	    die "ERROR::($nm) Value not defined for variable: $variable_name\n";
	}

	$self->{$group_name}->{$variable_name} = $variable_value;

	# reset the package variables used by the parser
	$variable_name  = undef;
	$variable_value = undef;
    }
}

#-----------------------------------------------------------------------------------------------

sub _split_namelist_value {

# Return a namelist value split up if longer than 70 characters

    my $self  = shift;
    my $value = shift;

    my $nm = "$pkg_nm\:\:_split_namelist_value";

    if ( length($value) > 70 ) {

	my $originalvalue = $value;
	my $expect = "value";
	my @list;

	# parse the long string and if it contains multiple values
        # then split the string into an array of single value strings
	while ( $value =~ /./ ) {
	    my $err = _parse_next($self, \$value, \$expect) ;
	    if ( $expect eq "error" ) { die "$nm::ERROR::$err"; }
	    push( @list, $variable_value );
	    $expect = "value";
	}

	# insert newlines into the output string value
	my $numberonline = ( 70*($#list+1) ) / length($originalvalue);
	my $i = 0;
	$value = shift @list;
	for my $item ( @list ) {
	    if ( ++$i >= $numberonline ) {
		$value = $value . ",\n         $item";
		$i = 0;
	    } else {
		$value = $value . ", $item";
	    }
	}
    }
    return( $value );
}

#-----------------------------------------------------------------------------------------------

# Quoting should be done in the Write method rather
# than when string values are added to the namelist hash.
# But the namelist variable type isn't known in the Write method.
sub quote_string {
    my $str = shift;
    $str =~ s/^\s+//;
    $str =~ s/\s+$//;
    unless ($str =~ /^['"]/) {        #"'
        $str = "\'$str\'";
    }
    return $str;
}

#-----------------------------------------------------------------------------------------------

sub convert_case {
#
# Convert the case of the keys in the main associative arrays to lowercase.
# Also terminate if there are two keys with the same name but different case.
#
  my $self = shift;

  my $class = ref($self);
  my $nm = "$class\:\:convert_case";

  my $ref = $self->{'NLREF'};
  my $key;
  foreach $key ( keys(%$ref) ) {
    if ( defined($$ref{$key}) ) {
      my $lckey = $key;
      $lckey =~ tr/[A-Z]/[a-z]/;
      my $value = $$ref{$key};
      if ( $key ne $lckey && defined($$ref{$lckey}) ) {
        print "$lckey already defined\n";
        die "$nm: Fix your namelist so that two definitions of $lckey do not exist";
      }
      $$ref{$key}   = undef;
      $$ref{$lckey} = $value;
    }
  }
}

#-----------------------------------------------------------------------------------------------

1   # to make use or require happy
