package Build::NamelistDefinition;
my $pkg = 'Build::NamelistDefinition';
#-----------------------------------------------------------------------------------------------
#
# SYNOPSIS
# 
#   use Build::Namelist;
#   use Build::NamelistDefinition;
# 
#   # Create a namelist definition object (read the namelist definition file).
#   my $nldef = Build::NamelistDefinition->new("namelist_definition.xml");
# 
#   # Create a namelist object from an input file that contains one or more namelist groups.
#   my $nl = Build::Namelist->new('namelist.in');
#
#   # Validate the namelist object.
#   my $nl_valid = $nldef->validate($nl);
#
#   # Query the definition to find which group a variable belongs to.
#   my $group = $nldef->get_group_name('variable_name');
#
#   # Write validated namelist to output file.
#   $nl_valid->write('namelist.out');
# 
# DESCRIPTION
# 
# Build::NamelistDefinition objects encapsulate a namelist definition.  They provide
# a method used to validate a namelist object (created
# by the Build::Namelist module) against a namelist definition file.  The validation
# currently consists of making sure that each variable in the namelist object
# is defined in the definition file.
#
# All variables are checked and problems reported to stdout.  If any problems were
# encountered the object will throw an exception.
#
# If all variables are successfully validated, then the returned namelist
# object will have all variables in the namelist group(s) that are
# specified in the definition file.  This means that the namelist groups
# in the input namelist object are ignored.
#
# METHODS
#
# new() Reads xml file that contains the namelist definition.
#
#       The "namelist_definition.xml" file contains all the allowable
#       variables with a description of each one.  Where appropriate a
#       list of valid values of a variable is given.
# 
# validate()
#
#       Validate a namelist object (created by the Build::Namelist module)
#       against a namelist definition file.  Each variable is checked to
#       verify that it is defined in the definition file.  Also the value
#       of each variable is checked to verify that it's a string or a numeric
#       type.
#
#       The returned namelist object will have all variables contained in
#       the correct namelist group as specified by the definition file.
#
#
# COLLABORATORS
# 
# IO::File
# XML::Lite
# Build::Namelist
#-----------------------------------------------------------------------------------------------
#
# Date        Author                  Modification
#-----------------------------------------------------------------------------------------------
# 2007-Sep    Brian Eaton             Original version
#-----------------------------------------------------------------------------------------------

use strict;
#use warnings;
#use diagnostics;

use IO::File;
use XML::Lite;
use Build::Namelist;

#-----------------------------------------------------------------------------------------------
# Public methods
#-----------------------------------------------------------------------------------------------

sub new
{
    my $class = shift;
    my ($definition_filepath) = @_;

    # bless the object here so the initialization has access to object methods
    my $nl_definition = {};
    bless( $nl_definition, $class );

    # Add the filepath of the definition file to the object attributes.
    $nl_definition->{'definition_filepath'} = $definition_filepath;

    # Initialize the object with the namelist definition.
    $nl_definition->_initialize($definition_filepath);

    return $nl_definition;
}

#-----------------------------------------------------------------------------------------------

sub validate
{

# Validate a namelist object (created by the Build::Namelist module)
# against a namelist definition file.  All variables are checked and
# problems reported to stdout.  If any problems were encountered the object
# will throw an exception.
#
# If all variables are successfully validated, then the returned namelist
# object will have all variables in the namelist group(s) that are
# specified in the definition file.  This means that the namelist groups
# in the input namelist object are ignored.

    my $self  = shift;
    my $nl    = shift;    # namelist to be validated
    
    # Create an empty namelist which will be populated with variables from the
    # input namelist as they are validated.
    my $nl_valid = Build::Namelist->new();

    # Loop over the groups in the input namelist
    my @groups = $nl->get_group_names();
    for my $group (@groups) {

	# Loop over the variables in the namelist group
	my @vars = $nl->get_variable_names($group);
	for my $var (@vars) {

	    # Get the variable's value
	    my $value = $nl->get_variable_value($group, $var);

	    # Validate the variable/value pair.  This method throws an exception
	    # when an error is encountered.  If the validation is successful, then
	    # the valid group name for the variable is returned.
	    my $valid_group = $self->_validate_pair($var, $value);

	    # Add the validated variable to the output namelist
	    $nl_valid->set_variable_value($valid_group, $var, $value);
	}
    }

    return $nl_valid;
}

#-----------------------------------------------------------------------------------------------

sub get_file_name
{
# Return the name of the file that contains the namelist definition.

    my $self = shift;

    return $self->{'definition_filepath'};
}

#-----------------------------------------------------------------------------------------------

sub get_group_name
{
# If the requested name is contained in the namelist definition then return its
# namelist group.  Otherwise return an empty string.

    my ($self, $name) = @_;

    return defined($self->{$name}) ? $self->{$name}->{'group'} : "";
}

#-----------------------------------------------------------------------------------------------

sub get_str_len
{
# Return 'str_len' attribute for requested variable

    my ($self, $name) = @_;

    return $self->{$name}->{'str_len'};
}


#-----------------------------------------------------------------------------------------------

sub is_input_pathname
{
# Return 'input_pathname' attribute for requested variable

    my ($self, $name) = @_;

    return $self->{$name}->{'input_pathname'};
}

#-----------------------------------------------------------------------------------------------
# Private methods
#-----------------------------------------------------------------------------------------------

sub _initialize
{
# Read the namelist definition file.  Add a hash entry to the object for each
# namelist entry in the definition file.  The hash entries look like this:
#
#   id =>                               # id is the name of the namelist variable
#         {type           => "...",     # variable's type
#          str_len        => "...",     # length of a string type, 0 if not a string
#          arr_len        => "...",     # size of an array type, 0 if not an array
#          input_pathname => "...",     # if value is an input pathname then this attribute is
#                                       # set to either 'abs' for an absolute pathname, or 'rel'
#                                       # for a relative pathname.  Set to '' otherwise.
#          catagory       => "...",     # catagory used in documentation
#          group          => "...",     # namelist group
#          valid_values   => "..."},    # valid values (if easy to list)
# 


    my ($self, $definition_file) = @_;

    # Process the definition file
    my $xml = XML::Lite->new( $definition_file );
    my $root = $xml->root_element();

    # Check for valid root node
    my $name = $root->get_name();
    $name eq "namelist_definition" or die
	"ERROR: $definition_file is not a namelist definition file\n";

    # Each namelist variable is contained in an "entry" element.  Get all these elements.
    my @elements = $xml->elements_by_name('entry');

    # Loop over the elements...
    foreach my $e (@elements) {

        # and extract the attributes and element content.
	my %attributes = $e->get_attributes();

	# Look for the specific attributes that are contained in the configuration definition.
	my $id             = $attributes{'id'};
	my $type           = $attributes{'type'};
        my $input_pathname = defined $attributes{'input_pathname'} ? $attributes{'input_pathname'} : "";
        my $catagory       = $attributes{'catagory'};
        my $group          = $attributes{'group'};
	my $valid_values   = defined $attributes{'valid_values'} ? $attributes{'valid_values'} : "";

	# Parse the type specification for the following info:

        # Is the type string or numeric?  A string type will be indicated by $str_len > 0.
	# $str_len = 0 indicates a numeric type.
	my $str_len = 0;
	if ( $type =~ m/char\*(\d+)/ ) {
	    $str_len = $1;
	}

	# Is the type an array or a scalar?  An array will be indicated by $arr_len > 0 
	# where $arr_len is the size of the array.  $arr_len = 0 indicates a scalar.
	my $arr_len = 0;
	if ( $type =~ m{\(         # opening paren
                        (.*)       # capture everything between the parens in $1
                        \)         # closing paren
                       }x ) {
	    # split the dimensions between the parenthesis on "," and multiply the 
	    # dimensions together to get the array size
	    my @dims = split /,/, $1;  #/
            $arr_len = 1;
	    foreach my $dim (@dims) {
		$arr_len *= $dim;
	    }
	}

	# Now add the attributes and content to the object's internal data structure.
	$self->{$id} = {'type'           => $type,
			'str_len'        => $str_len,
			'arr_len'        => $arr_len,
			'input_pathname' => $input_pathname,
			'catagory'       => $catagory,
			'group'          => $group,
			'valid_values'   => $valid_values,
	};
    }

}

#-----------------------------------------------------------------------------------------------

sub _validate_pair
{

# The validation consists of the following checks:
#
# . The variable must be defined in the namelist definition file.
#
#

    my $self  = shift;
    my $var   = shift;   # namelist variable
    my $value = shift;   # namelist variable's value

    my $def   = $self->{'definition_filepath'};

    # Is variable in namelist definition file?
    $self->_is_valid_name($var) or die 
	"ERROR: in _validate_pair (package $pkg): Variable name $var not found in $def \n";

    # Parse the value string

    # Is the input value a string or numeric type?
    # A string type must start with a quote with optional leading whitespace.
    my $value_is_string = 0;
    if ( $value =~ m{^\s*['"]} ) { $value_is_string = 1; }

    # Get string length from definition file (returns 0 for a numeric type)
    my $str_len_def = $self->get_str_len($var);
    my $type_def    = $self->_get_type($var);

    # Check for mismatch
    if ( $value_is_string and ($str_len_def == 0) ) {
	die "ERROR: in _validate_pair (package $pkg): Variable name $var has an input value of type string, \n".
	    "$value \n".
	    "but is defined as type $type_def in $def \n";
    }
    elsif ( ! $value_is_string and ($str_len_def > 0) ) {
	die "ERROR: in _validate_pair (package $pkg): Variable name $var has an input value of type numeric, \n".
	    "$value \n".
	    "but is defined as type $type_def in $def \n";
    }	

    # 22 September 2007, bee
    # The intention is to include more rigorous checking for valid input values.
    # But postpone this for now.  It requires re-parsing the value that's currently 
    # only available as a string from the namelist parser.  The functionality of 
    # breaking input values into arrays of the input type belongs in the namelist 
    # parser and shouldn't be done here.
    # The following validations depend on breaking the input value string into
    # an array of elements.
    # . The variable's value is checked to verify that list values aren't specified for scalar
    #   variables.
    # . For string input check that the length of input strings doesn't exceed the declared
    #   string length
    # . The variable's value is checked against any valid values specified by the definition.


    # Checks all passed.  Return valid group name.
    return $self->get_group_name($var);
}

#-----------------------------------------------------------------------------------------------

sub _is_valid_name
{
# Return true if the requested name is contained in the namelist definition.

    my ($self, $name) = @_;

    return defined($self->{$name}) ? 1 : 0;
}

#-----------------------------------------------------------------------------------------------

sub _get_type
{
# Return 'type' attribute for requested variable

    my ($self, $name) = @_;

    return $self->{$name}->{'type'};
}

#-----------------------------------------------------------------------------------------------

1; # to make use or require happy
