#!/usr/bin/env perl
#=======================================================================
#
#  This is a script to return the decomposition information for
#  either CICE or POP.
#
# Usage:
#
# generate_cice_pop_decomp [options]
#
# To get help on options and usage:
#
# generate_cice_pop_decomp -help
#
#=======================================================================

use Cwd;
use strict;
#use diagnostics;
use Getopt::Long;
use English;

#-----------------------------------------------------------------------------------------------

#Figure out where configure directory is and where can use the XML/Lite module from
my $ProgName;
($ProgName = $PROGRAM_NAME) =~ s!(.*)/!!; # name of program
my $ProgDir = $1;                         # name of directory where program lives

my $cwd = getcwd();  # current working directory
my $cfgdir;
my $utilroot = $ENV{'UTILROOT'};

if ($ProgDir) { $cfgdir = $ProgDir; }
else { $cfgdir = $cwd; }

# Horizontal grid and spectral resolution parameters.
my $horiz_grid_file = 'config_grid.xml';
if (-f "Tools/$horiz_grid_file") {
    $horiz_grid_file = "Tools/$horiz_grid_file";
} elsif (-f "$cfgdir/../../../../scripts/ccsm_utils/Case.template/$horiz_grid_file") {
    $horiz_grid_file = "$cfgdir/../../../../scripts/ccsm_utils/Case.template/$horiz_grid_file";
} else {
    die <<"EOF";
** (generate_cice_decomp): Cannot find horizonal grid parameters file \"$horiz_grid_file\" 
EOF
}

# The XML::Lite module is required to parse the XML configuration files.
my $xmldir;
if (-f "Tools/XML/Lite.pm") {
    $xmldir = "Tools";
} elsif (-f "$cfgdir/../../../../scripts/ccsm_utils/Tools/perl5lib/XML/Lite.pm") {
    $xmldir = "$cfgdir/../../../../scripts/ccsm_utils/Tools/perl5lib";
} else {
    die <<"EOF";
** (generate_cice_decomp): Cannot find perl module \"XML/Lite.pm\" 
EOF
}
	  
#-----------------------------------------------------------------------------------------------
# Add $cfgdir/perl5lib to the list of paths that Perl searches for modules
my @dirs = ( $cfgdir, "$cfgdir/perl5lib", "$cfgdir/../../../../scripts/ccsm_utils/Tools/perl5lib", "$utilroot/Tools/perl5lib" );
unshift @INC, @dirs;
require XML::Lite;

my $result = eval "require Decomp::Config";
if ( ! defined($result) ) {
   die <<"EOF";
** (generate_cice_decomp): Cannot find perl module \"Decomp::Config\" from directories: @INC
EOF
}
require Decomp::Config;

#-----------------------------------------------------------------------------------------------
my $model    = "cice";
my $platform = "XT";
my $res      = "gx1v6";
my $output   = "all";

sub usage {
    die <<EOF;
SYNOPSIS
     $ProgName [options]
OPTIONS
     -res <resolution>    (or -r)   Horizontal resolution (gx1v6 etc.). (default $res))
     -model <model>       (or -m)   Model type (pop or cice).	        (default $model)
     -platform <platform> (or -p)   Platform type (XT etc.).		(defualt $platform)
     -nproc <number>      (or -n)   Number of processors to use.	(required)
     -thrds <number>      (or -t)   Number of threads per processsor    (default 1)
     -output <type>	  (or -o)   Either output: all, maxblocks, bsize-x, bsize-y, or decomptype
			  (default: $output)
     -spacecurve          (or -s)   Forces spacecurve computation if the decomposition is not in the xml file        
                                    (default is off)

EXAMPLES

   $ProgName -res gx1v6 -model cice -platform XT -nproc 80 -output maxblocks

   will return a single value -- the optimum max number of blocks to use.

EOF
}

#------------------------------------------------------------------------------------------------

  my %opts = (
                res        => $res,
                model      => "cice",
                platform   => $platform,
                nproc      => undef,
                thrds      => 1,
                output     => $output,
                printing   => 1,
                help       => 0,
                file       => "$cfgdir/cice_decomp.xml",
                spacecurve => 0,
           );

  my $cmdline = @ARGV;
  GetOptions( 
              "r|res=s"      => \$opts{'res'},
              "m|model=s"    => \$opts{'model'},
              "p|platform=s" => \$opts{'platform'},
              "n|nproc=i"    => \$opts{'nproc'},
              "t|thrds=i"    => \$opts{'thrds'},
              "o|output=s"   => \$opts{'output'},
              "h|elp"        => \$opts{'help'},
              "s|spacecurve" => \$opts{'spacecurve'},
          ) or usage();

  # Check for unparsed arguments
  if (@ARGV) {
      print "ERROR: unrecognized arguments: @ARGV\n";
      usage();
  }
  if ( $opts{'help'} ) {
      usage();
  }

  foreach my $key ( keys( %opts ) ) {
     if ( $key ne "help" && ! defined($opts{$key}) ) {
        print "ERROR: required input $key was not set\n";
        usage();
     }
  }

$opts{'ProgName'} = $ProgName;
$opts{'ProgDir'}  = $cfgdir;
$opts{'cmdline'}  = $cmdline;

# Redefine nproc to be total procs, nproc*thrds
$opts{'nproc'} = $opts{'nproc'} * $opts{'thrds'};

# Set_horiz_grid sets the parameters for specific hgrid combinations.
if (defined $opts{'res'}) {$res = $opts{'res'};}
my %latlon = ( nlat=>0, nlon=>0);
set_horiz_grid("$horiz_grid_file", $res, \%latlon);
my $nlat = $latlon{'nlat'}; 
my $nlon = $latlon{'nlon'}; 

# Try to read from the xml file
my $dcmp = Decomp::Config->new( \%opts );
my %decomp = ( maxblocks=>0, bsize_x=>0, bsize_y=>0, decomptype=>"" );
my $matches = $dcmp->ReadXML( $opts{'file'}, \%decomp );

# If no xml entry, try to generate something
if ( $decomp{'maxblocks'} == 0) {
    %decomp = CalcDecompInfo( $nlat, $nlon, \%opts);
}

# adjust maxblocks to take into account threading
  if ($decomp{'decomptype'} ne "spacecurve")  {
     $decomp{'maxblocks'} = $decomp{'maxblocks'} * $opts{'thrds'};
  }

  if ( $decomp{'maxblocks'} == 0 ) {
     printf "%d %s",-1, "ERROR:($ProgName) No Decomp Created \n";
  } else {
     if (      $opts{'output'} eq "all"       ) {
	 printf "%d %d %d %d %d %s", 
	 $nlon, $nlat,
	 $decomp{'bsize_x'}, $decomp{'bsize_y'}, 
	 $decomp{'maxblocks'}, $decomp{'decomptype'};
      } elsif ( $opts{'output'} eq "maxblocks" ) {
        print $decomp{'maxblocks'};
      } elsif ( $opts{'output'} eq "bsize_x"   ) {
        print $decomp{'bsize_x'};
      } elsif ( $opts{'output'} eq "bsize_y"   ) {
        print $decomp{'bsize_y'};
      } elsif ( $opts{'output'} eq "decomptype") {
        print $decomp{'decomptype'};
      } else {
        print "ERROR:($ProgName) bad argument to output option $opts{'output'}\n";
        usage();
      }
      print "\n";
  }

#-----------------------------------------------------------------------------------------------
sub CalcDecompInfo {
#
# Calculate decomposition information
# Tries to first find an even cartesian decomposition (set = 1)
#   For cice, want nprocsy to be as small as possible, find "first" match
#      with preference for nprocsy = 1 or 2.  if it becomes larger than
#      that, the cice decomp may be valid but it will run poorly.
#   For pop, want bsize_x and bsize_y to be as equal as possible, use score 
#      to find best decomp
# If can't find an even decomp, tries a space filling curve (set = 2)
#
  my $nlats    = shift;
  my $nlons    = shift;
  my $opts_ref = shift;

  my %opts   = %$opts_ref;
  my $nprocs = $opts{'nproc'};
  my $model  = "cice";
  my $spacecurve = $opts{'spacecurve'};

  my ($maxblocks,$bsize_x,$bsize_y,$decomptype);
  my %decomp;
  my $set = 0;
  my $done = 0;
  my $nprocsx = 0;
  my $nprocsy = 0;
  my $nx = 0;
  my $ny = 0;
  my $nn = 0;
  my $tmp = 0;
  my $nscore = 0.0 ;
  my $bscore = $nlons * $nlats * $nprocs ;

  # cartesian decomp
  if (!$spacecurve) {
      $nn = 0;
      do {
	  $nn = $nn + 1;
	  $ny = $nn;
	  $nx = int($nprocs/$ny);
	  if ($ny * $nx == $nprocs &&
	      $nlats % $ny == 0 &&
	      $nlons % $nx == 0) {
	      $nprocsx = $nx;
	      $nprocsy = $ny;
	      $set = 1;
	      $done = 1;
	  }
	  # print "debug $nn $nx $ny $nprocsx $nprocsy $nscore $bscore $set \n";  
      } until ($done == 1 || $nn == $nprocs);
  }
  if ($set == 1) {
      $decomp{'nlats'}      = $nlats;
      $decomp{'nlons'}      = $nlons;
      $decomp{'maxblocks'}  = 1;
      $decomp{'decomptype'} = "cartesian";
      $decomp{'bsize_x'}    = int( $nlons / $nprocsx );
      $decomp{'bsize_y'}    = int( $nlats / $nprocsy );
  }

  # round robin decomp - only used in prescribed mode for cice on cam grid
  if ($set == 0) {
      my $pattern = "^gx*|^tx*/";
      if ($res =~ /$pattern/) {
	  print" resolution matches gx or tx - no round robin used \n";
	  # do nothing
      } else {
	  $opts{'nproc'} = $opts{'nproc'} / $opts{'thrds'};
	  $nprocs = $opts{'nproc'}; 
	  $decomp{'nlons'}      = $nlons;
	  $decomp{'nlats'}      = $nlats;
	  $decomp{'decomptype'} = "roundrobin";
	  $decomp{'bsize_x'}    = 1;
	  $decomp{'bsize_y'}    = 1;
	  $decomp{'maxblocks'}  = int( ($nlons*$nlats) / ($decomp{'bsize_x'}*$nprocs) ) + 1;
	  $set = 3;
      }
  }

  return(%decomp);
}

#-------------------------------------------------------------------------------

sub set_horiz_grid
{
    # Set the parameters for the specified horizontal grid.  The
    # parameters are read from an input file, and if no grid matches are
    # found then issue error message.
    # This routine uses the configuration defined at the package level ($cfg_ref).

    my ($hgrid_file, $hgrid, $latlon) = @_;
    my $xml = XML::Lite->new( $hgrid_file );
    my $root = $xml->root_element();

    # Check for valid root node
    my $name = $root->get_name();
    $name eq "config_horiz_grid" or die
	"(generate_cice_decomp): file $hgrid_file is not a horizontal grid parameters file\n";

    # Read the grid parameters from $hgrid_file.
    my @e = $xml->elements_by_name( "horiz_grid" );
    my %a = ();

    # Search for matching grid.
    my $found = 0;
  HGRID:
    while ( my $e = shift @e ) {
	%a = $e->get_attributes();
	if ( $hgrid eq $a{'GLOB_GRID'} ) {
	    $found = 1;
	    last HGRID;
	}
    }

    # Die unless search was successful.
    unless ($found) { die "(generate_cice_decomp): set_horiz_grid: no match for hgrid $hgrid\n"; }

    # Set nlat and nlon values
    $latlon{'nlat'} = $a{'ny'};
    $latlon{'nlon'} = $a{'nx'};
}

