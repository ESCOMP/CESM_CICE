#! /bin/csh -f 

if !(-d $CASEBUILD/ciceconf) mkdir -p $CASEBUILD/ciceconf


set hgrid = "-hgrid $ICE_GRID"
#if ($ICE_GRID =~ *T*) set hgrid = "-hgrid ${ICE_NX}x${ICE_NY}" TODO - fix this

cd $CASEBUILD/ciceconf || exit -1
# Invoke cice configure - output will go in $CASEBUILD/ciceconf 
$CODEROOT/ice/cice/bld/configure $hgrid -comp_intf $COMP_INTERFACE \
    -cice_mode $CICE_MODE -nodecomp $CICE_CONFIG_OPTS || exit -1

#--------------------------------------------------------------------
# Loop over ice instances
#--------------------------------------------------------------------

cd $CASEBUILD/ciceconf || exit -1 
set default_ice_in_filename = "ice_in"

set inst_counter = 1
while ($inst_counter <= $NINST_ICE)

if ($NINST_ICE > 1) then
   set inst_string = $inst_counter
   if ($inst_counter <= 999) set inst_string = 0$inst_string
   if ($inst_counter <=  99) set inst_string = 0$inst_string
   if ($inst_counter <=   9) set inst_string = 0$inst_string
   set inst_string = "_${inst_string}"    
else
   set inst_string = ""       
endif
set ice_in_filename = ${default_ice_in_filename}${inst_string}

if (-e $CASEROOT/user_nl_cice${inst_string}) then
  ${CASEROOT}/Tools/user_nlcreate -user_nl_file $CASEROOT/user_nl_cice${inst_string} \
	-namelist_name cice_inparm >! $CASEBUILD/ciceconf/cesm_namelist 
endif

# Invoke cice build-namelist - output will go in $CASEBUILD/ciceconf
setenv INST_STRING $inst_string
$CODEROOT/ice/cice/bld/build-namelist \
    -infile $CASEBUILD/ciceconf/cesm_namelist \
    -inputdata $CASEBUILD/cice.input_data_list \
    -caseroot $CASEROOT \
    -scriptsroot $SCRIPTSROOT  \
    -inst_string "$inst_string" \
    -namelist "&cice $CICE_NAMELIST_OPTS/" -config config_cache.xml || exit -1

# Copy resolved namelist to $CASEROOT/CaseDocs and $RUNDIR
if (-d ${RUNDIR}) then
  cp $CASEBUILD/ciceconf/ice_in ${RUNDIR}/$ice_in_filename || exit -2
endif

@ inst_counter = $inst_counter + 1

end



