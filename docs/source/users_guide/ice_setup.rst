.. _ice_setup:

Configuring and Building CICE
=============================

Overview
--------

The setup scripts for the coupled model are located in **cesm2/scripts**. 

The directory structure of CICE5 within CESM is shown below.

::

				       cesm2         (main directory)
                                         |
				         |
		      components --------+--------- scripts
			 |                        |
			 |               * * * * * * * * * * 
		        cice             *build scripts for*
		         |               *  coupled model  *
	                 |               * * * * * * * * * *
			 |
                 bld ----+---- cime_config ---- docs ------ src
                         |                       |           |
		       (Build scripts          (CICE         |
		        and CIME config)    documentation)   |
           				  	             |
	           				             |                  
            	           		  drivers --- mpi ---+--- io_pio --- serial --- source
			                     | 
			                     |
			                     |
                            cesm --- cice ---+--- hadley


The CIME scripts generate a set of “resolved scripts” for a specific configuration
determined by the user. The configuration includes components,
resolution, run type, and machine. The run and setup scripts that were
in the **/scripts** directory for previous versions are now generated
automatically. See the CESM2 User’s Guide for information on how to
use the new scripts.

http://www.cesm.ucar.edu/models/cesm2

The file that contains the ice model namelist is now located in
**$CASE/CaseDocs**. The file containing the environment variables
used for building the executable file for the ice model is in
**$CASE/env\_build.xml**. The contents of the ice model namelist are
described in section :ref:`namelist`.

Building the CICE library
-------------------------

The Build Environment
---------------------

The **cime_config/build_cpp** script sets all compile time parameters, such
as the horizontal grid, the sea ice mode (prognostic or prescribed),
tracers, etc. However, to change the CPP variables, one needs to add these to
the ``CICE_CONFIG_OPTS`` variable in the **env\_build.xml** file. Additional options
can be set here, such as the decomposition and the number of tasks.

CICE Preprocessor Flags
---------------------------

Preprocessor flags are activated in the form -Doption in the
**buildcpp** script. Only advanced users should change these
options. See the CESM User’s Guide or the CICE reference guide for more
information on these. The flags specific to the ice model are:

::

    CPPDEFS :=  $(CPPDEFS) -DCESMCOUPLED -Dcoupled -Dncdf -DNICECAT=5 -DNXGLOB=$()
    -DNYGLOB=$() -DNTRAERO=3 -DNTRISO=0 -DNBGCLYR=0 -DNICELYR=8 -DNSNWLYR=3
    -DTRAGE=1 -DTRFY=1 -DTRLVL=1 -DTRPND=1 -DTRBRI=0 -DTRBGCS=0
    -DBLCKX=$() -DBLCKY=$() -DMXBLCKS=$()

The options ``-DCESMCOUPLED`` and ``-Dcoupled`` are set to activate the coupling
interface. This will include the source code in **ice\_comp\_mct.F90**,
for example. In coupled runs, the CESM coupler multiplies the fluxes by
the ice area, so they are divided by the ice area in CICE to get the
correct fluxes. Note that the **ice\_forcing.F90** module is not used in
coupled runs.

The options ``-DBLCKX=$(CICE_BLCKX)`` and ``-DBLCKY=$(CICE_BLCKY)`` set the
block sizes used in each grid direction. These values are set
automatically in the scripts for the coupled model. Note that ``CICE_BLCKX`` and
``CICE_BLCKY`` must divide evenly into the grid, and are used only for MPI grid
decomposition. If ``CICE_BLCKX`` or ``CICE_BLCKY`` do not divide evenly into the grid,
which determines the number of blocks in each direction, the model setup
will exit from the setup script and print an error message to the
**ice.bldlog** (build log) file. To override these values, one must set
the variable ``CICE_AUTO_DECOMP`` to ``false`` in **env\_build.xml** and 
then the variables ``CICE_BLCKX``, ``CICE_BLCKY``, and ``CICE_MBLCKS`` 
can be set manually. 

The flag ``-DMXBLCKS`` is essentially the threading option. This controls
the number of “blocks” per processor. This can describe the number of
OpenMP threads on an MPI task, or can simply be that a single MPI task
handles a number of blocks. This is set automatically, but can be changed
as described above.

The number of categories ``-DNICECAT`` can be changed at build time. There is
a separate discussion of this in :ref:`ice-thickness-categories`.

The number of ice and snow layers are set at compile time via the CPP
flags. They can technically be changed via the ``CICE_CONFIG_OPTS``
variable in **env\_build.xml**, but it this is not recommended. We have provided
an option to use the older CICE4 physics, inluding 4 ice levels and 1 snow level.
This option also turns on ``ktherm=1`` and ``tr_pond_cesm=.true.`` To use the
older CICE4 physics options, one should add/change ``-phys cice4`` in the XML variable
``CICE_CONFIG_OPTS``.

The flag ``-DNTR\_AERO=n`` flag turns on the aerosol deposition physics in
the sea ice where n is the number of tracer species and 0 turns off the
tracers. More details on this are in the section on tracers. The default here
is 3 and should only be changed when adding additional aerosol tracers. This can
be turned off by setting ``CICE_CONFIG_OPTS`` to ``-ntr_aero=0`` in the
**env\_build.xml** file.

The flag ``-DNTR\_ISO=n`` flag turns on the isotopes and is not yet supported.

The flags ``-DBGCLYR``, ``-DTRBRI``, and ``-DTRBGCS`` are for the skeletal biogeochemistry.
These have not been tested within CESM and more information can be found in the CICE
reference guide :cite:`cice15`.

The other tracer flags, ``-DTRAGE``, ``-DTRFY``, ``-DTRLVL``, ``-DTRPND`` are for the age, first-year ice,
level ice, and melt pond tracers. These are either on or off using 1 or 0. By default, all are
turned on. More information on these can be found in the CICE reference guide :cite:`cice15`.

More information on the compile settings for CICE can be found here:

http://www.cesm.ucar.edu/models/cesm2/component_settings/cice_input.html

