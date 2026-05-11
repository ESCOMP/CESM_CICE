.. _ice_setup:

Configuring and Building CICE
=============================

Overview
--------

The setup scripts for the coupled model are located in **cesm3/cime/scripts**. 

The directory structure of CICE6 within CESM is shown below.

::

				       cesm3         (main directory)
                                         |
				         |
		      components --------+--------- cime/scripts
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
            	           		        cicecore ----+---- icepack 
			                           |                  |
			                           |                  |
			                           |  columnphysics --+-- configuration
                                                   |
                                     cicedyn ------+------ drivers ---- shared
                                         |                    |
                                         |                    |
        analysis -- dynamics -- general -+- infrastructure    | 
                                                              |
                                                        nuopc / cmeps

The CIME scripts generate a set of “resolved scripts” for a specific configuration
determined by the user. The configuration includes components,
resolution, run type, and machine. The run and setup scripts that were
in the **/scripts** directory for previous versions are now generated
automatically. See the CESM3 User’s Guide for information on how to
use the new scripts.

http://www.cesm.ucar.edu/models/cesm3

The file that contains the ice model namelist is now located in
**$CASE/CaseDocs**. The file containing the environment variables
used for building the executable file for the ice model is in
**$CASE/env\_build.xml**. The contents of the ice model namelist are
described in section :ref:`namelist`.

Building the CICE library
-------------------------

The Build Environment
---------------------

The **cime_config/buildlib** script sets all compile time parameters, such
as the horizontal grid, the sea ice mode (prognostic or prescribed),
tracers, etc. However, to change the CPP variables, one needs to add these to
the ``CICE_CONFIG_OPTS`` variable in the **env\_run.xml** file. Additional options
can be set here, such as the decomposition and the number of tasks.

CICE Preprocessor Flags
---------------------------

Very few of the C Preprocessor flags are activated in the form -Doption in the
**buildlib** script. The majority of these are now set in the CIME scripts.
Only advanced users should change these
options. See the CESM User’s Guide information on these. 

::

    cice_cppdefs = "-Dncdf"

The option ``-DCESMCOUPLED`` are set to activate the coupling
interface. This will include the source code in **ice\_comp\_nuopc.F90**,
for example. In coupled runs, the CESM coupler multiplies the fluxes by
the ice area, so they are divided by the ice area in CICE to get the
correct fluxes. Note that the **ice\_forcing.F90** module is not used in
coupled runs.

The dimensions of all arrays in CICE are now declared dynamically. One can change
the grid dimensions and block layout by modifying **user\_nl\_cice**. See the CICE
Consortium User's Guide on these options.  

https://cice-consortium-cice.readthedocs.io/en/main/

::

    block_size_x = 10
    block_size_y = 10
    max_blocks = 7
    distribution_type = "sectrobin"
    distribution_wght = "blockall"
    processor_shape = 'square-ice'
    nx_global = 540
    ny_global = 480

The number of categories ``ncat`` can be changed at run time also in **user\_nl\_cice**. There is
a separate discussion of this in :ref:`ice-thickness-categories`.

The number of ice and snow layers are set at run time via the namelist flags 
nilyr and nslyr. 

The namelist flags ``n\_aero`` and ``tr\_aero`` turn  on the aerosol deposition physics in
the sea ice where ``n\_aero`` is the number of tracer species. More details on this 
are in the section on tracers. The default here is 3 and should only be changed 
when adding additional aerosol tracers.

The flags ``n\_iso`` and ``tr\_iso`` turn on the water isotopes in CICE only. These are currently not on
for the whole CESM3.

The biogeochemisty has not been tested within CESM and more information can be found in the CICE
reference guide https://cice-consortium-cice.readthedocs.io/en/main/.

The other tracer flags, ``tr\_iage``, ``tr\_fy``, ``tr\_lvl``, ``tr\_pond\_sealvl`` are for the age, first-year ice,
level ice, and melt pond tracers. These are either on or off using ``.true.`` or ``.false.``. By default, all are
turned on. More information on these can be found in the CICE reference guide: 

https://cice-consortium-cice.readthedocs.io/en/main/

More information on the compile settings for CICE can be found here:

http://www.cesm.ucar.edu/models/cesm3/component_settings/cice_input.html

