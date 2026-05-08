.. _ice-thickness-categories:

CICE Thickness Categories
=========================

The number of ice thickness categories affects ice model input files in
three places:

-  ``$DNICECAT`` in the scripts

-  The source code module **ice\_domain\_size.F90**

-  The initial condition (restart) file in the input file directory

One must be very careful with changing the number of thickness categories as it impacts a number of
places in the code. The number of ice thickness categories can be changed in
**$CASE/env\_build.xml** using the xml variable ``CICE_CONFIG_OPTS``. 
One changes this by adding ``-ncat 5`` to the variable ``CICE_CONFIG_OPTS``.
The default value is 5 categories. ``$DNICECAT`` is used to determine the CPP
variable setting ``NICECAT`` in **ice\_domain\_size.F90**. More information on the CPP variables can
be found here:

http://www.cesm.ucar.edu/models/cesm2/component_settings/cice_input.html

The information in the initial restart file is dependent on the number
of ice thickness categories and the total number of layers in the ice
distribution. An initial condition file exists only for the default case
of 5 ice thickness categories, with four layers in each category. To
create an initial condition file for a different number of categories or
layers, these steps should be followed:

-  Set ncat to the desired number of categories in **$CASE/env\_build.xml**.

-  Set the namelist variable ``dumpfreq = ’m’`` in **$CASE/user\_nl\_cice** to print out restart files monthly.

-  Set the namelist variable ``ice_ic='default'`` in **$CASE/user\_nl\_cice** to use the initial conditions within the ice model.

-  Run the model to equilibrium.

-  The last restart file can be used as an initial condition file.

-  Change the name of the last restart file to **iced.0001-01-01.$GRID.nc**.

-  Copy the file into the input data directory or directly into the the
   executable directory.

-  There are a few restart files available in **$DIN_LOC_ROOT/ice/cice**.

``$GRID`` is the name of the POP grid with resolution, ``$RES`` of 100x116 (gx3) and 320x384 (gx1) for low and
medium resolution grids, respectively. Note that the date printed inside the binary restart file will not be
the same as 0001-01-01. For coupled runs, ``$BASEDATE`` will be the starting
o date and the date inside the file will not be used.

.. note:: To use one ice thickness category, the following changes will need to be made in the namelist and also adding ``-ncat 1`` to ``CICE_CONFIG_OPTS``. 

::

      , kitd          = 0
      , kstrength     = 0

With these settings, the model will use the delta scheme instead of
linear remapping and a strength parameterization based on open water
area and mean ice thickness.
