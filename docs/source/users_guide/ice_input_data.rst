.. _cice-input-data:


CICE Input Data
===============

**All runs**

The coupled CICE model requires a minimum of two files to run. 
Both are set in the ``&grid_nml`` section of the namelist (see :ref:`grid`)
for more information

-  ``grid_file`` is a binary or netcdf file containing grid information such as the latitude, longitude, grid cell area, etc.

-  ``kmt_file`` is a binary or netcdf file containing land mask information. This points to the ocean model KMT file or the depths of the ocean columns.

Depending on the grid selected in the scripts, the appropriate
``grid_file`` and ``kmt_file`` files will be used in the executable
directory. These files are read directly from the system input data
directory and not copied to the executable directory. Currently, only
the POP resolutions of gx3, gx1, tx1, and tx0.1 grids are supported for the ice and
ocean models. Note that these files can now be used in netCDF format.

**Initial and Hybrid runs**

For initial or hybrid runs, a third variable is required and is set in 
the ``&setup_nml`` section of the namelist (see :ref:`setupnml`). 

- ``ice_ic = 'none'`` initializes the sea ice to zero everywhere

- ``ice_ic = 'default'`` initializes the sea ice to 100% concentration where the SST is below the freezing point to a thickness of 2 m in the Northern Hemisphere or 1 m in the Southern Hemisphere.  

- ``ice_ic = 'filename.nc'`` will read the state information from an initial file named "filename.nc". The resolution of this file must match that in ``grid_file``, as set above.
 
**Restart and Branch runs**

Restart or branch runs are discussed later.

The input datasets are generally handled by the CESM driver.

http://www.cesm.ucar.edu/models/cesm2
