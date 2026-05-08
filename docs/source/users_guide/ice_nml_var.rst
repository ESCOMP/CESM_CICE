.. _namelist:

CICE Namelists 
==============

CICE uses the same namelists for both the coupled and uncoupled models.
This section describes the namelist variables,
which determine time management, output frequency, model physics, and
filenames. The ice namelists for the coupled model are now located in
**$CASE/CaseDocs**. Some additional documentation on the CICE namelist
is available here:

http://www.cesm.ucar.edu/models/cesm2/component_settings/cice_nml.html

A script reads the input namelist at runtime, and writes the namelist
information to the file **ice\_in** in the directory where the model
executable is located. Therefore, the namelist will be updated even if
the ice model is not recompiled. The default values of the ice setup,
grid, tracer, and physics namelists are set in **ice\_init.F90**. The
prescribed ice option along with the history namelist variables are set
in **ice\_prescribed.F90** and **ice\_history.F90** respectively. If
they are not set in the namelist in the script, they will assume the
default values listed in the following tables, which
list all available namelist parameters. The default values shown here
are for the coupled model, which is set up for a production run. Only a
few of these variables are required to be set in the namelist; these
values are noted in the paragraphs below. An example of the default
namelist is shown in Section :ref:`cice_namelist_examples`.

The main run management namelist options are shown in :ref:`setupnml`. 
While additional namelist variables are
available in the uncoupled version, they are set by the driver in
CESM. For a full list of namelist variables, you should consult the CICE
Reference Guide :cite:`cice15`. 

Variables set by the driver include: ``dt``, ``runid``, ``runtype``, ``istep0``,
``days_per_year``, ``restart`` and ``dumpfreq``. These should be changed in the
CESM configuration files.

.. _setupnml:

.. csv-table:: Table 1: Setup Namelist Options
   :header: "Variable Name","Type","Default","Description"
   :widths: 25,12,18,35

   "&setup_nml","","",""
   "``ice_ic``","character","default","Filename for initial and branch runs. Set by driver scripts"
   "``pointer_file``","character","’rpointer.ice’","Pointer file that contains the name of the restart file"
   "``restart_file``","character","none","Restart file prefix. Set by driver."
   "``restart_format``","character","none","Restart file format. bin \= binary, nc \= netcdf, pio \= use pio library (default)."
   "``restart_ext``","logical",".false.","Write ghost cells as a part of restarts"
   "``history_file``","character","'unknown'","History file prefix. Set by driver. ’default’ uses default initialization. ’none’ initializes with no ice."  
   "``days_per_year``","integer","365","Standard number of days per year for calendar. Does interact with Gregorian calendar setting. Set by driver."
   "``year_init``","integer","1","Used in leap year calculation. Do not change"
   "``ndtd``","integer","1","Number of dynamic timesteps per thermodynamic timestep"
   "``histfreq``","char array","’m’,’x’,’x’,’x’,’x’","Unit for frequency of output written to history streams" 
   "","","","’H’ or ’h’ writes hourly data"
   "","","","’D’ or ’d’ writes daily data"
   "","","","’M’ or ’m’ writes monthly data"
   "","","","’Y’ or ’y’ writes yearly data"
   "","","","’1’ writes every timestep"
   "","","","’x’ no history data is written"  
   "``histfreq_n``","integer","1,1,1,1,1","Frequency of histfreq history data is written to each stream"
   "``dumpfreq``","character","'x'","Unit for frequency of dump files. Set by driver."
   "``dumpfreq_n``","integer","1","Frequency of dumpfreq dump files. Set by driver."
   "``hist_avg``","logical",".true.","If true, averaged history information is written out at a frequency determined by histfreq. If false, instantaneous values are written in all streams."
   "``write_ic``","logical",".true.","If true, write initial conditions"
   "``diagfreq``","integer","24","Frequency of diagnostics written (min, max, hemispheric sums) to standard output."
   "","","","'24' = diagnostics written once every 24 timesteps"
   "","","","'1' = diagnostics written each timestep"
   "","","","'0' = no diagnostics written"
   "``print_global``","logical",".true.","Print global diagnostics"
   "``print_points``","logical",".true.","Print diagnostics at latpnt and lonpnt"
   "``latpnt``","float arr","90.0, -65.0","Latitudes for diagnostic points (``print_points``)"
   "``lonpnt``","float arr","0.0, -45.0","Longitudes for diagnostic points (``print_points``)"
   "``lcdf64``","logical",".false.","Use 64-bit offset in netcdf files"
   "``bfbflag``","logical",".false.","Require bit-for-bit global sums"


Changing the timestep
---------------------

``dt`` is the timestep in seconds for the ice model thermodynamics. The
thermodynamics component is stable but not necessarily accurate for any
value of the timestep. The value chosen for ``dt`` depends on the stability
of the transport and the grid resolution. A conservative estimate of ``dt``
for the transport using the upwind advection scheme is:

.. math:: \Delta t < \frac{min(\Delta x, \Delta y)}{4 * max(u, v)} .

Maximum values for ``dt`` for the two standard CESM POP grids, assuming
:math:`max(u,v) = 0.5\ m/s`, are shown in :ref:`timestep`.
The default timestep for CICE is 30 minutes for gx1, 
which must be equvialent to the coupling interval (``NCPL_ICE`` and ``NCPL_ATM``) 
set in the CESM configuration files **env\_run.xml**. One should only change the CICE
timestep using the ``NCPL_ATM`` variable in **env\_run.xml**. For more on this see:

http://www.cesm.ucar.edu/models/cesm2/component_settings/drv_input_cesm.html

.. _timestep:

.. csv-table:: Table 2: Recommended timesteps
   :header: "","",""
   :widths: 20,60,20

   "Grid"," :math:`min(\Delta x, \Delta y)`",":math:`max {\Delta} t`"
   "gx3","28845.9 m","4.0 hr"
   "gx1","8558.2 m","1.2 hr"

Occasionally, ice velocities are calculated that are larger than what is
assumed when the model timestep is chosen. This causes a CFL violation
in the transport scheme. A namelist option was added (``ndtd``) to
subcycle the dynamics to get through these instabilities that arise
during long integrations. The default value for this variable is one,
and is typically increased to two when the ice model reaches an
instability. The value in the namelist should be returned to one by the
user when the model integrates past that point.

Writing Output
--------------

The namelist variables that control the frequency of the model
diagnostics, netCDF history, and restart files are shown in
:ref:`setupnml`. By default, diagnostics are written out once
every 48 timesteps to the ascii file **ice.log.$LID** (see section
:ref:`standard-output`). $LID is a time stamp that is set in the main script.

The namelist variable ``histfreq`` controls the output frequency of the
netCDF history files; writing monthly averages is the default. The
content of the history files is described in section :ref:`history-files`. The
value of ``hist_avg`` determines if instantaneous or averaged variables are
written at the frequency set by ``histfreq``. If ``histfreq`` is set to ``1`` for
instantaneous output, ``hist_avg`` is set to ``.false.`` within the source code
to avoid conflicts. The latest version of CICE allows for multiple
history streams, currently set to a maximum of 5. The namelist
variables, ``histfreq`` and ``histfreq_n`` are now arrays which allow for
different frequency history file sets. More detail on this is available
in :ref:`history-files`.

The namelist variable ``pointer_file`` is set to the name of the pointer
file containing the restart file name that will be read when model
execution begins. The pointer file resides in the scripts directory and
is created initially by the ice setup script but is overwritten every
time a new restart file is created. It will contain the name of the
latest restart file. The default filename **ice.restart\_file** shown in
:ref:`setupnml` will not work unless some modifications
are made to the ice setup script and a file is created with this name
and contains the name of a valid restart file; this variable must be set
in the namelist. More information on restart pointer files can be found
in Section :ref:`restart-files`.

The variables ``dumpfreq`` and ``dumpfreq_n`` control the output frequency of
the netCDF restart files; writing one restart file per year is the
default and is set by the CESM driver. The default format for all reads and 
writes of files in CESM is now pio, but this can be changed to binary or
netCDF through the namelist variable, ``restart_format``. 

The Parallel Input/Output libraries or "PIO" are used within the CESM for more
efficient reading and writing. PIO includes options for binary, netCDF version3,
parallel netCDF, or netCDF version 4 parallel. More on this can be found here:
http://ncar.github.io/ParallelIO/

If ``print_points`` is ``.true.``, diagnostic data is printed out for two grid
points, one near the north pole and one near the Weddell Sea. The points
are set via namelist variables ``latpnt`` and ``lonpnt``. This option can be
helpful for debugging.

Model Physics
-------------

Some of the most commonly used namelist variables for the ice model physics 
are listed in the following tables. More information can be found in the 
CICE reference guide at :cite:`cice15`.

The calculation of the ice velocities is subcycled ``ndte`` times per
timestep so that the elastic waves are damped before the next timestep.
The subcycling timestep is calculated as :math:`dte = dt/ndte` and must be
sufficiently smaller than the damping timescale T, which needs to be
sufficiently shorter than dt.

.. math:: dte < T < dt

This relationship is discussed in :cite:`cice15`. The best ratio for 
:math:`[dte:T:dt]` is [1:40:120]. Typical combinations of (``dt``, ``ndte``) 
are (3600., 120), (7200., 240) (10800., 120). The default ``ndte``
is ``120`` as set in **ice\_init.F90**.

``kitd`` determines the scheme used to redistribute sea ice within the ice
thickness distribution (ITD) as the ice grows and melts. The linear
remapping scheme is the default and approximates the thickness
distribution in each category as a linear function. The delta
function method represents *g(h)* in each category as a delta function. 
This method can leave some categories mostly empty at any given time
and cause jumps in the properties of *g(h)*.

``kdyn`` determines the ice dynamics used in the model. The default is the
elastic-viscous-plastic (EVP) dynamics (``kdyn`` = 1). If ``kdyn`` is set 
to ``0``, the ice dynamics is inactive. In this case, ice velocities are 
not computed and ice is not transported. Since the initial ice velocities 
are read in from the restart file, the maximum and minimum velocities written 
to the log file will be non-zero in this case, but they are not used in any
calculations.

The value of ``kstrength`` determines which formulation is used to calculate
the strength of the pack ice. The calculation depends on mean ice
thickness and open water fraction. The calculation is based on
energetics and should not be used if the ice that participates in
ridging is not well resolved.

The variable ``advection`` determines the horizontal transport scheme used. The default
scheme is the incremental remapping method (``advection`` = "remap"). This method is less
diffusive and is computationally efficient for large numbers of
categories or tracers than other options. The upwind scheme is also available, but this 
scheme is only first order accurate.

.. _dynamics:

.. csv-table:: Table 3: Dynamics Namelist Options
   :header: "Variable Name","Type","Default","Description"
   :widths: 20,12,12,60

   "&dynamics_nml","","",""
   "``kdyn``","Integer","1","Determines ice dynamics, 0 = No ice dynamics, 1 = Elastic viscous plastic dynamics"
   "``revised_evp``","Logical",".false.","Revised EVP formulation"
   "``ndte``", "Integer", "1","Number of sub-cycles in EVP dynamics."
   "``advection``","Character","'remap'","Determines horizontal advection scheme. ’remap’ = incremental remapping, ’upwind’ = first order advection"
   "``kstrength``","Integer","1","Determines pressure formulation, 0 = parameterization, 1 = parameterization"
   "``krdg_partic``","Integer","1","Ridging participation function, 0 = Thorndike, 1 = Expontential"
   "``krdg_redist``","Integer","1","Ridging distribution function, 0 = Hibler , 1 = Expontential"
   "``mu_rdg``","Real","4.0","e-folding scale of ridged ice"
   "``cf``","Real","17.0","Ratio of ridging work to PE change"

A new thermodynamics option (``ktherm = 2``) is now the default. This is the
so-called mushy-layer thermodyanmics of :cite:`turner15`. The basic
idea of this is that prognostic salinity is now used in the vertical
thermodynamic calculation where this used to be a constant profile. The
CESM1 and older option of :cite:`bitz99`, (``ktherm = 1``) is still available.
There are several additional thermodynamic options not listed that go with 
``ktherm = 2``, that are described more thoroughly in :cite:`cice15`.

.. _thermo:

.. csv-table:: Table 4: Thermodynamics Namelist Options
   :header: "Variable Name","Type","Default","Description"
   :widths: 20,12,12,60

   "&thermo_nml","","",""
   "``kitd``","Integer","1","Determines ITD conversion, 0 = delta scheme, 1=linear remapping"
   "``ktherm``","Integer","1","Determines ice thermodynamics, 1 = BL99, 2 = mushy layer"
   "``conduct``","Character","'MU71'","Determines conductivity formulation used with ktherm = 1, MU71, bubbly"

For the newer delta-Eddington shortwave radiative transfer scheme ``shortwave = dEdd``, the
base albedos are computed based on the inherent optical properties of
snow, sea ice, and melt ponds. These albedos are most commonly changed through
adjustments to the snow grain radius, ``R_snw``, temperature to transition
to melting snow, ``dT_mlt_in``, and maximum snow grain radius, ``rsnw_mlt_in``. Note, the older CCSM3
radiation scheme is still available through ``shortwave = default``.

.. _shortwave:

.. csv-table:: Table 5: Radiation Namelist Options
   :header: "Variable Name","Type","Default: CESM-CAM4 gx3","Default: CESM-CAM4 gx1","Default: CESM-CAM5 gx1","Description"
   :widths: 24,14,16,16,16,42

   "&shortwave_nml","","","","",""
   "``shortwave``","Character","'dEdd'","'dEdd'","'dEdd'","Shortwave Radiative Transfer Scheme, ’dEdd’ = delta-Eddington Shortwave, ’default’ = CCSM3 Shortwave"
   "``albicev``","Real",0.68,0.75,0.75,"Visible ice albedo (CCSM3)"
   "``albicei``","Real",0.30,0.45,0.45,"Near-infrared ice albedo (CCSM3)"
   "``albsnowv``","Real",0.91,0.98,0.98,"Visible snow albedo (CCSM3)"
   "``albsnowi``","Real",0.63,0.73,0.73,"Near-infrared snow albedo (CCSM3)"
   "``r_ice``","Real",0.0,0.0,0.0,"Base ice tuning parameter (dEdd)"
   "``r_pnd``","Real",0.0,0.0,0.0,"Base pond tuning parameter (dEdd)"
   "``r_snw``","Real",-2.0,1.5,1.75,"Base snow grain radius tuning parameter (dEdd)"
   "``dt_mlt``","Real",2.0,1.5,1.0,"Snow melt onset temperature parameter (dEdd)"
   "``rsnw_mlt``","Real",2000.,1500.,1000.,"Snow melt maximum radius (dEdd)"

Tracer Namelist
---------------

The namelist parameters listed in :ref:`tracers` are
for adding tracers. The tracers should be added through the CESM
driver scripts via the ``CICE_CONFIG_OPTS`` variable.

.. _tracers:

.. csv-table:: Table 6: Tracer Namelist Options
   :header: "Variable Name","Type","Default","Description"
   :widths: 26,12,12,50

   "&tracer_nml","","",""
   "``tr_aero``","Logical",".true.","Aerosol physics and tracer"
   "``restart_aero``","Logical",".false.","Initialize aerosols to zero or from file."
   "``tr_iage``","Logical",".true.","Ice age passive tracer"
   "``restart_age``","Logical",".false.","Initialize iage to zero or from file."
   "``tr_FY``","Logical",".true.","First-year ice area passive tracer"
   "``restart_FY``","Logical",".false.","Initialize first-year ice to zero or from file."
   "``tr_lvl``","Logical",".false.","Level ice area passive tracer"
   "``restart_lvl``","Logical",".false.","Initialize level ice to zero or from file."
   "``tr_pond_cesm``","Logical",".false.","The older CESM melt pond option."
   "``restart_pond_cesm``","Logical",".false.","Initialize CESM ponds to zero or from file."
   "``tr_pond_lvl``","Logical",".true.","The Hunke et al. level ice pond formulation"
   "``restart_pond_lvl``","Logical",".false.","Initialize level ponds to zero or from file."
   "``tr_pond_topo``","Logical",".true.","The Felthem et al. topographic pond formulation"
   "``restart_pond_topo``","Logical",".false.","Initialize topgraphic ponds to zero or from file."

Prescribed Ice Namelist
-----------------------

The namelist parameters listed in :ref:`prescribed` are for the prescribed ice
option as used in AMIP and F compset (standalone CAM) runs [prescribed].

.. _prescribed:

.. csv-table:: Table 7: Prescribed Ice Namelist Options
   :header: "Variable Name","Type","Default","Description"
   :widths: 28,12,12,48

   "``prescribed_ice``","Logical",".false.","Flag to turn on prescribed ice"
   "``prescribed_ice_fill``","Logical",".false.","Flag to turn fill option"
   "``stream_year_first``","Integer","1","First year of prescribed ice data"
   "``stream_year_last``","Integer","1","Last year of prescribed ice data"
   "``model_year_align``","Integer","1","Year in model run that aligns with stream\_year\_first"
   "``stream_domfilename``","Character","'none'","Prescribed ice stream data file"
   "``stream_fldfilename``","Character","'none'","Prescribed ice stream data file"
   "``stream_fldvarname``","Character","'ice\_cov'","Ice fraction field name"

Grid Namelist
-------------

The namelist parameters listed in :ref:`grid` are for
grid and mask information. During execution, the ice model reads grid
and land mask information from the files ``grid_file`` and ``kmt_file`` that
should be located in the executable directory. There are commands in the
scripts that copy these files from the input data directory, rename them
from **global\_$ICE\_GRID.grid** and **global\_$ICE\_GRID.kmt** to the
default filenames shown in :ref:`grid`.

.. _grid:

.. csv-table:: Table 8: Grid Namelist Options
   :header: "Variable Name","Type","Default","Description"
   :widths: 20,12,28,48

   "&grid_nml","","",""
   "``grid_type``","Character","'displaced\_pole'","Determines grid type."
   " "," "," ","'displaced\_pole'"
   " "," "," ","tripole"
   " "," "," ","rectangular"
   "``grid_format``","Character","'binary'","Grid file format (binary or netCDF)"
   "``grid_file``","Character","'data.domain.grid'","Input filename containing grid information."
   "``gridcpl_file``","Character","'data.domain.grid'","Input filename containing grid information if coupling grid is different than computational grid."
   "``kmt_file``","Character","'data.domain.kmt'","Input filename containing land mask information."
   "``kcatbound``","Integer","0","How category boundaries are set (0 or 1)"

For coupled runs, supported grids include the ’displaced\_pole’ grids
(gx3 and gx1) and the ’tripole’ grids.

Domain Namelist
---------------

The namelist parameters listed in :ref:`domain` are
for computational domain decomposition information. These are generally
set in the build configure scripts through the variables ``CICE_DECOMPTYPE`` 
and ``CICE_DECOMPSETTING`` based on the number of processors.
See the CESM scripts documentation.

.. _domain:

.. csv-table:: Table 9: Domain Namelist Options
   :header: "Variable Name","Type","Default","Description"
   :widths: 36,12,16,36

   "&domain_nml","","",""
   "``processor_shape``","Character","'square-ice'","Approximate block shapes"
   "","","","'slenderX1'"
   "","","","'slenderX2'"
   "","","","'square-ice'"
   "","","","'square-pop'"
   "``distribution_type``","Character","'spacecurve'","How domain is split into blocks and distributed onto processors"
   "","","","'cartesian'"
   "","","","'rake'"
   "","","","'roundrobin'"
   "","","","'sectcart'"
   "","","","'sectrobin'"
   "","","","'spacecurve'"
   "``distribution_wght``","Character","'latitude'","How blocks are weighted when using space-filling curves"
   "","","","'block'"
   "","","","'latitude'"
   "","","","'erfc'"
   "","","","'file'"
   "``distribution_wght_file``","Character","'none'","File containing space-filling curve weights when using file weighting"
   "``ew_boundary_type``","Character","'cyclic'","Boundary conditions in E-W direction"
   "``ns_boundary_type``","Character","'open'","Boundary conditions in N-S direction"
   "``maskhalo_dyn``","Logical",".true.","Use masked halos in dynamics."
   "``maskhalo_remap``","Logical",".true.","Use masked halos in remapping."
   "``maskhalo_bound``","Logical",".true.","Use masked halos in state bound."

PIO Namelist
------------

PIO settings are now handled via the CESM driver.
