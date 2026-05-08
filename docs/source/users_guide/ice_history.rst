.. _history-files:

CICE History Files
==================

History files contain gridded data values written at specified times
during a model run. By default, the history files will be written to
the directory run directory defined in the CESM driver. The netCDF file
names are prepended by the character string set by the CESM driver.
This character string has been set according to
CESM Output Filename Requirements.  The user can
specify the frequency at which the data are written. Options are also
available to record averaged or instantaneous data. The form of the
history file names are as follows:

- Yearly averaged: **$CASE.cice.h?.yyyy.nc**
- Monthly averaged: **$CASE.cice.h?.yyyy-mm.nc**
- Daily averaged: **$CASE.cice.h?.yyyy-mm-dd.nc**
- Hourly averaged: **$CASE.cice.h?.yyyy-mm-dd-sssss.nc**
- Instantaneous (``hist_avg`` = .true.):  **$CASE.cice.h?.yyyy-mm-dd-sssss.nc**
- Instantaneous (written every dt, ``histfreq`` = 1): **$CASE.cice.h?.yyyy-mm-dd-sssss.nc**

$CASE is set in the main setup script. Note that the **?** denotes the
multiple stream option where the first stream is just .h. and subsequent
streams are h1, h2, etc. All history files are written in the executable
directory. Changes to the frequency and averaging will affect all output
fields. The best description of the history data comes from the file
itself using the netCDF command ``ncdump -h filename.nc``. Variables
containing grid information are written to every file and are listed in
:ref:`gridhistreq`. There are additional optional grid variables available
in :ref:`gridhistopt`. In addition to the
history files, a netCDF file containing a snapshot of the initial ice
state can be created at the start of each run by setting the namelist
variable ``write_ic=.true.`` The file name is
**$CASE.cice.i.yyyy-mm-dd-sssss.nc** and is written in the executable
directory. Note that variables without the ``f_`` string in front are
always written with every run :ref:`gridhistreq`, while the optional
ones are namelist options :ref:`gridhistopt`. 

.. _gridhistreq:

.. csv-table:: Table 10: Required Grid History Variables
   :header: "Field","Description","Units"
   :widths: 20,50,10

   "``time``","model time","days"              
   "``time_bounds``","boundaries for time-averaging interval","days"              
   "``TLON``","T grid center longitude","degrees"           
   "``TLAT``","T grid center latitude","degrees"           
   "``ULON``","U grid center longitude","degrees"           
   "``ULAT``","U grid center latitude","degrees"           
   "``NCAT``","category maximum thickness","m"
   "``VGRIDi``","vertical ice levels",""
   "``VGRIDs``","vertical snow levels",""

.. _gridhistopt: 

.. csv-table:: Table 11: Optional Grid History Variables
   :header: "Field","Description","Units"
   :widths: 20,50,10

   "``f_tmask``","ocean grid mask (0=land, 1=ocean)",""                                                        
   "``f_blkmask``","ice block mask",""                                                        
   "``f_tarea``","T grid cell area","m\ :math:`^{2}`"   
   "``f_uarea``","U grid cell area","m\ :math:`^{2}`"  
   "``f_dxt``","T cell width through middle","m"                 
   "``f_dyt``","T cell height through middle","m"                 
   "``f_dxu``","U cell width through middle","m"                
   "``f_dyu``","U cell height through middle","m"                 
   "``f_HTN``","T cell width North side","m"                 
   "``f_HTE``","T cell width East side","m"                 
   "``f_ANGLET``","angle grid makes with latitude line on T grid","radians"           
   "``f_ANGLE``","angle grid makes with latitude line on U grid","radians"           
   "``f_bounds``","corner points of grid cells","degrees"


Caveats Regarding Averaged Fields
----------------------------------------

In computing the monthly averages for output to the history files, most
arrays are zeroed out before being filled with data. These zeros are
included in the monthly averages where there is no ice. For some fileds,
this is not a problem, for example, ice thickness and ice area. For
other fields, this will result in values that are not representative of
the field when ice is present. Some of the fields affected are:

-  Flat, Fsens - latent and sensible heat fluxes

-  evap - evaporative water flux

-  Fhocn - ice/ocn net heat flux

-  Fswabs - snow/ice/ocn absorbed solar flux

-  strairx, strairy - zonal and meridional atm/ice stress

-  strcorx, strcory - zonal and meridional coriolis stress

For some fields, a non-zero value is set where there is no ice. For
example, Tsfc has the freezing point averaged in, and Flwup has
:math:`\sigma T_f^4` averaged in. At lower latitudes, these values can
be erroneous.

To aid in the interpretation of the fields, a field called
``ice_present`` is written to the history file. It contains information
on the fraction of the time-averaging interval when any ice was present
in the grid cell during the time-averaging interval in the history file.
This will give an idea of how many zeros were included in the average.

The second caveat results from the coupler multiplying fluxes it
receives from the ice model by the ice area. Before sending fluxes to
the coupler, they are divided by the ice area in the ice model. These
are the fluxes that are written to the history files, they are not what
affects the ice, ocean or atmosphere, nor are they useful for
calculating budgets. The division by the ice area also creates large
values of the fluxes at the ice edge. The affected fields are:

-  Flat, Fsens - latent and sensible heat fluxes

-  Flwup - outgoing longwave

-  evap - evaporative water flux

-  Fresh - ice/ocn fresh water flux

-  Fhnet - ice/ocn net heat flux

-  Fswabs - snow/ice/ocn absorbed solar flux

When applicable, two of the above fields will be written to the history
file: the value of the field that is sent to the coupler (divided by ice
area) and a value of the flux that has been multiplied by ice area (what
affects the ice). Fluxes multiplied by ice area will have the suffix
``_aice`` appended to the variable names in the history files. Fluxes sent
to the coupler will have “sent to coupler” appended to the ``long_name``.
Fields of rainfall and snowfall multiplied by ice area are written to
the history file, since the values are valid everywhere and represent
the precipitation rate on the ice cover.

Changing Frequency and Averaging
----------------------------------------

The frequency at which data are written to a history file as well as the
interval over which the time average is to be performed is controlled by
the namelist variable ``histfreq``. Data averaging is invoked by the
namelist variable ``hist_avg``. The averages are constructed by
accumulating the running sums of all variables in memory at each
timestep. The options for both of these variables are described in 
:ref:`setupnml`. If ``hist_avg`` is true, and ``histfreq`` is set to
monthly, for example, monthly averaged data is written out on the last
day of the month.

Changing Content
----------------------------------------

The second namelist in the setup script controls what variables are
written to the history file. To remove a field from this list, add the
name of the character variable associated with that field to the
**$CASE/user\_nl\_cice** file and assign it a value
of ’xxxxx’. For example, to remove ice thickness and snow cover from the
history file, add

::

    &icefields_nml
        f_hi   =  'xxxxx'
      , f_hs   =  'xxxxx'
    /

to the namelist. An incomplete list of history variables is available in
:ref:`history`. Note that there is a new flag ``f_CMIP`` that will turn on
all of the SIMIP variables.

.. _history:

.. csv-table:: Table 12: History Variables
   :header: "Logical Variable","Description","Units"
   :widths: 20,60,20

   ``f_hi`` , ice volume per unit area , m
   ``f_hs`` , snow volume per unit area , m
   ``f_snowfrac`` , snow fraction , 1
   ``f_Tsfc`` , snow/ice surface temperature , C
   ``f_aice`` , ice concentration (aggregate) , 1
   ``f_uvel`` , x component ice velocity , m s\ :math:`^{-1}`
   ``f_vvel`` , y component ice velocity , m s\ :math:`^{-1}`
   ``f_uatm`` , x component wind velocity , m s\ :math:`^{-1}`
   ``f_vatm`` , y component wind velocity , m s\ :math:`^{-1}`
   ``f_sice`` , bulk ice salinity, ppt
   ``f_fswdn`` , downwelling solar flux , W m\ :math:`^{-2}`
   ``f_fswup`` , upward reflected solar flux , W m\ :math:`^{-2}`
   ``f_flwdn`` , downwelling longwave flux , W m\ :math:`^{-2}`
   ``f_snow`` , snow fall rate received from coupler , cm day\ :math:`^{-1}`
   ``f_snow_ai`` , snow fall rate on ice cover , cm day\ :math:`^{-1}`
   ``f_rain`` , rain fall rate received from coupler , cm day\ :math:`^{-1}`
   ``f_rain_ai`` , rain fall rate on ice cover , cm day\ :math:`^{-1}`
   ``f_sst`` , sea surface temperature , C
   ``f_sss`` , sea surface salinity , g kg\ :math:`^{-1}`
   ``f_uocn`` , x component ocean current , m s\ :math:`^{-1}`
   ``f_vocn`` , y component ocean current , m s\ :math:`^{-1}`
   ``f_frzmlt`` , freeze/melt potential , W m\ :math:`^{-2}`
   ``f_fswabs`` , total absorbed solar flux sent to coupler , W m\ :math:`^{-2}`
   ``f_fswabs_ai`` , total absorbed solar flux in snow/ocn/ice , W m\ :math:`^{-2}`
   ``f_fswint_ai`` , internal absorbed solar flux in snow/ice , W m\ :math:`^{-2}`
   ``f_fswfac`` , shortwave scaling factor , 1
   ``f_coszen`` , cosine of the zenith angle, radians
   ``f_albsni`` , snow ice broadband albedo , %
   ``f_alvdr`` , visible direct albedo sent to coupler, %
   ``f_alidr`` , near-infrared direct albedo sent to coupler, %
   ``f_alvdf`` , visible diffuse albedo sent to coupler, %
   ``f_alidf`` , near-infrared diffuse albedo sent to coupler, %
   ``f_alvdr_ai`` , visible direct albedo , %
   ``f_alidr_ai`` , near-infrared direct albedo , %
   ``f_alvdf_ai`` , visible diffuse albedo , %
   ``f_alidf_ai`` , near-infrared diffuse albedo , %
   ``f_albsni`` , snow ice broadband albedo , %
   ``f_albsno`` , snow broadband albedo , %
   ``f_albpnd`` , pond broadband albedo , %
   ``f_albice`` , bare ice broadband albedo , %
   ``f_flat`` , latent heat flux sent to coupler , W m\ :math:`^{-2}`
   ``f_flat_ai`` , ice/atm latent heat flux , W m\ :math:`^{-2}`
   ``f_fsens`` , sensible heat flux sent to coupler , W m\ :math:`^{-2}`
   ``f_fsens_ai``\ , ice/atm sensible heat flux , W m\ :math:`^{-2}`
   ``f_flwup`` , outgoing longwave flux sent to coupler , W m\ :math:`^{-2}`
   ``f_flwup_ai`` , ice/atm outgoing longwave flux , W m\ :math:`^{-2}`
   ``f_evap`` , evaporative water flux sent to coupler , cm day\ :math:`^{-1}`
   ``f_evap_ai`` , ice/atm evaporative water flux , cm day\ :math:`^{-1}`
   ``f_Tair`` , air temperature , C
   ``f_Tref`` , 2 m reference temperature , C
   ``f_Qref`` , 2 m reference specific humidity , g/kg
   ``f_congel`` , basal ice growth , cm day\ :math:`^{-1}`
   ``f_frazil`` , frazil ice growth , cm day\ :math:`^{-1}`
   ``f_snoice`` , snow-ice formation , cm day\ :math:`^{-1}`
   ``f_meltb`` , basal ice melt , cm day\ :math:`^{-1}`
   ``f_melts`` , surface snow melt , cm day\ :math:`^{-1}`
   ``f_meltt`` , surface ice melt , cm day\ :math:`^{-1}`
   ``f_meltl`` , lateral ice melt , cm day\ :math:`^{-1}`
   ``f_fresh`` , ice/ocn fresh water flux sent to coupler , cm day\ :math:`^{-1}`
   ``f_fresh_ai`` , ice/ocn fresh water flux , cm day\ :math:`^{-1}`
   ``f_fsalt`` , ice to ocn salt flux sent to coupler , kg m\ :math:`^{-2}` day\ :math:`^{-1}`
   ``f_fsalt_ai`` , ice to ocn salt flux , kg m\ :math:`^{-2}` day\ :math:`^{-1}`
   ``f_fhocn`` , ice/ocn net heat flux sent to coupler, W m\ :math:`^{-2}`
   ``f_fhocn_ai`` , ice/ocn net heat flux , W m\ :math:`^{-2}`
   ``f_fswthru`` , SW transmitted through ice to ocean sent to coupler , W m\ :math:`^{-2}`
   ``f_fswthru_ai`` , SW transmitted through ice to ocean , W m\ :math:`^{-2}`
   ``f_strairx`` , zonal atm/ice stress , N m\ :math:`^{-2}`
   ``f_strairy`` , meridional atm/ice stress , N m\ :math:`^{-2}`
   ``f_strtltx`` , zonal sea surface tilt , m m\ :math:`^{-1}`
   ``f_strtlty`` , meridional sea surface tilt , m m\ :math:`^{-1}`
   ``f_strcorx`` , zonal coriolis stress , N m\ :math:`^{-2}`
   ``f_strcory`` , meridional coriolis stress , N m\ :math:`^{-2}`
   ``f_strocnx`` , zonal ocean/ice stress , N m\ :math:`^{-2}`
   ``f_strocny`` , meridional ocean/ice stress , N m\ :math:`^{-2}`
   ``f_strintx`` , zonal internal ice stress , N m\ :math:`^{-2}`
   ``f_strinty`` , meridional internal ice stress , N m\ :math:`^{-2}`
   ``f_strength``\ , compressive ice strength , N m\ :math:`^{-1}`
   ``f_divu`` , velocity divergence , % day\ :math:`^{-1}`
   ``f_shear`` , strain rate , % day\ :math:`^{-1}`
   ``f_opening`` , lead opening rate , % day\ :math:`^{-1}`
   ``f_sig1`` , normalized principal stress component ,
   ``f_sig2`` , normalized principal stress component ,
   ``f_daidtt`` , area tendency due to thermodynamics , % day\ :math:`^{-1}`
   ``f_daidtd`` , area tendency due to dynamics , % day\ :math:`^{-1}`
   ``f_dvidtt`` , ice volume tendency due to thermo. , cm day\ :math:`^{-1}`
   ``f_dvidtd`` , ice volume tendency due to dynamics , cm day\ :math:`^{-1}`
   ``f_mlt_onset`` , melt onset date ,
   ``f_frz_onset`` , freeze onset date ,
   ``f_icepresent`` , fraction of time with ice present in grid cell ,
   ``f_aicen`` , ice concentration (category), 1
   ``f_vicen`` , ice volume (category) , m
   ``f_vsnon`` , snow volume (category) , m
