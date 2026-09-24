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
:ref:`history`. SIMIP variables used for CMIP experiments are controlled
separately, as described in :ref:`cmip-history`.

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

.. _cmip-history:

SIMIP Variables and CMIP Experiments
----------------------------------------

Beginning with CICE v5, additional history variables requested by the Sea
Ice Model Intercomparison Project (SIMIP) were available as history output.
These variables use the standardized SIMIP/CMIP field names (for example
``sithick``, ``siconc``, ``sidmassgrowthbot``) rather than the native CICE
names, and are intended to make CESM output directly comparable to other
models participating in CMIP. The full list of monthly and daily SIMIP
variables, along with their formal definitions, is part of the CMIP data
request; the CICE implementation has been updated to match the
`CMIP7 data request <https://wcrp-cmip.org/cmip7-data-request-v1-0/>`_.

Each SIMIP variable is controlled by its own ``f_si*`` logical history
flag (see :ref:`cmip-table` below), using the same ``'m'``, ``'d'``,
``'h'``, ``'1'``, ``'x'`` stream-selection syntax as the other history
flags described in :ref:`history`. The older ``f_CMIP`` flag, which
previously turned on a fixed bundle of CMIP-related output, has been
**deprecated** and no longer has any effect; a warning is written to the
diagnostic output if it is set to anything other than its default. SIMIP
output is now controlled entirely through the individual ``f_si*`` flags
(and the standard flags such as ``f_aice``, ``f_apond``, ``f_alvl``,
``f_ardg`` that some SIMIP quantities reuse), which can be enabled or
disabled independently in **user\_nl\_cice**.

Turning on SIMIP output for a CMIP6 or CMIP7 case
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In CESM, the recommended way to turn on the SIMIP variable set is to
append ``%CMIP6`` or ``%CMIP7`` to the ice component of the compset
longname (for example ``...CICE%CMIP7_...``) when creating a case. This
sets ``CICE_USER_MODS`` to point at a ``usermods_dirs/cmip6`` or
``usermods_dirs/cmip7`` directory that is automatically applied to the
case and contains a pre-built **user\_nl\_cice** with:

- ``histfreq``, ``histfreq_n``, and ``hist_avg`` configured to produce
  the monthly, daily, and (for CMIP7) sub-daily streams expected by the
  CMIP data request, and

- the appropriate ``f_si*`` flags (and related flags such as ``f_aice``,
  ``f_apond``, ``f_alvl``, ``f_ardg``) set to ``'md'`` or ``'m'`` to write
  those variables to the monthly and/or daily history streams.

The CMIP7 usermods set is the more complete and current of the two, and
is kept in sync with the SIMIP flag list shown in :ref:`cmip-table`. The
contents of both usermods directories can be found under
**components/cice/cime\_config/usermods\_dirs/**.

Turning on SIMIP output manually
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For compsets that do not include the ``%CMIP6``/``%CMIP7`` modifier, or
to customize which SIMIP variables are written, users can copy the
namelist settings from
**components/cice/cime\_config/usermods\_dirs/cmip7/user\_nl\_cice**
(or the CMIP6 equivalent) directly into their own **$CASE/user\_nl\_cice**
file, then remove or edit individual ``f_si*`` lines as needed. As with
any other history flag, setting an entry to ``'xxxxx'`` removes that
field from all streams, for example:

::

    &icefields_nml
        f_sidmasstranx =  'xxxxx'
      , f_sidmasstrany =  'xxxxx'
    /

Caveats
~~~~~~~~

Several SIMIP quantities (for example ``sithick``, ``sisnthick``,
``simpthick``) are defined per unit *ice area* rather than per unit grid
cell area, and are computed by dividing by the local ice (or melt pond,
or ridged ice) fraction. Where that fraction is very small, the divided
quantity can appear unphysically large; these fields should be
interpreted with care, particularly near the ice edge or in
low-concentration regions. Increasing the precision of the history file
output from 4 bytes to 8 bytes, via the ``history_precision`` namelist
flag, can also help when debugging these fields.

.. _cmip-table:

.. csv-table:: Table 13: SIMIP History Variables
   :header: "Logical Variable","Description","Units"
   :widths: 24,60,16

   ``f_siage`` , sea ice age , s
   ``f_siconc`` , sea-ice area percentage (ocean grid) , %
   ``f_sifb`` , sea-ice freeboard , m
   ``f_sihc`` , sea-ice heat content , J m\ :math:`^{-2}`
   ``f_simass`` , sea-ice mass per area , kg m\ :math:`^{-2}`
   ``f_sisali`` , sea-ice salinity , ppt
   ``f_sisaltmass`` , mass of salt in sea ice per area , kg m\ :math:`^{-2}`
   ``f_sispeed`` , sea-ice speed , m s\ :math:`^{-1}`
   ``f_sithick`` , sea-ice thickness (volume / area) , m
   ``f_sitimefrac`` , fraction of time steps with sea ice present , 1
   ``f_siu`` , x-component of sea-ice velocity , m s\ :math:`^{-1}`
   ``f_siv`` , y-component of sea-ice velocity , m s\ :math:`^{-1}`
   ``f_sivol`` , sea-ice volume per area , m
   ``f_sicompstren`` , compressive sea-ice strength , N m\ :math:`^{-1}`
   ``f_sidragbot`` , oceanic drag coefficient , 1
   ``f_sidragtop`` , atmospheric drag coefficient , 1
   ``f_sidivvel`` , divergence of the sea-ice velocity field , s\ :math:`^{-1}`
   ``f_sishearvel`` , maximum shear of the sea-ice velocity field , s\ :math:`^{-1}`
   ``f_sistressave`` , average normal stress in sea ice , N m\ :math:`^{-1}`
   ``f_sistressmax`` , maximum shear stress in sea ice , N m\ :math:`^{-1}`
   ``f_sidconcdyn`` , sea-ice area fraction tendency due to dynamics , s\ :math:`^{-1}`
   ``f_sidconcth`` , sea-ice area fraction tendency due to thermodynamics , s\ :math:`^{-1}`
   ``f_sidmassdyn`` , sea-ice mass change from dynamics , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sidmassth`` , sea-ice mass change from thermodynamics , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sidmassevapsubl`` , sea-ice mass change through evaporation/sublimation , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sidmassgrowthbot`` , sea-ice mass change through basal growth , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sidmassgrowthsi`` , sea-ice mass change from snow-to-ice conversion , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sidmassgrowthwat`` , sea-ice mass change through frazil growth , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sidmassmeltbot`` , sea-ice mass change through bottom melting , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sidmassmeltlat`` , sea-ice mass change through lateral melting , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sidmassmelttop`` , sea-ice mass change through surface melting , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sidmasstranx`` , x-component of sea-ice mass transport , kg s\ :math:`^{-1}`
   ``f_sidmasstrany`` , y-component of sea-ice mass transport , kg s\ :math:`^{-1}`
   ``f_siflcondbot`` , net conductive heat flux at the ice base , W m\ :math:`^{-2}`
   ``f_siflcondtop`` , net conductive heat flux at the ice surface , W m\ :math:`^{-2}`
   ``f_siflfwbot`` , freshwater flux from sea ice to ocean , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_siflfwdrain`` , freshwater flux from sea-ice surface drainage , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sifllattop`` , net latent heat flux over sea ice , W m\ :math:`^{-2}`
   ``f_sifllwdtop`` , downwelling longwave flux over sea ice , W m\ :math:`^{-2}`
   ``f_sifllwutop`` , upwelling longwave flux over sea ice , W m\ :math:`^{-2}`
   ``f_siflsaltbot`` , salt flux from sea ice to ocean , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_siflsensbot`` , net upward sensible heat flux under sea ice , W m\ :math:`^{-2}`
   ``f_siflsenstop`` , net downward sensible heat flux over sea ice , W m\ :math:`^{-2}`
   ``f_siflswdbot`` , downwelling shortwave flux underneath sea ice , W m\ :math:`^{-2}`
   ``f_siflswdtop`` , downwelling shortwave flux over sea ice , W m\ :math:`^{-2}`
   ``f_siflswutop`` , upwelling shortwave flux over sea ice , W m\ :math:`^{-2}`
   ``f_siforcecoriolx`` , Coriolis force term in force balance (x) , N m\ :math:`^{-2}`
   ``f_siforcecorioly`` , Coriolis force term in force balance (y) , N m\ :math:`^{-2}`
   ``f_siforceintstrx`` , internal stress force term in force balance (x) , N m\ :math:`^{-2}`
   ``f_siforceintstry`` , internal stress force term in force balance (y) , N m\ :math:`^{-2}`
   ``f_siforcetiltx`` , sea-surface tilt term in force balance (x) , N m\ :math:`^{-2}`
   ``f_siforcetilty`` , sea-surface tilt term in force balance (y) , N m\ :math:`^{-2}`
   ``f_sipr`` , rainfall rate over sea ice , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sistrxdtop`` , x-component of atmospheric stress on sea ice , N m\ :math:`^{-2}`
   ``f_sistrydtop`` , y-component of atmospheric stress on sea ice , N m\ :math:`^{-2}`
   ``f_sistrxubot`` , x-component of ocean stress on sea ice , N m\ :math:`^{-2}`
   ``f_sistryubot`` , y-component of ocean stress on sea ice , N m\ :math:`^{-2}`
   ``f_sitempbot`` , temperature at ice-ocean interface , K
   ``f_sitempsnic`` , temperature at snow-ice interface , K
   ``f_sitemptop`` , surface temperature of sea ice , K
   ``f_siitdconc`` , sea-ice area percentage by thickness category , %
   ``f_siitdsnconc`` , snow area percentage by thickness category , %
   ``f_siitdsnthick`` , snow thickness by thickness category , m
   ``f_siitdthick`` , sea-ice (floe) thickness by thickness category , m
   ``f_simpconc`` , percentage of sea ice covered by melt ponds , %
   ``f_simpeffconc`` , percentage of sea ice covered by effective (open) melt ponds , %
   ``f_simprefrozen`` , thickness of refrozen ice on melt ponds , m
   ``f_simpthick`` , melt pond depth , m
   ``f_sirdgconc`` , percentage of ridged sea ice , %
   ``f_sirdgthick`` , ridged ice thickness , m
   ``f_sisnconc`` , snow area percentage , %
   ``f_sisndmassdyn`` , snow mass change through advection by sea-ice dynamics , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sisndmassmelt`` , snow mass change through melt , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sisndmasssi`` , snow mass change through snow-to-ice conversion , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sisndmasssnf`` , snow mass change through snowfall , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sisndmasssubl`` , snow mass change through evaporation/sublimation , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sisndmasswind`` , snow mass change through wind drift of snow , kg m\ :math:`^{-2}` s\ :math:`^{-1}`
   ``f_sisnhc`` , snow heat content , J m\ :math:`^{-2}`
   ``f_sisnmass`` , snow mass per area , kg m\ :math:`^{-2}`
   ``f_sisnthick`` , snow thickness (volume / area) , m
