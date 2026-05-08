.. _cice_namelist_examples:


CICE Namelist Examples
======================

This section shows several examples of namelists from the coupled ice
model. These examples are taken directly from **$CASE/CaseDocs/ice\_in** for
the coupled model. Most of the variables in the namelist are determined
from environment variables set elsewhere in the scripts. Since the
namelists from the coupled model are “resolved” by the scripts, meaning
that the values of most of the shell script variables are put directly
into the namelist, examples are shown for the most commonly used
configurations. Variables that are commonly changed directly in the
namelist are the timestep dt and the number of subcycles per timestep in
the ice dynamics ndte.

Example 1: CESM Fully Coupled
-------------------------------

The following example is the namelist used for CESM fully coupled, or
the B configuration. A completely resolved version of the namelist will 
be written to **$CASE/CaseDocs/ice\_in** and **ice\_in** in the executable 
directory. While this list includes additional physics and biogeochemistry
options, we have not tested these. More information can be found in the CICE
Reference Guide :cite:`cice15`. 
Note that modifications to the CICE namelist go in **$CASE/user\_nl\_cice**.

::

  &setup_nml
    bfbflag = .false.
    days_per_year = 365
    diagfreq = 24
    dumpfreq = "x"
    hist_avg = .true.
    histfreq = "m", "x", "x", "x", "x"
    histfreq_n = 1, 0, 0, 0, 0
    history_file = "unknown"
    history_precision = 4
    ice_ic = "b.e20.B1850.f09_g17.pi_control.all.297.cice.r.0130-01-01-00000.nc"
    latpnt = 90.0, -65.0
    lcdf64 = .true.
    lonpnt = 0.0, -45.0
    ndtd = 1
    pointer_file = "./rpointer.ice"
    print_global = .true.
    print_points = .false.
    restart_ext = .false.
    restart_file = ""
    restart_format = "pio"
    write_ic = .false.
    year_init = 1
  /
  &grid_nml
    grid_file = "/glade/p/cesmdata/cseg/inputdata/ocn/pop/gx1v7/grid/horiz_grid_20010402.ieeer8"
    grid_format = "bin"
    grid_type = "displaced_pole"
    gridcpl_file = "unknown_gridcpl_file"
    kcatbound = 0
    kmt_file = "/glade/p/cesmdata/cseg/inputdata/ocn/pop/gx1v7/grid/topography_20161215.ieeei4"
  /
  &tracer_nml
    restart_aero = .false.
    restart_age = .false.
    restart_fy = .false.
    restart_iso = .false.
    restart_lvl = .false.
    restart_pond_cesm = .false.
    restart_pond_lvl = .false.
    restart_pond_topo = .false.
    tr_aero = .true.
    tr_fy = .true.
    tr_iage = .true.
    tr_iso = .false.
    tr_lvl = .true.
    tr_pond_cesm = .false.
    tr_pond_lvl = .true.
    tr_pond_topo = .false.
  /
  &thermo_nml
    a_rapid_mode = 0.5e-03
    aspect_rapid_mode = 1.0
    conduct = "MU71"
    dsdt_slow_mode = -1.5e-07
    kitd = 1
    ktherm = 2
    phi_c_slow_mode = 0.05
    phi_i_mushy = 0.85
    rac_rapid_mode = 10
  /
  &dynamics_nml
    advection = "remap"
    cf = 17.0
    kdyn = 1
    krdg_partic = 1
    krdg_redist = 1
    kstrength = 1
    mu_rdg = 4.0
    ndte = 120
    revised_evp = .false.
  /
  &shortwave_nml
    ahmax = 0.3
    albedo_type = "default"
    albicei = 0.45
    albicev = 0.75
    albsnowi = 0.73
    albsnowv = 0.98
    dt_mlt = 1.50
    kalg = 0.0
    r_ice = 0.0
    r_pnd = 0.0
    r_snw = 1.25
    rsnw_mlt = 1500.
    shortwave = "dEdd"
  /
  &ponds_nml
    dpscale = 1.0e-3
    frzpnd = "cesm"
    hp1 = 0.01
    hs0 = 0.03
    hs1 = 0.03
    pndaspect = 0.8
    rfracmax = 0.85
    rfracmin = 0.15
  /
  &forcing_nml
    fbot_xfer_type = "constant"
    formdrag = .false.
    highfreq = .true.
    l_mpond_fresh = .false.
    natmiter = 5
  /
  &domain_nml
    distribution_type = "spacecurve"
    distribution_wght = "latitude"
    ew_boundary_type = "cyclic"
    maskhalo_bound = .true.
    maskhalo_dyn = .true.
    maskhalo_remap = .true.
    ns_boundary_type = "open"
    processor_shape = "square-ice"
  /
  &zbgc_nml
    bgc_data_dir = "unknown_bgc_data_dir"
    bgc_flux_type = "Jin2006"
    nit_data_type = "unknown"
    phi_snow = 0.5
    restart_bgc = .false.
    restart_hbrine = .false.
    restore_bgc = .false.
    sil_data_type = "unknown"
    skl_bgc = .false.
    tr_bgc_am_sk = .false.
    tr_bgc_c_sk = .false.
    tr_bgc_chl_sk = .false.
    tr_bgc_dms_sk = .false.
    tr_bgc_dmspd_sk = .false.
    tr_bgc_dmspp_sk = .false.
    tr_bgc_sil_sk = .false.
    tr_brine = .false.
  /

Example 2: History File Namelist
-------------------------------------

The next sets of namelists control what variables are written to the history
file. Variables that are not output are set in the namelists ``icefields*_nml``. 
Some of the following fields are not written to the history file since they can be
retrieved from the ocean history files. The melt and freeze onset fields
are not used, since the information they contain may not be correct if
the model is restarted mid-year. It is better to use daily data to compute these
quantities. The ice areas and volumes for
categories six through ten are not used, since the default thickness
distribution consists of five ice categories.

::

  &icefields_bgc_nml
    f_aero = "mxxxx"
    f_aeron = "xxxxx"
    f_bgc_am_ml = "xxxxx"
    f_bgc_am_sk = "xxxxx"
    f_bgc_c = "xxxxx"
    f_bgc_c_sk = "xxxxx"
    f_bgc_chl = "xxxxx"
    f_bgc_chl_sk = "xxxxx"
    f_bgc_dms = "xxxxx"
    f_bgc_dms_ml = "xxxxx"
    f_bgc_dms_sk = "xxxxx"
    f_bgc_dmsp_ml = "xxxxx"
    f_bgc_dmspd = "xxxxx"
    f_bgc_dmspd_sk = "xxxxx"
    f_bgc_dmspp = "xxxxx"
    f_bgc_dmspp_sk = "xxxxx"
    f_bgc_n = "xxxxx"
    f_bgc_n_sk = "xxxxx"
    f_bgc_nh = "xxxxx"
    f_bgc_nit_ml = "xxxxx"
    f_bgc_nit_sk = "xxxxx"
    f_bgc_no = "xxxxx"
    f_bgc_s = "xxxxx"
    f_bgc_sil = "xxxxx"
    f_bgc_sil_ml = "xxxxx"
    f_bgc_sil_sk = "xxxxx"
    f_bphi = "xxxxx"
    f_btin = "xxxxx"
    f_faero_atm = "mxxxx"
    f_faero_ocn = "mxxxx"
    f_fbri = "xxxxx"
    f_fn = "xxxxx"
    f_fn_ai = "xxxxx"
    f_fnh = "xxxxx"
    f_fnh_ai = "xxxxx"
    f_fno = "xxxxx"
    f_fno_ai = "xxxxx"
    f_fsil = "xxxxx"
    f_fsil_ai = "xxxxx"
    f_grownet = "xxxxx"
    f_hbri = "xxxxx"
    f_ppnet = "xxxxx"
  /
  &icefields_drag_nml
    f_cdn_atm = "xxxxx"
    f_cdn_ocn = "xxxxx"
    f_drag = "xxxxx"
  /
  &icefields_mechred_nml
    f_alvl = "xxxxx"
    f_aparticn = "xxxxx"
    f_araftn = "xxxxx"
    f_ardg = "xxxxx"
    f_ardgn = "xxxxx"
    f_aredistn = "xxxxx"
    f_dardg1dt = "xxxxx"
    f_dardg1ndt = "xxxxx"
    f_dardg2dt = "xxxxx"
    f_dardg2ndt = "xxxxx"
    f_dvirdgdt = "xxxxx"
    f_dvirdgndt = "xxxxx"
    f_krdgn = "xxxxx"
    f_opening = "xxxxx"
    f_vlvl = "xxxxx"
    f_vraftn = "xxxxx"
    f_vrdg = "xxxxx"
    f_vrdgn = "xxxxx"
    f_vredistn = "xxxxx"
  /
  &icefields_pond_nml
    f_apeff = "xxxxx"
    f_apeff_ai = "xxxxx"
    f_apeffn = "xxxxx"
    f_apond = "mxxxx"
    f_apond_ai = "mxxxx"
    f_apondn = "mxxxx"
    f_hpond = "mxxxx"
    f_hpond_ai = "mxxxx"
    f_hpondn = "mxxxx"
    f_ipond = "mxxxx"
    f_ipond_ai = "mxxxx"
  /
  &icefields_nml
    f_a11 = "xxxxx"
    f_a12 = "xxxxx"
    f_aice = "mxxxx"
    f_aicen = "mxxxx"
    f_aisnap = "xxxxx"
    f_albice = "mxxxx"
    f_albpnd = "mxxxx"
    f_albsni = "mxxxx"
    f_albsno = "mxxxx"
    f_alidf = "xxxxx"
    f_alidf_ai = "mxxxx"
    f_alidr = "xxxxx"
    f_alidr_ai = "mxxxx"
    f_alvdf = "xxxxx"
    f_alvdf_ai = "mxxxx"
    f_alvdr = "xxxxx"
    f_alvdr_ai = "mxxxx"
    f_angle = .true.
    f_anglet = .true.
    f_blkmask = .true.
    f_bounds = .false.
    f_cmip = "xxxxx"
    f_congel = "mxxxx"
    f_coszen = "xxxxx"
    f_daidtd = "mxxxx"
    f_daidtt = "mxxxx"
    f_divu = "mxxxx"
    f_dsnow = "xxxxx"
    f_dvidtd = "mxxxx"
    f_dvidtt = "mxxxx"
    f_dxt = .false.
    f_dxu = .false.
    f_dyt = .false.
    f_dyu = .false.
    f_e11 = "xxxxx"
    f_e12 = "xxxxx"
    f_e22 = "xxxxx"
    f_evap = "mxxxx"
    f_evap_ai = "xxxxx"
    f_fcondtop_ai = "mxxxx"
    f_fcondtopn_ai = "mxxxx"
    f_fhocn = "mxxxx"
    f_fhocn_ai = "mxxxx"
    f_flat = "mxxxx"
    f_flat_ai = "mxxxx"
    f_flatn_ai = "mxxxx"
    f_flwdn = "mxxxx"
    f_flwup = "mxxxx"
    f_flwup_ai = "xxxxx"
    f_fmeltt_ai = "mxxxx"
    f_fmelttn_ai = "mxxxx"
    f_frazil = "mxxxx"
    f_fresh = "mxxxx"
    f_fresh_ai = "mxxxx"
    f_frz_onset = "xxxxx"
    f_frzmlt = "xxxxx"
    f_fsalt = "mxxxx"
    f_fsalt_ai = "mxxxx"
    f_fsens = "mxxxx"
    f_fsens_ai = "xxxxx"
    f_fsensn_ai = "xxxxx"
    f_fsurf_ai = "mxxxx"
    f_fsurfn_ai = "mxxxx"
    f_fswabs = "mxxxx"
    f_fswabs_ai = "mxxxx"
    f_fswdn = "mxxxx"
    f_fswfac = "mxxxx"
    f_fswint_ai = "mxxxx"
    f_fswthru = "mxxxx"
    f_fswthru_ai = "mxxxx"
    f_fswup = "mxxxx"
    f_fy = "xxxxx"
    f_hi = "mxxxx"
    f_hisnap = "xxxxx"
    f_hs = "mxxxx"
    f_hte = .false.
    f_htn = .false.
    f_iage = "xxxxx"
    f_icepresent = "mxxxx"
    f_keffn_top = "xxxxx"
    f_meltb = "mxxxx"
    f_meltl = "mxxxx"
    f_melts = "mxxxx"
    f_meltt = "mxxxx"
    f_mlt_onset = "xxxxx"
    f_ncat = .true.
    f_qref = "mxxxx"
    f_rain = "mxxxx"
    f_rain_ai = "xxxxx"
    f_s11 = "xxxxx"
    f_s12 = "xxxxx"
    f_s22 = "xxxxx"
    f_shear = "mxxxx"
    f_sice = "xxxxx"
    f_sig1 = "mxxxx"
    f_sig2 = "mxxxx"
    f_sinz = "mxxxx"
    f_snoice = "mxxxx"
    f_snow = "mxxxx"
    f_snow_ai = "xxxxx"
    f_snowfrac = "mxxxx"
    f_snowfracn = "mxxxx"
    f_sss = "mxxxx"
    f_sst = "mxxxx"
    f_strairx = "mxxxx"
    f_strairy = "mxxxx"
    f_strcorx = "mxxxx"
    f_strcory = "mxxxx"
    f_strength = "mxxxx"
    f_strintx = "mxxxx"
    f_strinty = "mxxxx"
    f_strocnx = "mxxxx"
    f_strocny = "mxxxx"
    f_strtltx = "mxxxx"
    f_strtlty = "mxxxx"
    f_tair = "mxxxx"
    f_tarea = .true.
    f_tinz = "mxxxx"
    f_tmask = .true.
    f_tref = "mxxxx"
    f_trsig = "xxxxx"
    f_tsfc = "mxxxx"
    f_tsnz = "xxxxx"
    f_uarea = .true.
    f_uatm = "mxxxx"
    f_uocn = "xxxxx"
    f_uvel = "mxxxx"
    f_vatm = "mxxxx"
    f_vgrdb = .true.
    f_vgrdi = .true.
    f_vgrds = .true.
    f_vicen = "mxxxx"
    f_vocn = "xxxxx"
    f_vsnon = "mxxxx"
    f_vvel = "mxxxx"
    f_yieldstress11 = "xxxxx"
    f_yieldstress12 = "xxxxx"
    f_yieldstress22 = "xxxxx"
  /
