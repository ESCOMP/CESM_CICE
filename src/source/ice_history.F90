!
!BOP
!
! !MODULE: ice_history - ice model history files
!

!
! Output files: netCDF or binary data, Fortran unformatted dumps
!
! !REVISION HISTORY:
!  SVN:$Id: ice_history.F90 61 2007-04-25 17:50:16Z dbailey $
!
! authors Tony Craig and Bruce Briegleb, NCAR
!         Elizabeth C. Hunke and William H. Lipscomb, LANL
!         C. M. Bitz, UW
!
! 2004 WHL: Block structure added 
! 2006 ECH: Accepted some CCSM code into mainstream CICE
!           Added ice_present, aicen, vicen; removed aice1...10, vice1...1.
!           Added histfreq_n and histfreq='h' options, removed histfreq='w'
!           Converted to free source form (F90)
!           Added option for binary output instead of netCDF
!
! !INTERFACE:
!
      module ice_history
!
! !USES:
!
      use ice_kinds_mod
      use ice_broadcast
      use ice_communicate, only: my_task, master_task
      use ice_blocks
      use ice_grid
      use ice_read_write
      use ice_fileunits
!
!EOP
!
      implicit none
      save
      
      logical (kind=log_kind) :: &
         hist_avg  ! if true, write averaged data instead of snapshots

      character (len=char_len) :: &
         history_format, & ! file format ('bin'=binary or 'nc'=netcdf)
         history_file  , & ! output file for history
         incond_file       ! output file for snapshot initial conditions

      character (len=char_len_long) :: &
         history_dir   , & ! directory name for history file
         incond_dir        ! directory for snapshot initial conditions

      character (len=char_len_long) :: &
         pointer_file      ! input pointer file for restarts

      !---------------------------------------------------------------
      ! Instructions for adding a field:
      !     Here:
      ! (1) Add to logical flags
      ! (2) Add to namelist (here and also in ice_in)
      ! (3) Add unique index corresponding to logical flag.
      !     In init_hist:
      ! (4) Add define_hist_field call with vname, vdesc, vunit,
      !     and vcomment, vcellmeas, and conversion factor if necessary.
      ! (5) Add logical flag to broadcast list
      ! (6) Add accum_hist_field call with appropriate variable.
      !---------------------------------------------------------------

      type, public :: ice_hist_field
          character (len=16) :: vname     ! variable name
          character (len=16) :: vunit     ! variable units
          character (len=16) :: vcoord    ! variable coordinates
          character (len=16) :: vcellmeas ! variable cell measures
          character (len=55) :: vdesc     ! variable description
          character (len=55) :: vcomment  ! variable description
          real (kind=dbl_kind) :: cona    ! multiplicative conversion factor
          real (kind=dbl_kind) :: conb    ! additive conversion factor
      end type

      integer (kind=int_kind), parameter :: &
         max_avail_hist_fields = 600      ! Max number of history fields

      integer (kind=int_kind) :: &
         num_avail_hist_fields = 0        ! Current number of defined fields

      type (ice_hist_field), dimension(max_avail_hist_fields) :: &
         avail_hist_fields

      character (len=16) :: vname_in     ! variable name
      character (len=55) :: vdesc_in     ! variable description
      character (len=55) :: vcomment_in  ! variable description

      !---------------------------------------------------------------
      ! primary info for the history file
      !---------------------------------------------------------------

      integer (kind=int_kind), parameter :: &
         ncat_hist = ncat         ! number of ice categories written <= ncat

      integer (kind=int_kind), parameter :: &
         nvar = 11                  ! number of grid fields that can be written
                                    !   excluding grid vertices

      real (kind=real_kind) :: time_beg, time_end ! bounds for averaging

      real (kind=dbl_kind), allocatable :: &
         aa(:,:,:,:)       ! field accumulations and averages

      real (kind=dbl_kind) :: &
         avgct             ! average sample counter

      logical (kind=log_kind) :: &
         igrd(nvar)        ! true if grid field is written to output file

      character (len=16), parameter :: &
         tstr  = 'TLON TLAT time', & ! vcoord for T cell quantities
         ustr  = 'ULON ULAT time', & ! vcoord for U cell quantities
         tcstr = 'area: tarea'   , & ! vcellmeas for T cell quantities
         ucstr = 'area: uarea'       ! vcellmeas for U cell quantities

      !---------------------------------------------------------------
      ! logical flags: write to output file if true
      !---------------------------------------------------------------

       logical (kind=log_kind) :: &
           f_tmask     = .true., &
           f_tarea     = .true., f_uarea      = .true., &
           f_dxt       = .true., f_dyt        = .true., &
           f_dxu       = .true., f_dyu        = .true., &
           f_HTN       = .true., f_HTE        = .true., &
           f_ANGLE     = .true., f_ANGLET     = .true., &
           f_bounds    = .true.

      logical (kind=log_kind) :: &
           f_hi        = .true., f_hs         = .true., &
           f_Tsfc      = .true., f_aice       = .true., &
           f_uvel      = .true., f_vvel       = .true., &
           f_fswdn     = .true., f_flwdn      = .true., &
           f_snow      = .true., f_snow_ai    = .true., &
           f_rain      = .true., f_rain_ai    = .true., &
           f_faero     = .false., f_fsoot     = .false.,&
           f_sst       = .true., f_sss        = .true., &
           f_uocn      = .true., f_vocn       = .true., &
           f_frzmlt    = .true., &
           f_fswfac    = .true., &
           f_fswabs    = .true., f_fswabs_ai  = .true., &
           f_albsni    = .true., &
           f_alvdr     = .true., f_alidr      = .true., &
           f_flat      = .true., f_flat_ai    = .true., &
           f_fsens     = .true., f_fsens_ai   = .true., &
           f_flwup     = .true., f_flwup_ai   = .true., &
           f_evap      = .true., f_evap_ai    = .true., &
           f_Tair      = .true., &
           f_Tref      = .true., f_Qref       = .true., &
           f_congel    = .true., f_frazil     = .true., &
           f_snoice    = .true., f_meltt      = .true., &
           f_meltb     = .true., f_meltl      = .true., &
           f_fresh     = .true., f_fresh_ai   = .true., &
           f_fsalt     = .true., f_fsalt_ai   = .true., &
           f_fhocn     = .true., f_fhocn_ai   = .true., &
           f_fswthru   = .true., f_fswthru_ai = .true., &
           f_strairx   = .true., f_strairy    = .true., &
           f_strtltx   = .true., f_strtlty    = .true., &
           f_strcorx   = .true., f_strcory    = .true., &
           f_strocnx   = .true., f_strocny    = .true., &
           f_strintx   = .true., f_strinty    = .true., &
           f_strength  = .true., f_opening    = .true., &
           f_divu      = .true., f_shear      = .true., &
           f_sig1      = .true., f_sig2       = .true., &
           f_dvidtt    = .true., f_dvidtd     = .true., &
           f_daidtt    = .true., f_daidtd     = .true., &
           f_mlt_onset = .true., f_frz_onset  = .true., &
           f_dardg1dt  = .true., f_dardg2dt   = .true., &
           f_dvirdgdt  = .true., f_iage       = .false.,&
           f_FY        = .false.,                       &
           f_aeron     = .false., f_aero      = .false.,&
           f_hisnap    = .true., f_aisnap     = .true., &
           f_aicen     = .true., f_vicen      = .true., &
           f_apond     = .false.,f_apondn     = .false.,&
           f_trsig     = .true., f_icepresent = .true., &
           f_fsurf_ai  = .true., f_fcondtop_ai= .true., &
           f_fmeltt_ai = .true.,                        &
           f_fsurfn_ai = .true.,f_fcondtopn_ai= .true., &
           f_fmelttn_ai= .true., f_flatn_ai   = .true.

      !---------------------------------------------------------------
      ! namelist variables (same as logical flags)
      !---------------------------------------------------------------

      namelist / icefields_nml /     &
           f_tmask    , &
           f_tarea    , f_uarea    , &
           f_dxt      , f_dyt      , &
           f_dxu      , f_dyu      , &
           f_HTN      , f_HTE      , &
           f_ANGLE    , f_ANGLET   , &
           f_bounds   , &
           f_hi,        f_hs       , &
           f_Tsfc,      f_aice     , &
           f_uvel,      f_vvel     , &
           f_fswdn,     f_flwdn    , &
           f_snow,      f_snow_ai  , &     
           f_rain,      f_rain_ai  , &
           f_faero,     f_fsoot    , &
           f_sst,       f_sss      , &
           f_uocn,      f_vocn     , &
           f_frzmlt                , &
           f_fswfac                , &
           f_fswabs,    f_fswabs_ai, &
           f_albsni                , &
           f_alvdr,     f_alidr    , &
           f_flat,      f_flat_ai  , &
           f_fsens,     f_fsens_ai , &
           f_flwup,     f_flwup_ai , &
           f_evap,      f_evap_ai  , &
           f_Tair                  , &
           f_Tref,      f_Qref     , &
           f_congel,    f_frazil   , &
           f_snoice,    f_meltt    , &
           f_meltb,     f_meltl    , &
           f_fresh,     f_fresh_ai , &  
           f_fsalt,     f_fsalt_ai , &
           f_fhocn,     f_fhocn_ai , &
           f_fswthru,   f_fswthru_ai,&
           f_strairx,   f_strairy  , &
           f_strtltx,   f_strtlty  , &
           f_strcorx,   f_strcory  , &
           f_strocnx,   f_strocny  , &
           f_strintx,   f_strinty  , &
           f_strength,  f_opening  , &
           f_divu,      f_shear    , &
           f_sig1,      f_sig2     , &
           f_dvidtt,    f_dvidtd   , &
           f_daidtt,    f_daidtd   , &
           f_mlt_onset, f_frz_onset, &
           f_dardg1dt,  f_dardg2dt , &
           f_dvirdgdt              , &
           f_hisnap,    f_aisnap   , &
           f_aicen,     f_vicen    , &
           f_aeron,     f_aero     , &
           f_iage,      f_apond    , &
           f_FY, &
           f_apondn   , &
           f_trsig,     f_icepresent,&
           f_fsurf_ai,  f_fcondtop_ai,&
           f_fmeltt_ai,              &
           f_fsurfn_ai,f_fcondtopn_ai,&
           f_fmelttn_ai,f_flatn_ai

      !---------------------------------------------------------------
      ! field indices
      !---------------------------------------------------------------

       integer (kind=int_kind), parameter :: &
           n_tmask      = 1,  &
           n_tarea      = 2,  &
           n_uarea      = 3,  &
           n_dxt        = 4,  &
           n_dyt        = 5,  &
           n_dxu        = 6,  &
           n_dyu        = 7,  &
           n_HTN        = 8,  &
           n_HTE        = 9,  &
           n_ANGLE      = 10, &
           n_ANGLET     = 11, &

           n_lont_bnds  = 1, &
           n_latt_bnds  = 2, &
           n_lonu_bnds  = 3, &
           n_latu_bnds  = 4

       integer (kind=int_kind) :: &
           n_hi         , &
           n_hs         , &
           n_Tsfc       , &
           n_aice       , &
           n_uvel       , &
           n_vvel       , &
           n_fswdn      , &
           n_flwdn      , &
           n_snow       , &
           n_snow_ai    , &
           n_rain       , &
           n_rain_ai    , &
           n_sst        , &
           n_sss        , &
           n_uocn       , &
           n_vocn       , &
           n_frzmlt     , &
           n_fswabs     , &
           n_fswabs_ai  , &
           n_albsni     , &
           n_flat       , &
           n_flat_ai    , &
           n_fsens      , &
           n_fsens_ai   , &
           n_flwup      , &
           n_flwup_ai   , &
           n_evap       , &
           n_evap_ai    , &
           n_Tref       , &
           n_Qref       , &
           n_congel     , &
           n_frazil     , &
           n_snoice     , &
           n_meltt      , &
           n_meltb      , &
           n_meltl      , &
           n_fresh      , &
           n_fresh_ai   , &
           n_fsalt      , &
           n_fsalt_ai   , &
           n_fhocn      , &
           n_fhocn_ai   , &
           n_fswthru    , &
           n_fswthru_ai , &
           n_strairx    , &
           n_strairy    , &
           n_strtltx    , &
           n_strtlty    , &
           n_strcorx    , &
           n_strcory    , &
           n_strocnx    , &
           n_strocny    , &
           n_strintx    , &
           n_strinty    , &
           n_strength   , &
           n_divu       , &
           n_shear      , &
           n_sig1       , &
           n_sig2       , &
           n_dvidtt     , &
           n_dvidtd     , &
           n_daidtt     , &
           n_daidtd     , &
           n_mlt_onset  , &
           n_frz_onset  , &
           n_opening    , &
           n_alvdr      , &
           n_alidr      , &
           n_dardg1dt   , &
           n_dardg2dt   , &
           n_dvirdgdt   , &
           n_hisnap     , &
           n_aisnap     , &
           n_Tair       , &
           n_trsig      , &
           n_icepresent , &
           n_iage       , &
           n_FY         , &
           n_apond      , &
           n_fsurf_ai   , &
           n_fcondtop_ai, &
           n_fmeltt_ai  , &
           n_fswfac

      ! Category dependent variables
      integer(kind=int_kind) :: &
           n_aicen(ncat_hist)       , &
           n_vicen(ncat_hist)       , &
           n_apondn(ncat_hist)      , &
           n_fsurfn_ai(ncat_hist)   , &
           n_fcondtopn_ai(ncat_hist), &
           n_fmelttn_ai(ncat_hist)  , &
           n_flatn_ai(ncat_hist)    , &
           n_faero(n_aero)          , &
           n_fsoot(n_aero)          , &
           n_aerosn1(n_aero)        , &
           n_aerosn2(n_aero)        , &
           n_aeroic1(n_aero)        , &
           n_aeroic2(n_aero)        , &
           n_aerosn1n(n_aero*ncat_hist), &
           n_aerosn2n(n_aero*ncat_hist), &
           n_aeroic1n(n_aero*ncat_hist), &
           n_aeroic2n(n_aero*ncat_hist)

!=======================================================================

      contains

!=======================================================================
!
!BOP
!
! !IROUTINE: init_hist - initialize history files
!
! !INTERFACE:
!
      subroutine init_hist (dt)
!
! !DESCRIPTION:
!
! Initialize history files
!
! !REVISION HISTORY:
!
! authors Tony Craig, NCAR
!         Elizabeth C. Hunke, LANL
!         C.M. Bitz, UW
!         Bruce P. Briegleb, NCAR
!         William H. Lipscomb, LANL
!
! !USES:
!
      use ice_constants
      use ice_calendar, only: yday, days_per_year
      use ice_flux, only: mlt_onset, frz_onset
      use ice_restart, only: restart
      use ice_age, only: tr_iage
      use ice_FY, only: tr_FY
      use ice_aerosol, only: tr_aero
      use ice_meltpond, only: tr_pond
      use ice_exit
!
! !INPUT/OUTPUT PARAMETERS:
!
      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step
!
!EOP
!
      integer (kind=int_kind) :: n, k
      integer (kind=int_kind) :: nml_error ! namelist i/o error flag

      character (len=3) :: nchar
      character (len=40) :: tmp

      !-----------------------------------------------------------------
      ! read namelist
      !-----------------------------------------------------------------

      call get_fileunit(nu_nml)
      if (my_task == master_task) then
         open (nu_nml, file=nml_filename, status='old',iostat=nml_error)
         if (nml_error /= 0) then
            nml_error = -1
         else
            nml_error =  1
         endif
         do while (nml_error > 0)
            read(nu_nml, nml=icefields_nml,iostat=nml_error)
            if (nml_error > 0) read(nu_nml,*)  ! for Nagware compiler
         end do
         if (nml_error == 0) close(nu_nml)
      endif
      call release_fileunit(nu_nml)

      call broadcast_scalar(nml_error, master_task)
      if (nml_error /= 0) then
         close (nu_nml)
         call abort_ice('ice: error reading icefields_nml')
      endif

      if (.not. tr_iage) f_iage   = .false.
      if (.not. tr_FY)   f_FY     = .false.
      if (.not. tr_pond) f_apond  = .false.
      if (.not. tr_pond) f_apondn = .false.
      if (.not. tr_aero) f_faero = .false.
      if (.not. tr_aero) f_fsoot = .false.
      if (.not. tr_aero) f_aero = .false. 
      if (.not. tr_aero) f_aeron = .false.

#ifndef ncdf
      f_bounds = .false.
#endif

      call broadcast_scalar (f_tmask, master_task)
      call broadcast_scalar (f_tarea, master_task)
      call broadcast_scalar (f_uarea, master_task)
      call broadcast_scalar (f_dxt, master_task)
      call broadcast_scalar (f_dyt, master_task)
      call broadcast_scalar (f_dxu, master_task)
      call broadcast_scalar (f_dyu, master_task)
      call broadcast_scalar (f_HTN, master_task)
      call broadcast_scalar (f_HTE, master_task)
      call broadcast_scalar (f_ANGLE, master_task)
      call broadcast_scalar (f_ANGLET, master_task)
      call broadcast_scalar (f_bounds, master_task)

      call broadcast_scalar (f_hi, master_task)
      call broadcast_scalar (f_hs, master_task)
      call broadcast_scalar (f_Tsfc, master_task)
      call broadcast_scalar (f_aice, master_task)
      call broadcast_scalar (f_uvel, master_task)
      call broadcast_scalar (f_vvel, master_task)
      call broadcast_scalar (f_fswdn, master_task)
      call broadcast_scalar (f_flwdn, master_task)
      call broadcast_scalar (f_snow, master_task)
      call broadcast_scalar (f_snow_ai, master_task)
      call broadcast_scalar (f_rain, master_task)
      call broadcast_scalar (f_rain_ai, master_task)
      call broadcast_scalar (f_faero, master_task)
      call broadcast_scalar (f_fsoot, master_task)
      call broadcast_scalar (f_sst, master_task)
      call broadcast_scalar (f_sss, master_task)
      call broadcast_scalar (f_uocn, master_task)
      call broadcast_scalar (f_vocn, master_task)
      call broadcast_scalar (f_frzmlt, master_task)
      call broadcast_scalar (f_fswfac, master_task)
      call broadcast_scalar (f_fswabs, master_task)
      call broadcast_scalar (f_fswabs_ai, master_task)
      call broadcast_scalar (f_albsni, master_task)
      call broadcast_scalar (f_alvdr, master_task)
      call broadcast_scalar (f_alidr, master_task)
      call broadcast_scalar (f_flat, master_task)
      call broadcast_scalar (f_flat_ai, master_task)
      call broadcast_scalar (f_fsens, master_task)
      call broadcast_scalar (f_fsens_ai, master_task)
      call broadcast_scalar (f_flwup, master_task)
      call broadcast_scalar (f_flwup_ai, master_task)
      call broadcast_scalar (f_evap, master_task)
      call broadcast_scalar (f_evap_ai, master_task)
      call broadcast_scalar (f_Tair, master_task)
      call broadcast_scalar (f_Tref, master_task)
      call broadcast_scalar (f_Qref, master_task)
      call broadcast_scalar (f_congel, master_task)
      call broadcast_scalar (f_frazil, master_task)
      call broadcast_scalar (f_snoice, master_task)
      call broadcast_scalar (f_meltt, master_task)
      call broadcast_scalar (f_meltb, master_task)
      call broadcast_scalar (f_meltl, master_task)
      call broadcast_scalar (f_fresh, master_task)
      call broadcast_scalar (f_fresh_ai, master_task)
      call broadcast_scalar (f_fsalt, master_task)
      call broadcast_scalar (f_fsalt_ai, master_task)
      call broadcast_scalar (f_fhocn, master_task)
      call broadcast_scalar (f_fhocn_ai, master_task)
      call broadcast_scalar (f_fswthru, master_task)
      call broadcast_scalar (f_fswthru_ai, master_task)
      call broadcast_scalar (f_strairx, master_task)
      call broadcast_scalar (f_strairy, master_task)
      call broadcast_scalar (f_strtltx, master_task)
      call broadcast_scalar (f_strtlty, master_task)
      call broadcast_scalar (f_strcorx, master_task)
      call broadcast_scalar (f_strcory, master_task)
      call broadcast_scalar (f_strocnx, master_task)
      call broadcast_scalar (f_strocny, master_task)
      call broadcast_scalar (f_strintx, master_task)
      call broadcast_scalar (f_strinty, master_task)
      call broadcast_scalar (f_strength, master_task)
      call broadcast_scalar (f_opening, master_task)
      call broadcast_scalar (f_divu, master_task)
      call broadcast_scalar (f_shear, master_task)
      call broadcast_scalar (f_sig1, master_task)
      call broadcast_scalar (f_sig2, master_task)
      call broadcast_scalar (f_dvidtt, master_task)
      call broadcast_scalar (f_dvidtd, master_task)
      call broadcast_scalar (f_daidtt, master_task)
      call broadcast_scalar (f_daidtd, master_task)
      call broadcast_scalar (f_mlt_onset, master_task)
      call broadcast_scalar (f_frz_onset, master_task)
      call broadcast_scalar (f_dardg1dt, master_task)
      call broadcast_scalar (f_dardg2dt, master_task)
      call broadcast_scalar (f_dvirdgdt, master_task)
      call broadcast_scalar (f_aisnap, master_task)
      call broadcast_scalar (f_hisnap, master_task)
      call broadcast_scalar (f_aicen, master_task)
      call broadcast_scalar (f_vicen, master_task)
      call broadcast_scalar (f_trsig, master_task)
      call broadcast_scalar (f_icepresent, master_task)
      call broadcast_scalar (f_fsurf_ai, master_task)
      call broadcast_scalar (f_fcondtop_ai, master_task)
      call broadcast_scalar (f_fmeltt_ai, master_task)
      call broadcast_scalar (f_fsurfn_ai, master_task)
      call broadcast_scalar (f_fcondtopn_ai, master_task)
      call broadcast_scalar (f_fmelttn_ai, master_task)
      call broadcast_scalar (f_flatn_ai, master_task)

      call broadcast_scalar (f_aero, master_task)
      call broadcast_scalar (f_aeron, master_task)
      call broadcast_scalar (f_iage, master_task)
      call broadcast_scalar (f_FY, master_task)
      call broadcast_scalar (f_apond, master_task)
      call broadcast_scalar (f_apondn, master_task)

      if (f_hi) then
         call define_hist_field(n_hi,"hi","m",tstr, tcstr,         & 
             "grid cell mean ice thickness",                       &
             "ice volume per unit grid cell area", c1, c0)
      endif
      if (f_hs) then
         call define_hist_field(n_hs,"hs","m",tstr, tcstr,         &
             "grid cell mean snow thickness",                      &
             "snow volume per unit grid cell area", c1, c0)
      endif
      if (f_Tsfc) then
         call define_hist_field(n_Tsfc,"Tsfc","degC",tstr, tcstr,&
             "snow/ice surface temperature",                       &
             "averaged with Tf if no ice is present", c1, c0)
      endif
      if (f_aice) then
         call define_hist_field(n_aice,"aice","%",tstr, tcstr,   &
             "ice area  (aggregate)",                              &
             "none", c100, c0)
      endif
      if (f_uvel) then
         call define_hist_field(n_uvel,"uvel","cm/s",ustr, ucstr,   &
             "ice velocity (x)",                              &
             "positive is x direction on U grid", m_to_cm, c0)
      endif
      if (f_vvel) then
         call define_hist_field(n_vvel,"vvel","cm/s",ustr, ucstr,   &
             "ice velocity (y)",                              &
             "positive is y direction on U grid", m_to_cm, c0)
      endif
      if (f_fswdn) then
         call define_hist_field(n_fswdn,"fswdn","W/m^2",tstr, tcstr,   &
             "down solar flux",                              &
             "positive downward", c1, c0)
      endif
      if (f_flwdn) then
         call define_hist_field(n_flwdn,"flwdn","W/m^2",tstr, tcstr,   &
             "down longwave flux",                              &
             "positive downward", c1, c0)
      endif
      if (f_snow) then
         call define_hist_field(n_snow,"snow","cm/day",tstr, tcstr,   &
             "snowfall rate (cpl)",                              &
             "none", mps_to_cmpdy/rhofresh, c0)

      endif
      if (f_snow_ai) then
         call define_hist_field(n_snow_ai,"snow_ai","cm/day",tstr, tcstr,   &
             "snowfall rate",                              &
             "weighted by ice", mps_to_cmpdy/rhofresh, c0)
      endif
      if (f_rain) then
         call define_hist_field(n_rain,"rain","cm/day",tstr, tcstr,   &
             "rainfall rate (cpl)",                              &
             "none", mps_to_cmpdy/rhofresh, c0)
      endif
      if (f_rain_ai) then
         call define_hist_field(n_rain_ai,"rain_ai","cm/day",tstr, tcstr,   &
             "rainfall rate",                              &
             "weighted by ice", mps_to_cmpdy/rhofresh, c0)
      endif
      if (f_sst) then
         call define_hist_field(n_sst,"sst","C",tstr, tcstr,   &
             "sea surface temperature",                              &
             "none", c1, c0)
      endif
      if (f_sss) then
         call define_hist_field(n_sss,"sss","psu",tstr, tcstr,   &
             "sea surface salinity",                              &
             "none", c1, c0)
      endif
      if (f_uocn) then
         call define_hist_field(n_uocn,"uocn","cm/s",ustr, ucstr,   &
             "ocean current (x)",                              &
             "positive is x direction on U grid", m_to_cm, c0)
      endif
      if (f_vocn) then
         call define_hist_field(n_vocn,"vocn","cm/s",ustr, ucstr,   &
             "ocean current (y)",                              &
             "positive is y direction on U grid", m_to_cm, c0)
      endif
      if (f_frzmlt) then
         call define_hist_field(n_frzmlt,"frzmlt","W/m^2",tstr, tcstr,   &
             "freeze/melt potential",                              &
             "if >0, new ice forms; if <0, ice melts", c1, c0)
      endif
      if (f_fswabs) then
         call define_hist_field(n_fswabs,"fswabs","W/m^2",tstr, tcstr,   &
             "snow/ice/ocn absorbed solar flux (cpl)",                   &
             "positive downward", c1, c0)
      endif
      if (f_fswabs_ai) then
         call define_hist_field(n_fswabs_ai,"fswabs_ai","W/m^2",tstr, tcstr, &
             "snow/ice/ocn absorbed solar flux",                   &
             "weighted by ice area", c1, c0)
      endif
      if (f_albsni) then
         call define_hist_field(n_albsni,"albsni","%",tstr, tcstr,   &
             "snw/ice broad band albedo",                   &
             "none", c100, c0)
      endif
      if (f_flat) then
         call define_hist_field(n_flat,"flat","W/m^2",tstr, tcstr,   &
             "latent heat flux (cpl)",                   &
             "positive downward", c1, c0)
      endif
      if (f_flat_ai) then
         call define_hist_field(n_flat_ai,"flat_ai","W/m^2",tstr, tcstr, &
             "latent heat flux",                   &
             "weighted by ice area", c1, c0)
      endif
      if (f_fsens) then
         call define_hist_field(n_fsens,"fsens","W/m^2",tstr, tcstr,   &
             "sensible heat flux (cpl)",                   &
             "positive downward", c1, c0)
      endif
      if (f_fsens_ai) then
         call define_hist_field(n_fsens_ai,"fsens_ai","W/m^2",tstr, tcstr, &
             "sensible heat flux",                   &
             "weighted by ice area", c1, c0)
      endif
      if (f_flwup) then
         call define_hist_field(n_flwup,"flwup","W/m^2",tstr, tcstr,   &
             "upward longwave flux (cpl)",                   &
             "positive downward", c1, c0)
      endif
      if (f_flwup_ai) then
         call define_hist_field(n_flwup_ai,"flwup_ai","W/m^2",tstr, tcstr, &
             "upward longwave flux",                   &
             "weighted by ice area", c1, c0)
      endif
      if (f_evap) then
         call define_hist_field(n_evap,"evap","cm/day",tstr, tcstr,   &
             "evaporative water flux (cpl)",                   &
             "none", mps_to_cmpdy/rhofresh, c0)
      endif
      if (f_evap_ai) then
         call define_hist_field(n_evap_ai,"evap_ai","cm/day",tstr, tcstr, &
             "evaporative water flux",                   &
             "weighted by ice area", mps_to_cmpdy/rhofresh, c0)
      endif
      if (f_Tref) then
         call define_hist_field(n_Tref,"Tref","C",tstr, tcstr,   &
             "2m reference temperature",                   &
             "none", c1, c0)
      endif
      if (f_Qref) then
         call define_hist_field(n_Qref,"Qref","g/kg",tstr, tcstr,   &
             "2m reference specific humidity",                   &
             "none", kg_to_g, c0)
      endif
      if (f_congel) then
         call define_hist_field(n_congel,"congel","cm/day",tstr, tcstr,   &
             "congelation ice growth",                   &
             "none", mps_to_cmpdy/dt, c0)
      endif
      if (f_frazil) then
         call define_hist_field(n_frazil,"frazil","cm/day",tstr, tcstr,   &
             "frazil ice growth",                   &
             "none", mps_to_cmpdy/dt, c0)
      endif
      if (f_snoice) then
         call define_hist_field(n_snoice,"snoice","cm/day",tstr, tcstr,   &
             "snow-ice formation",                   &
             "none", mps_to_cmpdy/dt, c0)
      endif
      if (f_meltt) then
         call define_hist_field(n_meltt,"meltt","cm/day",tstr, tcstr,   &
             "top ice melt",                   &
             "none", mps_to_cmpdy/dt, c0)
      endif
      if (f_meltb) then
         call define_hist_field(n_meltb,"meltb","cm/day",tstr, tcstr,   &
             "basal ice melt",                   &
             "none", mps_to_cmpdy/dt, c0)
      endif
      if (f_meltl) then
         call define_hist_field(n_meltl,"meltl","cm/day",tstr, tcstr,   &
             "lateral ice melt",                   &
             "none", mps_to_cmpdy/dt, c0)
      endif
      if (f_fresh) then
         call define_hist_field(n_fresh,"fresh","cm/day",tstr, tcstr,   &
             "freshwtr flx ice to ocn (cpl)",                   &
             "if positive, ocean gains fresh water", mps_to_cmpdy/rhofresh, c0)
      endif
      if (f_fresh_ai) then
         call define_hist_field(n_fresh_ai,"fresh_ai","cm/day",tstr, tcstr, &
             "freshwtr flx ice to ocn",                   &
             "weighted by ice area", mps_to_cmpdy/rhofresh, c0)
      endif
      if (f_fsalt) then
         call define_hist_field(n_fsalt,"fsalt","kg/m^2/day",tstr, tcstr,   &
             "salt flux ice to ocn (cpl)",                   &
             "if positive, ocean gains salt", secday, c0)
      endif
      if (f_fsalt_ai) then
         call define_hist_field(n_fsalt_ai,"fsalt_ai","kg/m^2/day",tstr, tcstr,&
             "salt flux ice to ocean",                   &
             "weighted by ice area", secday, c0)
      endif
      if (f_fhocn) then
         call define_hist_field(n_fhocn,"fhocn","W/m^2",tstr, tcstr,   &
             "heat flux ice to ocn (cpl)",                   &
             "if positive, ocean gains heat", c1, c0)
      endif
      if (f_fhocn_ai) then
         call define_hist_field(n_fhocn_ai,"fhocn_ai","W/m^2",tstr, tcstr,&
             "heat flux ice to ocean",                   &
             "weighted by ice area", c1, c0)
      endif
      if (f_fswthru) then
         call define_hist_field(n_fswthru,"fswthru","W/m^2",tstr, tcstr,   &
             "SW thru ice to ocean (cpl)",                   &
             "if positive, ocean gains heat", c1, c0)
      endif
      if (f_fswthru_ai) then
         call define_hist_field(n_fswthru_ai,"fswthru_ai","W/m^2",tstr, tcstr,&
             "SW flux thru ice to ocean",                   &
             "weighted by ice area", c1, c0)
      endif
      if (f_strairx) then
         call define_hist_field(n_strairx,"strairx","N/m^2",ustr, ucstr,   &
             "atm/ice stress (x)",                   &
             "positive is x direction on U grid", c1, c0)
      endif
      if (f_strairy) then
         call define_hist_field(n_strairy,"strairy","N/m^2",ustr, ucstr,   &
             "atm/ice stress (y)",                   &
             "positive is y direction on U grid", c1, c0)
      endif
      if (f_strtltx) then
         call define_hist_field(n_strtltx,"strtltx","N/m^2",ustr, ucstr,   &
             "sea sfc tilt stress (x)",                   &
             "positive is x direction on U grid", c1, c0)
      endif
      if (f_strtlty) then
         call define_hist_field(n_strtlty,"strtlty","N/m^2",ustr, ucstr,   &
             "sea sfc tilt stress (y)",                   &
             "positive is y direction on U grid", c1, c0)
      endif
      if (f_strcorx) then
         call define_hist_field(n_strcorx,"strcorx","N/m^2",ustr, ucstr,   &
             "coriolis stress (x)",                   &
             "positive is x direction on U grid", c1, c0)
      endif
      if (f_strcory) then
         call define_hist_field(n_strcory,"strcory","N/m^2",ustr, ucstr,   &
             "coriolis stress (y)",                   &
             "positive is y direction on U grid", c1, c0)
      endif
      if (f_strocnx) then
         call define_hist_field(n_strocnx,"strocnx","N/m^2",ustr, ucstr,   &
             "ocean/ice stress (x)",                   &
             "positive is x direction on U grid", c1, c0)
      endif
      if (f_strocny) then
         call define_hist_field(n_strocny,"strocny","N/m^2",ustr, ucstr,   &
             "ocean/ice stress (y)",                   &
             "positive is y direction on U grid", c1, c0)
      endif
      if (f_strintx) then
         call define_hist_field(n_strintx,"strintx","N/m^2",ustr, ucstr,   &
             "internal ice stress (x)",                   &
             "positive is x direction on U grid", c1, c0)
      endif
      if (f_strinty) then
         call define_hist_field(n_strinty,"strinty","N/m^2",ustr, ucstr,   &
             "internal ice stress (y)",                   &
             "positive is y direction on U grid", c1, c0)
      endif
      if (f_strength) then
         call define_hist_field(n_strength,"strength","N/m",tstr, tcstr,   &
             "compressive ice strength",                   &
             "none", c1, c0)
      endif
      if (f_divu) then
         call define_hist_field(n_divu,"divu","%/day",tstr, tcstr,   &
             "strain rate (divergence)",                   &
             "none", secday*c100, c0)
      endif
      if (f_shear) then
         call define_hist_field(n_shear,"shear","%/day",tstr, tcstr,   &
             "strain rate (shear)",                   &
             "none", secday*c100, c0)
      endif
      if (f_sig1) then
         call define_hist_field(n_sig1,"sig1"," ",ustr, ucstr,   &
             "norm. principal stress 1",                   &
             "sig1 is instantaneous", c1, c0)
      endif
      if (f_sig2) then
         call define_hist_field(n_sig2,"sig2"," ",ustr, ucstr,   &
             "norm. principal stress 2",                   &
             "sig2 is instantaneous", c1, c0)
      endif
      if (f_dvidtt) then
         call define_hist_field(n_dvidtt,"dvidtt","cm/day",tstr, tcstr,   &
             "volume tendency thermo",                   &
             "none", mps_to_cmpdy, c0)
      endif
      if (f_dvidtd) then
         call define_hist_field(n_dvidtd,"dvidtd","cm/day",tstr, tcstr,   &
             "volume tendency dynamics",                   &
             "none", mps_to_cmpdy, c0)
      endif
      if (f_daidtt) then
         call define_hist_field(n_daidtt,"daidtt","%/day",tstr, tcstr,   &
             "area tendency thermo",                   &
             "none", secday*c100, c0)
      endif
      if (f_daidtd) then
         call define_hist_field(n_daidtd,"daidtd","%/day",tstr, tcstr,   &
             "area tendency dynamics",                   &
             "none", secday*c100, c0)
      endif
      if (f_mlt_onset) then
         call define_hist_field(n_mlt_onset,"mlt_onset","day of year",tstr, tcstr,   &
             "melt onset date",                   &
             "midyear restart gives erroneous dates", c1, c0)
      endif
      if (f_frz_onset) then
         call define_hist_field(n_frz_onset,"frz_onset","day of year",tstr, tcstr,   &
             "freeze onset date",                   &
             "midyear restart gives erroneous dates", c1, c0)
      endif
      if (f_opening) then
         call define_hist_field(n_opening,"opening","%/day",tstr, tcstr,   &
             "lead area opening rate",                   &
             "none", secday*c100, c0)
      endif
      if (f_alvdr) then
         call define_hist_field(n_alvdr,"alvdr","%",tstr, tcstr,   &
             "visible direct albedo",                   &
             "none", c100, c0)
      endif
      if (f_alidr) then
         call define_hist_field(n_alidr,"alidr","%",tstr, tcstr,   &
             "near IR direct albedo",                   &
             "none", c100, c0)
      endif
      if (f_dardg1dt) then
         call define_hist_field(n_dardg1dt,"dardg1dt","%/day",tstr, tcstr,   &
             "ice area ridging rate",                   &
             "none", secday*c100, c0)
      endif
      if (f_dardg2dt) then
         call define_hist_field(n_dardg2dt,"dardg2dt","%/day",tstr, tcstr,   &
             "ridge area formation rate",                   &
             "none", secday*c100, c0)
      endif
      if (f_dvirdgdt) then
         call define_hist_field(n_dvirdgdt,"dvirdgdt","cm/day",tstr, tcstr,   &
             "ice volume ridging rate",                   &
             "none", mps_to_cmpdy, c0)
      endif
      if (f_hisnap) then
         call define_hist_field(n_hisnap,"hisnap","m",tstr, tcstr,   &
             "ice volume snapshot",                   &
             "none", c1, c0)
      endif
      if (f_aisnap) then
         call define_hist_field(n_aisnap,"aisnap"," ",tstr, tcstr,   &
             "ice area snapshot",                   &
             "none", c1, c0)
      endif
      if (f_Tair) then
         call define_hist_field(n_Tair,"Tair","C",tstr, tcstr,   &
             "air temperature",                   &
             "none", c1, -tffresh)
      endif
      if (f_trsig) then
         call define_hist_field(n_trsig,"trsig","N/m^2",tstr, tcstr,   &
             "internal stress tensor trace",                   &
             "ice strength approximation", c1, c0)
      endif
      if (f_icepresent) then
         call define_hist_field(n_icepresent,"ice_present","1",tstr, tcstr,   &
             "fraction of time-avg interval that any ice is present",         &
             "ice extent flag", c1, c0)
      endif
      if (f_fsurf_ai) then
         call define_hist_field(n_fsurf_ai,"fsurf_ai","W/m^2",tstr, tcstr, &
             "net surface heat flux",                   &
             "positive downwards, excludes conductive flux, weighted by ice area", c1, c0)
      endif
      if (f_fcondtop_ai) then
         call define_hist_field(n_fcondtop_ai,"fcondtop_ai","W/m^2",tstr, tcstr, &
             "top surface conductive heat flux",                   &
             "positive downwards, weighted by ice area", c1, c0)
      endif
      if (f_fmeltt_ai) then
         call define_hist_field(n_fmeltt_ai,"fmeltt_ai","W/m^2",tstr, tcstr, &
             "net surface heat flux causing melt",                   &
             "always >= 0, weighted by ice area", c1, c0)
      endif
      if (f_fswfac) then
         call define_hist_field(n_fswfac,"fswfac","1",tstr, tcstr, &
             "shortwave scaling factor",                   &
             "ratio of netsw new:old", c1, c0)
      endif

      ! Category variables
      if (f_aicen) then
         do n=1,ncat_hist
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'aice', trim(nchar) ! aicen
            tmp = 'ice area, category '   ! aicen
            write(vdesc_in,'(a,2x,a)') trim(tmp), trim(nchar)
            call define_hist_field(n_aicen(n),vname_in,"%",tstr, tcstr, &
                vdesc_in, "Ice range:", c100, c0)
         enddo
      endif
      if (f_vicen) then
         do n=1,ncat_hist
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'vice', trim(nchar) ! vicen
            tmp = 'ice volume, category '   ! vicen
            write(vdesc_in,'(a,2x,a)') trim(tmp), trim(nchar)
            call define_hist_field(n_vicen(n),vname_in,"m",tstr, tcstr, &
                vdesc_in, "none", c1, c0)
         enddo
      endif
      if (f_fsurfn_ai) then
         do n=1,ncat_hist
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'fsurfn_ai', trim(nchar) ! fsurfn_ai
            tmp = 'net surface heat flux, category '   ! fsurfn_ai
            write(vdesc_in,'(a,2x,a)') trim(tmp), trim(nchar)
            call define_hist_field(n_fsurfn_ai(n),vname_in,"W/m^2",tstr, tcstr, &
                vdesc_in, "weighted by ice area", c1, c0)
         enddo
      endif
      if (f_fcondtopn_ai) then
         do n=1,ncat_hist
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'fcondtopn_ai', trim(nchar) ! fcondtopn_ai
            tmp = 'top sfc conductive heat flux, cat '   ! fcondtopn_ai
            write(vdesc_in,'(a,2x,a)') trim(tmp), trim(nchar)
            call define_hist_field(n_fcondtopn_ai(n),vname_in,"W/m^2",tstr, tcstr, &
                vdesc_in, "weighted by ice area", c1, c0)
         enddo
      endif
      if (f_fmelttn_ai) then
         do n=1,ncat_hist
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'fmelttn_ai', trim(nchar) ! fmelttn_ai
            tmp = 'net sfc heat flux causing melt, cat '   ! fmelttn_ai
            write(vdesc_in,'(a,2x,a)') trim(tmp), trim(nchar)
            call define_hist_field(n_fmelttn_ai(n),vname_in,"W/m^2",tstr, tcstr, &
                vdesc_in, "weighted by ice area", c1, c0)
         enddo
      endif
      if (f_flatn_ai) then
         do n=1,ncat_hist
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'flatn_ai', trim(nchar) ! flatn_ai
            tmp = 'latent heat flux, category '   ! flatn_ai
            write(vdesc_in,'(a,2x,a)') trim(tmp), trim(nchar)
            call define_hist_field(n_flatn_ai(n),vname_in,"W/m^2",tstr, tcstr, &
                vdesc_in, "weighted by ice area", c1, c0)
         enddo
      endif

      ! Tracers

      ! Ice Age
      if (f_iage) then
         call define_hist_field(n_iage,"iage","years",tstr, tcstr, &
             "sea ice age",                   &
             "none", c1/(secday*days_per_year), c0)
      endif

      ! FY Ice Concentration
      if (f_FY) then
         call define_hist_field(n_FY,"FYarea"," ",tstr, tcstr, &
             "first-year ice area",                   &
             "weighted by ice area", c1, c0)
      endif

      ! Aerosols
      if (f_aero) then
         do n=1,n_aero
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'aerosnossl', trim(nchar)
            call define_hist_field(n_aerosn1(n),vname_in,"kg/kg",tstr, tcstr, &
                "snow ssl aerosol mass",                   &
                "none", c1, c0)
            write(vname_in,'(a,a)') 'aerosnoint', trim(nchar)
            call define_hist_field(n_aerosn2(n),vname_in,"kg/kg",tstr, tcstr, &
                "snow int aerosol mass",                   &
                "none", c1, c0)
            write(vname_in,'(a,a)') 'aeroicessl', trim(nchar)
            call define_hist_field(n_aeroic1(n),vname_in,"kg/kg",tstr, tcstr, &
                "ice ssl aerosol mass",                   &
                "none", c1, c0)
            write(vname_in,'(a,a)') 'aeroiceint', trim(nchar)
            call define_hist_field(n_aeroic2(n),vname_in,"kg/kg",tstr, tcstr, &
                "ice int aerosol mass",                   &
                "none", c1, c0)
         enddo
      endif
      if (f_faero) then
         do n=1,n_aero
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'faero', trim(nchar)
            call define_hist_field(n_faero(n),vname_in,"kg/m^2 s",tstr, tcstr, &
                "aerosol deposition rate",                   &
                "none", c1, c0)
         enddo
      endif
      if (f_fsoot) then
         do n=1,n_aero
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'fsoot', trim(nchar)
            call define_hist_field(n_fsoot(n),vname_in,"kg/m^2 s",tstr, tcstr, &
                "aerosol flux to ocean",                   &
                "none", c1, c0)
         enddo
      endif

      ! Melt ponds
      if (f_apond) then
         call define_hist_field(n_apond,"apond","%",tstr, tcstr, &
             "melt pond concentration",                   &
             "none", c100, c0)
      endif
      if (f_apondn) then
         do n=1,ncat_hist
            write(nchar,'(i3.3)') n
            write(vname_in,'(a,a)') 'apond', trim(nchar) ! apondn
            tmp = 'melt pond concentration, category '   ! apondn
            write(vdesc_in,'(a,2x,a)') trim(tmp), trim(nchar)
            call define_hist_field(n_apondn(n),vname_in,"%",tstr, tcstr, &
                vdesc_in, "none", c100, c0)
         enddo
      endif

      allocate(aa(nx_block,ny_block,num_avail_hist_fields,max_blocks))

      !-----------------------------------------------------------------
      ! fill igrd array with namelist values
      !-----------------------------------------------------------------

      igrd=.true.

      igrd(n_tmask     ) = f_tmask
      igrd(n_tarea     ) = f_tarea
      igrd(n_uarea     ) = f_uarea
      igrd(n_dxt       ) = f_dxt
      igrd(n_dyt       ) = f_dyt
      igrd(n_dxu       ) = f_dxu
      igrd(n_dyu       ) = f_dyu
      igrd(n_HTN       ) = f_HTN
      igrd(n_HTE       ) = f_HTE
      igrd(n_ANGLE     ) = f_ANGLE
      igrd(n_ANGLET    ) = f_ANGLET

      if (my_task == master_task) then
        write(nu_diag,*) ' '
        write(nu_diag,*) 'The following variables will be ', &
                         'written to the history tape: '
        write(nu_diag,*) ' description                           units', &
             '     netcdf variable'
        do n=1,num_avail_hist_fields
           write(nu_diag,100) avail_hist_fields(n)%vdesc, &
               avail_hist_fields(n)%vunit, avail_hist_fields(n)%vname
        enddo
        write(nu_diag,*) ' '
      endif
  100 format (1x,a40,2x,a16,2x,a16)

      !-----------------------------------------------------------------
      ! initialize the history arrays
      !-----------------------------------------------------------------
      aa(:,:,:,:) = c0
      avgct = c0

      if (restart .and. yday >= c2) then
! restarting midyear gives erroneous onset dates
         mlt_onset = 999._dbl_kind 
         frz_onset = 999._dbl_kind 
      else
         mlt_onset = c0
         frz_onset = c0
      endif

      end subroutine init_hist

!=======================================================================
!
!BOP
!
! !IROUTINE: ice_write_hist - write average ice quantities or snapshots
!
! !INTERFACE:
!
      subroutine ice_write_hist (dt)
!
! !DESCRIPTION:
!
! write average ice quantities or snapshots
!
! !REVISION HISTORY:
!
! author:   Elizabeth C. Hunke, LANL
!
! !USES:
!
      use ice_blocks
      use ice_domain
      use ice_calendar, only: new_year, secday, yday, write_history, &
                              write_ic, time
      use ice_state
      use ice_constants
      use ice_flux
      use ice_dyn_evp
      use ice_timers
!
! !INPUT/OUTPUT PARAMETERS:
!
      real (kind=dbl_kind), intent(in) :: &
         dt      ! time step
!EOP
!
      integer (kind=int_kind) :: &
           i,j,k,n,nct      , &
           iblk             , & ! block index
           ilo,ihi,jlo,jhi      ! beginning and end of physical domain

      real (kind=dbl_kind) :: &
           ravgct               ! 1/avgct

      type (block) :: &
         this_block             ! block information for current block

      real (kind=dbl_kind) :: &
           worka(nx_block,ny_block), &
           workb(nx_block,ny_block)

      real (kind=dbl_kind) :: &
           ai                ,& ! aice_init
           ain                  ! aicen_init

      !---------------------------------------------------------------
      ! increment step counter
      !---------------------------------------------------------------

      if (.not. hist_avg) then  ! write snapshots
        aa(:,:,:,:) = c0
        avgct = c1
      else                      ! write averages over time histfreq
        avgct = avgct + c1
        if (avgct == c1) time_beg = (time-dt)/int(secday)
      endif

      !---------------------------------------------------------------
      ! increment field
      !---------------------------------------------------------------

     !$OMP PARALLEL DO PRIVATE(iblk, this_block, ilo, ihi, jlo, jhi)
      do iblk = 1, nblocks
         this_block = get_block(blocks_ice(iblk),iblk)
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

         workb(:,:) = aice_init(:,:,iblk)

         if (f_hi) call accum_hist_field(n_hi,   iblk, vice(:,:,iblk))
         if (f_hs) call accum_hist_field(n_hs,   iblk, vsno(:,:,iblk))
         if (f_Tsfc) call accum_hist_field(n_Tsfc, iblk, trcr(:,:,nt_Tsfc,iblk))
         if (f_aice) call accum_hist_field(n_aice, iblk, aice(:,:,iblk))
         if (f_uvel) call accum_hist_field(n_uvel, iblk, uvel(:,:,iblk))
         if (f_vvel) call accum_hist_field(n_vvel, iblk, vvel(:,:,iblk))

         if (f_fswdn) call accum_hist_field(n_fswdn, iblk, fsw(:,:,iblk))
         if (f_flwdn) call accum_hist_field(n_flwdn, iblk, flw(:,:,iblk))
         if (f_snow) call accum_hist_field(n_snow,  iblk, fsnow(:,:,iblk))
         if (f_snow_ai)  call accum_hist_field(n_snow_ai, iblk, &
             fsnow(:,:,iblk)*workb(:,:))
         if (f_rain) call accum_hist_field(n_rain,  iblk, frain(:,:,iblk))
         if (f_rain_ai) call accum_hist_field(n_rain_ai, iblk, &
             frain(:,:,iblk)*workb(:,:))

         if (f_sst) call accum_hist_field(n_sst,  iblk, sst(:,:,iblk))
         if (f_sss) call accum_hist_field(n_sss,  iblk, sss(:,:,iblk))
         if (f_uocn) call accum_hist_field(n_uocn, iblk, uocn(:,:,iblk))
         if (f_vocn) call accum_hist_field(n_vocn, iblk, vocn(:,:,iblk))
         if (f_frzmlt) call accum_hist_field(n_frzmlt,  iblk, frzmlt(:,:,iblk))

         if (f_fswfac) call accum_hist_field(n_fswfac,  iblk, fswfac(:,:,iblk))
         if (f_fswabs) call accum_hist_field(n_fswabs,  iblk, fswabs(:,:,iblk))
         if (f_fswabs_ai) call accum_hist_field(n_fswabs_ai, iblk, &
             fswabs(:,:,iblk)*workb(:,:))

         if (f_albsni) call accum_hist_field(n_albsni,  iblk, &
                                                awtvdr*alvdr(:,:,iblk) &
                                              + awtidr*alidr(:,:,iblk) &
                                              + awtvdf*alvdf(:,:,iblk) &
                                              + awtidf*alidf(:,:,iblk))

         if (f_alvdr) call accum_hist_field(n_alvdr,  iblk, alvdr(:,:,iblk))
         if (f_alidr) call accum_hist_field(n_alidr,  iblk, alidr(:,:,iblk))
         if (f_flat) call accum_hist_field(n_flat,   iblk, flat(:,:,iblk))
         if (f_flat_ai) call accum_hist_field(n_flat_ai,iblk, &
            flat(:,:,iblk)*workb(:,:))
         if (f_fsens) call accum_hist_field(n_fsens,   iblk, fsens(:,:,iblk))
         if (f_fsens_ai) call accum_hist_field(n_fsens_ai,iblk, &
            fsens(:,:,iblk)*workb(:,:))
         if (f_flwup) call accum_hist_field(n_flwup,   iblk, flwout(:,:,iblk))
         if (f_flwup_ai) call accum_hist_field(n_flwup_ai,iblk, &
            flwout(:,:,iblk)*workb(:,:))
         if (f_evap) call accum_hist_field(n_evap,   iblk, evap(:,:,iblk))
         if (f_evap_ai) call accum_hist_field(n_evap_ai,iblk, &
            evap(:,:,iblk)*workb(:,:))

         if (f_Tair) call accum_hist_field(n_Tair,   iblk, Tair(:,:,iblk))
         if (f_Tref) call accum_hist_field(n_Tref,   iblk, Tref(:,:,iblk))
         if (f_Qref) call accum_hist_field(n_Qref,   iblk, Qref(:,:,iblk))
         if (f_congel) call accum_hist_field(n_congel, iblk, congel(:,:,iblk))
         if (f_frazil) call accum_hist_field(n_frazil, iblk, frazil(:,:,iblk))
         if (f_snoice) call accum_hist_field(n_snoice, iblk, snoice(:,:,iblk))
         if (f_meltt) call accum_hist_field(n_meltt, iblk, meltt(:,:,iblk))
         if (f_meltb) call accum_hist_field(n_meltb, iblk, meltb(:,:,iblk))
         if (f_meltl) call accum_hist_field(n_meltl, iblk, meltl(:,:,iblk))

         if (f_fresh) call accum_hist_field(n_fresh, iblk, fresh(:,:,iblk))
         if (f_fresh_ai) call accum_hist_field(n_fresh_ai,iblk, fresh_gbm(:,:,iblk))
         if (f_fsalt) call accum_hist_field(n_fsalt, iblk, fsalt(:,:,iblk))
         if (f_fsalt_ai) call accum_hist_field(n_fsalt_ai,iblk, fsalt_gbm(:,:,iblk))
         if (f_fhocn) call accum_hist_field(n_fhocn, iblk, fhocn(:,:,iblk))
         if (f_fhocn_ai) call accum_hist_field(n_fhocn_ai,iblk, fhocn_gbm(:,:,iblk))
         if (f_fswthru) call accum_hist_field(n_fswthru, iblk, fswthru(:,:,iblk))
         if (f_fswthru_ai) call accum_hist_field(n_fswthru_ai,iblk, fswthru_gbm(:,:,iblk))
               
         if (f_strairx) call accum_hist_field(n_strairx, iblk, strairx(:,:,iblk))
         if (f_strairy) call accum_hist_field(n_strairy, iblk, strairy(:,:,iblk))
         if (f_strtltx) call accum_hist_field(n_strtltx, iblk, strtltx(:,:,iblk))
         if (f_strtlty) call accum_hist_field(n_strtlty, iblk, strtlty(:,:,iblk))
         if (f_strcorx) call accum_hist_field(n_strcorx, iblk, fm(:,:,iblk)*vvel(:,:,iblk))
         if (f_strcory) call accum_hist_field(n_strcory, iblk,-fm(:,:,iblk)*uvel(:,:,iblk))
         if (f_strocnx) call accum_hist_field(n_strocnx, iblk, strocnx(:,:,iblk))
         if (f_strocny) call accum_hist_field(n_strocny, iblk, strocny(:,:,iblk))
         if (f_strintx) call accum_hist_field(n_strintx, iblk, strintx(:,:,iblk))
         if (f_strinty) call accum_hist_field(n_strinty, iblk, strinty(:,:,iblk))
         if (f_strength) call accum_hist_field(n_strength, iblk, strength(:,:,iblk))

! The following fields (divu, shear, sig1, and sig2) will be smeared
!  if averaged over more than a few days.
! Snapshots may be more useful (see below).

!        if (f_divu) call accum_hist_field(n_divu, iblk, divu(:,:,iblk))
!        if (f_shear) call accum_hist_field(n_shear, iblk, shear(:,:,iblk))
!        if (f_sig1) call accum_hist_field(n_sig1, iblk, sig1(:,:,iblk))
!        if (f_sig2) call accum_hist_field(n_sig2, iblk, sig2(:,:,iblk))

         if (f_dvidtt) call accum_hist_field(n_dvidtt, iblk, dvidtt(:,:,iblk))
         if (f_dvidtd) call accum_hist_field(n_dvidtd, iblk, dvidtd(:,:,iblk))
         if (f_daidtt) call accum_hist_field(n_daidtt, iblk, daidtt(:,:,iblk))
         if (f_daidtd) call accum_hist_field(n_daidtd, iblk, daidtd(:,:,iblk))

         if (f_opening) call accum_hist_field(n_opening, iblk, opening(:,:,iblk))
         if (f_dardg1dt) call accum_hist_field(n_dardg1dt, iblk, dardg1dt(:,:,iblk))
         if (f_dardg2dt) call accum_hist_field(n_dardg2dt, iblk, dardg2dt(:,:,iblk))
         if (f_dvirdgdt) call accum_hist_field(n_dvirdgdt, iblk, dvirdgdt(:,:,iblk))

         if (f_icepresent) then
           worka(:,:) = c0
           do j = jlo, jhi
           do i = ilo, ihi
              if (aice(i,j,iblk) > puny) worka(i,j) = c1
           enddo
           enddo
           call accum_hist_field(n_icepresent, iblk, worka(:,:))
         endif

         if (f_fsurf_ai) call accum_hist_field(n_fsurf_ai, iblk, &
            fsurf(:,:,iblk)*workb(:,:))
         if (f_fcondtop_ai) call accum_hist_field(n_fcondtop_ai, iblk, &
            fcondtop(:,:,iblk)*workb(:,:))

         if (f_faero) then
            do n=1,n_aero
               call accum_hist_field(n_faero(n), iblk, faero(:,:,n,iblk))
            enddo
         endif
         if (f_fsoot) then
            do n=1,n_aero
               call accum_hist_field(n_fsoot(n), iblk, fsoot(:,:,n,iblk))
            enddo
         endif
         if (f_aero) then
            do n=1,n_aero
               call accum_hist_field(n_aerosn1(n), iblk, trcr(:,:,nt_aero  +4*(n-1),iblk)/rhos)
               call accum_hist_field(n_aerosn2(n), iblk, trcr(:,:,nt_aero+1+4*(n-1),iblk)/rhos)
               call accum_hist_field(n_aeroic1(n), iblk, trcr(:,:,nt_aero+2+4*(n-1),iblk)/rhoi)
               call accum_hist_field(n_aeroic2(n), iblk, trcr(:,:,nt_aero+3+4*(n-1),iblk)/rhoi)
            enddo
         endif

         nct = min(ncat, ncat_hist)
         do n=1,nct
            workb(:,:) = aicen_init(:,:,n,iblk)
            if (f_aicen) call accum_hist_field(n_aicen(n), iblk, aicen(:,:,n,iblk))
            if (f_vicen) call accum_hist_field(n_vicen(n), iblk, vicen(:,:,n,iblk))
            if (f_apondn) call accum_hist_field(n_apondn(n), iblk, apondn(:,:,n,iblk))
            if (f_fsurfn_ai) call accum_hist_field(n_fsurfn_ai(n), iblk, &
               fsurfn(:,:,n,iblk)*workb(:,:))
            if (f_fcondtopn_ai) call accum_hist_field(n_fcondtopn_ai(n), iblk, &
               fcondtopn(:,:,n,iblk)*workb(:,:))
            if (f_flatn_ai) call accum_hist_field(n_flatn_ai(n), iblk, &
               flatn(:,:,n,iblk)*workb(:,:))
            ! Calculate surface heat flux that causes melt as this
            ! is what is calculated by the atmos in HadGEM3 so
            ! needed for checking purposes
            if (f_fmelttn_ai) call accum_hist_field(n_fmelttn_ai(n), iblk, &
               max(fsurfn(:,:,n,iblk) - fcondtopn(:,:,n,iblk),c0)*workb(:,:))
        enddo                    ! n

        ! Calculate aggregate melt pond area by summing category values
        if (f_apond) then
           worka(:,:) = c0
           do j = jlo, jhi
           do i = ilo, ihi
            if (tmask(i,j,iblk)) then
              do n=1,nct
                 worka(i,j)  = worka(i,j) + aa(i,j,n_apondn(n),iblk)
              enddo            ! n
            endif              ! tmask
           enddo                ! i
           enddo                ! j
           aa(:,:,n_apond,iblk) = worka(:,:)
        endif
        ! Calculate aggregate surface melt flux by summing category values
        if (f_fmeltt_ai) then
           worka(:,:) = c0
           do j = jlo, jhi
           do i = ilo, ihi
            if (tmask(i,j,iblk)) then
              do n=1,nct
                 worka(i,j)  = worka(i,j) + aa(i,j,n_fmelttn_ai(n),iblk)
              enddo            ! n
            endif              ! tmask
           enddo                ! i
           enddo                ! j
           aa(:,:,n_fmeltt_ai,iblk) = worka(:,:)
        endif

      enddo                     ! iblk
      !$OMP END PARALLEL DO

      !---------------------------------------------------------------
      ! Write output files at prescribed intervals
      !---------------------------------------------------------------

      if (write_history .or. write_ic) then

      !---------------------------------------------------------------
      ! Mask out land points and convert units 
      !---------------------------------------------------------------

        ravgct = c1/avgct
	!$OMP PARALLEL DO PRIVATE(iblk,this_block,ilo,ihi,jlo,jhi,i,j,k)
        do iblk = 1, nblocks
           this_block = get_block(blocks_ice(iblk),iblk)         
           ilo = this_block%ilo
           ihi = this_block%ihi
           jlo = this_block%jlo
           jhi = this_block%jhi

           do k = 1, num_avail_hist_fields
              do j = jlo, jhi
              do i = ilo, ihi
                 if (.not. tmask(i,j,iblk)) then ! mask out land points
                    aa(i,j,k,iblk) = spval
                 else                            ! convert units
                    aa(i,j,k,iblk) = avail_hist_fields(k)%cona*aa(i,j,k,iblk) &
                                   * ravgct + avail_hist_fields(k)%conb
                 endif
              enddo             ! i
              enddo             ! j
           enddo                ! k

      !---------------------------------------------------------------
      ! snapshots
      !---------------------------------------------------------------

          ! compute sig1 and sig2
        
           call principal_stress (nx_block,  ny_block,  &
                                  stressp_1 (:,:,iblk), &
                                  stressm_1 (:,:,iblk), &
                                  stress12_1(:,:,iblk), &
                                  prs_sig   (:,:,iblk), &
                                  sig1      (:,:,iblk), &
                                  sig2      (:,:,iblk))
 
           do j = jlo, jhi
           do i = ilo, ihi
              if (.not. tmask(i,j,iblk)) then ! mask out land points
                 if (f_divu)      aa(i,j,n_divu,iblk)      = spval
                 if (f_shear)     aa(i,j,n_shear,iblk)     = spval
                 if (f_sig1)      aa(i,j,n_sig1,iblk )     = spval
                 if (f_sig2)      aa(i,j,n_sig2,iblk )     = spval
                 if (f_mlt_onset) aa(i,j,n_mlt_onset,iblk) = spval
                 if (f_frz_onset) aa(i,j,n_frz_onset,iblk) = spval
                 if (f_hisnap)    aa(i,j,n_hisnap,iblk)    = spval
                 if (f_aisnap)    aa(i,j,n_aisnap,iblk)    = spval
                 if (f_trsig)     aa(i,j,n_trsig,iblk )    = spval
                 if (f_iage)      aa(i,j,n_iage,iblk )     = spval
                 if (f_FY)        aa(i,j,n_FY,iblk )       = spval
              else
                 if (f_divu) aa(i,j,n_divu,iblk)  = &
                     divu (i,j,iblk)*avail_hist_fields(n_divu)%cona
                 if (f_shear) aa(i,j,n_shear,iblk) = &
                     shear(i,j,iblk)*avail_hist_fields(n_shear)%cona
                 if (f_sig1) aa(i,j,n_sig1,iblk)  = &
                     sig1 (i,j,iblk)*avail_hist_fields(n_sig1)%cona
                 if (f_sig2) aa(i,j,n_sig2,iblk)  = &
                     sig2 (i,j,iblk)*avail_hist_fields(n_sig2)%cona
                 if (f_mlt_onset) aa(i,j,n_mlt_onset,iblk) = mlt_onset(i,j,iblk)
                 if (f_frz_onset) aa(i,j,n_frz_onset,iblk) = frz_onset(i,j,iblk)
                 if (f_hisnap) aa(i,j,n_hisnap,iblk)    = vice(i,j,iblk)
                 if (f_aisnap) aa(i,j,n_aisnap,iblk)    = aice(i,j,iblk)
                 if (f_trsig) aa(i,j,n_trsig,iblk )    = &
                                       p25*(stressp_1(i,j,iblk) &
                                          + stressp_2(i,j,iblk) &
                                          + stressp_3(i,j,iblk) &
                                          + stressp_4(i,j,iblk))
                 if (f_iage) aa(i,j,n_iage,iblk)  = &
                       trcr(i,j,nt_iage,iblk)*avail_hist_fields(n_iage)%cona
                 if (f_FY) aa(i,j,n_FY,iblk)  = &
                       trcr(i,j,nt_FY,iblk)*avail_hist_fields(n_FY)%cona
            endif
           enddo                ! i
           enddo                ! j

        enddo                   ! iblk
        !$OMP END PARALLEL DO

        time_end = time/int(secday)

      !---------------------------------------------------------------
      ! write file
      !---------------------------------------------------------------

      call ice_timer_start(timer_readwrite)  ! reading/writing

      if (history_format == 'nc') then
        call icecdf         ! netcdf output
      else
        call icebin         ! binary output
      endif

      call ice_timer_stop(timer_readwrite)  ! reading/writing

      !---------------------------------------------------------------
      ! reset to zero
      !------------------------------------------------------------
        aa(:,:,:,:) = c0
        avgct = c0

      endif  ! write_history or write_ic

      !$OMP PARALLEL DO PRIVATE(iblk,this_block,ilo,ihi,jlo,jhi,i,j,k)
      do iblk = 1, nblocks
         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi

         if (new_year) then

            do j=jlo,jhi
            do i=ilo,ihi
               ! reset NH Jan 1
               if (lmask_n(i,j,iblk)) mlt_onset(i,j,iblk) = c0
               ! reset SH Jan 1 
               if (lmask_s(i,j,iblk)) frz_onset(i,j,iblk) = c0
            enddo
            enddo
         endif                  ! new_year

         if ((yday >= 181._dbl_kind) .and. &
             (yday <  181._dbl_kind+dt/secday)) then

            do j=jlo,jhi
            do i=ilo,ihi

               ! reset SH Jul 1
               if (lmask_s(i,j,iblk)) mlt_onset(i,j,iblk) = c0

               ! reset NH Jul 1
               if (lmask_n(i,j,iblk)) frz_onset(i,j,iblk) = c0
            enddo
            enddo

         endif                  ! yday
      enddo                     ! iblk

      write_ic = .false.        ! write initial condition once at most

      end subroutine ice_write_hist

!=======================================================================
!
!BOP
!
! !IROUTINE: icecdf - write netCDF history file
!
! !INTERFACE:
!
      subroutine icecdf
!
! !DESCRIPTION:
!
! write netCDF history file
!
! !REVISION HISTORY:
!
! authors:   E.C.Hunke, LANL
!            Bruce P. Briegleb, NCAR
!
! !USES:
!
#ifdef ncdf

      use ice_gather_scatter
      use ice_domain_size
      use ice_constants
      use ice_calendar, only: time, sec, idate, idate0, nyr, month, &
                              mday, write_ic, histfreq, histfreq_n, &
                              year_init, new_year, new_month, new_day, &
                              dayyr, daymo, days_per_year
      use ice_work, only: work_g1, work_gr, work_gr3
      use ice_restart, only: lenstr, runid
      use ice_domain, only: distrb_info
      use ice_itd, only: c_hi_range
      use ice_exit
      use netcdf
!
! !INPUT/OUTPUT PARAMETERS:
!
!EOP
!
      integer (kind=int_kind) :: i,j,n, &
         ncid,status,imtid,jmtid,timid,varid, &
         length,nvertexid,ivertex
      integer (kind=int_kind), dimension(3) :: dimid
      integer (kind=int_kind), dimension(3) :: dimid_nverts
      real (kind=real_kind) :: ltime
      character (char_len) :: title
      character (char_len_long) :: ncfile

      integer (kind=int_kind) :: iyear, imonth, iday
      integer (kind=int_kind) :: icategory,ind,i_aice,boundid

      character (char_len) :: start_time,current_date,current_time
      character (len=16) :: c_aice
      character (len=8) :: cdate

! Info for lat, lon and time invariant variables
      ! 4 coordinate variables: TLON, TLAT, ULON, ULAT
      INTEGER (kind=int_kind), PARAMETER :: ncoord = 4

      ! 4 vertices in each grid cell
      INTEGER (kind=int_kind), PARAMETER :: nverts = 4

      ! 4 variables describe T, U grid boundaries:
      ! lont_bounds, latt_bounds, lonu_bounds, latu_bounds
      INTEGER (kind=int_kind), PARAMETER :: nvar_verts = 4

      TYPE coord_attributes         ! netcdf coordinate attributes
        character (len=11)   :: short_name
        character (len=45)   :: long_name
        character (len=20)   :: units
      END TYPE coord_attributes

      TYPE req_attributes         ! req'd netcdf attributes
        type (coord_attributes) :: req
        character (len=20)   :: coordinates
      END TYPE req_attributes

      TYPE(req_attributes), dimension(nvar) :: var
      TYPE(coord_attributes), dimension(ncoord) :: coord_var
      TYPE(coord_attributes), dimension(nvar_verts) :: var_nverts
      CHARACTER (char_len), dimension(ncoord) :: coord_bounds

      real (kind=dbl_kind), dimension(nx_block,ny_block,max_blocks) :: &
         work1

      if (my_task == master_task) then

        ltime=time/int(secday)

        call construct_filename(ncfile,'nc')

        ! add local directory path name to ncfile
        if (write_ic) then
          ncfile = trim(incond_dir)//ncfile
        else
          ncfile = trim(history_dir)//ncfile
        endif

        ! create file
#ifdef _HIRES
        status = nf90_create(ncfile, NF90_64BIT_OFFSET, ncid)
#else
        status = nf90_create(ncfile, nf90_clobber, ncid)
#endif
        if (status /= nf90_noerr) call abort_ice( &
                        'ice: Error creating history ncfile')

      !-----------------------------------------------------------------
      ! define dimensions
      !-----------------------------------------------------------------

        if (hist_avg) then
          status = nf90_def_dim(ncid,'d2',2,boundid)
          if (status /= nf90_noerr) call abort_ice( &
                        'ice: Error defining dim d2')
        endif

        status = nf90_def_dim(ncid,'ni',nx_global,imtid)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error defining dim ni')

        status = nf90_def_dim(ncid,'nj',ny_global,jmtid)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error defining dim nj')

        status = nf90_def_dim(ncid,'time',NF90_UNLIMITED,timid)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error defining dim time')

        status = nf90_def_dim(ncid,'nvertices',nverts,nvertexid)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error defining dim nverts')

      !-----------------------------------------------------------------
      ! define coordinate variables
      !-----------------------------------------------------------------

        status = nf90_def_var(ncid,'time',nf90_float,timid,varid)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error defining var time')

        status = nf90_put_att(ncid,varid,'long_name','model time')
        if (status /= nf90_noerr) call abort_ice( &
                      'ice Error: time long_name')

        write(cdate,'(i8)') idate0
        write(title,'(a,a,a,a,a,a,a,a)') 'days since ', &
              cdate(1:4),'-',cdate(5:6),'-',cdate(7:8),' 00:00:00'
        status = nf90_put_att(ncid,varid,'units',title)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice Error: time units')

        if (days_per_year == 360) then
           status = nf90_put_att(ncid,varid,'calendar','360_day')
           if (status /= nf90_noerr) call abort_ice( &
                         'ice Error: time calendar')
        else
           status = nf90_put_att(ncid,varid,'calendar','noleap')
           if (status /= nf90_noerr) call abort_ice( &
                         'ice Error: time calendar')
        endif

        if (hist_avg) then
          status = nf90_put_att(ncid,varid,'bounds','time_bounds')
          if (status /= nf90_noerr) call abort_ice( &
                      'ice Error: time bounds')
        endif

      !-----------------------------------------------------------------
      ! Define attributes for time bounds if hist_avg is true
      !-----------------------------------------------------------------

        if (hist_avg) then
          dimid(1) = boundid
          dimid(2) = timid
          status = nf90_def_var(ncid,'time_bounds',nf90_float,dimid(1:2),varid)
          if (status /= nf90_noerr) call abort_ice( &
                        'ice: Error defining var time_bounds')
          status = nf90_put_att(ncid,varid,'long_name', &
                                'boundaries for time-averaging interval')
          if (status /= nf90_noerr) call abort_ice( &
                        'ice Error: time_bounds long_name')
          write(cdate,'(i8)') idate0
          write(title,'(a,a,a,a,a,a,a,a)') 'days since ', &
                cdate(1:4),'-',cdate(5:6),'-',cdate(7:8),' 00:00:00'
          status = nf90_put_att(ncid,varid,'units',title)
          if (status /= nf90_noerr) call abort_ice( &
                        'ice Error: time_bounds units')
        endif

      !-----------------------------------------------------------------
      ! define information for required time-invariant variables
      !-----------------------------------------------------------------

      ind = 0
      ind = ind + 1
      coord_var(ind) = coord_attributes('TLON', &
                       'T grid center longitude', 'degrees_east')
      coord_bounds(ind) = 'lont_bounds'
      ind = ind + 1
      coord_var(ind) = coord_attributes('TLAT', &
                       'T grid center latitude',  'degrees_north')
      coord_bounds(ind) = 'latt_bounds'
      ind = ind + 1
      coord_var(ind) = coord_attributes('ULON', &
                       'U grid center longitude', 'degrees_east')
      coord_bounds(ind) = 'lonu_bounds'
      ind = ind + 1
      coord_var(ind) = coord_attributes('ULAT', &
                       'U grid center latitude',  'degrees_north')
      coord_bounds(ind) = 'latu_bounds'

      !-----------------------------------------------------------------
      ! define information for optional time-invariant variables
      !-----------------------------------------------------------------

      var(n_tarea)%req = coord_attributes('tarea', &
                  'area of T grid cells', 'm^2')
      var(n_tarea)%coordinates = 'TLON TLAT'
      var(n_uarea)%req = coord_attributes('uarea', &
                  'area of U grid cells', 'm^2')
      var(n_uarea)%coordinates = 'ULON ULAT'
      var(n_dxt)%req = coord_attributes('dxt', &
                  'T cell width through middle', 'm')
      var(n_dxt)%coordinates = 'TLON TLAT'
      var(n_dyt)%req = coord_attributes('dyt', &
                  'T cell height through middle', 'm')
      var(n_dyt)%coordinates = 'TLON TLAT'
      var(n_dxu)%req = coord_attributes('dxu', &
                  'U cell width through middle', 'm')
      var(n_dxu)%coordinates = 'ULON ULAT'
      var(n_dyu)%req = coord_attributes('dyu', &
                  'U cell height through middle', 'm')
      var(n_dyu)%coordinates = 'ULON ULAT'
      var(n_HTN)%req = coord_attributes('HTN', &
                  'T cell width on North side','m')
      var(n_HTN)%coordinates = 'TLON TLAT'
      var(n_HTE)%req = coord_attributes('HTE', &
                  'T cell width on East side', 'm')
      var(n_HTE)%coordinates = 'TLON TLAT'
      var(n_ANGLE)%req = coord_attributes('ANGLE', &
                  'angle grid makes with latitude line on U grid', &
                  'radians')
      var(n_ANGLE)%coordinates = 'ULON ULAT'
      var(n_ANGLET)%req = coord_attributes('ANGLET', &
                  'angle grid makes with latitude line on T grid', &
                  'radians')
      var(n_ANGLET)%coordinates = 'TLON TLAT'

      ! These fields are required for CF compliance
      ! dimensions (nx,ny,nverts)
      var_nverts(n_lont_bnds) = coord_attributes('lont_bounds', &
                  'longitude boundaries of T cells', 'degrees_east')
      var_nverts(n_latt_bnds) = coord_attributes('latt_bounds', &
                  'latitude boundaries of T cells', 'degrees_north')
      var_nverts(n_lonu_bnds) = coord_attributes('lonu_bounds', &
                  'longitude boundaries of U cells', 'degrees_east')
      var_nverts(n_latu_bnds) = coord_attributes('latu_bounds', &
                  'latitude boundaries of U cells', 'degrees_north')

      !-----------------------------------------------------------------
      ! define attributes for time-invariant variables
      !-----------------------------------------------------------------

        dimid(1) = imtid
        dimid(2) = jmtid
        dimid(3) = timid

        do i = 1, ncoord
          status = nf90_def_var(ncid, coord_var(i)%short_name, nf90_float, &
                                dimid(1:2), varid)
          if (status /= nf90_noerr) call abort_ice( &
               'Error defining short_name for '//coord_var(i)%short_name)
          status = nf90_put_att(ncid,varid,'long_name',coord_var(i)%long_name)
          if (status /= nf90_noerr) call abort_ice( &
               'Error defining long_name for '//coord_var(i)%short_name)
          status = nf90_put_att(ncid, varid, 'units', coord_var(i)%units)
          if (status /= nf90_noerr) call abort_ice( &
                  'Error defining units for '//coord_var(i)%short_name)
          if (coord_var(i)%short_name == 'ULAT') then
             status = nf90_put_att(ncid,varid,'comment', &
                  'Latitude of NE corner of T grid cell')
             if (status /= nf90_noerr) call abort_ice( &
                   'Error defining comment for '//coord_var(i)%short_name)
          endif
          if (f_bounds) then
             status = nf90_put_att(ncid, varid, 'bounds', coord_bounds(i))
             if (status /= nf90_noerr) call abort_ice( &
                 'Error defining bounds for '//coord_var(i)%short_name)
          endif
        enddo

        ! Attributes for tmask defined separately, since it has no units
        if (igrd(n_tmask)) then
           status = nf90_def_var(ncid, 'tmask', nf90_float, dimid(1:2), varid)
           if (status /= nf90_noerr) call abort_ice( &
                         'ice: Error defining var tmask')
           status = nf90_put_att(ncid,varid, 'long_name', 'ocean grid mask')
           if (status /= nf90_noerr) call abort_ice('ice Error: tmask long_name')
           status = nf90_put_att(ncid, varid, 'coordinates', 'TLON TLAT')
           if (status /= nf90_noerr) call abort_ice('ice Error: tmask units')
           status = nf90_put_att(ncid,varid,'comment', '0 = land, 1 = ocean')
           if (status /= nf90_noerr) call abort_ice('ice Error: tmask comment')
        endif

        do i = 2, nvar       ! note: n_tmask=1
          if (igrd(i)) then
             status = nf90_def_var(ncid, var(i)%req%short_name, &
                                   nf90_float, dimid(1:2), varid)
             if (status /= nf90_noerr) call abort_ice( &
                  'Error defining variable '//var(i)%req%short_name)
             status = nf90_put_att(ncid,varid, 'long_name', var(i)%req%long_name)
             if (status /= nf90_noerr) call abort_ice( &
                  'Error defining long_name for '//var(i)%req%short_name)
             status = nf90_put_att(ncid, varid, 'units', var(i)%req%units)
             if (status /= nf90_noerr) call abort_ice( &
                  'Error defining units for '//var(i)%req%short_name)
             status = nf90_put_att(ncid, varid, 'coordinates', var(i)%coordinates)
             if (status /= nf90_noerr) call abort_ice( &
                  'Error defining coordinates for '//var(i)%req%short_name)
          endif
        enddo

        ! Fields with dimensions (nverts,nx,ny)
        dimid_nverts(1) = nvertexid
        dimid_nverts(2) = imtid
        dimid_nverts(3) = jmtid
        do i = 1, nvar_verts
          if (f_bounds) then
             status = nf90_def_var(ncid, var_nverts(i)%short_name, &
                                   nf90_float,dimid_nverts, varid)
             if (status /= nf90_noerr) call abort_ice( &
                  'Error defining variable '//var_nverts(i)%short_name)
             status = &
             nf90_put_att(ncid,varid, 'long_name', var_nverts(i)%long_name)
             if (status /= nf90_noerr) call abort_ice( &
                  'Error defining long_name for '//var_nverts(i)%short_name)
             status = &
             nf90_put_att(ncid, varid, 'units', var_nverts(i)%units)
             if (status /= nf90_noerr) call abort_ice( &
                  'Error defining units for '//var_nverts(i)%short_name)
          endif
        enddo

        do n=1,num_avail_hist_fields
            status  = nf90_def_var(ncid, avail_hist_fields(n)%vname, &
                         nf90_float, dimid, varid)
            if (status /= nf90_noerr) call abort_ice( &
               'Error defining variable '//avail_hist_fields(n)%vname)
            status = nf90_put_att(ncid,varid,'units', &
                        avail_hist_fields(n)%vunit)
            if (status /= nf90_noerr) call abort_ice( &
               'Error defining units for '//avail_hist_fields(n)%vname)
            status = nf90_put_att(ncid,varid, 'long_name', &
                        avail_hist_fields(n)%vdesc)
                   
            if (status /= nf90_noerr) call abort_ice( &
               'Error defining long_name for '//avail_hist_fields(n)%vname)
            status = nf90_put_att(ncid,varid,'coordinates', &
                        avail_hist_fields(n)%vcoord)
            if (status /= nf90_noerr) call abort_ice( &
               'Error defining coordinates for '//avail_hist_fields(n)%vname)
            status = nf90_put_att(ncid,varid,'cell_measures', &
                        avail_hist_fields(n)%vcellmeas)
            if (status /= nf90_noerr) call abort_ice( &
               'Error defining cell measures for '//avail_hist_fields(n)%vname)
            status = nf90_put_att(ncid,varid,'missing_value',spval)
            if (status /= nf90_noerr) call abort_ice( &
               'Error defining mising_value for '//avail_hist_fields(n)%vname)
            status = nf90_put_att(ncid,varid,'_FillValue',spval)
            if (status /= nf90_noerr) call abort_ice( &
               'Error defining _FillValue for '//avail_hist_fields(n)%vname)

      !-----------------------------------------------------------------
      ! Append ice thickness range to aicen comments
      !-----------------------------------------------------------------
            c_aice = TRIM(avail_hist_fields(n)%vname)
            i_aice = lenstr(c_aice)
            if (c_aice(1:4) == 'aice' .and. i_aice > 4 ) then
              if (i_aice == 5) then         ! categories 1-9
                read(c_aice(i_aice:i_aice), '(i1)') icategory
              else                          ! categories > 9
                read(c_aice(i_aice-1:i_aice), '(i2)') icategory
              endif
              avail_hist_fields(n)%vcomment = &
                 'Ice range: '//c_hi_range(icategory)
            endif
            status = nf90_put_att(ncid,varid,'comment', &
                        avail_hist_fields(n)%vcomment)
            if (status /= nf90_noerr) call abort_ice( &
               'Error defining comment for '//avail_hist_fields(n)%vname)
      !-----------------------------------------------------------------
      ! Add cell_methods attribute to variables if averaged
      !-----------------------------------------------------------------
            if (hist_avg) then
              if (TRIM(avail_hist_fields(n)%vname)/='sig1' &
              .or.TRIM(avail_hist_fields(n)%vname)/='sig2') then
                status = nf90_put_att(ncid,varid,'cell_methods','time: mean')
                if (status /= nf90_noerr) call abort_ice( &
                 'Error defining cell methods for '//avail_hist_fields(n)%vname)
              endif
            endif

            if (histfreq == '1'     .or. .not. hist_avg &
                .or. n==n_divu      .or. n==n_shear     &  ! snapshots
                .or. n==n_sig1      .or. n==n_sig2 .or. n==n_trsig &
                .or. n==n_mlt_onset .or. n==n_frz_onset &
                .or. n==n_hisnap    .or. n==n_aisnap) then
            status = nf90_put_att(ncid,varid,'time_rep','instantaneous')
            else
            status = nf90_put_att(ncid,varid,'time_rep','averaged')
            endif
        enddo

      !-----------------------------------------------------------------
      ! global attributes
      !-----------------------------------------------------------------
      ! ... the user should change these to something useful ...
      !-----------------------------------------------------------------
#ifdef CCSMCOUPLED
        status = nf90_put_att(ncid,nf90_global,'title',runid)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error in global attribute title')
#else
        title  = 'sea ice model output for CICE'
        status = nf90_put_att(ncid,nf90_global,'title',title)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error in global attribute title')
#endif
        title = 'Diagnostic and Prognostic Variables'
        status = nf90_put_att(ncid,nf90_global,'contents',title)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice Error: global attribute contents')

        title  = 'sea ice model: Community Ice Code (CICE)'
        status = nf90_put_att(ncid,nf90_global,'source',title)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice Error: global attribute source')

        write(title,'(a,i3,a)') 'All years have exactly ',int(dayyr),' days'
        status = nf90_put_att(ncid,nf90_global,'comment',title)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice Error: global attribute comment')

        write(title,'(a,i8)') 'File written on model date ',idate
        status = nf90_put_att(ncid,nf90_global,'comment2',title)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice Error: global attribute date1')

        write(title,'(a,i6)') 'seconds elapsed into model date: ',sec
        status = nf90_put_att(ncid,nf90_global,'comment3',title)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice Error: global attribute date2')

        title = 'CF-1.0'
        status =  &
             nf90_put_att(ncid,nf90_global,'conventions',title)
        if (status /= nf90_noerr) call abort_ice( &
             'Error in global attribute conventions')

        call date_and_time(date=current_date, time=current_time)
        write(start_time,1000) current_date(1:4), current_date(5:6), &
                               current_date(7:8), current_time(1:2), &
                               current_time(3:4), current_time(5:8)
1000    format('This dataset was created on ', &
                a,'-',a,'-',a,' at ',a,':',a,':',a)

        status = nf90_put_att(ncid,nf90_global,'history',start_time)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice Error: global attribute history')

      !-----------------------------------------------------------------
      ! end define mode
      !-----------------------------------------------------------------

        status = nf90_enddef(ncid)
        if (status /= nf90_noerr) call abort_ice('ice: Error in nf90_enddef')

      !-----------------------------------------------------------------
      ! write time variable
      !-----------------------------------------------------------------

        status = nf90_inq_varid(ncid,'time',varid)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error getting time varid')
        status = nf90_put_var(ncid,varid,ltime)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error writing time variable')

      !-----------------------------------------------------------------
      ! write time_bounds info
      !-----------------------------------------------------------------

        if (hist_avg) then
          status = nf90_inq_varid(ncid,'time_bounds',varid)
          if (status /= nf90_noerr) call abort_ice( &
                        'ice: Error getting time_bounds id')
          status = nf90_put_var(ncid,varid,time_beg,start=(/1/))
          if (status /= nf90_noerr) call abort_ice( &
                        'ice: Error writing time_beg')
          status = nf90_put_var(ncid,varid,time_end,start=(/2/))
          if (status /= nf90_noerr) call abort_ice( &
                        'ice: Error writing time_end')
        endif

      endif                     ! master_task

      if (my_task==master_task) then
         allocate(work_g1(nx_global,ny_global))
         allocate(work_gr(nx_global,ny_global))
      else
         allocate(work_gr(1,1))   ! to save memory
         allocate(work_g1(1,1))
      endif

      work_g1(:,:) = c0

      !-----------------------------------------------------------------
      ! write coordinate variables
      !-----------------------------------------------------------------

        do i = 1,ncoord
          call broadcast_scalar(coord_var(i)%short_name,master_task)
          SELECT CASE (coord_var(i)%short_name)
            CASE ('TLON')
              call gather_global(work_g1,TLON,master_task,distrb_info)
              if (my_task == master_task) then
              ! Convert T grid longitude from -180 -> 180 to 0 to 360
                 work_gr = work_g1*rad_to_deg + c360    ! single precision
                 where (work_gr > c360) work_gr = work_gr - c360
                 where (work_gr < c0 )  work_gr = work_gr + c360
              endif
            CASE ('TLAT')
              call gather_global(work_g1,TLAT,master_task,distrb_info)
              if (my_task == master_task) work_gr = work_g1*rad_to_deg
            CASE ('ULON')
              call gather_global(work_g1,ULON,master_task,distrb_info)
              if (my_task == master_task) work_gr = work_g1*rad_to_deg
            CASE ('ULAT')
              call gather_global(work_g1,ULAT,master_task,distrb_info)
              if (my_task == master_task) work_gr = work_g1*rad_to_deg
          END SELECT
          
          if (my_task == master_task) then
             status = nf90_inq_varid(ncid, coord_var(i)%short_name, varid)
             if (status /= nf90_noerr) call abort_ice( &
                  'ice: Error getting varid for '//coord_var(i)%short_name)
             status = nf90_put_var(ncid,varid,work_gr)
             if (status /= nf90_noerr) call abort_ice( &
                           'ice: Error writing'//coord_var(i)%short_name)
          endif
        enddo

      !-----------------------------------------------------------------
      ! write grid mask, area and rotation angle
      !-----------------------------------------------------------------

      if (igrd(n_tmask)) then
      call gather_global(work_g1, hm, master_task, distrb_info)
      if (my_task == master_task) then
        work_gr=work_g1
        status = nf90_inq_varid(ncid, 'tmask', varid)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error getting varid for tmask')
        status = nf90_put_var(ncid,varid,work_gr)
        if (status /= nf90_noerr) call abort_ice( &
                      'ice: Error writing variable tmask')
      endif
      endif

      do i = 2,nvar
        if (igrd(i)) then
        call broadcast_scalar(var(i)%req%short_name,master_task)
        SELECT CASE (var(i)%req%short_name)
          CASE ('tarea')
            call gather_global(work_g1, tarea, master_task, distrb_info)
          CASE ('uarea')
            call gather_global(work_g1, uarea, master_task, distrb_info)
          CASE ('dxu')
            call gather_global(work_g1,   dxu, master_task, distrb_info)
          CASE ('dyu')
            call gather_global(work_g1,   dyu, master_task, distrb_info)
          CASE ('dxt')
            call gather_global(work_g1,   dxt, master_task, distrb_info)
          CASE ('dyt')
            call gather_global(work_g1,   dyt, master_task, distrb_info)
          CASE ('HTN')
            call gather_global(work_g1,   HTN, master_task, distrb_info)
          CASE ('HTE')
            call gather_global(work_g1,   HTE, master_task, distrb_info)
          CASE ('ANGLE')
            call gather_global(work_g1, ANGLE, master_task, distrb_info)
          CASE ('ANGLET')
            call gather_global(work_g1, ANGLET,master_task, distrb_info)
        END SELECT

        if (my_task == master_task) then
          work_gr=work_g1
          status = nf90_inq_varid(ncid, var(i)%req%short_name, varid)
          if (status /= nf90_noerr) call abort_ice( &
                        'ice: Error getting varid for '//var(i)%req%short_name)
          status = nf90_put_var(ncid,varid,work_gr)
          if (status /= nf90_noerr) call abort_ice( &
                        'ice: Error writing variable '//var(i)%req%short_name)
        endif
        endif
      enddo

      deallocate(work_gr)
      !----------------------------------------------------------------
      ! Write coordinates of grid box vertices
      !----------------------------------------------------------------

      if (f_bounds) then
      if (my_task==master_task) then
         allocate(work_gr3(nverts,nx_global,ny_global))
      else
         allocate(work_gr3(1,1,1))   ! to save memory
      endif

      work_gr3(:,:,:) = c0
      work1   (:,:,:) = c0

      do i = 1, nvar_verts
        call broadcast_scalar(var_nverts(i)%short_name,master_task)
        SELECT CASE (var_nverts(i)%short_name)
        CASE ('lont_bounds')
        do ivertex = 1, nverts
           work1(:,:,:) = lont_bounds(ivertex,:,:,:)
           call gather_global(work_g1, work1, master_task, distrb_info)
           if (my_task == master_task) work_gr3(ivertex,:,:) = work_g1(:,:)
        enddo
        CASE ('latt_bounds')
        do ivertex = 1, nverts
           work1(:,:,:) = latt_bounds(ivertex,:,:,:)
           call gather_global(work_g1, work1, master_task, distrb_info)
           if (my_task == master_task) work_gr3(ivertex,:,:) = work_g1(:,:)
        enddo
        CASE ('lonu_bounds')
        do ivertex = 1, nverts
           work1(:,:,:) = lonu_bounds(ivertex,:,:,:)
           call gather_global(work_g1, work1, master_task, distrb_info)
           if (my_task == master_task) work_gr3(ivertex,:,:) = work_g1(:,:)
        enddo
        CASE ('latu_bounds')
        do ivertex = 1, nverts
           work1(:,:,:) = latu_bounds(ivertex,:,:,:)
           call gather_global(work_g1, work1, master_task, distrb_info)
           if (my_task == master_task) work_gr3(ivertex,:,:) = work_g1(:,:)
        enddo
        END SELECT

        if (my_task == master_task) then
          status = nf90_inq_varid(ncid, var_nverts(i)%short_name, varid)
          if (status /= nf90_noerr) call abort_ice( &
             'ice: Error getting varid for '//var_nverts(i)%short_name)
          status = nf90_put_var(ncid,varid,work_gr3)
          if (status /= nf90_noerr) call abort_ice( &
             'ice: Error writing variable '//var_nverts(i)%short_name)
        endif
      enddo
      deallocate(work_gr3)
      endif

      !-----------------------------------------------------------------
      ! write variable data
      !-----------------------------------------------------------------

      if (my_task==master_task) then
         allocate(work_gr3(nx_global,ny_global,1))
      else
         allocate(work_gr3(1,1,1))   ! to save memory
      endif

      do n=1,num_avail_hist_fields
          call gather_global(work_g1, aa(:,:,n,:), &
                             master_task, distrb_info)
          if (my_task == master_task) then
            work_gr3(:,:,1) = work_g1(:,:)
            status  = nf90_inq_varid(ncid,avail_hist_fields(n)%vname,varid)
            if (status /= nf90_noerr) call abort_ice( &
               'ice: Error getting varid for '//avail_hist_fields(n)%vname)
            status  = nf90_put_var(ncid,varid,work_gr3, &
                                   count=(/nx_global,ny_global,1/))
            if (status /= nf90_noerr) call abort_ice( &
               'ice: Error writing variable '//avail_hist_fields(n)%vname)
          endif
      enddo

      deallocate(work_gr3)
      deallocate(work_g1)

      !-----------------------------------------------------------------
      ! close output dataset
      !-----------------------------------------------------------------

      if (my_task == master_task) then
         status = nf90_close(ncid)
         if (status /= nf90_noerr) call abort_ice( &
                       'ice: Error closing netCDF history file')
         write(nu_diag,*) ' '
         write(nu_diag,*) 'Finished writing ',trim(ncfile)
      endif
#endif

      end subroutine icecdf

!=======================================================================
!
!BOP
!
! !IROUTINE: icebin - write binary history file
! This routine writes fewer grid variables compared with the netcdf
! version, to reduce file size.  Grid variables can be obtained from
! the original grid input files.
!
! !INTERFACE:
!
      subroutine icebin
!
! !DESCRIPTION:
!
! write binary history file
!
! !REVISION HISTORY:
!
! authors:   E.C.Hunke, LANL
!
! !USES:
!
      use ice_gather_scatter
      use ice_domain_size
      use ice_constants
      use ice_restart, only: lenstr, runid
      use ice_itd, only: c_hi_range
      use ice_calendar, only: write_ic, dayyr, histfreq
!
! !INPUT/OUTPUT PARAMETERS:
!
!EOP
!
      integer (kind=int_kind) :: i,j,n,nrec,nbits
      character (char_len) :: title
      character (char_len_long) :: ncfile, hdrfile

      integer (kind=int_kind) :: icategory,i_aice

      character (char_len) :: current_date,current_time
      character (len=16) :: c_aice
      logical (kind=log_kind) :: dbug

      dbug = .false.

      if (my_task == master_task) then

        call construct_filename(ncfile,'da')

        ! add local directory path name to ncfile
        if (write_ic) then
          ncfile = trim(incond_dir)//ncfile
        else
          ncfile = trim(history_dir)//ncfile
        endif
        hdrfile = trim(ncfile)//'.hdr'

        !-----------------------------------------------------------------
        ! create history files
        !-----------------------------------------------------------------
        nbits = 32 ! single precision
        call ice_open(nu_history, ncfile, nbits) ! direct access
        open(nu_hdr,file=hdrfile,form='formatted',status='unknown') ! ascii

!echmod call ice_write(nu_history, nrec, work, rda8 or ida4, dbug)

        title  = 'sea ice model: Community Ice Code (CICE)'
        write (nu_hdr, 999) 'source',title,' '

        write (nu_hdr, 999) 'file name contains model date',trim(ncfile),' '
#ifdef CCSMCOUPLED
        write (nu_hdr, 999) 'runid',runid,' '
#endif
        write (nu_hdr, 999) 'calendar','noleap',' '
        write (title,'(a,i3,a)') 'All years have exactly ',int(dayyr),' days'
        write (nu_hdr, 999) 'comment',title,' '
        write (nu_hdr, 999) 'conventions','CICE',' '
        write (nu_hdr, 997) 'missing_value',spval
        write (nu_hdr, 997) '_FillValue',spval

        call date_and_time(date=current_date, time=current_time)
        write (nu_hdr,1000) current_date(1:4), current_date(5:6), &
                                current_date(7:8), current_time(1:2), &
                                current_time(3:4), current_time(5:8)
        write (nu_hdr, *  ) ' '
        write (nu_hdr, *  ) 'Grid size:'
        write (nu_hdr, 998) '  ni',nx_global
        write (nu_hdr, 998) '  nj',ny_global

        write (nu_hdr, *  ) 'Grid variables: (left column = nrec)'
        nrec = 1
        write (nu_hdr, 996) nrec,'tarea','area of T grid cells','m^2'
        write (nu_hdr, *  ) 'History variables: (left column = nrec)'
      endif  ! my_task = master_task
      call ice_write(nu_history, nrec, tarea, 'rda4', dbug)

      do n=1,num_avail_hist_fields
          nrec = nrec + 1
          if (my_task == master_task) then
            write (nu_hdr, 996) nrec,trim(avail_hist_fields(n)%vname), &
               trim(avail_hist_fields(n)%vdesc),trim(avail_hist_fields(n)%vunit)

            ! Append ice thickness range to aicen comments
            c_aice = TRIM(avail_hist_fields(n)%vname)
            i_aice = lenstr(c_aice)
            if (c_aice(1:4) == 'aice' .and. i_aice > 4 ) then
              if (i_aice == 5) then         ! categories 1-9
                read(c_aice(i_aice:i_aice), '(i1)') icategory
              else                          ! categories > 9
                read(c_aice(i_aice-1:i_aice), '(i2)') icategory
              endif
              avail_hist_fields(n)%vcomment = &
                 'Ice range: '//c_hi_range(icategory)
            endif
            write (nu_hdr, 995) nrec,trim(avail_hist_fields(n)%vname), &
               trim(avail_hist_fields(n)%vcomment)

            if (histfreq == '1'     .or. .not. hist_avg &
                .or. n==n_divu      .or. n==n_shear     &  ! snapshots
                .or. n==n_sig1      .or. n==n_sig2 .or. n==n_trsig &
                .or. n==n_mlt_onset .or. n==n_frz_onset &
                .or. n==n_hisnap    .or. n==n_aisnap) then
               write (nu_hdr, 996) nrec,trim(avail_hist_fields(n)%vname), &
                  'time_rep','instantaneous'
            else
               write (nu_hdr, 996) nrec,trim(avail_hist_fields(n)%vname), &
                  'time_rep','averaged'
            endif
          endif

          call ice_write(nu_history, nrec, aa(:,:,n,:), 'rda4', dbug)

      enddo

995     format(i3,2x,a,' comment: ',a)
996     format(i3,2x,a,': ',a,',',2x,a)
997     format(a,': ',es13.6)
998     format(a,': ',i6)
999     format(a,': ',a,2x,a)
1000    format('This dataset was created on ', &
                a,'-',a,'-',a,' at ',a,':',a,':',a)

      if (my_task == master_task) then
        close (nu_hdr)     ! header file
        close (nu_history) ! data file
        write (nu_diag,*) ' '
        write (nu_diag,*) 'Finished writing ',trim(ncfile)
      endif

      end subroutine icebin

!=======================================================================

      subroutine construct_filename(ncfile,suffix)

      use ice_calendar, only: time, sec, idate, nyr, month, daymo,  &
                              mday, write_ic, histfreq, histfreq_n, &
                              year_init, new_year, new_month, new_day, &
                              dayyr, dt
      use ice_restart, only: lenstr

      character (char_len_long), intent(inout) :: ncfile
      character (len=2), intent(in) :: suffix

      integer (kind=int_kind) :: iyear, imonth, iday, isec

        iyear = nyr + year_init - 1 ! set year_init=1 in ice_in to get iyear=nyr
        imonth = month
        iday = mday
        isec = sec - dt

        ! construct filename
        if (write_ic) then
           write(ncfile,'(a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
              incond_file(1:lenstr(incond_file)),iyear,'-', &
              imonth,'-',iday,'-',isec,'.',suffix
        else

         if (hist_avg) then
          if (histfreq.eq.'h'.or.histfreq.eq.'H') then
           ! do nothing
          elseif (new_year) then
           iyear = iyear - 1
           imonth = 12
           iday = daymo(imonth)
          elseif (new_month) then
           imonth = month - 1
           iday = daymo(imonth)
          elseif (new_day) then
           iday = iday - 1
          endif
         endif

         if (histfreq == '1') then ! instantaneous, write every dt
           write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
            history_file(1:lenstr(history_file)),'_inst.', &
             iyear,'-',imonth,'-',iday,'-',sec,'.',suffix

         elseif (hist_avg) then    ! write averaged data

          if (histfreq.eq.'d'.or.histfreq.eq.'D') then     ! daily
           write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,a)')  &
            history_file(1:lenstr(history_file)), &
             '.',iyear,'-',imonth,'-',iday,'.',suffix
          elseif (histfreq.eq.'h'.or.histfreq.eq.'H') then ! hourly
           write(ncfile,'(a,a,i2.2,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
            history_file(1:lenstr(history_file)),'_',histfreq_n,'h.', &
             iyear,'-',imonth,'-',iday,'-',sec,'.',suffix
          elseif (histfreq.eq.'m'.or.histfreq.eq.'M') then ! monthly
           write(ncfile,'(a,a,i4.4,a,i2.2,a,a)')  &
            history_file(1:lenstr(history_file)),'.', &
             iyear,'-',imonth,'.',suffix
          elseif (histfreq.eq.'y'.or.histfreq.eq.'Y') then ! yearly
           write(ncfile,'(a,a,i4.4,a,a)') &
            history_file(1:lenstr(history_file)),'.', iyear,'.',suffix
          endif

         else                     ! instantaneous with histfreq > dt
           write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
            history_file(1:lenstr(history_file)),'_inst.', &
             iyear,'-',imonth,'-',iday,'-',sec,'.',suffix
         endif
        endif

      end subroutine construct_filename

      subroutine define_hist_field(id, vname, vunit, vcoord, vcellmeas, &
                                   vdesc, vcomment, cona, conb)

!     !DESCRIPTION:
!     Initializes description of an available field and returns location
!     in the available fields array for use in later calls.
!
!     !REVISION HISTORY:
!     same as module

!     !OUTPUT PARAMETERS:

      integer (int_kind), intent(out) :: &
         id                ! location in avail_fields array for use in
                           ! later routines

!     !INPUT PARAMETERS

      character (len=*), intent(in) :: &
         vname      , & ! variable names
         vunit      , & ! variable units
         vcoord     , & ! variable coordinates
         vcellmeas      ! variables cell measures

      character (len=*), intent(in) :: &
         vdesc      , & ! variable descriptions
         vcomment       ! variable comments

      real (kind=dbl_kind), intent(in) :: &
         cona       , & ! multiplicative conversion factor
         conb           ! additive conversion factor

      num_avail_hist_fields = num_avail_hist_fields + 1

      if (num_avail_hist_fields > max_avail_hist_fields) &
         call abort_ice("Need to increase max_avail_hist_fields")

      id = num_avail_hist_fields

      avail_hist_fields(id)%vname = trim(vname)
      avail_hist_fields(id)%vunit = trim(vunit)
      avail_hist_fields(id)%vcoord = trim(vcoord)
      avail_hist_fields(id)%vcellmeas = trim(vcellmeas)
      avail_hist_fields(id)%vdesc = trim(vdesc)
      avail_hist_fields(id)%vcomment = trim(vcomment)
      avail_hist_fields(id)%cona = cona
      avail_hist_fields(id)%conb = conb

      end subroutine define_hist_field

      subroutine accum_hist_field(id, iblk, field_accum)

!     !DESCRIPTION:
!     Accumulates a history field
!
!     !REVISION HISTORY:
!     same as module

      use ice_domain

!     !OUTPUT PARAMETERS:

      integer (int_kind), intent(in) :: &
         id                ! location in avail_fields array for use in
                           ! later routines

      integer (kind=int_kind), intent(in) :: iblk

      real (kind=dbl_kind), intent(in) :: field_accum(nx_block,ny_block)

      type (block) :: &
         this_block           ! block information for current block

      integer (kind=int_kind) :: i,j, ilo, ihi, jlo, jhi

      !---------------------------------------------------------------
      ! increment field
      !---------------------------------------------------------------

       this_block = get_block(blocks_ice(iblk),iblk)
       ilo = this_block%ilo
       ihi = this_block%ihi
       jlo = this_block%jlo
       jhi = this_block%jhi

       do j = jlo, jhi
       do i = ilo, ihi
          if (tmask(i,j,iblk)) then
             aa(i,j,id, iblk) = aa(i,j,id, iblk) + field_accum(i,j)
          endif
       enddo
       enddo

      end subroutine accum_hist_field

!=======================================================================

      end module ice_history

!=======================================================================
