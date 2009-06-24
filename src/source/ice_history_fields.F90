!=======================================================================
!
!BOP
!
! !MODULE: ice_history_fields - data types for ice_history

      module ice_history_fields

! !REVISION HISTORY:
! authors Mariana Vertenstein, NCAR
!
! !USES:
!
      use ice_kinds_mod
      use ice_domain_size	
!
!EOP
!
      logical (kind=log_kind) :: &
         hist_avg  ! if true, write averaged data instead of snapshots

      character (len=char_len_long) :: &
         history_dir   , & ! directory name for history file
         incond_dir        ! directory for snapshot initial conditions

      character (len=char_len) :: &
         history_format, & ! file format ('bin'=binary or 'nc'=netcdf)
         history_file  , & ! output file for history
         incond_file       ! output file for snapshot initial conditions

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
          character (len=1) :: vhistfreq  ! frequency of history output
          integer (kind=int_kind) :: vhistfreq_n ! number of vhistfreq intervals
      end type

      integer (kind=int_kind), parameter :: &
         max_avail_hist_fields = 600      ! Max number of history fields

      integer (kind=int_kind) :: &
         num_avail_hist_fields   = 0      ! Current number of defined fields

      type (ice_hist_field), dimension(max_avail_hist_fields) :: &
         avail_hist_fields

      !---------------------------------------------------------------
      ! primary info for the history file
      !---------------------------------------------------------------

      integer (kind=int_kind), parameter :: &
         ncat_hist = ncat         ! number of ice categories written <= ncat

      integer (kind=int_kind), parameter :: &
         nvar = 11                  ! number of grid fields that can be written
                                    !   excluding grid vertices

      real (kind=real_kind) :: time_beg(nstreams), &
                               time_end(nstreams) ! bounds for averaging
      real (kind=real_kind) :: time_bounds(2)

      real (kind=dbl_kind), allocatable :: &
         aa(:,:,:,:)       ! field accumulations and averages
         
      logical (kind=log_kind) :: &
         igrd(nvar)        ! true if grid field is written to output file

      !---------------------------------------------------------------
      ! logical flags: write to output file if true
      !---------------------------------------------------------------

       logical (kind=log_kind) :: &
           f_bounds    = .true.

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
           n_faero(n_aeromx)        , &
           n_fsoot(n_aeromx)        , &
           n_aerosn1(n_aeromx)      , &
           n_aerosn2(n_aeromx)      , &
           n_aeroic1(n_aeromx)      , &
           n_aeroic2(n_aeromx)      , &
           n_aerosn1n(n_aero*ncat_hist), &
           n_aerosn2n(n_aero*ncat_hist), &
           n_aeroic1n(n_aero*ncat_hist), &
           n_aeroic2n(n_aero*ncat_hist)

!
!EOP
!
      implicit none
      save

!=======================================================================

      contains

!=======================================================================

      subroutine construct_filename(ncfile,suffix,ns)

      use ice_calendar, only: time, sec, idate, nyr, month, daymo,  &
                              mday, write_ic, histfreq, histfreq_n, &
                              year_init, new_year, new_month, new_day, &
                              dayyr, dt
      use ice_restart, only: lenstr

      integer (kind=int_kind), intent(in) :: ns
      character (char_len_long), intent(inout) :: ncfile
      character (len=2), intent(in) :: suffix
      character (len=1) :: cstream

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
          if (histfreq(ns).eq.'h'.or.histfreq(ns).eq.'H') then
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

         cstream = ''
         if (ns > 1) write(cstream,'(i1.1)') ns-1

         if (histfreq(ns) == '1') then ! instantaneous, write every dt

          write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
            history_file(1:lenstr(history_file))//trim(cstream),'_inst.', &
             iyear,'-',imonth,'-',iday,'-',sec,'.',suffix

         elseif (hist_avg) then    ! write averaged data

          if (histfreq(ns).eq.'d'.or.histfreq(ns).eq.'D') then     ! daily
           write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,a)')  &
            history_file(1:lenstr(history_file))//trim(cstream), &
             '.',iyear,'-',imonth,'-',iday,'.',suffix
          elseif (histfreq(ns).eq.'h'.or.histfreq(ns).eq.'H') then ! hourly
           write(ncfile,'(a,a,i2.2,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
            history_file(1:lenstr(history_file))//trim(cstream),'_',   &
             histfreq_n(ns),'h.',iyear,'-',imonth,'-',iday,'-',sec,'.',suffix
          elseif (histfreq(ns).eq.'m'.or.histfreq(ns).eq.'M') then ! monthly
           write(ncfile,'(a,a,i4.4,a,i2.2,a,a)')  &
            history_file(1:lenstr(history_file))//trim(cstream),'.', &
             iyear,'-',imonth,'.',suffix
          elseif (histfreq(ns).eq.'y'.or.histfreq(ns).eq.'Y') then ! yearly
           write(ncfile,'(a,a,i4.4,a,a)') &
            history_file(1:lenstr(history_file))//trim(cstream),'.', &
	      iyear,'.',suffix
          endif

         else                     ! instantaneous with histfreq > dt
           write(ncfile,'(a,a,i4.4,a,i2.2,a,i2.2,a,i5.5,a,a)')  &
            history_file(1:lenstr(history_file)),'_inst.', &
             iyear,'-',imonth,'-',iday,'-',sec,'.',suffix
         endif
        endif

      end subroutine construct_filename

      end module ice_history_fields
