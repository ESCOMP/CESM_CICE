!=======================================================================
!
!BOP
!
! !MODULE: CICE_RunMod - contains main run method for CICE
!
! !DESCRIPTION:
!
!  Contains main driver routine for time stepping of CICE.
!
! !REVISION HISTORY:
!  SVN:$Id: CICE_RunMod.F90 52 2007-01-30 18:04:24Z eclare $
!
!  authors Elizabeth C. Hunke, LANL
!          Philip W. Jones, LANL
!          William H. Lipscomb, LANL
!
! 2006 ECH: moved exit timeLoop to prevent execution of unnecessary timestep
! 2006 ECH: Streamlined for efficiency 
! 2006 ECH: Converted to free source form (F90)
! 2007 BPB: Modified Delta-Eddington shortwave interface
! 2007 MV : Moved thermodynamic and dynamics step routines to ice_step_mod
!
! !INTERFACE:
!

      module CICE_RunMod
!
! !USES:
!
#ifdef USE_ESMF
      use esmf_mod
#endif
      use ice_step_mod
      use ice_calendar
      use ice_diagnostics
      use ice_timers
      use ice_prescribed_mod
      use ice_coupling
      use ice_exit
      use ice_history	
      use ice_restart 	
#ifdef popcice
      use ice_to_drv, only: to_drv
#endif

      implicit none
      private
      save

! !PUBLIC MEMBER FUNCTIONS:

      public :: CICE_Run
!
!EOP
!
!=======================================================================

      contains

!=======================================================================
!BOP
!
! !ROUTINE: CICE_Run - advances CICE model forward in time
!
! !DESCRIPTION:
!
!  This is the main driver routine for advancing CICE forward in time.
!  Accepts forcing fields at beginning of time step and returns the
!  model state at the end of the time step.
!
!  The module is divided into three parts that are called independently:
!  step_therm1, step_therm2, and step_dynamics.  The thermodynamics is
!  split up such that the fields needed for coupling are computed in
!  step_therm1, and the rest of the work is done in step_therm2.
!
! !REVISION HISTORY:
!
!  author Elizabeth C. Hunke, LANL
!         Philip W. Jones, LANL
!         William H. Lipscomb, LANL
!
! !INTERFACE:
!

      subroutine CICE_Run(CICE_Comp,  importState, exportState, &
                          synchClock, errorCode)

!
! !USES:
!
! !INPUT/OUTPUT PARAMETERS:

#ifdef USE_ESMF

      type (ESMF_GridComp), intent(inout) :: &
           CICE_Comp            ! defined ESMF component for CICE

      type (ESMF_State), intent(in) :: &
           importState          ! CICE import state (forcing data)

      type (ESMF_State), intent(out) :: &
           exportState          ! CICE export state (surface fields)

      type (ESMF_Clock), intent(inout) :: &
           synchClock           ! ESMF clock to check init time

      integer (int_kind), intent(inout) :: &
           errorCode            ! returns an error code if any init fails

#else
! declare as integer dummy arguments

      integer (int_kind), intent(inout), optional :: &
           CICE_Comp        , & ! dummy argument
           importState      , & ! dummy argument
           exportState      , & ! dummy argument
           synchClock       , & ! dummy argument
           errorCode            ! dummy argument

#endif

!
!EOP
!BOC
!
   !--------------------------------------------------------------------
   !  local variables
   !--------------------------------------------------------------------

      real (dbl_kind) :: &
           coupledInterval      ! time (seconds) for each coupling interval

      integer (kind=int_kind) :: k

   !--------------------------------------------------------------------
   !  initialize error code and step timer
   !--------------------------------------------------------------------

      call ice_timer_start(timer_step)   ! start timing entire run

#ifdef ESMF  
      errorCode = ESMF_SUCCESS

   !--------------------------------------------------------------------
   !  check clock to make sure models agree on time info
   !--------------------------------------------------------------------
   !
   !   call CICE_CoupledCheckTime(synchClock, 'start', errorCode)
   !
   !   if (errorCode /= ESMF_SUCCESS) then
   !      write (nu_diag,*) &
   !         '(ice) CICE_Run: error in clock synchronization'
   !      return
   !   endif
   !
   !UNCOMMENT THESE TWO SECTIONS FOR COUPLED MODEL SIMULATIONS WITH ESMF
   !--------------------------------------------------------------------
   !  reset timer to stop when requested by input clock
   !--------------------------------------------------------------------
   !
   ! Use appropriate ESMF Clock query functions to determine the
   ! coupling interval in seconds.  Because this depends on how the
   ! coupled model driver is using an ESMF clock, this is left to
   ! the user of the coupled model.
   !
   !coupledInterval = ?
   !
   ! reset number of time steps based on coupling interval
   !
   !npt = nint(coupledInterval/dt)
   !
   !--------------------------------------------------------------------
   !  extract data from import state
   !--------------------------------------------------------------------
   
      call CICE_CoupledExtractImport(importState, errorCode)
   
      if (errorCode /= ESMF_SUCCESS) then
         write(nu_diag,*) &
              '(ice) CICE_Run: error extracting data from import state'
         return
      endif
#endif

   !--------------------------------------------------------------------
   ! timestep loop
   !--------------------------------------------------------------------

      timeLoop: do

         istep  = istep  + 1    ! update time step counters
         istep1 = istep1 + 1
         time = time + dt       ! determine the time and date
         call calendar(time)    ! at the end of the timestep

#ifndef coupled
         call get_forcing_atmo     ! atmospheric forcing from data
         call get_forcing_ocn(dt)  ! ocean forcing from data
#endif

         if (stop_now >= 1) exit timeLoop

         call ice_step

#ifdef USE_ESMF
         call CICE_CoupledAccumulateExport(errorCode)
#endif

      enddo timeLoop

   !--------------------------------------------------------------------
   ! end of timestep loop
   !--------------------------------------------------------------------

      call ice_timer_stop(timer_step)   ! end timestepping loop timer     


#ifdef USE_ESMF
   !--------------------------------------------------------------------
   !  check clock to make sure models agree on time info
   !--------------------------------------------------------------------
   !
   !   call CICE_CoupledCheckTime(synchClock, 'stop', errorCode)
   !
   !   if (errorCode /= ESMF_SUCCESS) then
   !      write(nu_diag,*)
   !          '(ice) CICE_Run: error in stop time synchronization'
   !      return
   !   endif
   !
   !--------------------------------------------------------------------
   !  fill the export state
   !--------------------------------------------------------------------

      call CICE_CoupledFillExport(exportState, errorCode)

      if (errorCode /= ESMF_SUCCESS) then
         write(nu_diag,*) &
              '(ice) CICE_Run: error filling export state'
         return
      endif
#endif
!
!EOC
!

      end subroutine CICE_Run

!=======================================================================

      subroutine ice_step

      integer (kind=int_kind) :: k

         call init_mass_diags   ! diagnostics per timestep

      !-----------------------------------------------------------------
      ! thermodynamics
      !-----------------------------------------------------------------

         call step_therm1 (dt)  ! pre-coupler thermodynamics

#ifdef popcice
         call to_drv
#endif

         call step_therm2 (dt)  ! post-coupler thermodynamics

      !-----------------------------------------------------------------
      ! dynamics, transport, ridging
      !-----------------------------------------------------------------

         do k = 1, ndyn_dt
            call step_dynamics (dyn_dt)
         enddo

      !-----------------------------------------------------------------
      ! write data
      !-----------------------------------------------------------------

         call ice_timer_start(timer_readwrite)  ! reading/writing

         if (mod(istep,diagfreq) == 0) call runtime_diags(dt) ! log file

         call ice_write_hist (dt)    ! history file

         if (write_restart == 1) call dumpfile ! dumps for restarting

         call ice_timer_stop(timer_readwrite)  ! reading/writing

      end subroutine ice_step
    
!=======================================================================

      end module CICE_RunMod

!=======================================================================
