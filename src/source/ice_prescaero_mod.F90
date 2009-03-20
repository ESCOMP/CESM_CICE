!===================================================================
!BOP 
!
! !MODULE: ice_prescaero_mod - Prescribed Ice Model
!
! !DESCRIPTION:
!
! The prescribed aerosol model reads in aerosol deposition data from a netCDF
! file. Regridding and data cycling capabilities are included.
!
! !REVISION HISTORY:
!  SVN:$Id: ice_prescaero_mod.F90 40 2006-12-01 19:09:30Z eclare $
!
! 2008-Sep-24 - D.A. Bailey, first version.
!
! !INTERFACE: ----------------------------------------------------------
 
module ice_prescaero_mod

! !USES:

   use shr_stream_mod
   use shr_map_mod
   use shr_ncread_mod
   use shr_string_mod
   use shr_sys_mod

   use ice_broadcast
   use ice_communicate, only : my_task, master_task
   use ice_kinds_mod
   use ice_fileunits
   use ice_exit,       only : abort_ice
   use ice_domain_size, only : nx_global, ny_global, ncat, nilyr, max_blocks, &
                               n_aero
   use ice_constants
   use ice_blocks,     only : nx_block, ny_block
   use ice_domain,     only : nblocks, distrb_info
   use ice_grid,       only : TLAT,TLON,hm,tmask,tarea
   use ice_calendar,   only : idate, sec
   use ice_itd,        only : ilyr1, hin_max
   use ice_work,       only : work_g1, work_g2

   implicit none
   save

   private ! except


! !PUBLIC TYPES:

! !PUBLIC MEMBER FUNCTIONS:

   public :: ice_prescaero_init      ! initialize input data stream
   public :: ice_prescaero_run       ! get time slices and time interp
   public :: ice_prescaero_readField ! reads field from input data file
   public :: ice_prescaero_phys      ! set prescribed aerosols

! !PUBLIC DATA MEMBERS:

   logical(kind=log_kind)      , public :: prescribed_aero      ! true if prescribed aerosols
   integer(kind=int_kind)      , public :: stream_year_first_aero   ! first year in stream to use
   integer(kind=int_kind)      , public :: stream_year_last_aero    ! last year in stream to use
   integer(kind=int_kind)      , public :: model_year_align_aero    ! align stream_year_first_aero 
                                                               ! with this model year
   character(len=char_len_long), public :: stream_fldVarName_aero
   character(len=char_len_long), public :: stream_fldFileName_aero
   character(len=char_len_long), public :: stream_domTvarName_aero
   character(len=char_len_long), public :: stream_domXvarName_aero
   character(len=char_len_long), public :: stream_domYvarName_aero
   character(len=char_len_long), public :: stream_domAreaName_aero
   character(len=char_len_long), public :: stream_domMaskName_aero
   character(len=char_len_long), public :: stream_domFileName_aero
   logical(kind=log_kind)      , public :: prescribed_aero_fill ! true if data fill required

!EOP

   real(kind=dbl_kind),    allocatable :: dataXCoord(:,:)  ! input data longitudes 
   real(kind=dbl_kind),    allocatable :: dataYCoord(:,:)  ! input data latitudes
   integer(kind=int_kind),    allocatable :: dataMask(:,:)    ! input data mask
   real(kind=dbl_kind),    allocatable :: dataUB(:,:)      ! upper bound on model domain
   real(kind=dbl_kind),    allocatable :: dataLB(:,:)      ! lower bound on model domain

   type(shr_stream_streamType), save  :: aero_stream        ! aero data stream
   type(shr_map_mapType)        :: cice_map           ! used by shr_map_mapSet 
   type(shr_map_mapType)        :: cice_fill          ! used by shr_map_mapSet

   real(kind=dbl_kind), allocatable :: dataInLB(:,:,:)  ! input data LB
   real(kind=dbl_kind), allocatable :: dataInUB(:,:,:)  ! input data UB
   real(kind=dbl_kind), allocatable :: dataSrcLB(:,:)   ! reformed data for mapping
   real(kind=dbl_kind), allocatable :: dataSrcUB(:,:)   ! reformed data for mapping
   real(kind=dbl_kind), allocatable :: dataDstLB(:,:)   ! output from mapping
   real(kind=dbl_kind), allocatable :: dataDstUB(:,:)   ! output from mapping
   real(kind=dbl_kind), allocatable :: dataOutLB(:,:,:) ! output for model use
   real(kind=dbl_kind), allocatable :: dataOutUB(:,:,:) ! output for model use

   real(kind=dbl_kind), allocatable :: aero_lb(:,:,:,:) ! scattered aerosols
   real(kind=dbl_kind), allocatable :: aero_ub(:,:,:,:) ! scattered aerosols
   real(kind=dbl_kind), allocatable :: aero_loc(:,:,:,:) ! scattered aerosols
   real(kind=dbl_kind)              :: worka(nx_block,ny_block,max_blocks)

   logical(kind=log_kind) :: regrid                      ! true if remapping required

   integer(kind=int_kind) :: dateUB, dateLB, secUB, secLB
   integer(kind=int_kind) :: nlon           ! longitudes in netCDF data file
   integer(kind=int_kind) :: nlat           ! latitudes  in netCDF data file
   integer(kind=int_kind) :: nflds          ! number of fields in list
   integer(kind=int_kind) :: n              ! counter

   character(len=char_len_long) :: fldList      ! list of fields in data stream
   character(len=char_len)      :: fldName      ! name of field in stream

   logical :: read_data     ! must read in new ice coverage data slice

!=======================================================================
contains
!=======================================================================
!BOP ===================================================================
!
! !IROUTINE: ice_prescaero_init --  initialize data stream information
!
! !DESCRIPTION:
! (1) Initialize data stream information.  
! (2) Check input data domain - currently supports a regular lat-lon grid
!     or cpl6 history output.   If input data domain does not match ice
!     model domain, initialize mapping weights and set 'regrid' to true.
!
! !REVISION HISTORY:
!     2008-Sep-24 - D.A. Bailey - first version
!
! !INTERFACE: -----------------------------------------------------------

subroutine ice_prescaero_init(prescribed_aero_in)

! !USES:

   use ice_gather_scatter, only : gather_global

   implicit none

! !INPUT/OUTPUT PARAMETERS:

!EOP
   !----- Local ------
   real(kind=dbl_kind), allocatable :: work4d(:,:,:,:) ! 4D work array
   integer(kind=int_kind), allocatable :: cice_mask_g(:,:) ! global aero land mask
   real(kind=dbl_kind)     :: aice_max                 ! maximun ice concentration
   character(len=char_len_long) :: first_data_file     ! first data file in stream
   character(len=char_len_long) :: domain_info_fn      ! file with domain info
   character(len=char_len_long) :: data_path           ! path/location of stream data files
   integer(kind=int_kind)  :: ndims                    ! # dimensions in domain info
   character(len=char_len) :: timeName                 ! domain time var name
   character(len=char_len) :: latName                  ! domain latitude var name
   character(len=char_len) :: lonName                  ! domain longitude var name
   character(len=char_len) :: maskName                 ! domain mask var name
   character(len=char_len) :: areaName                 ! domain area var name
   logical(kind=log_kind)  :: check                    ! true if field is found

   integer(kind=int_kind)  :: nml_error ! namelist i/o error flag

   !----- formats -----
   character(*),parameter :: subName = "('ice_prescaero_init')"
   character(*),parameter :: F00 = "('(ice_prescaero_init) ',4a)"
   character(*),parameter :: F01 = "('(ice_prescaero_init) ',a,2i6)"
   character(*),parameter :: F02 = "('(ice_prescaero_init) ',a,2g20.13)"

   logical(kind=log_kind), optional, intent(in) :: prescribed_aero_in

! ech moved from ice_init.F
   namelist /ice_prescaero_nml/  prescribed_aero, prescribed_aero_fill, &
      stream_year_first_aero ,stream_year_last_aero  ,model_year_align_aero, &
      stream_fldVarName_aero ,stream_fldFileName_aero, &
      stream_domTvarName_aero,stream_domXvarName_aero,stream_domYvarName_aero, &
      stream_domAreaName_aero,stream_domMaskName_aero,stream_domFileName_aero

   ! default values for namelist
   prescribed_aero        = .false.          ! if true, prescribe ice
   stream_year_first_aero      = 1                ! first year in  pice stream to use
   stream_year_last_aero       = 1                ! last  year in  pice stream to use
   model_year_align_aero       = 1                ! align stream_year_first_aero with this model year
   stream_fldVarName_aero      = ' '
   stream_fldFileName_aero     = ' '
   stream_domTvarName_aero     = 'time'
   stream_domXvarName_aero     = 'xc'
   stream_domYvarName_aero     = 'yc'
   stream_domFileName_aero     =  ' '
   prescribed_aero_fill    = .false.           ! true if pice data fill required

   ! read from input file
   call get_fileunit(nu_nml)
   if (my_task == master_task) then
      open (nu_nml, file=nml_filename, status='old',iostat=nml_error)
      if (nml_error /= 0) then
         nml_error = -1
      else
         nml_error =  1
      endif
      do while (nml_error > 0)
         read(nu_nml, nml=ice_prescaero_nml,iostat=nml_error)
         if (nml_error > 0) read(nu_nml,*)  ! for Nagware compiler
      end do
      if (nml_error == 0) close(nu_nml)
   endif
   call release_fileunit(nu_nml)

   call broadcast_scalar(nml_error,master_task)

   if (nml_error /= 0) then
      call abort_ice ('ice: Namelist read error in ice_prescaero_mod')
   endif

   call broadcast_scalar(prescribed_aero,master_task)
   call broadcast_scalar(stream_year_first_aero,master_task)
   call broadcast_scalar(stream_year_last_aero,master_task)
   call broadcast_scalar(model_year_align_aero,master_task)
   call broadcast_scalar(stream_fldVarName_aero,master_task)
   call broadcast_scalar(stream_fldFileName_aero,master_task)
   call broadcast_scalar(stream_domTvarName_aero,master_task)
   call broadcast_scalar(stream_domXvarName_aero,master_task)
   call broadcast_scalar(stream_domYvarName_aero,master_task)
   call broadcast_scalar(stream_domAreaName_aero,master_task)
   call broadcast_scalar(stream_domMaskName_aero,master_task)
   call broadcast_scalar(stream_domFileName_aero,master_task)
   call broadcast_scalar(prescribed_aero_fill,master_task)

   if (present(prescribed_aero_in)) prescribed_aero = prescribed_aero_in

   if (.not.prescribed_aero) return

   if (my_task == master_task) then
      write(nu_diag,*) ' prescribed_aero            = ', prescribed_aero
      write(nu_diag,*) ' stream_year_first_aero         = ', stream_year_first_aero
      write(nu_diag,*) ' stream_year_last_aero          = ', stream_year_last_aero
      write(nu_diag,*) ' model_year_align_aero          = ', model_year_align_aero
      write(nu_diag,*) ' prescribed_aero_fill       = ', prescribed_aero_fill
      !TODO: add above variables

! ech moved from ice_diagnostics.F
! set print_global to .false. in ice_in to prevent global diagnostics
      write (nu_diag,*)   'This is the prescribed aerosol option.'
   endif

! end ech changes

   !------------------------------------------------------------------
   ! Create integer CICE mask with global dimensions
   !------------------------------------------------------------------
   if (my_task == master_task) then

      allocate (work_g1(nx_global,ny_global),work_g2(nx_global,ny_global),&
      &  cice_mask_g(nx_global,ny_global))

   else

      allocate (work_g1(1,1),work_g2(1,1),&
      &  cice_mask_g(1,1))

   end if

   call gather_global(work_g1, hm, master_task, distrb_info)

   if (my_task == master_task) then

      cice_mask_g = work_g1     ! Convert to integer array

   endif

   call gather_global(work_g1, TLAT, master_task, distrb_info)
   call gather_global(work_g2, TLON, master_task, distrb_info)

   if (my_task == master_task) then

      work_g1(:,:) = work_g1(:,:)*rad_to_deg
      work_g2(:,:) = work_g2(:,:)*rad_to_deg

      !---------------------------------------------------------------------
      ! Parse info file, initialize aero_stream datatype and load with info
      !---------------------------------------------------------------------
      aero_stream%nFiles           = 0  
      aero_stream%file(:)%name     = 'not_set' 
      aero_stream%file(:)%nt       = 0
      aero_stream%file(:)%haveData = .false.
      aero_stream%yearFirst        = stream_year_first_aero
      aero_stream%yearLast         = stream_year_last_aero
      aero_stream%yearAlign        = model_year_align_aero
      aero_stream%fldListFile      = ' '
      aero_stream%fldListModel     = ' '
      aero_stream%FilePath         = ' '
      aero_stream%domFilePath      = ' '
      aero_stream%k_lvd            = -1 
      aero_stream%n_lvd            = -1 
      aero_stream%found_lvd        = .false.
      aero_stream%k_gvd            = -1 
      aero_stream%n_gvd            = -1 
      aero_stream%found_gvd        = .false.
      
      aero_stream%dataSource   = 'cice ifrac/sst file'
      aero_stream%fldListFile  =  stream_fldVarName_aero

      !build logic hear to determine how many names are on the input file
      aero_stream%File(1)%name =  stream_fldFileName_aero
      aero_stream%nFiles       =  1 

      aero_stream%domTvarName  =  stream_domTvarName_aero
      aero_stream%domXvarName  =  stream_domXvarName_aero
      aero_stream%domYvarName  =  stream_domYvarName_aero
      aero_stream%domAreaName  =  stream_domAreaName_aero
      aero_stream%domMaskName  =  stream_domMaskName_aero
      aero_stream%domFileName  =  stream_domFileName_aero
      aero_stream%init         =  .true.

      !---------------------------------------------------------------------
      ! Check that ice cover data exists
      ! Assumes that stream has one field, ice fraction
      !---------------------------------------------------------------------
      call shr_stream_getFileFieldList(aero_stream,fldList)
      nflds = shr_string_listGetNum(fldList)                ! How many fields?

      do n=1,nflds

      call shr_string_listGetName(fldList,n,fldName) ! Get name of field

      call shr_stream_getFirstFileName(aero_stream,first_data_file,data_path)
      call shr_stream_getFile         (data_path,first_data_file)
      check = shr_ncread_varExists(first_data_file,fldName) 

      if (.not.check) then
         write(nu_diag,F00) "ERROR: field does not exist"
         call abort_ice(subName)
      end if

      enddo

      !---------------------------------------------------------------------
      ! Get size of the input data domain and allocate arrays
      !---------------------------------------------------------------------
      call shr_stream_getDomainInfo(aero_stream,data_path,domain_info_fn, &
         timeName, lonName, latName, maskName, areaName)

      call shr_ncread_varDimSizes(domain_info_fn,lonName,nlon)
      call shr_ncread_varDimSizes(domain_info_fn,latName,nlat)
      write (nu_diag,*) 'nlon,nlat',nlon,nlat
      call shr_stream_getFile(data_path,domain_info_fn)
      call shr_sys_flush(nu_diag)

      allocate(dataXCoord(nlon,nlat)) 
      allocate(dataYCoord(nlon,nlat))

      !------------------------------------------------------------------
      ! Read in lat-lon grid info from input data file, ALL output is 2D
      !------------------------------------------------------------------
      call shr_ncread_domain(domain_info_fn, lonName, dataXCoord, latName, dataYCoord)

      write (nu_diag,F02) 'min/max dataXCoord  = ',minval(dataXCoord) ,maxval(dataXCoord)
      write (nu_diag,F02) 'min/max dataYCoord  = ',minval(dataYCoord) ,maxval(dataYCoord)
      write (nu_diag,F02) 'min/max TLON        = ',minval(work_g2)    ,maxval(work_g2)
      write (nu_diag,F02) 'min/max TLAT        = ',minval(work_g1)    ,maxval(work_g1)
      write (nu_diag,F01) 'min/max cice_mask_g = ',minval(cice_mask_g),maxval(cice_mask_g)

      !------------------------------------------------------------------
      ! Determine if need to regrid
      !------------------------------------------------------------------
      call ice_prescaero_checkDomain(work_g2, work_g1, dataXCoord, dataYCoord, regrid)

      !------------------------------------------------------------------
      ! If regrid, read in domain again, obtain mask array and initialize mapping
      !------------------------------------------------------------------
      if (regrid) then
         allocate(dataMask(nlon,nlat))
         dataMask(:,:) = 1

         if (prescribed_aero_fill) then
            call shr_map_mapSet(cice_fill, work_g2, work_g1, cice_mask_g, &
            &                              work_g2, work_g1, cice_mask_g, &
            &                   name='cice_map',type='fill',algo='nnoni', &
            &                   mask='nomask')
         end if

         write (nu_diag,F00) 'Computing mapping weights'

         call shr_map_mapSet(cice_map, dataXCoord, dataYCoord, dataMask, &
         &                             work_g2,   work_g1,  cice_mask_g, &
         &                   name='cice_map',type='remap',algo='bilinear', &
         &                   mask='dstmask',vect='scalar')
         deallocate(dataMask)
      end if


      deallocate(dataXCoord,dataYCoord)

   end if           ! master_task

   !------------------------------------------------------------------
   ! Allocate input and output bundles
   !------------------------------------------------------------------

   call broadcast_scalar(nflds, master_task)

   allocate(dataOutLB(nx_global,ny_global,nflds))  ! output for model use
   allocate(dataOutUB(nx_global,ny_global,nflds))
   allocate(aero_lb(nx_block,ny_block,nflds,max_blocks))
   allocate(aero_ub(nx_block,ny_block,nflds,max_blocks))
   allocate(aero_loc(nx_block,ny_block,nflds,max_blocks))

   deallocate(work_g1,work_g2)
   deallocate(cice_mask_g)

   !-----------------------------------------------------------------
   ! For one ice category, set hin_max(1) to something big
   !-----------------------------------------------------------------
   if (ncat == 1) then
      hin_max(1) = 999._dbl_kind
   end if
    
end subroutine ice_prescaero_init
  
!=======================================================================
!BOP ===================================================================
!
! !IROUTINE: ice_prescaero_run -- Obtain two time slices of data for 
!                                  current time
!
! !DESCRIPTION:
! 
!  Finds two time slices bounding current model time, remaps if necessary
!
! !REVISION HISTORY:
!     2008-Sep-24 - D.A. Bailey - first version
!
! !INTERFACE: -----------------------------------------------------------

subroutine ice_prescaero_run(mDateIn, secIn)

! !USES:

   use shr_tInterp_mod
   use ice_constants, only: field_loc_center, field_type_scalar
   use ice_gather_scatter, only : scatter_global

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer(kind=int_kind), intent(in) :: mDateIn  ! Current model date (yyyymmdd)
   integer(kind=int_kind), intent(in) :: secIn    ! Elapsed seconds on model date

!EOP

   real(kind=dbl_kind)    :: fLB         ! weight for lower bound
   real(kind=dbl_kind)    :: fUB         ! weight for upper bound
   integer(kind=int_kind) :: mDateLB     ! model date    of lower bound
   integer(kind=int_kind) :: mDateUB     ! model date    of upper bound
   integer(kind=int_kind) :: dDateLB     ! data  date    of lower bound
   integer(kind=int_kind) :: dDateUB     ! data  date    of upper bound
   integer(kind=int_kind) ::   secUB     ! elap sec      of upper bound
   integer(kind=int_kind) ::   secLB     ! elap sec      of lower bound
   integer(kind=int_kind) ::    n_lb     ! t-coord index of lower bound
   integer(kind=int_kind) ::    n_ub     ! t-coord index of upper bound
   character(len=char_len_long) ::  fileLB     ! file containing  lower bound
   character(len=char_len_long) ::  fileUB     ! file containing  upper bound

   integer(kind=int_kind) :: mDateLB_old = -999
   integer(kind=int_kind) :: secLB_old = -999
   integer(kind=int_kind) :: i,j,n,icnt,iblk  ! loop indices and counter

   !------------------------------------------------------------------------
   ! get two time slices of monthly ice coverage data
   ! check that upper and lower time bounds have not changed
   !------------------------------------------------------------------------

   if (my_task ==  master_task) then

      !------------------------------------------------------------------
      ! Allocate input and output bundles
      !------------------------------------------------------------------

      call shr_stream_findBounds(aero_stream, mDateIn,secIn,             &
      &                          mDateLB, dDateLB, secLB, n_lb, fileLB, &
      &                          mDateUB, dDateUB, secUB, n_ub, fileUB)

      read_data = .false.
      if (mDateLB_old /= mDateLB .or. secLB_old /= secLB) then

         read_data = .true.

         allocate(dataInLB(nlon,nlat,nflds))      ! netCDF input data size
         allocate(dataInUB(nlon,nlat,nflds))

         do n=1,nflds
            call shr_string_listGetName(fldList,n,fldName) ! Get name of field

            call ice_prescaero_readField(fileLB, fldName, n_lb, dataInLB(:,:,n))
            call ice_prescaero_readField(fileUB, fldName, n_ub, dataInUB(:,:,n))
         enddo

         if (regrid) then

            allocate(dataSrcLB(nflds,nlon*nlat))     ! reformed data for mapping
            allocate(dataSrcUB(nflds,nlon*nlat))
            allocate(dataDstLB(nflds,nx_global*ny_global)) ! output from mapping
            allocate(dataDstUB(nflds,nx_global*ny_global))

            !-------------------------------------------------
            ! copy input data to arrays ordered for mapping
            !-------------------------------------------------
            do n=1,nflds
              icnt = 0
              do j=1,nlat
              do i=1,nlon
                 icnt = icnt + 1
                 dataSrcLB(n,icnt) = dataInLB(i,j,n)
                 dataSrcUB(n,icnt) = dataInUB(i,j,n)
              enddo
              enddo
            enddo

            !-------------------------------------------------
            ! map the ordered arrays
            !-------------------------------------------------
            call shr_map_mapData(dataSrcLB, dataDstLB, cice_map)
            call shr_map_mapData(dataSrcUB, dataDstUB, cice_map)

            if (prescribed_aero_fill) then
               call shr_map_mapData(dataDstLB, dataDstLB, cice_fill)
               call shr_map_mapData(dataDstUB, dataDstUB, cice_fill)
            end if
            !-------------------------------------------------
            ! copy mapped fields back to general 3d arrays
            !-------------------------------------------------
            do n=1,nflds
               icnt = 0
               do j=1,ny_global
               do i=1,nx_global
                  icnt = icnt + 1
                  dataOutLB(i,j,n) = dataDstLB(n,icnt)
                  dataOutUB(i,j,n) = dataDstUB(n,icnt)
               enddo
               enddo
            write (nu_diag,*) 'min/max dataOutLB = ',n,minval(dataOutLB(:,:,n)),maxval(dataOutLB(:,:,n))
            write (nu_diag,*) 'min/max dataOutUB = ',n,minval(dataOutUB(:,:,n)),maxval(dataOutUB(:,:,n))
            enddo
            deallocate(dataSrcLB)
            deallocate(dataSrcUB)
            deallocate(dataDstLB)
            deallocate(dataDstUB)
            deallocate(dataInLB)
            deallocate(dataInUB)

         else       ! no regrid
            dataOutLB = dataInLB
            dataOutUB = dataInUB

            deallocate(dataInLB)
            deallocate(dataInUB)
         end if    ! regrid

         mDateLB_old = mDateLB
         secLB_old = secLB

      end if       ! mDate
    
      call shr_tInterp_getFactors(mDateLB, secLB, mDateUB, secUB, &
      &                           mDateIn, secIN, fLB, fUB)

   end if    ! master_task

   call broadcast_scalar(read_data, master_task)
   call broadcast_scalar(fLB, master_task)
   call broadcast_scalar(fUB, master_task)

   if (read_data) then

  !-----------------------------------------------------------------
  ! Scatter aerosols to all processors
  !-----------------------------------------------------------------
   if (my_task == master_task) then
      allocate(work_g1(nx_global,ny_global))
   else
      allocate(work_g1(1,1))
   endif

   do n=1,nflds

      if (my_task == master_task) work_g1(:,:) = dataOutLB(:,:,n)
      call scatter_global(worka,   work_g1, &
                          master_task,  distrb_info, & 
                          field_loc_center, field_type_scalar)

      do iblk = 1,nblocks
      do j = 1,ny_block
      do i = 1,nx_block
         aero_lb(i,j,n,iblk) = worka(i,j,iblk)
      enddo
      enddo
      enddo
   
      if (my_task == master_task) work_g1(:,:) = dataOutUB(:,:,n)
      call scatter_global(worka,   work_g1, &
                          master_task,  distrb_info, & 
                          field_loc_center, field_type_scalar)

      do iblk = 1,nblocks
      do j = 1,ny_block
      do i = 1,nx_block
         aero_ub(i,j,n,iblk) = worka(i,j,iblk)
      enddo
      enddo
      enddo

   enddo ! nflds

   deallocate(work_g1)

   endif ! read_data

   !-----------------------------------------------------------------
   ! Time interpolate aerosol data
   !-----------------------------------------------------------------

   do iblk = 1,nblocks
   do n = 1,nflds
   do j = 1,ny_block
   do i = 1,nx_block
      aero_loc(i,j,n,iblk) = fLB*aero_lb(i,j,n,iblk) + fUB*aero_ub(i,j,n,iblk)
   enddo
   enddo
   enddo
   enddo


  !-----------------------------------------------------------------
  ! Set prescribed aerosols
  !-----------------------------------------------------------------

   call ice_prescaero_phys

end subroutine ice_prescaero_run

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: ice_prescaero_readField -- Read a field from input data
!
! !DESCRIPTION:
!     Read a field from input data
!
! !REVISION HISTORY:
!     2008-Sep-25 - D.A. Bailey - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine ice_prescaero_readField(fileName, fldName, nTime, fldRead)

! !USES:

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   character(*), intent(in)  :: fileName     ! netCDF input file
   character(*), intent(in)  :: fldName      ! name of field in stream
   integer(kind=int_kind), intent(in)  :: nTime          ! index of time slice to read
   real   (kind=dbl_kind), intent(out) :: fldRead(:,:) ! field read in

!EOP

   real(kind=dbl_kind), allocatable :: A4d(:,:,:,:)    ! 4D field array
   integer(kind=int_kind)           :: ni, nj          ! size of field to read

   ni = size(fldRead,1)
   nj = size(fldRead,2)

   allocate(A4d(ni,nj,1,1))

   call shr_ncread_field4dG(fileName, fldName, rfld=A4d, dim3i=nTime)
   fldRead(:,:) = A4d(:,:,1,1)

   deallocate(A4d)

end subroutine ice_prescaero_readField

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: ice_prescaero_checkDomain -- Compare input data domain and ice domain 
!
! !DESCRIPTION:
!     Check that input data domain matches that of ice model domain
!
! !REVISION HISTORY:
!     2008-Sep-24 - D.A. Bailey - first version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine ice_prescaero_checkDomain(aeroXCoord, aeroYCoord, dataXCoord, dataYCoord, regrid)

! !USES:

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   real(kind=dbl_kind),    intent(in) :: aeroXCoord(:,:) ! aero longitudes (degrees)
   real(kind=dbl_kind),    intent(in) :: aeroYCoord(:,:) ! aero latitudes  (degrees)
   real(kind=dbl_kind),    intent(in) :: dataXCoord(:,:) ! data longitudes (degrees)
   real(kind=dbl_kind),    intent(in) :: dataYCoord(:,:) ! data latitudes  (degrees)
   logical(kind=log_kind), intent(out):: regrid          ! true if remapping required

!EOP

   integer(kind=int_kind)           :: ni_aero, nj_aero     ! size of aero domain
   integer(kind=int_kind)           :: ni_data, nj_data     ! size of data domain
   real(kind=dbl_kind)              :: max_XDiff, max_YDiff ! max x,y grid diffs
   real(kind=dbl_kind), allocatable :: aeroXTemp(:,:)       ! aero grid 0 to 360

   !----- formats -----
   character(*),parameter :: F00 = "('(ice_prescaero_checkDomain) ',a)"
   character(*),parameter :: F01 = "('(ice_prescaero_checkDomain) ',a,2i6)"
   character(*),parameter :: F02 = "('(ice_prescaero_checkDomain) ',a,g20.13)"

   regrid = .false.
   !------------------------------------------------------------------------
   ! Check domain size
   !------------------------------------------------------------------------
   ni_aero = size(aeroXCoord,1)
   nj_aero = size(aeroXCoord,2)
   ni_data = size(dataXCoord,1)
   nj_data = size(dataXCoord,2)

   !------------------------------------------------------------------------
   ! Convert T grid longitude from -180 -> 180 to 0 to 360
   !------------------------------------------------------------------------
   allocate(aeroXTemp(ni_aero,nj_aero)) 
   aeroXTemp = aeroXCoord
   where (aeroXTemp >= c360) aeroXTemp = aeroXTemp - c360
   where (aeroXTemp < c0 )   aeroXTemp = aeroXTemp + c360

   if (ni_data == ni_aero .and. nj_data == nj_aero) then
      max_XDiff = maxval(abs(aeroXTemp  - dataXCoord))
      max_YDiff = maxval(abs(aeroYCoord - dataYCoord))
      if (max_XDiff > eps04) regrid = .true.
      if (max_YDiff > eps04) regrid = .true.

      write(nu_diag,F02) 'Maximum X difference = ', max_XDiff
      write(nu_diag,F02) 'Maximum Y difference = ', max_YDiff
      write(nu_diag,F00) 'CICE and input data domain sizes match'
      write(nu_diag,F01) 'CICE and data grids are', ni_aero,nj_aero
   else
      regrid = .true.
   end if
   deallocate(aeroXTemp) 

   if (regrid) then
      write(nu_diag,F00) 'Aerosols will be interpolated to CICE grid'
   else
      write(nu_diag,F00) 'Aerosol grid and CICE grid the same'
   end if

end subroutine ice_prescaero_checkDomain

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: ice_prescaero_phys -- set prescribed ice state and fluxes
!
! !DESCRIPTION:
!
! Set prescribed aerosol state using input aerosol deposition.
!
! !REVISION HISTORY:
!     2008-Sep-24    - D.A.Bailey - Original version
!
! !INTERFACE: ------------------------------------------------------------------
 
subroutine ice_prescaero_phys

! !USES:
 
   use ice_flux

   implicit none
 
! !INPUT/OUTPUT PARAMETERS:
 
!EOP

   !----- Local ------
   integer(kind=int_kind) :: i,j,n   ! longitude, latitude indices
   integer(kind=int_kind) :: iblk

   do iblk = 1,nblocks
   do j = 1,ny_block
   do i = 1,nx_block
      faero(i,j,1,iblk) = aero_loc(i,j, 1,iblk)
      faero(i,j,2,iblk) = aero_loc(i,j, 2,iblk)+aero_loc(i,j, 3,iblk)
!     Reduce dust to a single category
      faero(i,j,3,iblk) = aero_loc(i,j, 4,iblk)+aero_loc(i,j, 5,iblk) &
                        + aero_loc(i,j, 6,iblk)+aero_loc(i,j, 7,iblk) &
                        + aero_loc(i,j, 8,iblk)+aero_loc(i,j, 9,iblk) &
                        + aero_loc(i,j,10,iblk)+aero_loc(i,j,11,iblk)
!     do n=2,n_aero
!        faero(i,j,n,iblk) = aero_loc(i,j,2*n-2,iblk)+aero_loc(i,j,2*n-1,iblk)
!     enddo
   enddo                 ! i
   enddo                 ! j
   enddo                 ! iblk

end subroutine ice_prescaero_phys

!==============================================================================

end module ice_prescaero_mod

!==============================================================================
