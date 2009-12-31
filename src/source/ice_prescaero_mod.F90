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
! 2009-Oct-15 - M.Vertensten, updated to work with new parallel data mods
!
! !INTERFACE: ----------------------------------------------------------
 
module ice_prescaero_mod

! !USES:

   use shr_strdata_mod
   use shr_dmodel_mod
   use shr_stream_mod
   use shr_string_mod
   use shr_sys_mod
   use shr_mct_mod
   use mct_mod

   use pio

   use ice_broadcast
   use ice_communicate, only : my_task, master_task, MPI_COMM_ICE
   use ice_kinds_mod
   use ice_fileunits
   use ice_exit,        only : abort_ice
   use ice_domain_size, only : nx_global, ny_global, ncat, nilyr, max_blocks, &
                               n_aero
   use ice_constants
   use ice_blocks,     only : nx_block, ny_block, block, get_block
   use ice_domain,     only : nblocks, distrb_info, halo_info, blocks_ice
   use ice_itd,        only : hin_max
   use ice_boundary,   only : ice_HaloUpdate

   implicit none
   save

   private ! except


! !PUBLIC TYPES:

! !PUBLIC MEMBER FUNCTIONS:

   public :: ice_prescaero_init      ! initialize input data stream
   public :: ice_prescaero_init2     ! second phase of initializeation
   public :: ice_prescaero_run       ! get time slices and time interp
   public :: ice_prescaero_phys      ! set prescribed aerosols

! !PUBLIC DATA MEMBERS:

   logical(kind=log_kind), public :: prescribed_aero  ! true if prescribed aerosols

!EOP

   logical(kind=log_kind) :: prescribed_aero_fill ! true if data fill required

   integer(kind=int_kind) :: stream_year_first_aero   ! first year in stream to use
   integer(kind=int_kind) :: stream_year_last_aero    ! last year in stream to use
   integer(kind=int_kind) :: model_year_align_aero    ! align stream_year_first_aero with 
                                                      ! this model year
   character(len=char_len_long) :: stream_fldVarName_aero
   character(len=char_len_long) :: stream_fldFileName_aero
   character(len=char_len_long) :: stream_domTvarName_aero
   character(len=char_len_long) :: stream_domXvarName_aero
   character(len=char_len_long) :: stream_domYvarName_aero
   character(len=char_len_long) :: stream_domAreaName_aero
   character(len=char_len_long) :: stream_domMaskName_aero
   character(len=char_len_long) :: stream_domFileName_aero
   character(len=char_len_long) :: stream_io_type_aero
   integer(kind=int_kind)       :: stream_num_iotasks_aero
   integer(kind=int_kind)       :: stream_io_root_aero
   integer(kind=int_kind)       :: stream_io_stride_aero
   character(len=char_len_long) :: stream_mapread_aero
   character(len=char_len_long) :: stream_mapwrite_aero
   character(len=char_len_long) :: stream_fillread_aero
   character(len=char_len_long) :: stream_fillwrite_aero

   type(shr_strdata_type)       :: sdat         ! prescribed data stream
   character(len=char_len_long) :: fldList      ! list of fields in data stream
   integer(kind=int_kind)       :: nflds        ! number of fields in list
   real(kind=dbl_kind), allocatable :: aero_loc(:,:,:,:) ! scattered aerosols
   character(len=*), parameter  :: shr_strdata_unset = 'NOT_SET'

!=======================================================================
contains
!=======================================================================
!BOP ===================================================================
!
! !IROUTINE: ice_prescaero_init --  initialize data stream information
!
! !DESCRIPTION:
! Initialize data stream information.  
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
   integer(kind=int_kind)  :: nml_error ! namelist i/o error flag

   logical(kind=log_kind), optional, intent(in) :: prescribed_aero_in

   namelist /ice_prescaero_nml/  prescribed_aero, prescribed_aero_fill, &
      stream_year_first_aero ,stream_year_last_aero  ,model_year_align_aero, &
      stream_fldVarName_aero ,stream_fldFileName_aero, &
      stream_domTvarName_aero,stream_domXvarName_aero,stream_domYvarName_aero, &
      stream_domAreaName_aero,stream_domMaskName_aero,stream_domFileName_aero, &
      stream_io_type_aero, stream_io_root_aero, stream_io_stride_aero, stream_num_iotasks_aero, &
      stream_mapwrite_aero, stream_mapread_aero, stream_fillwrite_aero, stream_fillread_aero


   ! default values for namelist
   prescribed_aero         = .false.          ! if true, prescribe ice
   stream_year_first_aero  = 1                ! first year in  pice stream to use
   stream_year_last_aero   = 1                ! last  year in  pice stream to use
   model_year_align_aero   = 1                ! align stream_year_first_aero with this model year
   stream_fldVarName_aero  = ' '
   stream_fldFileName_aero = ' '
   stream_domTvarName_aero = 'time'
   stream_domXvarName_aero = 'xc'
   stream_domYvarName_aero = 'yc'
   stream_domFileName_aero =  ' '
   prescribed_aero_fill    = .false.          ! true if pice data fill required
   stream_io_type_aero     = 'pio_netcdf'     ! values are 'netcdf','pio_netcdf',pio_pnetcdf' 
   stream_num_iotasks_aero = 1
   stream_io_root_aero     = 0
   stream_io_stride_aero   = 4
   stream_mapwrite_aero    = trim(shr_strdata_unset)
   stream_mapread_aero     = trim(shr_strdata_unset)
   stream_fillwrite_aero   = trim(shr_strdata_unset)
   stream_fillread_aero    = trim(shr_strdata_unset)

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
   call broadcast_scalar(prescribed_aero_fill,master_task)
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
   call broadcast_scalar(stream_io_type_aero,master_task)
   call broadcast_scalar(stream_io_root_aero,master_task)
   call broadcast_scalar(stream_io_stride_aero,master_task)
   call broadcast_scalar(stream_mapwrite_aero,master_task)
   call broadcast_scalar(stream_mapread_aero,master_task)
   call broadcast_scalar(stream_fillwrite_aero,master_task)
   call broadcast_scalar(stream_fillread_aero,master_task)

   if (present(prescribed_aero_in)) prescribed_aero = prescribed_aero_in

   if (my_task == master_task) then
      write(nu_diag,*) ' '
      write(nu_diag,*) 'ice_prescaero_nml settings:'
      write(nu_diag,*) '  prescribed_aero        = ',prescribed_aero
      write(nu_diag,*) '  stream_year_first_aero = ',stream_year_first_aero  
      write(nu_diag,*) '  stream_year_last_aero  = ',stream_year_last_aero   
      write(nu_diag,*) '  model_year_align_aero  = ',model_year_align_aero   
      write(nu_diag,*) '  stream_fldVarName_aero = ',trim(stream_fldVarName_aero)
      write(nu_diag,*) '  stream_fldFileName_aero= ',trim(stream_fldFileName_aero)
      write(nu_diag,*) '  stream_domTvarName_aero= ',trim(stream_domTvarName_aero)
      write(nu_diag,*) '  stream_domXvarName_aero= ',trim(stream_domXvarName_aero)
      write(nu_diag,*) '  stream_domYvarName_aero= ',trim(stream_domYvarName_aero)
      write(nu_diag,*) '  stream_domFileName_aero= ',trim(stream_domFileName_aero)
      write(nu_diag,*) '  prescribed_aero_fill   = ',prescribed_aero_fill
      write(nu_diag,*) '  stream_io_type_aero    = ',trim(stream_io_type_aero)
      write(nu_diag,*) '  stream_num_iotasks_aero= ',stream_num_iotasks_aero 
      write(nu_diag,*) '  stream_io_root_aero    = ',stream_io_root_aero     
      write(nu_diag,*) '  stream_io_stride_aero  = ',stream_io_stride_aero   
      write(nu_diag,*) '  stream_mapwrite_aero   = ',trim(stream_mapwrite_aero)    
      write(nu_diag,*) '  stream_mapread_aero    = ',trim(stream_mapread_aero)  
      write(nu_diag,*) '  stream_fillwrite_aero  = ',trim(stream_fillwrite_aero)   
      write(nu_diag,*) '  stream_fillread_aero   = ',trim(stream_fillread_aero)
      write(nu_diag,*) ' '
   endif

 end subroutine ice_prescaero_init

!=======================================================================
!BOP ===================================================================
!
! !IROUTINE: ice_prescaero_init2 --  second phase of initialization
!
! !DESCRIPTION:
!    Second phase of ice prescribed aero initialization - needed to 
!    work with new shr_strdata module derived type 
!
! !REVISION HISTORY:
!     2008-Sep-24 - D.A. Bailey - first version
!     2009-Oct-15 - M.Vertenstein - updated to work with new parallel shr
!
! !INTERFACE: -----------------------------------------------------------

subroutine ice_prescaero_init2(compid, gsmap, dom)

! !USES:

   implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer(kind=int_kind), intent(in)   :: compid
   type(mct_gsMap) :: gsmap
   type(mct_gGrid) :: dom
   
!EOP
   !----- Local ------
   integer(kind=int_kind), pointer :: dof(:)
   type(mct_sMat):: sMati
   
   !----- formats -----
   character(*),parameter :: subName = "('ice_prescaero_init2')"
   character(*),parameter :: F00 = "('(ice_prescaero_init2) ',4a)"

   character(len=char_len_long) :: first_data_file     ! first data file in stream
   character(len=char_len_long) :: domain_info_fn      ! file with domain info
   character(len=char_len_long) :: data_path           ! path/location of stream data files
   
   integer(kind=int_kind) :: ncid        ! netcdf file index
   character(char_len)    :: timeName    ! domain file: time variable name
   character(char_len)    ::  lonName    ! domain file: lon  variable name
   character(char_len)    ::  latName    ! domain file: lat  variable name
   character(char_len)    :: maskName    ! domain file: mask variable name
   character(char_len)    :: areaName    ! domain file: area variable name
   logical(kind=log_kind) :: check       ! true if field is found
   
   !---------------------------------------------------------------------
   ! Write diagnostic output
   !---------------------------------------------------------------------
   if (my_task == master_task) then
      write(nu_diag,*) ' prescribed_aero            = ', prescribed_aero
      write(nu_diag,*) ' stream_year_first_aero     = ', stream_year_first_aero
      write(nu_diag,*) ' stream_year_last_aero      = ', stream_year_last_aero
      write(nu_diag,*) ' model_year_align_aero      = ', model_year_align_aero
      write(nu_diag,*) ' prescribed_aero_fill       = ', prescribed_aero_fill
      ! ech moved from ice_diagnostics.F
      ! set print_global to .false. in ice_in to prevent global diagnostics
      write(nu_diag,*)   'This is the prescribed aerosol option.'
   endif
   
   !---------------------------------------------------------------------
   ! Parse info file, initialize sdat datatype and load with info
   ! Need gsmap and dom to finish initialization
   !---------------------------------------------------------------------
   if (trim(stream_io_type_aero) == 'netcdf') then
      sdat%io_type  = iotype_std_netcdf
   elseif (trim(stream_io_type_aero) == 'pio_netcdf') then
      sdat%io_type  = iotype_netcdf
   elseif (trim(stream_io_type_aero) == 'pio_pnetcdf') then
      sdat%io_type  = iotype_pnetcdf
   else
      write(nu_diag,F00) 'ERROR: unknown io_type, '//trim(stream_io_type_aero)
      call shr_sys_abort(subName//": unknown io_type, "//trim(stream_io_type_aero))
   endif
   if (my_task == master_task) then
      write(nu_diag,*)' Prescribed aero stream io type is ',trim(stream_io_type_aero)
   end if

   sdat%dataMode               = 'PRES_ICEAERO'
   sdat%nstreams               = 1  
   sdat%streams(:)             = shr_strdata_nullstr
   sdat%streams(1)             = 'prescribed aero'
   sdat%vectors(:)             = shr_strdata_nullstr
   sdat%fillalgo(:)            = 'nn'
   sdat%fillmask(:)            = 'nomask'
   sdat%fillwrit(:)            = trim(shr_strdata_unset)
   sdat%fillread(:)            = trim(shr_strdata_unset)
   sdat%mapalgo(:)             = 'bilinear'
   sdat%mapmask(:)             = 'dstmask'
   sdat%mapwrit(:)             = trim(shr_strdata_unset)
   sdat%mapread(:)             = trim(shr_strdata_unset)
   sdat%tintalgo(:)            = 'linear'
   sdat%nvectors               = 0
   sdat%io_stride              = stream_io_stride_aero
   sdat%io_root                = stream_io_root_aero
   sdat%num_iotasks            = stream_num_iotasks_aero
   sdat%num_agg                = 0
   sdat%lsize                  = mct_gsmap_lsize(gsmap,MPI_COMM_ICE)
   sdat%nxg                    = nx_global
   sdat%nyg                    = ny_global
   sdat%domainfile             = shr_strdata_nullstr

   sdat%stream(1)%init         = .true.
   sdat%stream(1)%nFiles       = 1
   sdat%stream(1)%dataSource   = 'prescribed aerososol data'
   sdat%stream(1)%fldListFile  = stream_fldVarName_aero
   sdat%stream(1)%fldListModel = stream_fldVarName_aero
   sdat%stream(1)%FilePath     = ' '
   sdat%stream(1)%domFilePath  = ' '
   sdat%stream(1)%file(1)%name = stream_fldFileName_aero
   sdat%stream(1)%yearFirst    = stream_year_first_aero 
   sdat%stream(1)%yearLast     = stream_year_last_aero
   sdat%stream(1)%yearAlign    = model_year_align_aero
   sdat%stream(1)%fldListModel = stream_fldVarName_aero
   sdat%stream(1)%fldListFile  = stream_fldVarName_aero
   sdat%stream(1)%domTvarName  = stream_domTvarName_aero
   sdat%stream(1)%domXvarName  = stream_domXvarName_aero
   sdat%stream(1)%domYvarName  = stream_domYvarName_aero
   sdat%stream(1)%domAreaName  = stream_domAreaName_aero
   sdat%stream(1)%domMaskName  = stream_domMaskName_aero
   sdat%stream(1)%domFileName  = stream_domFileName_aero

   sdat%fillwrit(1) = stream_fillwrite_aero
   sdat%fillread(1) = stream_fillread_aero
   sdat%mapwrit(1)  = stream_mapwrite_aero
   sdat%mapread(1)  = stream_mapread_aero

   !---------------------------------------------------------------------
   ! Initialize stream input pio subsystem 
   !---------------------------------------------------------------------
   call pio_init(my_task, MPI_COMM_ICE, sdat%num_iotasks, sdat%num_agg, sdat%io_stride, &
                 PIO_REARR_BOX, sdat%pio_subsystem, base=sdat%io_root)

   !---------------------------------------------------------------------
   ! Initialize model gsmap and model general grid 
   ! This sets sdat%gsmap and sdat%grid
   !---------------------------------------------------------------------
   call mct_gsmap_OrderedPoints( gsmap, my_task, dof )
   call mct_gsMap_init( sdat%gsmap, dof, MPI_COMM_ICE, compid, sdat%lsize, nx_global*ny_global )
   call mct_ggrid_init( sdat%grid, dom, sdat%lsize)    
   call mct_aVect_copy( dom%data, sdat%grid%data)
   deallocate(dof)

   !---------------------------------------------------------------------
   ! Check that prescribed aero forcing data exists
   ! Assumes that stream has one field
   ! Note: 1 index below refers to the fact that we have only 1 stream
   !---------------------------------------------------------------------
   call shr_stream_getDomainInfo(sdat%stream(1), data_path,domain_info_fn, timeName, &
        lonName, latName, maskName, areaName)
   call shr_dmodel_readgrid(sdat%gridR(1),sdat%gsmapR(1),sdat%strnxg(1),sdat%strnyg(1), &
        domain_info_fn, compid, MPI_COMM_ICE, '1d', lonName, latName, maskName, areaName)

   !---------------------------------------------------------------------
   ! Initialize pio decomp
   !---------------------------------------------------------------------
   sdat%lsizeR(1) = mct_gsmap_lsize(sdat%gsmapR(1),MPI_COMM_ICE)
   call mct_gsmap_OrderedPoints(sdat%gsmapR(1), my_task, dof) 
   call pio_initdecomp(sdat%pio_subsystem, pio_double, &
        (/sdat%strnxg(1),sdat%strnyg(1)/), dof, sdat%pio_iodesc(1))
   deallocate(dof)

   !---------------------------------------------------------------------
   ! Initialize mapping 
   !---------------------------------------------------------------------
   if (prescribed_aero_fill) then
      sdat%dofill(1) = .true.
   else
      sdat%dofill(1) = .false.
   end if

   if (shr_dmodel_gGridCompare(sdat%gridR(1),sdat%gsmapR(1),sdat%grid,sdat%gsmap, &
                               shr_dmodel_gGridCompareXYabs,MPI_COMM_ICE,0.01_dbl_kind)) then
      sdat%domaps(1) = .false.
   else
      sdat%domaps(1) = .true.
   end if

   if (sdat%dofill(1)) then
      if (trim(sdat%fillread(1)) == trim(shr_strdata_unset)) then
         call shr_dmodel_mapSet(sdat%sMatPf(1), &
              sdat%gridR(1),sdat%gsmapR(1),sdat%strnxg(1),sdat%strnyg(1), &
              sdat%gridR(1),sdat%gsmapR(1),sdat%strnxg(1),sdat%strnyg(1), &
              name='mapFill', type='cfill', &
              algo=trim(sdat%fillalgo(1)),mask=trim(sdat%fillmask(1)),vect='scalar', &
              compid=compid,mpicom=MPI_COMM_ICE)
         if (trim(sdat%fillwrit(1)) /= trim(shr_strdata_unset)) then
            if (my_task == master_task) write(nu_diag,F00)' writing ',trim(sdat%fillwrit(1))
            call shr_mct_sMatWritednc(sdat%sMatPf(1)%Matrix,sdat%fillwrit(1),compid,MPI_COMM_ICE)
         end if
      else
         if (my_task == master_task) write(nu_diag,F00)' reading ',trim(SDAT%fillread(1))
         call shr_mct_sMatReaddnc(sMati,SDAT%gsmapR(1),SDAT%gsmapR(1),'src', &
              filename=trim(SDAT%fillread(1)),mytask=my_task,mpicom=MPI_COMM_ICE)
         call mct_sMatP_Init(SDAT%sMatPf(1),sMati,SDAT%gsMapR(1),SDAT%gsmapR(1),0, MPI_COMM_ICE, compid)
         call mct_sMat_Clean(sMati)
      endif
   end if

   if (sdat%domaps(1)) then
      if (trim(sdat%mapread(1)) == trim(shr_strdata_unset)) then
         if (my_task == master_task) write(nu_diag,F00)' calling shr_dmodel_mapSet for remap'
         call shr_dmodel_mapSet(sdat%sMatPs(1), &
              sdat%gridR(1),sdat%gsmapR(1),sdat%strnxg(1),sdat%strnyg(1), &
              sdat%grid    ,sdat%gsmap    ,sdat%nxg      ,sdat%nyg      , &
              name='mapScalar', type='remap', &
              algo=trim(sdat%mapalgo(1)),mask=trim(sdat%mapmask(1)), vect='scalar', &
              compid=compid,mpicom=MPI_COMM_ICE)
         if (trim(sdat%mapwrit(1)) /= trim(shr_strdata_unset)) then
            if (my_task == master_task) write(nu_diag,F00)' writing ',trim(sdat%mapwrit(1))
            call shr_mct_sMatWritednc(sdat%sMatPs(1)%Matrix,sdat%mapwrit(1),compid,MPI_COMM_ICE)
         endif
      else
         if (my_task == master_task) write(nu_diag,F00)' reading ',trim(sdat%mapread(1))
         call shr_mct_sMatReaddnc(sMati,sdat%gsmapR(1),sdat%gsmap,'src', &
              filename=trim(sdat%mapread(1)),mytask=my_task,mpicom=MPI_COMM_ICE)
         call mct_sMatP_Init(sdat%sMatPs(1),sMati,sdat%gsMapR(1),sdat%gsmap,0, MPI_COMM_ICE, compid)
         call mct_sMat_Clean(sMati)
      endif
   else
      call mct_rearr_init(sdat%gsmapR(1), sdat%gsmap, MPI_COMM_ICE, sdat%rearrR(1))
   end if
      
   ! --- setup datatypes ---
   if (my_task == master_task) then
      call shr_stream_getModelFieldList(sdat%stream(1),fldList)
   endif
   call shr_mpi_bcast(fldList,MPI_COMM_ICE)
   call mct_aVect_init(sdat%avs(1)  ,rlist=fldList,lsize=sdat%lsize)
   call mct_aVect_init(sdat%avFLB(1),rlist=fldList,lsize=sdat%lsize)
   call mct_aVect_init(sdat%avFUB(1),rlist=fldList,lsize=sdat%lsize)
   call mct_aVect_init(sdat%avRLB(1),rlist=fldList,lsize=sdat%lsizeR(1))
   call mct_aVect_init(sdat%avRUB(1),rlist=fldList,lsize=sdat%lsizeR(1))

   nflds = shr_string_listGetNum(fldList)                
   allocate(aero_loc(nx_block,ny_block,nflds,max_blocks))

   !-----------------------------------------------------------------
   ! For one ice category, set hin_max(1) to something big
   !-----------------------------------------------------------------
   if (ncat == 1) then
      hin_max(1) = 999._dbl_kind
   end if
    
   if (my_task == master_task) then
      call shr_strdata_print(sdat,'SPRESAERO data')
   endif

 end subroutine ice_prescaero_init2
  
!=======================================================================
!BOP ===================================================================
!
! !IROUTINE: ice_prescaero_run -- Update prescribed aerosol
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
   implicit none

! !INPUT/OUTPUT PARAMETERS:

   integer(kind=int_kind), intent(in) :: mDateIn  ! Current model date (yyyymmdd)
   integer(kind=int_kind), intent(in) :: secIn    ! Elapsed seconds on model date

!EOP

   integer(kind=int_kind) :: i,j,n,ig,icnt,iblk ! loop indices and counter
   integer(kind=int_kind) :: ilo,ihi,jlo,jhi    ! beginning and end of physical domain
   type (block)           :: this_block         ! block index

   !------------------------------------------------------------------------
   ! Interpolate to new prescribed aerosols
   ! Will need to do halo update???
   !------------------------------------------------------------------------

   call shr_strdata_advance(sdat,mDateIn,SecIn,MPI_COMM_ICE,'cice_paero')

   !-----------------------------------------------------------------
   ! Time interpolate aerosol data
   !-----------------------------------------------------------------

   ig=0
   do iblk = 1, nblocks
      this_block = get_block(blocks_ice(iblk),iblk)         
      ilo = this_block%ilo
      ihi = this_block%ihi
      jlo = this_block%jlo
      jhi = this_block%jhi
      
      do j = jlo, jhi
      do i = ilo, ihi
         ig = ig+1
         do n = 1,nflds
            aero_loc(i,j,n,iblk) = sdat%avs(1)%rAttr(n,ig)
         end do
      end do
      end do	
   end do

   call ice_HaloUpdate (aero_loc,            halo_info,     &
                        field_loc_center,    field_type_scalar)

  !-----------------------------------------------------------------
  ! Set prescribed aerosols
  !-----------------------------------------------------------------

   call ice_prescaero_phys

end subroutine ice_prescaero_run

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
