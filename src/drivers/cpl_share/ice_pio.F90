!BOP ===========================================================================
!
! !MODULE: ice_pio -- reads and writes driver files
!
! !DESCRIPTION:
!  Writes netcdf files
!
! !REMARKS:
!
! !REVISION HISTORY:
!    Created by Mariana Vertenstein, June 2009
!
! !INTERFACE: ------------------------------------------------------------------

module ice_pio

  ! !USES:

  use shr_kind_mod, only: r8 => shr_kind_r8, in=>shr_kind_in
  use shr_kind_mod, only: cl => shr_kind_cl
  use shr_sys_mod , only: shr_sys_flush
  use ice_kinds_mod
  use ice_blocks
  use ice_broadcast
  use ice_communicate
  use ice_domain, only : nblocks, blocks_ice
  use ice_domain_size
  use ice_fileunits  
  use ice_exit
  use pio

  implicit none
  private

  ! !PUBLIC TYPES:

  ! none

  !PUBLIC MEMBER FUNCTIONS:

  interface ice_pio_initdecomp
     module procedure ice_pio_initdecomp_2d
     module procedure ice_pio_initdecomp_3d
     module procedure ice_pio_initdecomp_3d_inner
  end interface

  public ice_pio_init
  public ice_pio_finalize
  public ice_pio_initdecomp

  ! !PUBLIC DATA MEMBERS

  !EOP

  !----------------------------------------------------------------------------
  ! Local data
  !----------------------------------------------------------------------------

  integer, private      :: ice_pio_stride, ice_num_iotasks, &
	                    ice_pio_root,ice_pio_type

  type(iosystem_desc_t) :: ice_pio_subsystem

!===============================================================================

contains

!===============================================================================
!BOP
!
! !IROUTINE: ice_pio_finalize - finalize io for input or output
!
! !INTERFACE: 
   subroutine ice_pio_finalize
   
     integer  :: ierr

!    call pio_finalize(ice_pio_subsystem,ierr)

   end subroutine ice_pio_finalize
!===============================================================================
!BOP
!
! !IROUTINE: ice_pio_init - initialize io for input or output
!
! !INTERFACE: 
   subroutine ice_pio_init(mode, filename, File, clobber, cdf64)
!
! !DESCRIPTION:
!    Read the pio_inparm namelist and initialize the io subsystem
!
! !REVISION HISTORY:
!    2009-Feb-17 - J. Edwards - initial version
!
! !INPUT/OUTPUT PARAMETERS:
!
   implicit none
   character(len=*)     , intent(in)    :: mode
   character(len=*)     , intent(in)    :: filename
   type(file_desc_t)    , intent(inout) :: File
   logical,optional     , intent(in)    :: clobber
   logical,optional     , intent(in)    :: cdf64
!
!EOP
!
   integer (int_kind) :: &
      nml_error          ! namelist read error flag

   character(len=16)  :: ice_pio_type_name

   logical :: exists
   logical :: lclobber
   logical :: lcdf64
   integer :: status
   integer :: nmode
   character(*),parameter :: subName = '(ice_pio_wopen) '

   !  Input namelist
    
   namelist /ice_pio_nml/    &
        ice_num_iotasks, &
        ice_pio_stride,  &
        ice_pio_type_name 

   ice_pio_root      =  1  ! defaulted to 1

   ice_num_iotasks   = -1  ! set based on io_stride value when initialized < 0
   ice_pio_stride    = -1  ! set based on num_iotasks value when initialized < 0
   ice_pio_type_name = 'netcdf'

   if (my_task == master_task) then
      call get_fileunit(nu_nml)
      open (nu_nml, file=nml_filename, status='old',iostat=nml_error)
      if (nml_error /= 0) then
         nml_error = -1
      else
         nml_error =  1
      endif
      do while (nml_error > 0)
         read(nu_nml, nml=ice_pio_nml,iostat=nml_error)
	 if (nml_error > 0) read(nu_nml,*)  ! for Nagware compiler
      end do
      if (nml_error == 0) close(nu_nml)
      call ice_pio_set_params(ice_pio_type_name)
      call release_fileunit(nu_nml)

      write(nu_diag,*) 'CICE PIO parameter settings...'
      write(nu_diag,*) '  ice_pio_stride    = ',ice_pio_stride
      write(nu_diag,*) '  ice_num_iotasks   = ',ice_num_iotasks
      write(nu_diag,*) '  ice pio_type_name = ',ice_pio_type_name
      call shr_sys_flush(nu_diag) 
   endif

   call broadcast_scalar(nml_error, master_task)
   if (nml_error /= 0) then
      call abort_ice('ice: error reading pio_nml')
   endif

   call broadcast_scalar(ice_num_iotasks, master_task)
   call broadcast_scalar(ice_pio_root,    master_task)
   call broadcast_scalar(ice_pio_stride,  master_task)
   call broadcast_scalar(ice_pio_type,    master_task)

   call pio_init(my_task, MPI_COMM_ICE, ice_num_iotasks, &
	ice_pio_root, ice_pio_stride, PIO_REARR_BOX, ice_pio_subsystem)

   if (trim(mode) == 'write') then
      lclobber = .false.
      if (present(clobber)) lclobber=clobber
   
      lcdf64 = .false.
      if (present(cdf64)) lcdf64=cdf64

      if (File%fh<0) then
         ! filename not open
         inquire(file=trim(filename),exist=exists)
         if (exists) then
            if (lclobber) then
               nmode = pio_clobber
               if (lcdf64) nmode = ior(nmode,PIO_64BIT_OFFSET)
               status = pio_createfile(ice_pio_subsystem, File, ice_pio_type, trim(filename), nmode)
               if (my_task == master_task) then
                  write(nu_diag,*) subname,' create file ',trim(filename)
               end if
            else
               status = pio_openfile(ice_pio_subsystem, File, ice_pio_type, trim(filename), pio_write)
               if (my_task == master_task) then
                  write(nu_diag,*) subname,' open file ',trim(filename)
               end if
            endif
         else
            nmode = pio_noclobber
            if (lcdf64) nmode = ior(nmode,PIO_64BIT_OFFSET)
            status = pio_createfile(ice_pio_subsystem, File, ice_pio_type, trim(filename), nmode)
            if (my_task == master_task) then
               write(nu_diag,*) subname,' create file ',trim(filename)
            end if
         endif
      else
         ! filename is already open, just return
      endif
   end if

   if (trim(mode) == 'read') then
      inquire(file=trim(filename),exist=exists)
      if (exists) then
         status = pio_openfile(ice_pio_subsystem, File, ice_pio_type, trim(filename), pio_nowrite)
         call pio_seterrorhandling(File,PIO_RETURN_ERROR)
      else
         if(my_task==master_task) then
            write(nu_diag,*) 'ice_pio_ropen ERROR: file invalid ',trim(filename)
         end if
         call abort_ice('aborting in ice-pio_ropen with invalid file')
      endif
   end if
      
  end subroutine ice_pio_init

   

!===============================================================================
!BOP
!
! !IROUTINE: ice_pio_set_params - set pio parameters
!
! !INTERFACE: 
    subroutine ice_pio_set_params(ice_pio_type_name)
!
! !DESCRIPTION:
!    Set the pio parameters for the subsystem
!
! !USES:
!
   use shr_string_mod,only: shr_string_toUpper
!
! !INPUT/OUTPUT PARAMETERS:
!
   implicit none
   character(len=*), intent(in) :: ice_pio_type_name
!
!EOP
!
   character(len=16) :: tmpname
   integer (kind=int_kind) :: npes

   tmpname = shr_string_toupper(ice_pio_type_name)

   if (trim(tmpname) == 'NETCDF') then
      ice_pio_type = iotype_netcdf
   else if (trim(tmpname) == 'PNETCDF') then
      ice_pio_type = iotype_pnetcdf
   else
      if (my_task == master_task) then
         write(nu_diag,*)' Bad io_type argument - using iotype_netcdf'
      end if
      ice_pio_type = iotype_netcdf
   end if

   npes = get_num_procs()
   if      (ice_pio_stride>0 .and. ice_num_iotasks<0) then
      ice_num_iotasks = npes/ice_pio_stride
   else if (ice_num_iotasks>0 .and. ice_pio_stride<0) then
      ice_pio_stride = npes/ice_num_iotasks
   else if (ice_num_iotasks<0 .and. ice_pio_stride<0) then
      ice_pio_stride = max(min(npes,4),npes/8)
      ice_num_iotasks = npes/ice_pio_stride
   end if

   if (ice_pio_root<0) then
      ice_pio_root = 1
   endif
   ice_pio_root = min(ice_pio_root,npes-1)
   
    if(ice_pio_root + (ice_pio_stride)*(ice_num_iotasks-1) >= npes .or. &
       ice_pio_stride<=0 .or. ice_num_iotasks<=0 .or. ice_pio_root < 0 .or. &
       ice_pio_root > npes-1) then
       if (my_task == master_task) then
          write(nu_diag,*)&
               'ice_pio_stride or ice_num_iotasks out of bounds, resetting to defaults ',&
               ice_pio_stride, ice_num_iotasks, ice_pio_root
       end if
       ice_pio_stride = max(1,npes/4)
       ice_num_iotasks = npes/ice_pio_stride
       ice_pio_root = min(1,npes-1)
    end if
    if (my_task == master_task) then
       write(nu_diag,*)'Using io_type=',tmpname,' stride=',ice_pio_stride,&
            ' iotasks=',ice_num_iotasks,' root=',ice_pio_root
    end if

   end subroutine ice_pio_set_params

!================================================================================

   subroutine ice_pio_initdecomp_2d(iodesc)

      type(io_desc_t), intent(out) :: iodesc

      integer (kind=int_kind) :: &
          iblk,ilo,ihi,jlo,jhi,lon,lat,i,j,n,k

      type(block) :: this_block 

      integer(kind=int_kind), pointer :: dof2d(:)

      allocate(dof2d(nx_block*ny_block*nblocks))

      n=0
      do iblk = 1, nblocks
         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi
         
         do j=1,ny_block
         do i=1,nx_block
            n = n+1
            if (j < jlo .or. j>jhi) then
               dof2d(n) = 0
            else if (i < ilo .or. i > ihi) then
               dof2d(n) = 0
            else
               lon = this_block%i_glob(i)
               lat = this_block%j_glob(j)
               dof2d(n) = (lat-1)*nx_global + lon
            endif
         enddo !i
         enddo !j
      end do

      call pio_initdecomp(ice_pio_subsystem, pio_double, (/nx_global,ny_global/), &
           dof2d, iodesc)

      deallocate(dof2d)
 
    end subroutine ice_pio_initdecomp_2d

!================================================================================

   subroutine ice_pio_initdecomp_3d (ndim3, iodesc)

      integer(kind=int_kind), intent(in) :: ndim3
      type(io_desc_t), intent(out) :: iodesc

      integer (kind=int_kind) :: &
          iblk,ilo,ihi,jlo,jhi,lon,lat,i,j,n,k 

      type(block) :: this_block 

      integer(kind=int_kind), pointer :: dof3d(:)

      allocate(dof3d(nx_block*ny_block*nblocks*ndim3))

      n=0
      do iblk = 1, nblocks
         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi
         
         do k=1,ndim3
         do j=1,ny_block
         do i=1,nx_block
            n = n+1
            if (j < jlo .or. j>jhi) then
               dof3d(n)=0
            else if (i < ilo .or. i > ihi) then
               dof3d(n) = 0
            else
               lon = this_block%i_glob(i)
               lat = this_block%j_glob(j)
               dof3d(n) = ((lat-1)*nx_global + lon) + (k-1)*nx_global*ny_global 
            endif
         enddo !i
         enddo !j
         enddo !ndim3
      end do

      call pio_initdecomp(ice_pio_subsystem, pio_double, (/nx_global,ny_global,ndim3/), &
           dof3d, iodesc)

      deallocate(dof3d)

    end subroutine ice_pio_initdecomp_3d

!================================================================================

    subroutine ice_pio_initdecomp_3d_inner(ndim3, inner_dim, iodesc)

      integer(kind=int_kind), intent(in) :: ndim3
      logical, intent(in) :: inner_dim
      type(io_desc_t), intent(out) :: iodesc

      integer (kind=int_kind) :: &
          iblk,ilo,ihi,jlo,jhi,lon,lat,i,j,n,k 

      type(block) :: this_block 

      integer(kind=int_kind), pointer :: dof3d(:)

      allocate(dof3d(nx_block*ny_block*nblocks*ndim3))

      n=0
      do iblk = 1, nblocks
         this_block = get_block(blocks_ice(iblk),iblk)         
         ilo = this_block%ilo
         ihi = this_block%ihi
         jlo = this_block%jlo
         jhi = this_block%jhi
         
         do j=1,ny_block
         do i=1,nx_block
         do k=1,ndim3
            n = n+1
            if (j < jlo .or. j>jhi) then
               dof3d(n) = 0
            else if (i < ilo .or. i > ihi) then
               dof3d(n) = 0
            else
               lon = this_block%i_glob(i)
               lat = this_block%j_glob(j)
               dof3d(n) = k + ((lon-1) + (lat-1)*nx_global)*ndim3
            endif
         end do !ndim3
         enddo  !i
         enddo  !j
      end do    !iblk

      call pio_initdecomp(ice_pio_subsystem, pio_double, (/ndim3,nx_global,ny_global/), &
           dof3d, iodesc)

      deallocate(dof3d)

    end subroutine ice_pio_initdecomp_3d_inner


end module ice_pio
