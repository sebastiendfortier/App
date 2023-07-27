!---------------------------------- LICENCE BEGIN -------------------------------
! GEM - Library of kernel routines for the GEM numerical atmospheric model
! Copyright (C) 1990-2010 - Division de Recherche en Prevision Numerique
!                       Environnement Canada
! This library is free software; you can redistribute it and/or modify it
! under the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, version 2.1 of the License. This library is
! distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
! without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
! PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
! You should have received a copy of the GNU Lesser General Public License
! along with this library; if not, write to the Free Software Foundation, Inc.,
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!---------------------------------- LICENCE END ---------------------------------

module app_mpmd
   implicit none
   private
   save

   include 'mpif.h'

   public :: MiMd_init

   type :: component
      character(len=256) name_S
      integer :: color, wpe0, rank
   end type component
   type (component) :: MiMd_world(100)
   
   character(len=256), dimension(:), allocatable :: names_S
   integer :: MiMd_Wmyproc, MiMd_Wnumproc, MiMd_ncolors
   integer, dimension(:,:), allocatable :: partners
      
contains
      
   subroutine MiMd_init (F_component_S, F_color, F_COMM)
      use ISO_C_BINDING
      implicit none
      
      character(len=*), intent(IN) :: F_component_S
      integer, intent(IN ) :: F_color
      integer, intent(OUT) :: F_COMM

      integer i,j,ierr,color,numproc,myproc,required,provided
      integer :: me(4)!, win
!      integer, dimension (:), pointer :: glb_color
!      type(C_PTR), save :: base
!     
!--------------------------------------------------------------------
!
!MPI_THREAD_SINGLE: Only one thread will execute. 
!MPI_THREAD_FUNNELED: The process may be multi-threaded, but only the main thread will make MPI calls (all MPI calls are funneled to the main thread). 
!MPI_THREAD_SERIALIZED: The process may be multi-threaded, and multiple threads may make MPI calls, but only one at a time: MPI calls are not made concurrently from two distinct threads (all MPI calls are serialized). 
!MPI_THREAD_MULTIPLE: Multiple threads may call MPI, with no restrictions.
      required = MPI_THREAD_MULTIPLE
      call MPI_Init_thread (required, provided, ierr)
      call MPI_COMM_size (MPI_COMM_WORLD,MiMd_Wnumproc,ierr)
      call MPI_COMM_rank (MPI_COMM_WORLD,MiMd_Wmyproc ,ierr)

      if (provided /= required ) then
         if (MiMd_Wmyproc==0) write (6,'(/3x,a/)') 'FAILED in MPI_Init_thread: your system does NOT support MPI_THREAD_MULTIPLE -ABORT-'
         call MPI_finalize (ierr)
         stop
      endif
      
      ! write(6, '(A, I4, A, A, A, A)') 'Initializing component with color ', F_color, ' hostname ', trim(get_hostname()), ' name ', trim(F_component_S)
      
!     F_COMM=-999
!      call MPI_Init (ierr)
!!$      call MPI_WIN_ALLOCATE ( 4, 4, MPI_INFO_NULL, MPI_COMM_WORLD, base, win, ierr)
!!$      call C_F_POINTER ( base, glb_color, [1] )
!!$      if (MiMd_Wmyproc==0) glb_color=-1
!!$      call MPI_barrier(MPI_COMM_WORLD,ierr)      
      
      call MPI_Comm_split (MPI_COMM_WORLD, F_color, MiMd_Wmyproc, F_COMM, ierr)
      call MPI_barrier(MPI_COMM_WORLD,ierr)

      allocate (partners(4,MiMd_Wnumproc),names_S(MiMd_Wnumproc))
      
      call MPI_COMM_size (F_COMM,numproc,ierr)
      call MPI_COMM_rank (F_COMM,myproc ,ierr)     
      me(1) = F_color
      me(2) = MiMd_Wmyproc
      me(3) = myproc
      me(4) = numproc
      call MPI_barrier(MPI_COMM_WORLD,ierr)
      call MPI_Allgather(F_component_S,256,MPI_CHARACTER,names_S,256,MPI_CHARACTER,MPI_COMM_WORLD,ierr)
      call MPI_Allgather(me,4,MPI_INTEGER,partners,4,MPI_INTEGER,MPI_COMM_WORLD,ierr)
      color=-1 ;  MiMd_ncolors=0
      do i=1, MiMd_Wnumproc
         if ( partners(1,i)/=color) then
            MiMd_ncolors= MiMd_ncolors+1
            MiMd_world(MiMd_ncolors)%color  = partners(1,i)
            MiMd_world(MiMd_ncolors)%rank   = partners(4,i)
            MiMd_world(MiMd_ncolors)%name_S = names_S (  i)
            color= partners(1,i)
         end if
      end do
      do j=1, MiMd_ncolors
         do i=1, MiMd_Wnumproc
            if ( partners(1,i)==MiMd_world(j)%color) then
               if ( partners(3,i)==0) MiMd_world(j)%wpe0=partners(2,i)
            endif
         enddo
      enddo
!     
!--------------------------------------------------------------------
!
      return
   end subroutine MiMd_init

end module app_mpmd
