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
   use mpi
   use app
   implicit none
   private
   save

   integer, parameter         :: MPMD_NONE            = 0
   integer, parameter, public :: MPMD_GEM_ID          = 1
   integer, parameter, public :: MPMD_GEM_IOSERVER_ID = 2
   integer, parameter, public :: MPMD_IRIS_ID         = 3
   integer, parameter, public :: MPMD_NEMO_ID         = 4

   integer, parameter, public :: MPMD_TEST1_ID = 1001
   integer, parameter, public :: MPMD_TEST2_ID = 1002
   integer, parameter, public :: MPMD_TEST3_ID = 1003
   integer, parameter, public :: MPMD_TEST4_ID = 1004
   integer, parameter, public :: MPMD_TEST5_ID = 1005

   !> Context that groups a set of components (executables) inside a single MPI communicator (world)
   !> Each \ref component should correspond to a single executable, launched through the same MPI command as the
   !> others in this context. The \ref component to which a certain PE belongs is self-declared. See \ref component.
   !> The context allows to query the list of who is participating in it, and to retrieve a communicator for
   !> any set of components within it.
   type, public :: mpmd_context
      private
      integer :: main_comm      = MPI_COMM_NULL    !< Communicator that groups all executables from this context
      integer :: world_rank     = -1               !< Global rank of this PE
      integer :: component_rank = -1               !< Local rank of this PE (within its component)
      type(component), pointer :: self_component => NULL()  !< Pointer to this PE's component
      !> Pointer to the list of components in this context
      type(component), dimension(:), pointer :: all_components => NULL()
      integer :: num_sets = 0    !< How many sets of components are stored in this context
      !> Pointer to the list of sets that are already stored in this context
      type(component_set), dimension(:), pointer :: sets => NULL()
      
   contains
      private
      procedure, pass, public :: init            => mpmd_context_init !< \copydoc mpmd_context_init
      procedure, pass, public :: finalize        => mpmd_context_finalize !< \copydoc mpmd_context_finalize
      procedure, pass, public :: get_own_comm    => mpmd_context_get_own_comm !< \copydoc mpmd_context_get_own_comm
      procedure, pass, public :: get_shared_comm => mpmd_context_get_shared_comm !< \copydoc mpmd_context_get_shared_comm
      procedure, pass, public :: has_component   => mpmd_context_has_component !< \copydoc mpmd_context_has_component

      procedure, pass :: find_set   => mpmd_context_find_set !< \private \copydoc mpmd_context_find_set
      procedure, pass :: create_set => mpmd_context_create_set !< \private \copydoc mpmd_context_create_set
   end type mpmd_context

   !> Derived type that describes a single component within an MPMD context. See \ref mpmd_context.
   !> It can describe components *other* than the one to which the current PE belongs.
   type :: component
      private
      integer :: id               = MPMD_NONE      !< ID of this component
      integer :: comm             = MPI_COMM_NULL  !< Communicator for the PEs of this component
      integer :: num_pes          = 0              !< How many PEs are members of this component
      logical :: already_shared = .false. !< Whether this component has been shared to other PEs of this PE's component
      !> Global ranks (in the main_comm of the context) of the members of this component
      integer, dimension(:), pointer :: ranks => NULL()
   end type component
      
   !> A series of components that share a communicator
   type :: component_set
      private
      integer, dimension(:), pointer :: component_ids !< IDs of the components in this set
      integer :: communicator = MPI_COMM_NULL !< Communicator shared by these components
   end type component_set

contains

   !> Initialize a common MPMD context by telling everyone who we are as a process.
   !> This is a collective call. Everyone who participate in it will know who else
   !> is on board and will be able to ask for a communicator in common with any other
   !> participant (or even multiple other participants at once).
   subroutine mpmd_context_init(this, component_id)
      implicit none
      class(mpmd_context), intent(inout) :: this          !< mpmd_context instance we are initializing
      integer,             intent(in)    :: component_id  !< Identifier by which the calling program will be known to the others

      integer :: main_comm
      integer :: ierr
      integer :: world_size, world_rank
      integer :: required, provided
      integer :: component_comm, roots_comm, dummy_comm

      integer, dimension(:), allocatable :: all_ids, unique_ids
      integer, dimension(:), allocatable :: root_world_ranks
      integer :: num_components, component_pos
      integer :: i
      integer :: root_rank, old_log_rank

      ! -------------------------
      ! Start by initializing MPI
      required = MPI_THREAD_MULTIPLE
      main_comm = MPI_COMM_WORLD
      
      call MPI_Init_thread (required, provided, ierr)
      call MPI_COMM_size (main_comm, world_size, ierr)
      call MPI_COMM_rank (main_comm, this % world_rank, ierr)

      app_ptr=app_init(0, component_id_to_name(component_id), '1.0-alpha', 'mpmd context attempt', 'now')
      call app_start()

      write(app_msg, '(A, I5, 1X, A)') 'Calling MPI_Init (thread)', component_id, component_id_to_name(component_id)
      call app_log(APP_DEBUG, app_msg)

      if (provided /= required) then
         if (this % world_rank == 0)                      &
            call app_log(APP_ERROR, 'In MPMD_context_init: your system does NOT support MPI_THREAD_MULTIPLE')
         call MPI_finalize (ierr)
         stop
      endif

      this % main_comm = main_comm

      ! ------------------------------------
      ! Determine which programs are present
      call MPI_Comm_split(main_comm, component_id, this % world_rank, component_comm, ierr)
      call MPI_Comm_rank(component_comm, this % component_rank, ierr)

      if (this % component_rank == 0) old_log_rank = app_logrank(this % world_rank)

      write(app_msg, '(A, A)') 'MPMD Context init: initializing component ', component_id_to_name(component_id)
      call app_log(APP_INFO, app_msg)

      if (this % world_rank == 0 .and. this % component_rank /= 0) then
         call app_log(APP_FATAL, 'Global root should also be the root of its own component')
      end if

      !---------------------------------
      ! Initialize individual components
      roots_comm = MPI_COMM_NULL
      dummy_comm = MPI_COMM_NULL
      if (this % component_rank == 0) then
         ! Use component ID as key so that the components are sorted in ascending order
         call MPI_Comm_split(main_comm, 0, component_id, roots_comm, ierr)
         call MPI_Comm_size(roots_comm, num_components, ierr)
         call MPI_Comm_rank(roots_comm, component_pos, ierr)
         component_pos = component_pos + 1

         write(app_msg, '(A, A, I3, A)')     &
            component_id_to_name(component_id), ': There are ', num_components, ' components present'
         call app_log(APP_DEBUG, app_msg)
      else
         call MPI_Comm_split(main_comm, 1, component_id, dummy_comm, ierr)
      end if

      !-------------------------------------------
      ! Transmit basic component information to every PE
      call MPI_Bcast(num_components, 1, MPI_INTEGER, 0, main_comm, ierr)
      allocate(this % all_components(num_components))
      allocate(root_world_ranks(num_components))

      ! Each component root should have a list of the world rank of every other root
      ! (we don't know which one is the WORLD root)
      if (this % component_rank == 0) then
         call MPI_Allgather(this % world_rank, 1, MPI_INTEGER, root_world_ranks, 1, MPI_INTEGER, roots_comm, ierr)
      end if

      ! Send the component roots to everyone
      call MPI_Bcast(root_world_ranks, num_components, MPI_INTEGER, 0, main_comm, ierr)
      call MPI_Bcast(component_pos, 1, MPI_INTEGER, 0, component_comm, ierr)

      ! Select which component belongs to this PE and initialize its struct within this PE
      this % self_component => this % all_components(component_pos)

      this % self_component % id = component_id
      call MPI_Comm_size(component_comm, this % self_component % num_pes, ierr)

      ! Send basic component information to everyone
      do i = 1, num_components
         call MPI_Bcast(this % all_components(i), storage_size(this % all_components(i)) / 8, MPI_BYTE,              &
                        root_world_ranks(i), main_comm, ierr)
      end do

      ! Self-component info that is only valid on a PE from this component
      this % self_component % comm = component_comm
      allocate(this % self_component % ranks(this % self_component % num_pes))

      !------------------------------------------------------------------------------------
      ! Determine global ranks of the PEs in this component and share with other components

      ! Each component gathers the world ranks of its members
      call MPI_Allgather(this % world_rank, 1, MPI_INTEGER, this % self_component % ranks, 1, MPI_INTEGER,           &
                         component_comm, ierr)
      this % self_component % already_shared = .true.

      ! Every component root (one by one) sends the ranks of its members to every other component root
      ! This way seems easier than to set up everything to be able to use MPI_Allgatherv
      ! We only send the list of all other ranks to the roots (rather than aaalllll PEs) to avoid a bit
      ! of communication and storage
      if (this % component_rank == 0) then
         do i = 1, num_components
            if (.not. associated(this % all_components(i) % ranks)) then
               allocate(this % all_components(i) % ranks(this % all_components(i) % num_pes))
            end if
            call MPI_Bcast(this % all_components(i) % ranks,                                                         &
                           this % all_components(i) % num_pes, MPI_INTEGER,                                          &
                           i - 1, roots_comm, ierr)
         end do
      end if

      ! Print some info about the components, for debugging
      if (this % world_rank == 0) then
         write(app_msg, '(A, I4)') 'Num components = ', num_components
         call app_log(APP_DEBUG, app_msg)
         call app_log(APP_DEBUG, 'Components are .........')
         do i = 1, num_components
            call print_component(this % all_components(i), .false.)
         end do
      end if

   end subroutine mpmd_context_init

   !> \return The communicator for all PEs part of the same component as me.
   pure function mpmd_context_get_own_comm(this) result(comm)
      implicit none
      class(mpmd_context), intent(in) :: this   !< mpmd_context instance
      integer :: comm
      comm = this % self_component % comm
   end function mpmd_context_get_own_comm

   !> Retrieve a communicator that encompasses all PEs part of one of the components
   !> in the given list. If the communicator does not already exist, it will be created.
   !> _This function call is collective if and only if the communicator must be created._
   function mpmd_context_get_shared_comm(this, component_list) result(comm)
      implicit none
      class(mpmd_context),   intent(inout) :: this !< mpmd_context instance
      !> The list of components IDs for which we want a shared communicator.
      !> This list *must* contain the component of the calling PE. It may contain
      !> duplicate IDs and does not have to be in a specific order.
      integer, dimension(:), intent(in)    :: component_list
      integer :: comm

      integer, dimension(:), allocatable  :: sorted_components
      type(component_set) :: set

      !> Sanitize the list of components
      sorted_components = get_clean_component_list(component_list)

      write(app_msg, '(A, 8I7)') 'Retrieving shared comm for ', sorted_components(1:min(8, size(sorted_components)))
      call app_log(APP_DEBUG, app_msg)

      comm = MPI_COMM_NULL

      ! Make sure there are enough components in the list
      if (size(sorted_components) < 2) then
         write(app_msg, '(A)') &
            'MPMD_context_get_shared_comm: There need to be at least 2 components (including this one) '//           &
            'to share a communicator.'
         call app_log(APP_ERROR, app_msg)
         return
      end if

      ! Make sure this component is included in the list
      if (.not. any(sorted_components(:) == this % self_component % id)) then
         write(app_msg, '(A, A, A, I4, A)')                                                                          &
            'MPMD_context_get_shared_comm: You must include "self" (',                                               &
            component_id_to_name(this % self_component % id), ', ', this % self_component % id,                      &
            ') in the list of components when requesting a shared communicator.'
         call app_log(APP_WARNING, app_msg)
         return
      end if

      ! Check whether a communicator has already been created for this set of components
      set = this % find_set(sorted_components)

      if (.not. associated(set % component_ids)) then
         ! Not created yet, so we have to do it now
         set = this % create_set(sorted_components)

         ! Oh nooo, something went wrong
         if (.not. associated(set % component_ids)) then
            write(app_msg, '(A, I3, A, 8(I8))')                                                                      &
               'MPMD_context_get_shared_comm: Unable to create a communicator for the given set of ',                &
               size(sorted_components), ' components ', sorted_components(1:min(8, size(sorted_components)))
            call app_log(APP_ERROR, app_msg)
         end if
      end if

      comm = set % communicator

      if (comm == MPI_COMM_NULL) then
         write(app_msg, '(A, 10(I7))') 'Communicator is NULL for components ', sorted_components(1:min(10, size(sorted_components)))
         call app_log(APP_ERROR, app_msg)
      end if
   
   contains

      !> From the given list of components, get the same list in ascending order and without any duplicate.
      function get_clean_component_list(in_list) result(out_list)
         implicit none
         integer, dimension(:), intent(in)  :: in_list !< Input list of component IDs
         integer, dimension(:), allocatable :: out_list !< Output list of sorted unique component IDs

         integer, dimension(size(in_list) + 1) :: tmp_list

         integer :: i, j, item, num_items

         ! Perform an insertion sort, while checking for duplicates
         tmp_list(:) = -1
         tmp_list(1) = in_list(1)
         num_items = 1
         do i = 2, size(in_list)
            item = in_list(i)

            if (any(tmp_list(1:num_items) == item)) cycle ! Duplicate

            do j = i - 1, 1, -1 ! Loop through sorted list (so far)
               if (tmp_list(j) > item) then
                  ! Not the right position yet
                  tmp_list(j + 1) = tmp_list(j)
                  
                  if (j == 1) then
                     ! Last one, so we know the 1st slot is the right one
                     tmp_list(1) = item
                     num_items = num_items + 1
                  end if

               else if (tmp_list(j) < item) then
                  ! Found the right position
                  tmp_list(j + 1) = item
                  num_items = num_items + 1
                  exit
               end if
            end do
         end do

         ! Put in list with the right size
         allocate(out_list(num_items))
         out_list(:) = tmp_list(1:num_items)
      end function get_clean_component_list

   end function mpmd_context_get_shared_comm

   !> Find the component set that corresponds to the given list of IDs within this MPMD context.
   function mpmd_context_find_set(this, component_list) result(set)
      implicit none
      class(mpmd_context),   intent(in) :: this !< mpmd_context instance
      !> List of components IDs in the set. *Must be sorted in ascending order and without duplicates*.
      integer, dimension(:), intent(in) :: component_list

      type(component_set) :: set  !< The set, if it exists (will contain a null pointer if not)

      type(component_set), pointer :: local_set
      integer :: i

      do i = 1, this % num_sets
         local_set => this % sets(i)
         if (size(local_set % component_ids) == size(component_list)) then
            if (all(local_set % component_ids == component_list)) then
               set = local_set
               return
            end if
         end if
      end do
   end function mpmd_context_find_set

   !> Create a set of components within this MPMD context. This will create a communicator for them.
   function mpmd_context_create_set(this, component_list) result(set)
      implicit none
      class(mpmd_context),   intent(inout) :: this !< mpmd_context instance. *Must be initialized already.*
      !> List of components IDs in the set. *Must be sorted in ascending order and without duplicates*.
      integer, dimension(:), intent(in)    :: component_list
      type(component_set) :: set !< The set we created (will contain a NULL pointer if there was an error)

      integer, dimension(size(component_list)) :: component_pos
      integer :: num_pes_total, num_pes_component
      integer :: i, pos, ierr
      type(component), pointer :: comp_tmp

      ! Choose (arbitrarily) initial array size to be the total number of components in the context
      if (.not. associated(this % sets)) allocate(this % sets(size(this % all_components)))

      ! Make sure the array is large enough to contain the new set.
      ! We need to allocate a new (larger) one if not, and transfer all existing sets to it
      if (this % num_sets >= size(this % sets)) then
         block
            integer :: old_size, new_size
            type(component_set), dimension(:), pointer :: new_list

            old_size = size(this % sets)
            new_size = old_size * 2

            allocate(new_list(new_size))
            new_list(1:old_size) = this % sets(1:old_size)
            deallocate(this % sets)
            this % sets => new_list
         end block
      end if

      ! Find the position of each target component within the list of all components
      pos = 1
      do i = 1, size(component_list)
         do while (this % all_components(pos) % id < component_list(i))
            pos = pos + 1
         end do
         if (this % all_components(pos) % id == component_list(i)) then
            component_pos(i) = pos
            pos = pos + 1
         end if
      end do

      ! Share the world ranks of other components among all PEs of this component (if not done already)
      do i = 1, size(component_list)
         comp_tmp => this % all_components(component_pos(i))
         if (.not. comp_tmp % already_shared) then
            if (.not. associated(comp_tmp % ranks)) allocate(comp_tmp % ranks(comp_tmp % num_pes))
            call MPI_Bcast(comp_tmp % ranks, comp_tmp % num_pes, MPI_INTEGER, 0, this % self_component % comm, ierr)
            comp_tmp % already_shared = .true.
         end if
      end do

      ! Create the set
      block
         integer :: main_group
         integer, dimension(size(component_list)) :: subgroups
         integer :: union_group
         integer :: union_comm

         call MPI_Comm_group(this % main_comm, main_group, ierr)
         do i = 1, size(component_list)
            comp_tmp => this % all_components(component_pos(i))
            call MPI_Group_incl(main_group, comp_tmp % num_pes, comp_tmp % ranks, subgroups(i), ierr)
         end do

         ! Get the union of all these groups. Using a function to avoid overwriting result during the process
         union_group = merge_groups(subgroups(1), subgroups(2))
         do i = 3, size(component_list)
            union_group = merge_groups(union_group, subgroups(i))
         end do

         ! This call is collective among all group members, but not main_comm
         call MPI_Comm_create_group(this % main_comm, union_group, size(component_list), union_comm, ierr)

         set = new_component_set(component_list, union_comm)

         do i = 1, size(component_list)
            call MPI_Group_free(subgroups(i), ierr)
         end do
      end block

      ! Add it to the list
      this % num_sets = this % num_sets + 1
      this % sets(this % num_sets) = set

      contains

         !> Create an MPI group that is the union of the two input groups
         function merge_groups(g1, g2) result(merged_group)
            implicit none
            integer, intent(in) :: g1 !< 1st group
            integer, intent(in) :: g2 !< 2nd group
            integer :: merged_group
            integer :: mpierr
            call MPI_Group_union(g1, g2, merged_group, mpierr)
         end function merge_groups

   end function mpmd_context_create_set

   !> Create an instance of \ref component_set
   pure function new_component_set(component_list, communicator) result(new_set)
      implicit none
      integer, dimension(:), intent(in) :: component_list   !< IDs of the components in the new set
      integer,               intent(in) :: communicator     !< MPI communicator that joins all these components
      type(component_set) :: new_set
      allocate(new_set % component_ids(size(component_list)))
      new_set % component_ids(:) = component_list(:)
      new_set % communicator  = communicator
   end function new_component_set

   !> Get the name associated with the given component ID
   function component_id_to_name(component_id) result(component_name)
      implicit none

      integer, intent(in) :: component_id    !< Identifier of the component whose name we want
      character(len=:), allocatable :: component_name
      select case (component_id)
      case (MPMD_NONE)
         component_name = '[none]'
      case (MPMD_GEM_ID)
         component_name = 'GEM'
      case (MPMD_GEM_IOSERVER_ID)
         component_name = 'GEM-IOSERVER'
      case (MPMD_IRIS_ID)
         component_name = 'IRIS'
      case (MPMD_NEMO_ID)
         component_name = 'NEMO'
      case (MPMD_TEST1_ID)
         component_name = 'TEST1'
      case (MPMD_TEST2_ID)
         component_name = 'TEST2'
      case (MPMD_TEST3_ID)
         component_name = 'TEST3'
      case (MPMD_TEST4_ID)
         component_name = 'TEST4'
      case (MPMD_TEST5_ID)
         component_name = 'TEST5'
      case default
         component_name = '[unknown component]'
      end select
   end function component_id_to_name
      
   !> \return Whether the given component is present in this MPMD context
   function mpmd_context_has_component(this, component_id) result(has_component)
      implicit none
      class(mpmd_context), intent(in) :: this !< mpmd_context instance
      integer,             intent(in) :: component_id !< ID of the component whose presence we want to check
      logical :: has_component

      integer :: i

      has_component = .false.
      if (.not. associated(this % all_components)) then
         call app_log(APP_WARNING, 'all_components is not associated. Context is most likely NOT initialized.')
         return
      end if

      write(app_msg, '(A, A)') 'MPMD_context has component: Looking for component ', component_id_to_name(component_id)
      call app_log(APP_DEBUG, app_msg)

      do i = 1, size(this % all_components)
         if (this % all_components(i) % id == component_id) then
            has_component = .true.
            return
         end if
      end do
   end function mpmd_context_has_component

   subroutine mpmd_context_finalize(this)
      implicit none
      class(mpmd_context), intent(inout) :: this !< mpmd_context instance

      integer :: i, ierr

      if (this % main_comm /= MPI_COMM_NULL) then
         if (associated(this % sets)) then
            do i = 1, this % num_sets
               deallocate(this % sets(i) % component_ids)
            end do
            deallocate(this % sets)
            nullify(this % sets)
         end if
         this % num_sets = 0

         do i = 1, size(this % all_components)
            if (associated(this % all_components(i) % ranks))              &
               deallocate(this % all_components(i) % ranks)
         end do
         deallocate(this % all_components)
         nullify(this % all_components)
         nullify(this % self_component)

         call MPI_Finalize(ierr)
         this % main_comm = MPI_COMM_NULL
      end if
   end subroutine mpmd_context_finalize

   !> Print the content of the given \ref component instance
   subroutine print_component(comp, verbose)
      implicit none
      type(component), intent(in) :: comp !< The component whose info we want to print
      logical,         intent(in) :: verbose !< Whether to print the global rank of every PE in the component

      write(app_msg, '((A, A, A, A), (A, I4, A), (A, A, A), (A, I6, A), (A, L2, A), (A, A))')                        &
         'Component ', component_id_to_name(comp % id), ': ', EOL,                                                   &
         '  id:             ', comp % id, EOL,                                                                       &
         '  comm:           ', get_comm_string(comp % comm), EOL,                                                    &
         '  num_pes:        ', comp % num_pes, EOL,                                                                  &
         '  already_shared: ', comp % already_shared, EOL,                                                           &
         '  ranks:          ', get_ranks_string(comp % ranks, verbose)
      call app_log(APP_DEBUG, app_msg)

      contains

         function get_comm_string(comm) result(string)
            implicit none
            integer, intent(in) :: comm
            character(len=:), allocatable :: string

            character(len=50) :: tmp_string
            
            if (comm == MPI_COMM_NULL) then
               string = 'MPI_COMM_NULL'
            else
               write(tmp_string, '(I16)') comm
               string = adjustl(tmp_string)
            end if
         end function

         function get_ranks_string(ranks, with_numbers) result(string)
            implicit none
            integer, dimension(:), pointer, intent(in) :: ranks
            logical,                        intent(in) :: with_numbers
            character(len=:), allocatable :: string

            character(len=128) :: tmp_string

            if (.not. associated(ranks)) then
               string = '[null]'
            else if (.not. with_numbers) then
               string = '[present]'
            else
               write(tmp_string, '(15I7)') ranks(1:min(15, size(ranks)))
               string = trim(tmp_string)
            end if
         end function get_ranks_string

   end subroutine print_component

end module app_mpmd
