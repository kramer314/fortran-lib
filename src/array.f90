! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.
!
! This module implements basic operations on 1D arrays.
module array

  use globvars, only: dp

  implicit none

  private

  public array_quicksort
  public array_swap
  public array_reverse
  public array_pack

  interface array_quicksort
     ! Quicksort in increasing order using Lomuto partitioning
     ! Note; this is *not* at all optimized; use an optimized library if
     ! sorting performance is critical.
     module procedure array_quicksort_dp
     module procedure array_quicksort_int
  end interface array_quicksort

  interface array_swap
     ! Swap array elements in place
     module procedure array_swap_dp
     module procedure array_swap_int
  end interface array_swap

  interface array_reverse
     ! Reverse array in place
     module procedure array_reverse_dp
     module procedure array_reverse_int
  end interface array_reverse

  interface array_pack
     ! Pack new array from optional mask values
     module procedure array_pack_dp
     module procedure array_pack_int
  end interface array_pack

contains

  subroutine array_swap_dp(arr, i, j)
    ! Swap values of arr(i) and arr(j)

    real(dp), intent(inout) :: arr(:)
    integer, intent(in) :: i, j

    real(dp) :: temp

    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp

  end subroutine array_swap_dp

  subroutine array_swap_int(arr, i, j)
    ! Swap values of arr(i) and arr(j)

    integer, intent(inout) :: arr(:)
    integer, intent(in) :: i, j

    integer :: temp

    temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp

  end subroutine array_swap_int

  subroutine array_reverse_dp(arr)
    ! Reverse array

    real(dp), intent(inout) :: arr(:)

    integer :: i, j, n

    n = size(arr)
    j = n
    do i = 1, n / 2
       call array_swap_dp(arr, i, j)
       j = j -1
    end do

  end subroutine array_reverse_dp

  subroutine array_reverse_int(arr)
    ! Reverse array

    integer, intent(inout) :: arr(:)

    integer :: i, j, n

    n = size(arr)
    j = n
    do i = 1, n / 2
       call array_swap_int(arr, i, j)
       j = j -1
    end do
  end subroutine array_reverse_int

  recursive subroutine array_quicksort_dp(arr, i_min, i_max)
    ! Quicksort with Lomuto partitioning

    real(dp), intent(inout) :: arr(:)
    integer, intent(in) :: i_min, i_max

    integer :: part

    if (i_min < i_max) then
       call partition_lomuto_dp(arr, i_min, i_max, part)
       call array_quicksort_dp(arr, i_min, part - 1)
       call array_quicksort_dp(arr, part + 1, i_max)
    end if

  contains

    subroutine partition_lomuto_dp(arr, i_min, i_max, part)
      ! Lomuto partition scheme for quicksort

      real(dp), intent(inout) :: arr(:)
      integer, intent(in) :: i_min, i_max
      integer, intent(out) :: part

      integer :: i, j
      real(dp) :: pivot

      pivot = arr(i_max)

      i = i_min

      do j = i_min, i_max - 1
         if (arr(j) .le. pivot) then
            call array_swap_dp(arr, i, j)
            i = i + 1
         end if
      end do
      call array_swap_dp(arr, i, i_max)

      part = i
    end subroutine partition_lomuto_dp

  end subroutine array_quicksort_dp

  recursive subroutine array_quicksort_int(arr, i_min, i_max)
    ! Quicksort with Lomuto partitioning

    integer, intent(inout) :: arr(:)
    integer, intent(in) :: i_min, i_max

    integer :: part

    if (i_min < i_max) then
       call partition_lomuto_int(arr, i_min, i_max, part)
       call array_quicksort_int(arr, i_min, part - 1)
       call array_quicksort_int(arr, part + 1, i_max)
    end if

  contains

    subroutine partition_lomuto_int(arr, i_min, i_max, part)
      ! Lomuto partition scheme for quicksort

      integer, intent(inout) :: arr(:)
      integer, intent(in) :: i_min, i_max
      integer, intent(out) :: part

      integer :: i, j
      integer :: pivot

      pivot = arr(i_max)

      i = i_min

      do j = i_min, i_max - 1
         if (arr(j) .le. pivot) then
            call array_swap_int(arr, i, j)
            i = i + 1
         end if
      end do
      call array_swap_int(arr, i, i_max)

      part = i
    end subroutine partition_lomuto_int

  end subroutine array_quicksort_int

  subroutine array_pack_dp(in_arr, out_arr, mask)
    real(dp), intent(in) :: in_arr(:)
    real(dp), allocatable, intent(out) :: out_arr(:)
    logical, intent(in), optional :: mask(:)

    integer :: arr_size

    if (present(mask)) then
       arr_size = count(mask)
       allocate(out_arr(arr_size))
       out_arr = pack(in_arr, mask)
    else
       arr_size = size(in_arr)
       allocate(out_arr(arr_size))
       out_arr(:) = in_arr(:)
    end if

  end subroutine array_pack_dp

  subroutine array_pack_int(in_arr, out_arr, mask)
    integer, intent(in) :: in_arr(:)
    integer, allocatable, intent(out) :: out_arr(:)
    logical, intent(in), optional :: mask(:)

    integer :: arr_size

    if (present(mask)) then
       arr_size = count(mask)
       allocate(out_arr(arr_size))
       out_arr = pack(in_arr, mask)
    else
       arr_size = size(in_arr)
       allocate(out_arr(arr_size))
       out_arr(:) = in_arr(:)
    end if

  end subroutine array_pack_int

end module array
