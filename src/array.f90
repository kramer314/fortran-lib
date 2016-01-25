! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.
!
! This module implements basic operations on 1D arrays.
module array

  use globvars, only: dp, sp, ip

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
     module procedure array_quicksort_sp
     module procedure array_quicksort_int
  end interface array_quicksort

  interface array_swap
     ! Swap array elements in place
     module procedure array_swap_dp
     module procedure array_swap_sp
     module procedure array_swap_int
  end interface array_swap

  interface array_reverse
     ! Reverse array in place
     module procedure array_reverse_dp
     module procedure array_reverse_sp
     module procedure array_reverse_int
  end interface array_reverse

  interface array_pack
     ! Pack new array from optional mask values
     module procedure array_pack_dp
     module procedure array_pack_sp
     module procedure array_pack_int
  end interface array_pack

contains

  subroutine array_swap_dp(arr, i, j)
    ! Swap values of arr(i) and arr(j). For double precision real arrays
    !
    ! arr :: array
    ! i :: first index
    ! j :: second index
    real(dp), intent(inout) :: arr(:)
    integer(ip), intent(in) :: i, j

    real(dp) :: temp

    include "./array_src/swap.src"
  end subroutine array_swap_dp

  subroutine array_swap_sp(arr, i, j)
    ! Duplicate of array_swap_dp for single precision real arrays
    ! precision reals
    real(sp), intent(inout) :: arr(:)
    integer(ip), intent(in) :: i, j

    real(sp) :: temp

    include "./array_src/swap.src"
  end subroutine array_swap_sp

  subroutine array_swap_int(arr, i, j)
    ! Duplicate of array_swap_dp for integer arrays
    integer(ip), intent(inout) :: arr(:)
    integer(ip), intent(in) :: i, j

    integer(ip) :: temp

    include "./array_src/swap.src"
  end subroutine array_swap_int

  subroutine array_reverse_dp(arr)
    ! Reverse array
    !
    ! arr :: array to reverse
    real(dp), intent(inout) :: arr(:)

    integer(ip) :: i, j, n

    include "./array_src/reverse.src"
  end subroutine array_reverse_dp

  subroutine array_reverse_sp(arr)
    ! Duplicate of array_reverse_dp for single precision real arrays
    real(sp), intent(inout) :: arr(:)

    integer(ip) :: i, j, n

    include "./array_src/reverse.src"
  end subroutine array_reverse_sp

  subroutine array_reverse_int(arr)
    ! Duplicate of array_reverse_dp for integer arrays
    integer(ip), intent(inout) :: arr(:)

    integer(ip) :: i, j, n

    include "./array_src/reverse.src"
  end subroutine array_reverse_int

  recursive subroutine array_quicksort_dp(arr, i_min, i_max)
    ! Quicksort with Lomuto partitioning
    !
    ! arr :: array to sort (where i_min / i_max denote the slice to sort)
    ! i_min :: left end of slice to sort
    ! i_max :: right end of slice to sort
    real(dp), intent(inout) :: arr(:)
    integer(ip), intent(in) :: i_min, i_max

    integer(ip) :: part

    if (i_min < i_max) then
       call partition_lomuto_dp(arr, i_min, i_max, part)
       call array_quicksort_dp(arr, i_min, part - 1)
       call array_quicksort_dp(arr, part + 1, i_max)
    end if

  contains

    subroutine partition_lomuto_dp(arr, i_min, i_max, part)
      ! Lomuto partition operation
      !
      ! The partition operation reorders an array using a pivot value so that
      ! all values less than the pivot value are before the pivot location,
      ! while all values greater than the pivot value are after the pivot
      ! location.
      !
      ! array :: array to partition
      ! i_min :: left end of partition slice
      ! i_max :: right end of partition slice
      ! part :: partition value to return (final pivot position)
      real(dp), intent(inout) :: arr(:)
      integer(ip), intent(in) :: i_min, i_max
      integer(ip), intent(out) :: part

      real(dp) :: pivot

      include "./array_src/partition_lomuto.src"
    end subroutine partition_lomuto_dp

  end subroutine array_quicksort_dp

  recursive subroutine array_quicksort_sp(arr, i_min, i_max)
    ! Duplicate of array_quicksort_sp, but for a single-precision real array
    real(sp), intent(inout) :: arr(:)
    integer(ip), intent(in) :: i_min, i_max

    integer(ip) :: part

    if (i_min < i_max) then
       call partition_lomuto_sp(arr, i_min, i_max, part)
       call array_quicksort_sp(arr, i_min, part - 1)
       call array_quicksort_sp(arr, part + 1, i_max)
    end if

  contains

    subroutine partition_lomuto_sp(arr, i_min, i_max, part)
      ! Duplication of partition_lomuto_dp in array_quicksort_dp, but for a
      ! single-precision real array.
      real(sp), intent(inout) :: arr(:)
      integer(ip), intent(in) :: i_min, i_max
      integer(ip), intent(out) :: part

      real(sp) :: pivot

      include "./array_src/partition_lomuto.src"
    end subroutine partition_lomuto_sp

  end subroutine array_quicksort_sp

  recursive subroutine array_quicksort_int(arr, i_min, i_max)
    ! Duplicate of array_quicksort_dp, but for an integer array.
    integer(ip), intent(inout) :: arr(:)
    integer(ip), intent(in) :: i_min, i_max

    integer(ip) :: part

    if (i_min < i_max) then
       call partition_lomuto_int(arr, i_min, i_max, part)
       call array_quicksort_int(arr, i_min, part - 1)
       call array_quicksort_int(arr, part + 1, i_max)
    end if

  contains

    subroutine partition_lomuto_int(arr, i_min, i_max, part)
      ! Duplication of partition_lomuto_dp in array_quicksort_dp, but for an
      ! integer array.
      integer(ip), intent(inout) :: arr(:)
      integer(ip), intent(in) :: i_min, i_max
      integer(ip), intent(out) :: part

      integer(ip) :: pivot

      include "./array_src/partition_lomuto.src"
    end subroutine partition_lomuto_int

  end subroutine array_quicksort_int

  subroutine array_pack_dp(in_arr, out_arr, mask)
    ! Pack an array using an (optional) masking array.
    !
    ! pack() is an intrinsic routine, but the mask argument is required; when
    ! we may or may not have a mask, this method is convenient. If no mask is
    ! present, then this routine simply copies the input array.
    !
    ! in_arr :: input array
    ! out_arr :: packed array
    ! mask :: optional masking array
    real(dp), intent(in) :: in_arr(:)
    real(dp), allocatable, intent(out) :: out_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./array_src/pack.src"
  end subroutine array_pack_dp

  subroutine array_pack_sp(in_arr, out_arr, mask)
    ! Duplicate of array_pack_dp, but for single-precision real arrays.
    real(sp), intent(in) :: in_arr(:)
    real(sp), allocatable, intent(out) :: out_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./array_src/pack.src"
  end subroutine array_pack_sp

  subroutine array_pack_int(in_arr, out_arr, mask)
    ! Duplicate of array_pack_dp, but for integer arrays.
    integer(ip), intent(in) :: in_arr(:)
    integer, allocatable, intent(out) :: out_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./array_src/pack.src"
  end subroutine array_pack_int

end module array
