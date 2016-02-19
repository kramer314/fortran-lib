! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

! Basic statistics functionality
module stats
  use globvars, only: dp, sp, ip
  use array, only: array_quicksort, array_pack

  implicit none

  private

  public :: stats_residuals
  interface stats_residuals
     module procedure stats_residuals_dp
     module procedure stats_residuals_sp
  end interface stats_residuals

  public :: stats_residuals_rel
  interface stats_residuals_rel
     module procedure stats_residuals_rel_dp
     module procedure stats_residuals_rel_sp
  end interface stats_residuals_rel

  public :: stats_mean
  interface stats_mean
     module procedure stats_mean_dp
     module procedure stats_mean_sp
  end interface

  public :: stats_fivenum
  interface stats_fivenum
     module procedure stats_fivenum_dp
     module procedure stats_fivenum_sp
  end interface stats_fivenum

  public :: stats_max
  interface stats_max
     module procedure stats_max_dp
     module procedure stats_max_sp
  end interface stats_max

  public :: stats_min
  interface stats_min
     module procedure stats_min_dp
     module procedure stats_min_sp
  end interface stats_min

  public :: stats_median
  interface stats_median
     module procedure stats_median_dp
     module procedure stats_median_sp
  end interface stats_median

  public :: stats_lower_quartile
  interface stats_lower_quartile
     module procedure stats_lower_quartile_dp
     module procedure stats_lower_quartile_sp
  end interface stats_lower_quartile

  public :: stats_upper_quartile
  interface stats_upper_quartile
     module procedure stats_upper_quartile_dp
     module procedure stats_upper_quartile_sp
  end interface stats_upper_quartile

  public :: stats_mean_sq_err
  interface stats_mean_sq_err
     module procedure stats_mean_sq_err_dp
     module procedure stats_mean_sq_err_sp
  end interface stats_mean_sq_err

  public :: stats_mean_abs_err
  interface stats_mean_abs_err
     module procedure stats_mean_abs_err_dp
     module procedure stats_mean_abs_err_sp
  end interface stats_mean_abs_err

  public :: stats_variance
  interface stats_variance
     module procedure stats_variance_dp
     module procedure stats_variance_sp
  end interface stats_variance

  public :: stats_stdev
  interface stats_stdev
     module procedure stats_stdev_dp
     module procedure stats_stdev_sp
  end interface stats_stdev

contains

  subroutine stats_residuals_rel_dp(data_arr, theor_arr, resid_rel_arr)
    ! Calculate relative residuals for double precision real values
    !
    ! data_arr :: array of experimental data values
    ! theor_arr :: arry of theoretical values, parallel to data_arr
    ! resid_rel_arr :: array to populate with relative residuals
    real(dp), intent(in) :: data_arr(:), theor_arr(:)
    real(dp), intent(inout) :: resid_rel_arr(:)

    include "./stats_src/residuals_rel.src"
  end subroutine stats_residuals_rel_dp

  subroutine stats_residuals_rel_sp(data_arr, theor_arr, resid_rel_arr)
    ! Duplicate of stats_residuals_rel_dp, but for single precision reals
    real(sp), intent(in) :: data_arr(:), theor_arr(:)
    real(sp), intent(inout) :: resid_rel_arr(:)

    include "./stats_src/residuals_rel.src"
  end subroutine stats_residuals_rel_sp

  subroutine stats_residuals_dp(data_arr, theor_arr, resid_arr)
    ! Calculate residuals, for double precision real arrays
    !
    ! data_arr :: array of experimental data values
    ! theor_arr :: array of theoretical values, parallel to data_arr
    ! resid_arr :: array to populate with residuals
    real(dp), intent(in) :: data_arr(:), theor_arr(:)
    real(dp), intent(inout) :: resid_arr(:)

    include "./stats_src/residuals.src"
  end subroutine stats_residuals_dp

  subroutine stats_residuals_sp(data_arr, theor_arr, resid_arr)
    ! Duplicate of stats_residuals, but for single precision real arrays
    real(sp), intent(in) :: data_arr(:), theor_arr(:)
    real(sp), intent(inout) :: resid_arr(:)

    include "./stats_src/residuals.src"
  end subroutine stats_residuals_sp

  pure real(dp) function stats_mean_dp(data_arr, mask) result(val)
    ! Calculate mean of array of double precision values
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/mean.src"
  end function stats_mean_dp

  pure real(sp) function stats_mean_sp(data_arr, mask) result(val)
    ! Duplicate of stats_mean_dp, but for single precision reals
    real(sp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/mean.src"
  end function stats_mean_sp

  subroutine stats_fivenum_dp(data_arr, summary_arr, mask, sorted)
    ! Generate five-number summary statistics, for double precision real arrays
    !
    ! data_arr :: array of data values
    ! summary_arr :: 5-element array for summary values, with entries:
    !  [min, lower_quartile, median, upper_quartile, max]
    ! mask :: logical masking array
    ! sorted :: whether data_arr is already sorted
    real(dp), intent(in) :: data_arr(:)
    real(dp), intent(out) :: summary_arr(5)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    real(dp) :: min, lq, med, uq, max

    include "./stats_src/fivenum.src"

    summary_arr = [min, lq, med, uq, max]

  end subroutine stats_fivenum_dp

  subroutine stats_fivenum_sp(data_arr, summary_arr, mask, sorted)
    ! Duplicate of stats_fivenum_dp, but for single precision real arrays
    real(sp), intent(in) :: data_arr(:)
    real(sp), intent(out) :: summary_arr(5)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    real(sp) :: min, lq, med, uq, max

    include "./stats_src/fivenum.src"

    summary_arr = [min, lq, med, uq, max]

  end subroutine stats_fivenum_sp

  real(dp) function stats_lower_quartile_dp(data_arr, mask, sorted) result(val)
    ! Calculate lower quartile value
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    ! sorted :: whether data_arr is already sorted
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    integer(ip), parameter :: fp = dp

    include "./stats_src/quartile.src"
    include "./stats_src/lower_quartile.src"
  end function stats_lower_quartile_dp

  real(sp) function stats_lower_quartile_sp(data_arr, mask, sorted) result(val)
    ! Duplicate of stats_lower_quartile_dp, but for single precision reals
    real(sp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    integer(ip), parameter :: fp = sp

    include "./stats_src/quartile.src"
    include "./stats_src/lower_quartile.src"
  end function stats_lower_quartile_sp

  real(dp) function stats_upper_quartile_dp(data_arr, mask, sorted) result(val)
    ! Calculate upper quartile value
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    ! sorted :: whether data_arr is already sorted
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    integer(ip), parameter :: fp = dp

    include "./stats_src/quartile.src"
    include "./stats_src/upper_quartile.src"
  end function stats_upper_quartile_dp

  real(sp) function stats_upper_quartile_sp(data_arr, mask, sorted) result(val)
    ! Duplicate of stats_upper_quartile_dp, but for single precision reals
    real(sp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    integer(ip), parameter :: fp = sp

    include "./stats_src/quartile.src"
    include "./stats_src/upper_quartile.src"
  end function stats_upper_quartile_sp

  pure real(dp) function stats_max_dp(data_arr, mask) result(val)
    ! Find max value, for double precision real arrays
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/max.src"
  end function stats_max_dp

  pure real(sp) function stats_max_sp(data_arr, mask) result(val)
    ! Duplicate of stats_max_dp, but for single precision reals
    real(sp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/max.src"
  end function stats_max_sp

  pure real(dp) function stats_min_dp(data_arr, mask) result(val)
    ! Find min value, for double precision real arrays
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/min.src"
  end function stats_min_dp

  pure real(sp) function stats_min_sp(data_arr, mask) result(val)
    ! Duplicate of stats_min_sp, but for single precision reals
    real(sp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/min.src"
  end function stats_min_sp

  real(dp) function stats_median_dp(data_arr, mask, sorted) result(val)
    ! Calculate median of double precision real array
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    ! sorted :: whether data_arr is already sorted
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    real(dp), allocatable :: data_work_arr(:)

    include "./stats_src/median.src"

    deallocate(data_work_arr)

  end function stats_median_dp

  real(sp) function stats_median_sp(data_arr, mask, sorted) result(val)
    real(sp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    real(sp), allocatable :: data_work_arr(:)

    include "./stats_src/median.src"

    deallocate(data_work_arr)

  end function stats_median_sp

  real(dp) function stats_mean_abs_err_dp(resid_arr, mask) result(val)
    ! Calculate mean absolute error from residuals
    !
    ! resid_arr :: residual array
    ! mask :: logical masking array
    real(dp), intent(in) :: resid_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/mean_abs_err.src"
  end function stats_mean_abs_err_dp

  real(sp) function stats_mean_abs_err_sp(resid_arr, mask) result(val)
    ! Calculate mean absolute error from residuals
    !
    ! resid_arr :: residual array
    ! mask :: logical masking array
    real(sp), intent(in) :: resid_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/mean_abs_err.src"
  end function stats_mean_abs_err_sp

  pure real(dp) function stats_mean_sq_err_dp(resid_arr, mask) result(val)
    ! Calculate mean squared error from residuals
    !
    ! resid_arr :: residual array
    ! mask :: logical masking array
    real(dp), intent(in) :: resid_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/mean_sq_err.src"
  end function stats_mean_sq_err_dp

  pure real(sp) function stats_mean_sq_err_sp(resid_arr, mask) result(val)
    ! Calculate mean squared error from residuals
    !
    ! resid_arr :: residual array
    ! mask :: logical masking array
    real(sp), intent(in) :: resid_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/mean_sq_err.src"
  end function stats_mean_sq_err_sp

  pure real(dp) function stats_variance_dp(data_arr, mask) result(val)
    ! Calculate variance
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    real(dp) :: mean

    include "./stats_src/variance.src"
  end function stats_variance_dp

  pure real(sp) function stats_variance_sp(data_arr, mask) result(val)
    ! Duplicate of stats_variance_dp, but for single precision reals
    real(sp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    real(sp) :: mean

    include "./stats_src/variance.src"
  end function stats_variance_sp

  pure real(dp) function stats_stdev_dp(data_arr, mask) result(val)
    ! Compute standard deviation, for double precision real arrays
    !
    ! data :: array of data values
    ! mask :: logical masking array
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/stdev.src"
  end function stats_stdev_dp

  pure real(sp) function stats_stdev_sp(data_arr, mask) result(val)
    ! Duplicate of stats_stdev_dp, but for single precision reals
    real(sp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    include "./stats_src/stdev.src"
  end function stats_stdev_sp

  pure logical function stats_to_sort(sorted) result(val)
    ! Private helper function to determine if an array needs to be sorted baesd
    ! on input arguments. This is here to reduce code duplication.
    logical, optional, intent(in) :: sorted
    logical :: to_sort

    to_sort = .true.
    ! This can't be a single if statement since Fortran doesn't have short-
    ! circuit evaluation
    if (present(sorted)) then
       if (sorted) then
          to_sort = .false.
       end if
    end if

    val = to_sort
  end function stats_to_sort

end module stats
