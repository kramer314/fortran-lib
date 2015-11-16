! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

! Basic statistics functionality
module stats
  use globvars
  use array, only: array_quicksort, array_pack

  implicit none

  private

  public :: stats_residuals
  public :: stats_mean
  public :: stats_fivenum
  public :: stats_max
  public :: stats_min
  public :: stats_median
  public :: stats_lower_quartile
  public :: stats_upper_quartile
  public :: stats_mean_sq_err
  public :: stats_mean_abs_err
  public :: stats_variance
  public :: stats_stdev

contains

  subroutine stats_residuals(data_arr, theor_arr, resid_arr)
    ! Calculate residuals
    !
    ! data_arr :: array of experimental data values
    ! theor_arr :: array of theoretical values, parallel to data_arr
    ! resid_arr :: array to populate with residuals
    real(dp), intent(in) :: data_arr(:), theor_arr(:)
    real(dp), intent(inout) :: resid_arr(:)

    resid_arr(:) = abs(data_arr(:) - theor_arr(:))

  end subroutine stats_residuals

  real(dp) function stats_mean(data_arr, mask) result(val)
    ! Calculate mean
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    integer :: n

    if (present(mask)) then
       n = count(mask)
       val = sum(data_arr, mask=mask) / n
    else
       n = size(data_arr)
       val = sum(data_arr) / n
    end if

  end function stats_mean

  subroutine stats_fivenum(data_arr, summary_arr, mask, sorted)
    ! Generate five-number summary statistics
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

    min = stats_min(data_arr, mask=mask)
    lq = stats_lower_quartile(data_arr, mask=mask, sorted=sorted)
    med= stats_median(data_arr, mask=mask, sorted=sorted)
    uq = stats_upper_quartile(data_arr, mask=mask, sorted=sorted)
    max = stats_max(data_arr, mask=mask)

    summary_arr = [min, lq, med, uq, max]

  end subroutine stats_fivenum

  real(dp) function stats_lower_quartile(data_arr, mask, sorted) result(val)
    ! Calculate lower quartile value
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    ! sorted :: whether data_arr is already sorted
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    real(dp), allocatable :: data_work_arr(:)
    integer :: data_size

    call array_pack(data_arr, data_work_arr, mask=mask)
    data_size = size(data_work_arr)

    if ((.not. present(sorted)) .or. (.not. sorted)) then
       call array_quicksort(data_work_arr, 1, size(data_work_arr))
    end if

    ! lower quartile is median of values to the left of the median
    val = stats_median(data_arr(:data_size / 2), sorted=.true.)

    deallocate(data_work_arr)

  end function stats_lower_quartile

  real(dp) function stats_upper_quartile(data_arr, mask, sorted) result(val)
    ! Calculate upper quartile value
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    ! sorted :: whether data_arr is already sorted
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    real(dp), allocatable :: data_work_arr(:)
    integer :: data_size

    call array_pack(data_arr, data_work_arr, mask=mask)
    data_size = size(data_work_arr)

    if ((.not. present(sorted)) .or. (.not. sorted)) then
       call array_quicksort(data_work_arr, 1, size(data_work_arr))
    end if

    ! upper quartile value is the median of values to the right of the median
    val = stats_median(data_arr(data_size / 2:), sorted=.true.)

    deallocate(data_work_arr)

  end function stats_upper_quartile

  real(dp) function stats_max(data_arr, mask) result(val)
    ! Find max value
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    val = maxval(data_arr, mask=mask)

  end function stats_max

  real(dp) function stats_min(data_arr, mask) result(val)
    ! Find min value
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    val = minval(data_arr, mask=mask)

  end function stats_min

  real(dp) function stats_median(data_arr, mask, sorted) result(val)
    ! Calculate median
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    ! sorted :: whether data_arr is already sorted
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)
    logical, intent(in), optional :: sorted

    real(dp), allocatable :: data_work_arr(:)
    integer :: data_size

    call array_pack(data_arr, data_work_arr, mask=mask)
    data_size = size(data_work_arr)

    if (.not. sorted) then
       call array_quicksort(data_work_arr, 1, size(data_work_arr))
    end if

    if (mod(data_size, 2) .eq. 0) then
       val = 0.5_dp * (data_work_arr(data_size / 2) + &
            data_work_arr(data_size / 2 + 1))
    else
       val = data_work_arr( (data_size - 1) / 2 + 1 )
    end if

    deallocate(data_work_arr)

  end function stats_median

  real(dp) function stats_mean_abs_err(resid_arr, mask) result(val)
    ! Calculate mean absolute error from residuals
    !
    ! resid_arr :: residual array
    ! mask :: logical masking array
    real(dp), intent(in) :: resid_arr(:)
    logical, intent(in), optional :: mask(:)

    integer :: n

    if (present(mask)) then
       n = count(mask)
       val = sum(resid_arr, mask=mask) / n
    else
       n = size(resid_arr)
       val = sum(resid_arr) / n
    end if

  end function stats_mean_abs_err

  real(dp) function stats_mean_sq_err(resid_arr, mask) result(val)
    ! Calculate mean squared error from residuals
    !
    ! resid_arr :: residual array
    ! mask :: logical masking array
    real(dp), intent(in) :: resid_arr(:)
    logical, intent(in), optional :: mask(:)

    integer :: n

    if (present(mask)) then
       n = count(mask)
       val = sum(resid_arr**2, mask=mask) / n
    else
       n = size(resid_arr)
       val = sum(resid_arr**2) / n
    end if

  end function stats_mean_sq_err

  real(dp) function stats_variance(data_arr, mask) result(val)
    ! Calculate variance
    !
    ! data_arr :: array of data values
    ! mask :: logical masking array
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    real(dp) :: mean
    integer :: n


    if (present(mask)) then
       n = count(mask)
       mean = stats_mean(data_arr, mask=mask)
       ! Biased sample variance
       val = sum(data_arr**2, mask=mask) / n - mean**2
       ! Convert to unbiased sample variance
       val = n / (n - 1.0_dp) * val
    else
       n = size(data_arr)
       mean = stats_mean(data_arr)
       val = sum((data_arr - mean)**2) / (n - 1)
    end if

  end function stats_variance

  real(dp) function stats_stdev(data_arr, mask) result(val)
    ! Compute standard deviation
    !
    ! data :: array of data values
    ! mask :: logical masking array
    real(dp), intent(in) :: data_arr(:)
    logical, intent(in), optional :: mask(:)

    if (present(mask)) then
       val = sqrt(stats_variance(data_arr, mask=mask))
    else
       val = sqrt(stats_variance(data_arr))
    end if

  end function stats_stdev

end module stats
