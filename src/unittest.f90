! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.
!
! This module implements basic unit testing.
module unittest

  use globvars, only: dp

  implicit none

  private

  public :: unittest_init
  public :: unittest_cleanup

  public :: unittest_assert_equal
  public :: unittest_assert_not_equal
  public :: unittest_assert_true
  public :: unittest_assert_false
  public :: unittest_assert_le
  public :: unittest_assert_lt
  public :: unittest_assert_ge
  public :: unittest_assert_gt

  interface unittest_assert_equal
     ! Equality test
     module procedure unittest_assert_equal_dp
     module procedure unittest_assert_equal_int
  end interface unittest_assert_equal

  interface unittest_assert_not_equal
     ! Not equality test
     module procedure unittest_assert_not_equal_dp
     module procedure unittest_assert_not_equal_int
  end interface unittest_assert_not_equal

  interface unittest_assert_le
     ! Less than or equal test
     module procedure unittest_assert_le_dp
     module procedure unittest_assert_le_int
  end interface unittest_assert_le

  interface unittest_assert_lt
     ! Less than test
     module procedure unittest_assert_lt_dp
     module procedure unittest_assert_lt_int
  end interface unittest_assert_lt

  interface unittest_assert_ge
     ! Greater than or equal test
     module procedure unittest_assert_ge_dp
     module procedure unittest_assert_ge_int
  end interface unittest_assert_ge

  interface unittest_assert_gt
     ! Greater than test
     module procedure unittest_assert_gt_dp
     module procedure unittest_assert_gt_int
  end interface unittest_assert_gt

  real(dp) :: eps_dp
contains

  subroutine unittest_init(dp_eps)
    ! Module initializaiton routine
    !
    ! dp_eps :: threshold for double precision equality
    real(dp), intent(in) :: dp_eps

    eps_dp = dp_eps

  end subroutine unittest_init

  subroutine unittest_cleanup()
    ! Module cleanup routine
    ! Currently only included for completeness; this doesn't actually do
    ! anything
  end subroutine unittest_cleanup

  pure logical function unittest_assert_equal_dp(a, b, eps) result(val)
    ! Equality test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    ! eps :: optional custom threshold for double precision equality
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp), intent(in), optional :: eps

    if (present(eps)) then
       val = (abs(a - b) .lt. eps)
    else
       val = (abs(a - b) .lt. eps_dp)
    end if

  end function unittest_assert_equal_dp

  pure logical function unittest_assert_equal_int(a, b) result(val)
    ! Equality test for integers
    !
    ! a :: 1st value
    ! b :: 2nd value
    integer, intent(in) :: a
    integer, intent(in) :: b

    val = (a .eq. b)

  end function unittest_assert_equal_int

  pure logical function unittest_assert_not_equal_dp(a, b, eps) result(val)
    ! Not equality test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    ! eps :: optional custom threshold for double precision equality
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp), intent(in), optional :: eps

    if (present(eps)) then
       val = (abs(a - b) .gt. eps)
    else
       val = (abs(a - b) .gt. eps_dp)
    end if

  end function unittest_assert_not_equal_dp

  pure logical function unittest_assert_not_equal_int(a, b) result(val)
    ! Not equality test for integers
    !
    ! a :: 1st value
    ! b :: 2nd value
    integer, intent(in) :: a
    integer, intent(in) :: b

    val = (a .ne. b)

  end function unittest_assert_not_equal_int

  pure logical function unittest_assert_true(a) result(val)
    ! Boolean true test
    !
    ! a :: logical / boolean value
    logical, intent(in) :: a

    val = a

  end function unittest_assert_true

  pure logical function unittest_assert_false(a) result(val)
    ! Boolean false test
    !
    ! a :: logical / boolean value
    logical, intent(in) :: a

    val = (.not. a)

  end function unittest_assert_false

  pure logical function unittest_assert_le_dp(a, b, eps) result(val)
    ! Less than or equal test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    ! eps :: optional custom threshold for double precision equality
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp), intent(in), optional :: eps

    if (present(eps)) then
       val = (unittest_assert_lt(a, b) .or. unittest_assert_equal(a, b, eps))
    else
       val = (unittest_assert_lt(a, b) .or. unittest_assert_equal(a,b))
    end if

  end function unittest_assert_le_dp

  pure logical function unittest_assert_lt_dp(a, b) result(val)
    ! Less than test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    val = (a .lt. b)

  end function unittest_assert_lt_dp

  pure logical function unittest_assert_ge_dp(a, b, eps) result(val)
    ! Greater than or equal test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    ! eps :: optional custom threshold for double precision equality
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp), intent(in), optional :: eps

    if (present(eps)) then
       val = (unittest_assert_gt(a, b) .or. unittest_assert_equal(a, b, eps))
    else
       val = (unittest_assert_gt(a, b) .or. unittest_assert_equal(a,b))
    end if

  end function unittest_assert_ge_dp

  pure logical function unittest_assert_gt_dp(a, b) result(val)
    ! Greater than test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    val = (a .gt. b)

  end function unittest_assert_gt_dp

  pure logical function unittest_assert_le_int(a, b) result(val)
    ! Less than or equal test for integers
    !
    ! a :: 1st value
    ! b :: 2nd value
    integer, intent(in) :: a
    integer, intent(in) :: b

    val = (a .le. b)

  end function unittest_assert_le_int

  pure logical function unittest_assert_lt_int(a, b) result(val)
    ! Less than test for integers
    !
    ! a :: 1st value
    ! b :: 2nd value
    integer, intent(in) :: a
    integer, intent(in) :: b

    val = (a .lt. b)

  end function unittest_assert_lt_int

  pure logical function unittest_assert_ge_int(a, b) result(val)
    ! Greater than or equal test for integers
    !
    ! a :: 1st value
    ! b :: 2nd value
    integer, intent(in) :: a
    integer, intent(in) :: b

    val = (a .ge. b)

  end function unittest_assert_ge_int

  pure logical function unittest_assert_gt_int(a, b) result(val)
    ! Greater than test for integers
    !
    ! a :: 1st value
    ! b :: 2nd value
    integer, intent(in) :: a
    integer, intent(in) :: b

    val = (a .gt. b)

  end function unittest_assert_gt_int

end module unittest
