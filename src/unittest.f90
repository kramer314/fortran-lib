! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.
!
! This module implements basic unit testing.
module unittest

  use globvars, only: dp, sp, ip, dp_epsilon, sp_epsilon

  implicit none

  private

  public :: unittest_init

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
     module procedure unittest_assert_equal_sp
     module procedure unittest_assert_equal_int
  end interface unittest_assert_equal

  interface unittest_assert_not_equal
     ! Not equality test
     module procedure unittest_assert_not_equal_dp
     module procedure unittest_assert_not_equal_sp
     module procedure unittest_assert_not_equal_int
  end interface unittest_assert_not_equal

  interface unittest_assert_le
     ! Less than or equal test
     module procedure unittest_assert_le_dp
     module procedure unittest_assert_le_sp
     module procedure unittest_assert_le_int
  end interface unittest_assert_le

  interface unittest_assert_lt
     ! Less than test
     module procedure unittest_assert_lt_dp
     module procedure unittest_assert_lt_sp
     module procedure unittest_assert_lt_int
  end interface unittest_assert_lt

  interface unittest_assert_ge
     ! Greater than or equal test
     module procedure unittest_assert_ge_dp
     module procedure unittest_assert_ge_sp
     module procedure unittest_assert_ge_int
  end interface unittest_assert_ge

  interface unittest_assert_gt
     ! Greater than test
     module procedure unittest_assert_gt_dp
     module procedure unittest_assert_gt_sp
     module procedure unittest_assert_gt_int
  end interface unittest_assert_gt

  real(dp) :: eps_dp
  real(sp) :: eps_sp
  integer(ip) :: eps_int
contains

  subroutine unittest_init(sp_eps, dp_eps, int_eps)
    ! Module initializaiton routine
    !
    ! dp_eps :: optional threshold for double-precision comparison
    !   If this is not set, the machine epsilon is used
    ! sp_eps :: optional threshold for single-precision comparison
    !   If this is not set, the machine epsilon is used
    ! int_eps :: optional threshold for integer comparison
    !   If this is not set, 0 is used (exact comparison)
    real(sp), intent(in), optional :: sp_eps
    real(dp), intent(in), optional :: dp_eps
    integer(ip), intent(in), optional :: int_eps

    if (present(dp_eps)) then
       eps_dp = dp_eps
    else
       eps_dp = dp_epsilon
    end if

    if (present(sp_eps)) then
       eps_sp = sp_eps
    else
       eps_sp = sp_epsilon
    end if

    if (present(int_eps)) then
       eps_int = int_eps
    else
       eps_int = 0_ip
    end if

  end subroutine unittest_init

  pure logical function unittest_assert_equal_dp(a, b, eps) result(val)
    ! Equality test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    ! eps :: optional custom threshold for equality
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp), intent(in), optional :: eps

    if (present(eps)) then
       val = (abs(a - b) .lt. eps)
    else
       val = (abs(a - b) .lt. eps_dp)
    end if

  end function unittest_assert_equal_dp

  pure logical function unittest_assert_equal_sp(a, b, eps) result(val)
    ! Equality test for single precision reals. Duplicate of
    ! unittest_assert_equal_dp
    real(sp), intent(in) :: a
    real(sp), intent(in) :: b
    real(sp), intent(in), optional :: eps

    if (present(eps)) then
       val = (abs(a - b) .lt. eps)
    else
       val = (abs(a - b) .lt. eps_dp)
    end if

  end function unittest_assert_equal_sp

  pure logical function unittest_assert_equal_int(a, b, eps) result(val)
    ! Equality test for integers. Duplicate of unittest_assert_equal_dp
    integer(ip), intent(in) :: a
    integer(ip), intent(in) :: b
    integer(ip), intent(in), optional :: eps

    if (present(eps)) then
       val = (abs(a - b) .lt. eps)
    else
       val = (abs(a - b) .lt. eps_int)
    end if

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

    val = .not. unittest_assert_equal_dp(a, b, eps)

  end function unittest_assert_not_equal_dp

  pure logical function unittest_assert_not_equal_sp(a, b, eps) result(val)
    ! Not equality test for single precision reals. Duplicate of
    ! unittest_not_equal_dp
    real(sp), intent(in) :: a
    real(sp), intent(in) :: b
    real(sp), intent(in), optional :: eps

    val = .not. unittest_assert_equal_sp(a, b, eps)

  end function unittest_assert_not_equal_sp

  pure logical function unittest_assert_not_equal_int(a, b, eps) result(val)
    ! Not equality test for integers. Duplicate of unittest_assert_not_equal_dp
    integer(ip), intent(in) :: a
    integer(ip), intent(in) :: b
    integer(ip), intent(in), optional :: eps

    val = .not. unittest_assert_equal_int(a, b, eps)

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

    include "./unittest_src/assert_le.src"
  end function unittest_assert_le_dp

  pure logical function unittest_assert_le_sp(a, b, eps) result(val)
    ! Less than or equal test for single precision reals. Duplicate of
    ! unittest_assert_le_dp
    real(sp), intent(in) :: a
    real(sp), intent(in) :: b
    real(sp), intent(in), optional :: eps

    include "./unittest_src/assert_le.src"
  end function unittest_assert_le_sp

  pure logical function unittest_assert_lt_dp(a, b, eps) result(val)
    ! Less than test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    ! eps :: optional threshold for comparison
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp), intent(in), optional :: eps

    if (present(eps)) then
       val = ((a - b) .lt. eps)
    else
       val = ((a - b) .lt. eps_dp)
    end if

  end function unittest_assert_lt_dp

  pure logical function unittest_assert_lt_sp(a, b, eps) result(val)
    ! Less than test for single precision reals. Duplicate of
    ! unittest_assert_lt_dp
    real(sp), intent(in) :: a
    real(sp), intent(in) :: b
    real(sp), intent(in), optional :: eps

    if (present(eps)) then
       val = ((a - b) .lt. eps)
    else
       val = ((a - b) .lt. eps_sp)
    end if

  end function unittest_assert_lt_sp

  pure logical function unittest_assert_ge_dp(a, b, eps) result(val)
    ! Greater than or equal test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    ! eps :: optional custom threshold for double precision equality
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp), intent(in), optional :: eps

    include "./unittest_src/assert_ge.src"
  end function unittest_assert_ge_dp

  pure logical function unittest_assert_ge_sp(a, b, eps) result(val)
    ! Greater than or equal test for single precision reals. Duplicate of
    ! unittest_assert_get_dp
    real(sp), intent(in) :: a
    real(sp), intent(in) :: b
    real(sp), intent(in), optional :: eps

    include "./unittest_src/assert_ge.src"
  end function unittest_assert_ge_sp

  pure logical function unittest_assert_gt_dp(a, b, eps) result(val)
    ! Greater than test for double precision reals
    !
    ! a :: 1st value
    ! b :: 2nd value
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp), intent(in), optional :: eps

    if (present(eps)) then
       val = ((a - b) .gt. eps)
    else
       val = ((a - b) .gt. eps_dp)
    end if

  end function unittest_assert_gt_dp

  pure logical function unittest_assert_gt_sp(a, b, eps) result(val)
    ! Greater than test for single precision reals. Duplicate of
    ! unittest_assert_gt_dp
    real(sp), intent(in) :: a
    real(sp), intent(in) :: b
    real(sp), intent(in), optional :: eps

    if (present(eps)) then
       val = ((a - b) .gt. eps)
    else
       val = ((a - b) .gt. eps_sp)
    end if

  end function unittest_assert_gt_sp

  pure logical function unittest_assert_le_int(a, b, eps) result(val)
    ! Less than or equal test for integers. Duplicate of unittest_assert_le_dp
    integer(ip), intent(in) :: a
    integer(ip), intent(in) :: b
    integer(ip), intent(in), optional :: eps

    include "./unittest_src/assert_le.src"
  end function unittest_assert_le_int

  pure logical function unittest_assert_lt_int(a, b, eps) result(val)
    ! Less than test for integers. Duplicate of unittest_assert_lt_dp
    integer(ip), intent(in) :: a
    integer(ip), intent(in) :: b
    integer(ip), intent(in), optional :: eps

    if (present(eps)) then
       val = ((a - b) .lt. eps)
    else
       val = ((a - b) .lt. eps_int)
    end if

  end function unittest_assert_lt_int

  pure logical function unittest_assert_ge_int(a, b, eps) result(val)
    ! Greater than or equal test for integers. Duplicate of
    ! unittest_assert_ge_dp
    integer(ip), intent(in) :: a
    integer(ip), intent(in) :: b
    integer(ip), intent(in), optional :: eps

    include "./unittest_src/assert_ge.src"
  end function unittest_assert_ge_int

  pure logical function unittest_assert_gt_int(a, b, eps) result(val)
    ! Greater than test for integers. Duplicate of unittest_assert_gt_dp
    integer(ip), intent(in) :: a
    integer(ip), intent(in) :: b
    integer(ip), intent(in), optional :: eps

    if (present(eps)) then
       val = ((a - b) .gt. eps)
    else
       val = ((a - b) .gt. eps_int)
    end if

  end function unittest_assert_gt_int

end module unittest
