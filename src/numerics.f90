! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file in the top-level directory of this distribution.
!
! This module includes an assortment of common numerical routines
module numerics
  use globvars, only: dp, sp, ip

  implicit none

  private

  public :: numerics_linspace
  public :: numerics_d1
  public :: numerics_d2
  public :: numerics_rk4
  public :: numerics_cmplx_phase
  public :: numerics_factorial
  public :: numerics_trapz

  interface numerics_linspace
     module procedure numerics_linspace_dp
     module procedure numerics_linspace_sp
  end interface numerics_linspace

  interface numerics_d1
     ! Calculate the 1st derivative of an array-valued function with respect to
     ! a real-valued coordinate grid with constant spacing.
     !
     ! This uses a three-point finite-difference estimate:
     !
     !   f'(x) ~= ( f(x + dx) - f(x - dx) ) / ( 2 dx )
     !
     ! See documentation for specific module procedures for more details.
     module procedure numerics_d1_real_dp
     module procedure numericS_d1_real_sp
     module procedure numerics_d1_cmplx_dp
     module procedure numerics_d1_cmplx_sp
  end interface numerics_d1

  interface numerics_d2
     ! Calculate the 2nd derivative of an array-valued function with respect to
     ! a real-valued coordinate grid with constant spacing.
     !
     ! This uses a three-point finite difference estimate:
     !
     !   f''(x) ~= (f(x - dx) - 2 f(x) + f(x + dx)) / (dx**2)
     !
     ! See documentation for specific module procedures for more details.
     module procedure numerics_d2_real_dp
     module procedure numerics_d2_real_sp
     module procedure numerics_d2_cmplx_dp
     module procedure numerics_d2_cmplx_sp
  end interface numerics_d2

  interface numerics_rk4
     ! 4th-order Runge-Kutta propagator for the first order system of
     ! differential equations dy/dt = f(y, t)
     !
     ! See documentation for specific module procedures for more details.
     module procedure numerics_rk4_real_dp
     module procedure numerics_rk4_real_sp
     module procedure numerics_rk4_cmplx_dp
     module procedure numerics_rk4_cmplx_sp
  end interface numerics_rk4

  interface numerics_cmplx_phase
     ! Calculate the phase of a complex number z = A exp(i phi)
     !
     ! See documentation for specific module procedures for more details.
     module procedure numerics_cmplx_phase_dp
     module procedure numerics_cmplx_phase_sp
  end interface numerics_cmplx_phase

  interface numerics_trapz
     ! Trapezoidal integration scheme:
     !
     ! int_a^b f(x) dx ~= dx / 2 \sum_{k=1}^N ( f(i_k) + f(i_{k+1}) )
     !
     ! See documentation for specific module procedures for more details
     module procedure numerics_trapz_dp
     module procedure numerics_trapz_sp
  end interface numerics_trapz

contains

   subroutine numerics_linspace_dp(x_min, x_max, x_arr, dx)
     ! Populate an array with linearly-spaced values between x_min and x_max
     !
     ! x_min :: minimum array value
     ! x_max :: maximum array value
     ! x_arr(:) :: array to populate, must already be allocated
     ! dx :: step size of `x_arr(:)`, given its size and `x_min`, `x_max`
    real(dp), intent(in) :: x_min, x_max
    real(dp), intent(out) :: x_arr(:)
    real(dp), intent(out) :: dx

    include "./numerics_src/linspace.src"
  end subroutine numerics_linspace_dp

  subroutine numerics_linspace_sp(x_min, x_max, x_arr, dx)
    ! Duplicate of numerics_linspace_dp for single precision reals
    real(sp), intent(in) :: x_min, x_max
    real(sp), intent(out) :: x_arr(:)
    real(sp), intent(out) :: dx

    include "./numerics_src/linspace.src"
  end subroutine numerics_linspace_sp

  subroutine numerics_d1_real_dp(arr, d_arr, dx)
    ! Calculate the 1st derivative of a double-precision real array-valued
    ! function with respect to a real-valued coordinate grid with constant
    ! grid spacing.
    !
    ! arr :: function array
    ! d_arr :: array to populate with 1st derivative values
    ! dx :: coordinate spacing
    real(dp), intent(in) :: arr(:)
    real(dp), intent(out) :: d_arr(:)
    real(dp), intent(in) :: dx

    include "./numerics_src/d1_dp.src"
  end subroutine numerics_d1_real_dp

  subroutine numerics_d1_real_sp(arr, d_arr, dx)
    real(sp), intent(in) :: arr(:)
    real(sp), intent(out) :: d_arr(:)
    real(sp), intent(in) :: dx

    include "./numerics_src/d1_sp.src"
  end subroutine numerics_d1_real_sp

  subroutine numerics_d1_cmplx_dp(arr, d_arr, dx)
    ! Duplication of numerics_d1_real_dp, but for double precision complex types
    !
    ! Once Fortran 2008 support is here, we can get rid of the duplicated code
    ! and instead use the numerics_d1_real method on the real and imaginary
    ! parts separately:
    !   call subroutine numerics_d1_real(arr%re, d_arr%re, dx)
    !   call subroutine numerics_d1_real(arr%im, d_arr%im, dx)
    complex(dp), intent(in) :: arr(:)
    complex(dp), intent(out) :: d_arr(:)
    real(dp), intent(in) :: dx

    include "./numerics_src/d1_dp.src"
  end subroutine numerics_d1_cmplx_dp

  subroutine numerics_d1_cmplx_sp(arr, d_arr, dx)
    ! Duplication of numerics_d1_real_dp, but for single precision complex types
    complex(sp), intent(in) :: arr(:)
    complex(sp), intent(out) :: d_arr(:)
    real(sp), intent(in) :: dx

    include "./numerics_src/d1_sp.src"
  end subroutine numerics_d1_cmplx_sp

  subroutine numerics_d2_real_dp(arr, d2_arr, dx)
    ! Calculate the 2nd derivative of a complex array-valued function with
    ! respect to a real-valued coordinate with constant spacing:
    !
    ! arr :: function array
    ! d2_arr :: array to populate with 2nd derivative values
    ! dx :: coordinate spacing
    real(dp), intent(in) :: arr(:)
    real(dp), intent(inout) :: d2_arr(:)
    real(dp), intent(in) :: dx

    include "./numerics_src/d2_dp.src"
  end subroutine numerics_d2_real_dp

  subroutine numerics_d2_real_sp(arr, d2_arr, dx)
    ! Duplicate of numerics_d2_real_sp, but for single precision reals
    real(sp), intent(in) :: arr(:)
    real(sp), intent(inout) :: d2_arr(:)
    real(sp), intent(in) :: dx

    include "./numerics_src/d2_sp.src"
  end subroutine numerics_d2_real_sp

  subroutine numerics_d2_cmplx_dp(arr, d2_arr, dx)
    ! Duplicate of numerics_d2_real_dp, but for double precision complex types
    complex(dp), intent(in) :: arr(:)
    complex(dp), intent(inout) :: d2_arr(:)
    real(dp), intent(in) :: dx

    include "./numerics_src/d2_dp.src"
  end subroutine numerics_d2_cmplx_dp

  subroutine numerics_d2_cmplx_sp(arr, d2_arr, dx)
    ! Duplicate of numerics_d2_real_dp, but for single precision complex types
    complex(sp), intent(in) :: arr(:)
    complex(sp), intent(inout) :: d2_arr(:)
    real(sp), intent(in) :: dx

    include "./numerics_src/d2_sp.src"
  end subroutine numerics_d2_cmplx_sp

  subroutine numerics_rk4_real_dp(f, y, t, dt)
    ! 4th order Runge-Kutta propagator for the first order system of differential
    ! equations dy/dt = f(t, y)
    !
    ! f :: differential equation dy/dt = f(y, t) that returns values of the same
    !   dimension as y
    ! y :: current state array
    ! t :: current time parameter
    ! dt :: time step
    !
    ! Note that the state array y is overwritten during propagation.
    real(dp), intent(inout) :: y(:)
    real(dp), intent(in) :: t
    real(dp), intent(in) :: dt

    real(dp), dimension(size(y)) :: k1, k2, k3, k4

    ! Explicit interface for external differential equation system
    interface

       pure function f(y, t) result(val)
         import dp
         real(dp), intent(in) :: y(:)
         real(dp), intent(in) :: t
         real(dp) :: val(size(y))
       end function f

    end interface

    include "./numerics_src/rk4_dp.src"
  end subroutine numerics_rk4_real_dp

  subroutine numerics_rk4_real_sp(f, y, t, dt)
    ! Duplicate of numerics_rk4_real_dp, but for single precision reals
    real(sp), intent(inout) :: y(:)
    real(sp), intent(in) :: t
    real(sp), intent(in) :: dt

    real(sp), dimension(size(y)) :: k1, k2, k3, k4

    interface

       pure function f(y, t) result(val)
         import sp
         real(sp), intent(in) :: y(:)
         real(sp), intent(in) :: t
         real(sp) :: val(size(y))
       end function f

    end interface

    include "./numerics_src/rk4_sp.src"
  end subroutine numerics_rk4_real_sp

  subroutine numerics_rk4_cmplx_dp(f, y, t, dt)
    ! Duplicate of numerics_rk4_real_dp, but for double precision complex numbers
    complex(dp), intent(inout) :: y(:)
    real(dp), intent(in) :: t
    real(dp), intent(in) :: dt

    complex(dp), dimension(size(y)) :: k1, k2, k3, k4

    interface

       pure function f(y, t) result(val)
         import dp
         complex(dp), intent(in) :: y(:)
         real(dp), intent(in) :: t
         complex(dp) :: val(size(y))
       end function f

    end interface

    include "./numerics_src/rk4_dp.src"
  end subroutine numerics_rk4_cmplx_dp

  subroutine numerics_rk4_cmplx_sp(f, y, t, dt)
    ! Duplicate of numerics_rk4_real_dp, but for single precision complex numbers
    complex(sp), intent(inout) :: y(:)
    real(sp), intent(in) :: t
    real(sp), intent(in) :: dt

    complex(sp), dimension(size(y)) :: k1, k2, k3, k4

    interface

       pure function f(y, t) result(val)
         import sp
         complex(sp), intent(in) :: y(:)
         real(sp), intent(in) :: t
         complex(sp) :: val(size(y))
       end function f

    end interface

    include "./numerics_src/rk4_sp.src"
  end subroutine numerics_rk4_cmplx_sp

  pure real(dp) function numerics_cmplx_phase_dp(z) result(val)
    ! Get the phase of a complex number z = A exp(i phi), where A determines the
    ! magnitude, and phi determines the phase angle.
    !
    ! z :: complex number
    complex(dp), intent(in) :: z

    include "./numerics_src/cmplx_phase.src"

  end function numerics_cmplx_phase_dp

  pure real(sp) function numerics_cmplx_phase_sp(z) result(val)
    ! Duplicate of numerics_cmplx_phase_sp, but for single precision numbers
    complex(sp), intent(in) :: z

    include "./numerics_src/cmplx_phase.src"

  end function numerics_cmplx_phase_sp

  pure real(dp) function numerics_trapz_dp(f_arr, dx) result(val)
    ! Trapezoidal integration scheme
    !
    ! f_arr :: array-valued function to integrate
    ! dx :: integration step
    real(dp), intent(in) :: f_arr(:)
    real(dp), intent(in) :: dx

    integer(ip) :: i_x
    real(dp) :: trapz_cnst

    trapz_cnst = dx / 2.0_dp
    val = 0.0_dp

    include "./numerics_src/trapz.src"
  end function numerics_trapz_dp

  pure real(sp) function numerics_trapz_sp(f_arr, dx) result(val)
    ! Duplicate of numerics_trapz for single precision reals
    real(sp), intent(in) :: f_arr(:)
    real(sp), intent(in) :: dx

    integer(ip) :: i_x
    real(sp) :: trapz_cnst

    trapz_cnst = dx / 2.0_sp
    val = 0.0_sp

    include "./numerics_src/trapz.src"
  end function numerics_trapz_sp

  pure integer(ip) function numerics_factorial(n) result(val)
    ! Factorial function
    !
    ! We use Fortran 2003's built in gamma-function implementation for this
    integer(ip), intent(in) :: n

    val = int(gamma( 1.0_dp * (n + 1) ), kind=ip)
  end function numerics_factorial

end module numerics
