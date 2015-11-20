! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file in the top-level directory of this distribution.
!
! This module includes an assortment of common numerical routines
module numerics
  use globvars, only: dp

  implicit none

  private

  public :: numerics_linspace
  public :: numerics_d1
  public :: numerics_d2
  public :: numerics_rk4
  public :: numerics_cmplx_phase
  public :: numerics_trapz

  interface numerics_d1
     ! Calculate the 1st derivative of an array-valued function with respect to
     ! a real-valued coordinate grid with constant spacing.
     !
     ! This uses a three-point finite-difference estimate:
     !
     !   f'(x) ~= ( f(x + dx) - f(x - dx) ) / ( 2 dx )
     !
     ! See documentation for specific module procedures for more details.
     module procedure numerics_d1_real
     module procedure numerics_d1_cmplx
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
     module procedure numerics_d2_real
     module procedure numerics_d2_cmplx
  end interface numerics_d2

  interface numerics_rk4
     ! 4th-order Runge-Kutta propagator for the first order system of
     ! differential equations dy/dt = f(y, t)
     !
     ! See documentation for specific module procedures for more details.
     module procedure numerics_rk4_real
     module procedure numerics_rk4_cmplx
  end interface numerics_rk4

contains

   subroutine numerics_linspace(x_min, x_max, x_arr, dx)
     ! Populate an array with linearly-spaced values between x_min and x_max
     !
     ! x_min :: minimum array value
     ! x_max :: maximum array value
     ! x_arr(:) :: array to populate, must already be allocated
     ! dx :: step size of `x_arr(:)`, given its size and `x_min`, `x_max`
    real(dp), intent(in) :: x_min, x_max
    real(dp), intent(out) :: x_arr(:)
    real(dp), intent(out) :: dx

    integer :: i_x, n_x

    n_x = size(x_arr)
    dx = (x_max - x_min) / n_x

    do i_x = 1, n_x
       x_arr(i_x) = x_min + i_x * dx
    end do

  end subroutine numerics_linspace

  subroutine numerics_d1_real(arr, d_arr, dx)
    ! Calculate the 1st derivative of an real array-valued function with
    ! respect to a real-valued coordinate grid with constant grid spacing.
    !
    ! arr :: function array
    ! d_arr :: array to populate with 1st derivative values
    ! dx :: coordinate spacing
    real(dp), intent(in) :: arr(:)
    real(dp), intent(out) :: d_arr(:)
    real(dp), intent(in) :: dx

    integer :: i_x, n_x
    real(dp) :: scale

    n_x = size(arr)
    scale = 1.0_dp / (2.0_dp * dx)

    ! Special edge cases - we assume the grid is fine enough and the function
    ! is smooth enough that this is accurate enough. For the left case:
    !
    !   f(x + h) = arr(2), f(x - h) = arr(1)
    !
    ! For the right case:
    !
    !   f(x + h) = arr(n), f(x - h) = arr(n - 1)
    d_arr(1) = arr(2) - arr(1)
    do i_x = 2, n_x - 1
       d_arr(i_x) = arr(i_x + 1) - arr(i_x - 1)
    end do
    d_arr(n_x) = arr(n_x) - arr(n_x - 1)

    d_arr(:) = scale * d_arr(:)

  end subroutine numerics_d1_real

  subroutine numerics_d1_cmplx(arr, d_arr, dx)
    ! Duplication of numerics_d1_real, but for a complex array-valued function.
    !
    ! Once Fortran 2008 support is here, we can get rid of the duplicated code
    ! and instead use the numerics_d1_real method on the real and imaginary
    ! parts separately:
    !   call subroutine numerics_d1_real(arr%re, d_arr%re, dx)
    !   call subroutine numerics_d1_real(arr%im, d_arr%im, dx)
    complex(dp), intent(in) :: arr(:)
    complex(dp), intent(out) :: d_arr(:)
    real(dp), intent(in) :: dx

    integer :: i_x, n_x
    real(dp) :: scale

    n_x = size(arr)
    scale = 1.0_dp / (2.0_dp * dx)

    d_arr(1) = arr(2) - arr(1)
    do i_x = 2, n_x - 1
       d_arr(i_x) = arr(i_x + 1) - arr(i_x - 1)
    end do
    d_arr(n_x) = arr(n_x) - arr(n_x - 1)

    d_arr(:) = scale * d_arr(:)

  end subroutine numerics_d1_cmplx

  subroutine numerics_d2_real(arr, d2_arr, dx)
    ! Calculate the 2nd derivative of a complex array-valued function with
    ! respect to a real-valued coordinate with constant spacing:
    !
    ! arr :: function array
    ! d2_arr :: array to populate with 2nd derivative values
    ! dx :: coordinate spacing
    real(dp), intent(in) :: arr(:)
    real(dp), intent(inout) :: d2_arr(:)
    real(dp), intent(in) :: dx
    integer :: i_x, n_x
    real(dp) :: scale

    n_x = size(arr)
    scale = 1.0_dp / dx**2

    ! Special edge cases - we assume the grid is fine enough and the function
    ! is smooth enough that this is accurate enough. For the left case:
    !
    !   f(x + h) = arr(2), f(x - h) = arr(1)
    !
    ! For the right case:
    !
    !   f(x + h) = arr(n), f(x - h) = arr(n - 1)
    d2_arr(1) = arr(1) + arr(2)
    do i_x = 2, n_x - 1
       d2_arr(i_x) = arr(i_x - 1) - 2 * arr(i_x) + arr(i_x + 1)
    end do
    d2_arr(n_x) = arr(n_x) + arr(n_x - 1)

    d2_arr(:) = scale * d2_arr(:)

  end subroutine numerics_d2_real

  subroutine numerics_d2_cmplx(arr, d2_arr, dx)
    ! Duplication of numerics_d2_real, but for a complex array-valued function.
    complex(dp), intent(in) :: arr(:)
    complex(dp), intent(inout) :: d2_arr(:)
    real(dp), intent(in) :: dx
    integer :: i_x, n_x
    real(dp) :: scale

    n_x = size(arr)
    scale = 1.0_dp / dx**2

    d2_arr(1) = arr(1) + arr(2)
    do i_x = 2, n_x - 1
       d2_arr(i_x) = arr(i_x - 1) - 2 * arr(i_x) + arr(i_x + 1)
    end do
    d2_arr(n_x) = arr(n_x) + arr(n_x - 1)

    d2_arr(:) = scale * d2_arr(:)

  end subroutine numerics_d2_cmplx

  subroutine numerics_rk4_real(f, y, t, dt)
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

       function f(y, t) result(val)
         import dp
         real(dp) :: y(:)
         real(dp) :: t
         real(dp) :: val(size(y))
       end function f

    end interface

    k1 = f(y, t)
    k2 = f(y + dt / 2 * k1, t + dt / 2)
    k3 = f(y + dt / 2 * k2, t + dt / 2)
    k4 = f(y + dt * k3, t + dt)

    y = y + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)

  end subroutine numerics_rk4_real

  subroutine numerics_rk4_cmplx(f, y, t, dt)
    ! Duplication of numerics_rk4_real, but for a complex-valued function.
    complex(dp), intent(inout) :: y(:)
    real(dp), intent(in) :: t
    real(dp), intent(in) :: dt

    complex(dp), dimension(size(y)) :: k1, k2, k3, k4

    interface

       function f(y, t) result(val)
         import dp
         complex(dp) :: y(:)
         real(dp) :: t
         complex(dp) :: val(size(y))
       end function f

    end interface

    k1 = f(y, t)
    k2 = f(y + dt / 2 * k1, t + dt / 2)
    k3 = f(y + dt / 2 * k2, t + dt / 2)
    k4 = f(y + dt * k3, t + dt)

    y = y + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)

  end subroutine numerics_rk4_cmplx

  real(dp) function numerics_cmplx_phase(z) result(val)
    ! Get the phase of a complex number z = A exp(i phi), where A determines the
    ! magnitude, and phi determines the phase angle.
    !
    ! z :: complex number
    complex(dp), intent(in) :: z

    val = atan2(aimag(z),real(z))

  end function numerics_cmplx_phase

  real(dp) function numerics_trapz(f_arr, dx) result(val)
    ! Trapezoidal integration scheme
    !
    ! f_arr :: array-valued function to integrate
    ! dx :: integration step
    real(dp), intent(in) :: f_arr(:)
    real(dp), intent(in) :: dx

    integer :: i_x
    real(dp) :: trapz_cnst

    trapz_cnst = dx / 2.0_dp
    val = 0.0_dp

    do i_x = 1, size(f_arr) - 1
       val = val + (f_arr(i_x) + f_arr(i_x + 1))
    end do

    val = val * trapz_cnst

  end function numerics_trapz

end module numerics
