! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file in the top-level directory of this distribution
!
! Global variables / numerical constants
module globvars
  use iso_fortran_env, only: real64, int32

  implicit none

  ! Double precision real type / formatting
  integer, parameter :: dp = real64
  character(*), parameter :: dp_format = "(es23.16e3)"
  character(*), parameter :: dp_format_raw = "es23.16e3"

  ! Integer single precision type / formatting
  integer, parameter :: ip = int32
  character(*), parameter :: int_format = "(i0)"
  character(*), parameter :: int_format_raw = "i0"

  ! Useful numbers
  real(dp), parameter :: zero = 0.0_dp
  real(dp), parameter :: one = 1.0_dp
  real(dp), parameter :: two = 2.0_dp
  real(dp), parameter :: half = 0.5_dp

  real(dp), parameter :: pi = 4.0_dp * atan(one)
  real(dp), parameter :: sqrt_pi = sqrt(pi)
  real(dp), parameter :: tau = two * pi
  real(dp), parameter :: e = exp(one)

  complex(dp), parameter :: j = (zero, one)

end module globvars
