! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file in the top-level directory of this distribution
!
! Global variables / numerical constants
module globvars
  use iso_fortran_env, only: real32, real64, int32

  implicit none

  ! Single precision real type / formatting
  integer(int32), parameter :: sp = real32
  real(sp), parameter :: sp_epsilon = epsilon(0.0_sp)
  character(*), parameter :: sp_format_raw = "es16.8e3"
  character(*), parameter :: sp_format = "("//sp_format_raw//")"

  ! Double precision real type / formatting
  integer(int32), parameter :: dp = real64
  real(dp), parameter :: dp_epsilon = epsilon(0.0_dp)
  character(*), parameter :: dp_format_raw = "es23.16e3"
  character(*), parameter :: dp_format = "("//dp_format_raw//")"

  ! Integer single precision type / formatting
  integer(int32), parameter :: ip = int32
  character(*), parameter :: int_format = "(i0)"
  character(*), parameter :: int_format_raw = "i0"

  ! Useful numbers
  real(dp), parameter :: pi_dp = 4.0_dp * atan(1.0_dp)
  real(dp), parameter :: e_dp = exp(1.0_dp)
  complex(dp), parameter :: j_dp = (0.0_dp, 1.0_dp)

  real(sp), parameter :: pi_sp = real(pi_dp, kind=sp)
  real(sp), parameter :: e_sp = real(e_dp, kind=sp)
  complex(sp), parameter :: j_sp = cmplx(j_dp, kind=sp)

end module globvars
