! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file in the top-level directory of this distribution
!
! This module implements basic string conversions.
module string

  use globvars, only: dp, dp_format, sp, sp_format, ip, int_format

  implicit none

  private

  public :: string_val
  public :: string_to_val

  interface string_val
     ! Get string representation of values
     module procedure string_dp_real
     module procedure string_sp_real
     module procedure string_int
     module procedure string_logical
  end interface string_val

  interface string_to_val
     ! Convert string representations to values
     module procedure string_to_dp_real
     module procedure string_to_sp_real
     module procedure string_to_int
     module procedure string_to_logical
  end interface string_to_val

contains

  pure function string_dp_real(num, fmt) result(val)
    ! Get string representation of double precision real
    !
    ! num :: double precision real value to convert
    ! fmt :: optional custom format string
    character(:), allocatable :: val
    real(dp), intent(in) :: num
    character(*), optional, intent(in) :: fmt

    character(120) :: tmp

    if (present(fmt)) then
       write(tmp, fmt) num
    else
       write(tmp, dp_format) num
    end if

    val = trim(adjustl(tmp))

  end function string_dp_real

  pure function string_sp_real(num, fmt) result(val)
    ! Get string representation of double precision real
    !
    ! num :: double precision real value to convert
    ! fmt :: optional custom format string
    character(:), allocatable :: val
    real(sp), intent(in) :: num
    character(*), optional, intent(in) :: fmt

    character(120) :: tmp

    if (present(fmt)) then
       write(tmp, fmt) num
    else
       write(tmp, sp_format) num
    end if

    val = trim(adjustl(tmp))

  end function string_sp_real

  pure function string_int(num, fmt) result(val)
    ! Get string representation of integer
    !
    ! num :: integer value to convert
    ! fmt :: optional custom format string
    character(:), allocatable :: val

    integer(ip), intent(in) :: num
    character(*), optional, intent(in) :: fmt

    character(120) :: tmp

    if (present(fmt)) then
       write(tmp, fmt) num
    else
       write(tmp, int_format) num
    end if

    val = trim(adjustl(tmp))

  end function string_int

  pure function string_logical(bool, full) result(val)
    ! Get string representation of logical value
    !
    ! bool :: logical value to convert
    ! full :: toggle whether only first letter (T/F) or full word is used
    character(:), allocatable :: val

    logical, intent(in) :: bool
    logical, intent(in), optional :: full

    logical :: full_str

    if (present(full)) then
       full_str = full
    else
       full_str = .true.
    end if

    if (bool) then
       if (full_str) then
          val = "True"
       else
          val = "T"
       end if
    else
       if (full_str) then
          val = "False"
       else
          val = "F"
       end if
    end if

  end function string_logical

  subroutine string_to_int(str, val)
    ! Get integer value from string
    !
    ! str :: string to convert
    ! val :: output value
    character(*), intent(in) :: str
    integer(ip), intent(out) :: val

    read(str, *) val
  end subroutine string_to_int

  subroutine string_to_dp_real(str, val)
    ! Get double precision real value from string
    !
    ! str :: string to convert
    ! val :: output value
    character(*), intent(in) :: str
    real(dp), intent(out) :: val

    read(str, *) val
  end subroutine string_to_dp_real

  subroutine string_to_sp_real(str, val)
    ! Get single precision real value from string
    !
    ! str :: string to convert
    ! val :: output value
    character(*), intent(in) :: str
    real(sp), intent(out) :: val

    read(str, *) val
  end subroutine string_to_sp_real

  subroutine string_to_logical(str, val)
    ! Get logical value from string
    !
    ! str :: string to convert
    ! val :: output value
    character(*), intent(in) :: str
    logical, intent(out) :: val

    select case(trim(adjustl(str)))
    case ("T", "True", "true", ".true.")
       val = .true.
    case ("F", "False", "false", ".false.")
       val = .false.
    end select

  end subroutine string_to_logical

end module string
