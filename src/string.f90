module string

  use globvars, only: dp, dp_format, int_format

  implicit none

  private

  public str_num
  interface str_num
     ! Get string representation of numbers
     module procedure str_dp_real
     module procedure str_int
  end interface str_num

contains

  function str_dp_real(num, fmt) result(val)
    ! Get string representation of double precision real
    character(:), allocatable :: val

    real(dp), intent(in) :: num
    character(*), optional, intent(in) :: fmt

    character(:), allocatable :: tmp

    if (present(fmt)) then
       write(tmp, fmt) num
    else
       write(tmp, dp_format) num
    end if

    val = trim(adjustl(tmp))

  end function str_dp_real

  function str_int(num, fmt) result(val)
    ! Get string representation of single precision integer
    character(:), allocatable :: val

    integer, intent(in) :: num
    character(*), optional, intent(in) :: fmt

    character(:), allocatable :: tmp

    if (present(fmt)) then
       write(tmp, fmt) num
    else
       write(tmp, int_format) num
    end if

    val = trim(adjustl(tmp))

  end function str_int

  function str_logical(bool, full) result(val)
    ! Get string representation of logical value

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

  end function str_logical

end module string
