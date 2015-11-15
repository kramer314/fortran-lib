
module config

  use globvars, only: dp
  use string, only: string_to_val

  implicit none

  private

  public :: config_init
  public :: config_cleanup
  public :: config_get_param

  interface config_get_param
     ! Get parameter
     module procedure config_get_int
     module procedure config_get_real_dp
     module procedure config_get_logical
     module procedure config_get_char
  end interface config_get_param

  integer, parameter :: default_line_length = 80

  integer :: num_params
  integer :: line_length

  character(:), allocatable :: param_lines(:)

contains

  subroutine config_init(filename, max_line_length)
    character(*), intent(in) :: filename
    integer, intent(in), optional :: max_line_length

    integer :: file_unit
    integer :: io_err
    integer :: i_param

    character(line_length) :: tmp_readin

    if (present(max_line_length)) then
       line_length = max_line_length
    else
       line_length = default_line_length
    end if

    open(newunit=file_unit, file=filename)

    ! First sweep to determine number of parameters
    num_params = 0
    do
       read(file_unit, *, iostat=io_err) tmp_readin

       if (io_err .ne. 0) then
          exit
       else
          if (is_valid_param(tmp_readin)) then
             num_params = num_params + 1
          end if
       end if

    end do
    close(file_unit)

    allocate(character(line_length) :: param_lines(num_params))

    open(newunit=file_unit, file=filename)

    ! Second sweep to read in parameters
    i_param = 1
    do
       read(file_unit, *, iostat=io_err) tmp_readin

       if (io_err .ne. 0) then
          exit
       else
          if (is_valid_param(tmp_readin)) then
             param_lines(i_param) = tmp_readin
             i_param = i_param + 1
          end if

       end if

    end do

    close(file_unit)

  contains

    logical function is_valid_param(str) result(val)
      ! Check if the parameter string is valid
      character(*), intent(in) :: str
      character(:), allocatable :: tmp

      tmp = trim(adjustl(str))

      if (tmp(1:1) .ne. "!" .and. tmp .ne. "") then
         val = .true.
      else
         val = .false.
      end if
    end function is_valid_param

  end subroutine config_init

  subroutine config_cleanup()
    ! Module cleanup method
    deallocate(param_lines)
  end subroutine config_cleanup

  subroutine config_get_int(param_name, val, found)
    character(*), intent(in) :: param_name
    integer, intent(out) :: val
    logical, intent(out) :: found

    character(line_length) :: param_str

    call config_get_param_str(param_name, param_str, found)
    if (found) then
       call str_to_val(param_str, val)
    end if

  end subroutine config_get_int

  subroutine config_get_real_dp(param_name, val, found)
    character(*), intent(in) :: param_name
    real(dp), intent(out) :: val
    logical, intent(out) :: found

    character(line_length) :: param_str

    call config_get_param_str(param_name, param_str, found)
    if (found) then
       call str_to_val(param_str, val)
    end if

  end subroutine config_get_real_dp

  subroutine config_get_char(param_name, val, found)
    character(*), intent(in) :: param_name
    character(:), allocatable, intent(out) :: val
    logical, intent(out) :: found

    character(line_length) :: param_str

    call config_get_param_str(param_name, param_str, found)
    if (found) then
       val = trim(adjustl(param_str))
    end if

  end subroutine config_get_char

  subroutine config_get_logical(param_name, val, found)
    character(*), intent(in) :: param_name
    logical, intent(out) :: val
    logical, intent(out) :: found

    character(line_length) :: param_str

    call config_get_param_str(param_name, param_str, found)
    if (found) then
       call string_to_val(param_str, val)
    end if

  end subroutine config_get_logical

  subroutine config_get_param_str(param_name, str, found)
    character(*), intent(in) :: param_name
    character(*), intent(out) :: str
    logical, intent(out) :: found

    integer :: i_param
    integer :: val_idex

    found = .false.
    do i_param = 1, num_params

       ! Match parameter name and get string value
       if (index(param_lines(i_param), trim(param_name)) .gt. 0) then
          val_idex = index(param_lines(i_param), "=")
          str = param_lines(i_param)(val_idex + 1:)
          found = .true.
          exit
       end if

    end do

  end subroutine config_get_param_str

end module config
