! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.
!
! This module implements basic exception-handling / logging functionality.
!
! Motivated by Python's logging module, we implement five different exception
! (logging) levels; these are given below, along with the corresponding integer
! exception level code.
!   (1) CRITICAL
!   (2) ERROR
!   (3) WARNING
!   (4) INFO
!   (5) DEBUG
! The ability to raise exceptions at all five levels is included for
! completeness and compatibility with the fie logging levels; standard practice
! is that "hard" exceptions occur at the ERROR or CRITICAL level, while "soft"
! exceptions are at the WARNING level. The INFO and DEBUG levels are not
! typically used for exceptions.
!
! Basic module usage:
!   call exception_raise("Exception message", exception_[LEVEL])
! where exception_[LEVEL] is the proper integer exception level code (which are
! set automatically in exception_[LEVEL] variables in this module).

! TODO
! Logging functionality: Write to STDOUT / STDERR / FILES
module exception

  use log, only: log_log, log_stderr, log_stdout, log_critical, log_error, &
       log_warning, log_info, log_debug

  implicit none

  private

  ! Public setup / cleanup methods
  public :: exception_init
  public :: exception_cleanup

  ! Methods to raise exceptions
  public :: exception_raise
  public :: exception_raise_critical
  public :: exception_raise_error
  public :: exception_raise_warning
  public :: exception_raise_info
  public :: exception_raise_debug

  ! Methods to view / set module configuration
  public :: exception_get_break_level
  public :: exception_set_break_level
  public :: exception_get_log_level
  public :: exception_set_log_level

  ! Exception levels
  public :: exception_critical
  public :: exception_error
  public :: exception_warning
  public :: exception_info
  public :: exception_debug
  integer, parameter :: exception_critical = log_critical
  integer, parameter :: exception_error = log_error
  integer, parameter :: exception_warning = log_warning
  integer, parameter :: exception_info = log_info
  integer, parameter :: exception_debug = log_debug

  ! Internal module variables
  integer :: break_level
  integer :: log_level

  integer :: log_file_unit
  logical :: log_to_stdout
  logical :: log_to_stderr

contains

  subroutine exception_init(break_lvl, log_lvl, log_unit, log_stdout,&
       log_stderr)
    ! Initialize exception module
    !
    ! break_lvl :: threshold for halting execution; see exception levels
    !   Default break threshold: ERROR exceptions and higher.
    ! log_lvl :: threshold for logging exception; see exception levels
    !   Default log threshold: WARNING exceptions and higher.
    ! log_file_unit :: unit number for open log file
    ! log_to_stdout :: flag for logging to stdout (as well as other locations)
    ! log_to_stderr :: flag for logging to stderr (as well as other locations)
    integer, optional, intent(in) :: break_lvl
    integer, optional, intent(in) :: log_lvl
    integer, optional, intent(in) :: log_unit
    logical, optional, intent(in) :: log_stdout
    logical, optional, intent(in) :: log_stderr

    if (present(break_lvl)) then
       break_level = break_lvl
    else
       break_level = exception_error
    end if

    if (present(log_lvl)) then
       log_level = log_lvl
    else
       log_level = exception_warning
    end if

    if (present(log_unit)) then
       log_file_unit = log_unit
    else
       log_file_unit = -1
    end if

    if (present(log_stdout) .and. log_stdout) then
       log_to_stdout = .true.
    else
       log_to_stdout = .false.
    end if

    if (present(log_stderr) .and. log_stderr) then
       log_to_stderr = .true.
    else
       log_to_stderr = .false.
    end if

  end subroutine exception_init

  subroutine exception_cleanup()
    ! Cleanup exception module
  end subroutine exception_cleanup

  subroutine exception_raise(msg, level)
    ! Raise an exception at a particular level
    !
    ! msg :: exception message
    ! level :: exception level integer
    !
    ! If the level is not properly specified, the exception is not raised and a
    ! WARNING-level exception message noting such is raised instead.
    character(*), intent(in) :: msg
    integer, intent(in) :: level

    select case(level)

    case (exception_critical)
       call exception_raise_critical(msg)

    case (exception_error)
       call exception_raise_error(msg)

    case (exception_warning)
       call exception_raise_warning(msg)

    case (exception_debug)
       call exception_raise_debug(msg)

    case (exception_info)
       call exception_raise_info(msg)

    case default
       call exception_raise_warning( &
            "Invalid exception level specification; not raising exception")

    end select

  end subroutine exception_raise

  subroutine exception_break(level)
    ! Process exception breaking
    !
    ! level :: exception level integer
    integer, intent(in) :: level

    if (level .le. break_level) then
       stop
    end if

  end subroutine exception_break

  subroutine exception_log(msg, level)
    ! Process exception logging.
    !
    ! msg :: exception message to log
    ! level :: exception level integer
    character(*), intent(in) :: msg
    integer, intent(in) :: level

    if (level .le. log_level) then

       if (log_file_unit .ge. 0) then
          call log_log(msg, log_file_unit, level)
       end if

       if (log_to_stdout) then
          call log_log(msg, log_stdout, level)
       end if

       if (log_to_stderr) then
          call log_log(msg, log_stderr, level)
       end if

    end if
  end subroutine exception_log

  subroutine exception_raise_critical(msg)
    ! Raise CRITICAL exception
    !
    ! msg :: exception message
    character(*), intent(in) :: msg

    call exception_log(msg, exception_critical)
    call exception_break(exception_critical)

  end subroutine exception_raise_critical

  subroutine exception_raise_error(msg)
    ! Raise ERROR exception
    !
    ! msg :: exception message
    character(*), intent(in) :: msg

    call exception_log(msg, exception_error)
    call exception_break(exception_error)

  end subroutine exception_raise_error

  subroutine exception_raise_warning(msg)
    ! Raise WARNING exception
    !
    ! msg :: exception message
    character(*), intent(in) :: msg

    call exception_log(msg, exception_warning)
    call exception_break(exception_warning)

  end subroutine exception_raise_warning

  subroutine exception_raise_info(msg)
    ! Raise INFO exception
    !
    ! msg :: exception message
    character(*), intent(in) :: msg

    call exception_log(msg, exception_info)
    call exception_break(exception_info)

  end subroutine exception_raise_info

  subroutine exception_raise_debug(msg)
    ! Raise DEBUG exception
    !
    ! msg :: exception message
    character(*), intent(in) :: msg

    call exception_log(msg, exception_debug)
    call exception_break(exception_debug)

  end subroutine exception_raise_debug

  integer function exception_get_break_level() result(val)
    ! Get current exception breaking threshold
    val = break_level

  end function exception_get_break_level

  integer function exception_get_log_level() result(val)
    ! Get current exception logging threshold
    val = log_level

  end function exception_get_log_level

  subroutine exception_set_break_level(level)
    ! Set exception breaking threshold
    !
    ! level :: exception level integer
    integer, intent(in) :: level

    break_level = level

  end subroutine exception_set_break_level

  subroutine exception_set_log_level(level)
    ! Set exception logging level
    !
    ! level :: exception level integer
    integer, intent(in) :: level

    log_level = level
  end subroutine exception_set_log_level

end module exception
