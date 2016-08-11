! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.
!
! This module implements basic logging functionality.
!
! Motivated by Python's logging module, we implement five different exception
! (logging) levels; these are given below, along with teh corresponding integer
! logging level code.
!   (1) CRITICAL
!   (2) ERROR
!   (3) WARNING
!   (4) INFO
!   (5) DEBUG
!
! Basic module usage:
!   call log_log("Logging message", log_stdout, log_info)
! logs "Logging message" to stdout at the INFO logging level.
module log

  use iso_fortran_env, only: output_unit, error_unit

  use globvars, only: ip

  implicit none

  private

  ! Methods to log messages
  public :: log_log
  public :: log_log_critical
  public :: log_log_error
  public :: log_log_warning
  public :: log_log_info
  public :: log_log_debug
  public :: log_message

  ! stdout / stderr file units
  public log_stderr
  public log_stdout
  integer, parameter :: log_stderr = error_unit
  integer, parameter :: log_stdout = output_unit

  ! Logging levels
  public :: log_critical
  public :: log_error
  public :: log_warning
  public :: log_info
  public :: log_debug
  integer, parameter :: log_critical = 1
  integer, parameter :: log_error = 2
  integer, parameter :: log_warning = 3
  integer, parameter :: log_info = 4
  integer, parameter :: log_debug = 5

contains
  subroutine log_log(msg, unit, level)
    ! Log message at a particular level
    !
    ! msg :: log message
    ! unit :: log file unit
    ! level :: logging level
    !
    ! If the level is not properly specifies, the message is not logeed and a
    ! WARNING-level message is sent to stderr.
    character(*), intent(in) :: msg
    integer(ip), intent(in) :: unit
    integer(ip), intent(in) :: level

    select case(level)

    case (log_critical)
       call log_log_critical(msg, unit)

    case (log_error)
       call log_log_error(msg, unit)

    case (log_warning)
       call log_log_warning(msg, unit)

    case (log_info)
       call log_log_info(msg, unit)

    case (log_debug)
       call log_log_debug(msg, unit)

    case default
       call log_log_warning("Invalidid logging level specification", &
            log_stderr)

    end select

  end subroutine log_log

  subroutine log_log_critical(msg, unit)
    ! Log CRITICAL message
    !
    ! msg :: message to log
    ! unit :: log file unit
    character(*), intent(in) :: msg
    integer(ip), intent(in) :: unit

    call log_message(msg, "CRITICAL", unit)

  end subroutine log_log_critical

  subroutine log_log_error(msg, unit)
    ! Log ERROR message
    !
    ! msg :: message to log
    ! unit :: log file unit
    character(*), intent(in) :: msg
    integer(ip), intent(in) :: unit

    call log_message(msg, "ERROR", unit)

  end subroutine log_log_error

  subroutine log_log_warning(msg, unit)
    ! Log WARNING message
    !
    ! msg :: message to log
    ! unit :: log file unit
    character(*), intent(in) :: msg
    integer(ip), intent(in) :: unit

    call log_message(msg, "WARNING", unit)

  end subroutine log_log_warning

  subroutine log_log_info(msg, unit)
    ! Log INFO message
    !
    ! msg :: message to log
    ! unit :: log file unit
    character(*), intent(in) :: msg
    integer(ip), intent(in) :: unit

    call log_message(msg, "INFO", unit)

  end subroutine log_log_info

  subroutine log_log_debug(msg, unit)
    ! Log DEBUG message
    !
    ! msg :: message to log
    ! unit :: log file unit
    character(*), intent(in) :: msg
    integer(ip), intent(in) :: unit

    call log_message(msg, "DEBUG", unit)

  end subroutine log_log_debug

  subroutine log_message(msg, level_msg, unit)
    ! Subroutine to write (custom) log messages to logs
    !
    ! msg :: message to log
    ! level_msg :: level message ("CRITICAL", etc.)
    ! unit :: log file unit
    character(*), intent(in) :: msg
    character(*), intent(in) :: level_msg
    integer(ip), intent(in) :: unit

    character(:), allocatable :: log_msg

    log_msg = level_msg // " :: " // log_timestamp() // " :: " // msg
    write(unit, *) log_msg
    flush(unit)

  end subroutine log_message

  function log_timestamp() result(stamp)
    ! Private function to generate a formatted timestamp for logging
    character(:), allocatable :: stamp

    character(8) :: date
    character(10) :: time
    character(5) :: zone
    integer(ip) :: values(8)

    call date_and_time(date, time, zone, values)
    stamp = date // " " // time

  end function log_timestamp

end module log
