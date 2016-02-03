module dists
  use globvars, only: dp, sp, ip, pi_dp, pi_sp
  use numerics, only: numerics_factorial

  implicit none

  private

  public :: dists_binomial_coeff

  public :: dists_binomial
  interface dists_binomial
     ! Binomial distribution:
     !
     ! B(x) = (n k) p^k (1-p)^(n-k)
     module procedure dists_binomial_dp
     module procedure dists_binomial_sp
  end interface dists_binomial

  public :: dists_gaussian
  interface dists_gaussian
     ! Normalized Gaussian distribution:
     !
     ! G(x) = 1 / sqrt( 2 pi sig^2 ) exp( -1/2 (x - mu)^2 / sig^2 )
     !
     ! See documentation for specific module procedures for more details
     module procedure dists_gaussian_dp
     module procedure dists_gaussian_sp
  end interface dists_gaussian

  real(dp), parameter :: tau_dp = 2.0_dp * pi_dp
  real(sp), parameter :: tau_sp = 2.0_sp * pi_sp

contains

  pure integer(ip) function dists_binomial_coeff(n, k) result(val)
    ! Binomial coefficient (n, k) = n! / ( k! (n - k)! )
    integer(ip), intent(in) :: n
    integer(ip), intent(in) :: k

    val = numerics_factorial(n) / &
         ( numerics_factorial(k) * numerics_factorial(n - k) )
  end function dists_binomial_coeff

  pure real(dp) function dists_binomial_dp(k, n, p) result(val)
    ! Binomial distribution for double precision values
    !
    ! k :: successes
    ! n :: tries
    ! p :: probability of success
    integer(ip), intent(in) :: k
    integer(ip), intent(in) :: n
    real(dp), intent(in) :: p

    real(dp) :: p_f

    p_f = 1.0_dp - p

    include "./dists_src/binomial.src"
  end function dists_binomial_dp

  pure real(sp) function dists_binomial_sp(k, n, p) result(val)
    ! Duplicate of dists_binomial_sp, but for single precision values
    integer(ip), intent(in) :: k
    integer(ip), intent(in) :: n
    real(sp), intent(in) :: p

    real(sp) :: p_f

    p_f = 1.0_sp - p

    include "./dists_src/binomial.src"
  end function dists_binomial_sp

  pure real(dp) function dists_gaussian_dp(x, mu, var) result(val)
    ! Normalized Gaussian distribution for double precision values
    !
    ! x :: evaluation point
    ! mu :: distribution mean
    ! var :: distribution variance
    real(dp), intent(in) :: x
    real(dp), intent(in) :: mu
    real(dp), intent(in) :: var

    real(dp) :: norm2, exp1

    norm2 = tau_dp * var

    include "./dists_src/gaussian.src"
  end function dists_gaussian_dp

  pure real(sp) function dists_gaussian_sp(x, mu, var) result(val)
    ! Duplicate of dists_gaussian_dp, but for single precision reals
    real(sp), intent(in) :: x
    real(sp), intent(in) :: mu
    real(sp), intent(in) :: var

    real(sp) :: norm2, exp1

    norm2 = tau_sp * var

    include "./dists_src/gaussian.src"
  end function dists_gaussian_sp

end module dists
