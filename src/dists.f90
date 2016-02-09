module dists
  use globvars, only: dp, sp, ip, pi_dp, pi_sp
  use numerics, only: numerics_factorial, numerics_beta

  implicit none

  private

  public :: dists_binomial_coeff

  public :: dists_poisson
  interface dists_poisson
     module procedure dists_poisson_dp
     module procedure dists_poisson_sp
  end interface dists_poisson

  public :: dists_laplace
  interface dists_laplace
     module procedure dists_laplace_dp
     module procedure dists_laplace_sp
  end interface dists_laplace

  public :: dists_lognormal
  interface dists_lognormal
     module procedure dists_lognormal_dp
     module procedure dists_lognormal_sp
  end interface dists_lognormal

  public :: dists_weibull
  interface dists_weibull
     module procedure dists_weibull_dp
     module procedure dists_weibull_sp
  end interface dists_weibull

  public :: dists_exp
  interface dists_exp
     module procedure dists_exp_dp
     module procedure dists_exp_sp
  end interface dists_exp

  public :: dists_chi2
  interface dists_chi2
     module procedure dists_chi2_dp
     module procedure dists_chi2_sp
  end interface dists_chi2

  public :: dists_f
  interface dists_f
     module procedure dists_f_dp
     module procedure dists_f_sp
  end interface dists_f

  public :: dists_cauchy
  interface dists_cauchy
     ! Cauchy distribution
     module procedure dists_cauchy_dp
     module procedure dists_cauchy_sp
  end interface dists_cauchy

  public :: dists_gamma
  interface dists_gamma
     ! Gamma distribution
     module procedure dists_gamma_dp
     module procedure dists_gamma_sp
  end interface dists_gamma

  public :: dists_t
  interface dists_t
     ! Student's t distribution
     module procedure dists_t_dp
     module procedure dists_t_sp
  end interface dists_t

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

  pure real(dp) function dists_poisson_dp(k, lambda) result(val)
    ! Poisson distribution for double precision real values
    !
    ! k :: observed events in interval
    ! lambda :: Average # of events per interval
    integer(ip), intent(in) :: k
    real(dp), intent(in) :: lambda

    include "./dists_src/poisson.src"
  end function dists_poisson_dp

  pure real(sp) function dists_poisson_sp(k, lambda) result(val)
    ! Duplicate of dists_poisson_dp, but for single precision reals
    integer(ip), intent(in) :: k
    real(sp), intent(in) :: lambda

    include "./dists_src/poisson.src"
  end function dists_poisson_sp

  pure real(dp) function dists_laplace_dp(x, mu, b) result(val)
    ! Laplace (double exponential) distribution for double precision real
    ! values
    !
    ! x :: evaluation point
    ! mu :: location parameter
    ! b :: scale parameter > 0
    real(dp), intent(in) :: x
    real(dp), intent(in) :: mu
    real(dp), intent(in) :: b

    include "./dists_src/laplace.src"
  end function dists_laplace_dp

  pure real(sp) function dists_laplace_sp(x, mu, b) result(val)
    ! Duplicate of dists_laplace_dp, but for single precision reals
    real(sp), intent(in) :: x
    real(sp), intent(in) :: mu
    real(sp), intent(in) :: b

    include "./dists_src/laplace.src"
  end function dists_laplace_sp

  pure real(dp) function dists_lognormal_dp(x, mu_var, var_var) result(val)
    ! Lognormal distribution for double precision real values
    !
    ! x :: evaluation point
    ! mu_var :: mean of variance
    ! var_var :: variance of variance
    real(dp), intent(in) :: x
    real(dp), intent(in) :: mu_var
    real(dp), intent(in) :: var_var

    include "./dists_src/lognormal.src"
  end function dists_lognormal_dp

  pure real(sp) function dists_lognormal_sp(x, mu_var, var_var) result(val)
    ! Duplicate of dists_lognormal_sp, but for single precision real values
    real(sp), intent(in) :: x
    real(sp), intent(in) :: mu_var
    real(sp), intent(in) :: var_var

    include "./dists_src/lognormal.src"
  end function dists_lognormal_sp

  pure real(dp) function dists_weibull_dp(x, lambda, k) result(val)
    ! Weibull distribution for double precision real values
    !
    ! x :: evaluation point, x > 0
    ! lambda :: shape parameter, k > 0
    ! lambda :: scale parameter, lambda > 0
    real(dp), intent(in) :: x
    real(dp), intent(in) :: lambda
    real(dp), intent(in) :: k

    include "./dists_src/weibull.src"
  end function dists_weibull_dp

  pure real(dp) function dists_weibull_sp(x, lambda, k) result(val)
    ! Duplicate of dists_weibull_sp, but for single precision real values
    real(sp), intent(in) :: x
    real(sp), intent(in) :: lambda
    real(sp), intent(in) :: k

    include "./dists_src/weibull.src"
  end function dists_weibull_sp

  pure real(dp) function dists_exp_dp(x, lambda) result(val)
    ! Exponential distribution for double precision real values
    !
    ! x :: evaluation point, x >= 0
    ! lambda :: rate parameter, lambda > 0
    real(dp), intent(in) :: x
    real(dp), intent(in) :: lambda

    include "./dists_src/exp.src"
  end function dists_exp_dp

  pure real(dp) function dists_exp_sp(x, lambda) result(val)
    ! Duplicate of dists_exp_dp, but for single precision values
    real(sp), intent(in) :: x
    real(sp), intent(in) :: lambda

    include "./dists_src/exp.src"
  end function dists_exp_sp

  pure real(dp) function dists_chi2_dp(x, k) result(val)
    ! Chi-squared distribution for double precision real values
    !
    ! x :: evaluation point
    ! k :: degrees of freedom
    real(dp), intent(in) :: x
    integer(ip), intent(in) :: k

    real(dp) :: k_2_fp
    real(dp) :: numer
    real(dp) :: denom

    k_2_fp = k / 2.0_dp

    include "./dists_src/chi2.src"
  end function dists_chi2_dp

  pure real(dp) function dists_chi2_sp(x, k) result(val)
    ! Duplicate of dists_chi2_dp, but for single precision real values
    real(sp), intent(in) :: x
    integer(ip), intent(in) :: k

    real(sp) :: k_2_fp
    real(sp) :: numer
    real(sp) :: denom

    k_2_fp = k / 2.0_sp

    include "./dists_src/chi2.src"
  end function dists_chi2_sp

  pure real(dp) function dists_f_dp(x, d1, d2) result(val)
    ! F distribution for double precision real values
    !
    ! x :: evaluation point
    ! d1 :: 1st shape parameter
    ! d2 :: 2nd shaper parameter
    real(dp), intent(in) :: x
    real(dp), intent(in) :: d1
    real(dp), intent(in) :: d2

    real(dp) :: numer
    real(dp) :: denom

    include "./dists_src/f.src"
  end function dists_f_dp

  pure real(sp) function dists_f_sp(x, d1, d2) result(val)
    ! Duplicate of dists_f_dp, but for single precision values
    real(sp), intent(in) :: x
    real(sp), intent(in) :: d1
    real(sp), intent(in) :: d2

    real(sp) :: numer
    real(sp) :: denom

    include "./dists_src/f.src"
  end function dists_f_sp

  pure real(dp) function dists_cauchy_dp(x, x0, gamma) result(val)
    ! Cauchy distribution for double precision values
    !
    ! x :: evaluation point
    ! x0 :: location parameter
    ! gamma :: scale parameter
    real(dp), intent(in) :: x
    real(dp), intent(in) :: x0
    real(dp), intent(in) :: gamma

    real(dp), parameter :: pi_fp = pi_dp

    include "./dists_src/cauchy.src"

  end function dists_cauchy_dp

  pure real(sp) function dists_cauchy_sp(x, x0, gamma) result(val)
    ! Duplicate of dists_cauchy_dp, but for single precision values
    ! gamma :: scale parameter
    real(sp), intent(in) :: x
    real(sp), intent(in) :: x0
    real(sp), intent(in) :: gamma

    real(sp), parameter :: pi_fp = pi_sp

    include "./dists_src/cauchy.src"

  end function dists_cauchy_sp

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

  pure real(dp) function dists_gamma_dp(x, k, theta) result(val)
    ! Gamma distribution for double precision real values
    !
    ! x :: evaluation point
    ! k :: shape parameter, k > 0
    ! theta :: scale parameter, theta > 0
    real(dp), intent(in) :: x
    real(dp), intent(in) :: k
    real(dp), intent(in) :: theta

    real(dp) :: numer
    real(dp) :: denom

    include "./dists_src/gamma.src"
  end function dists_gamma_dp

  pure  real(sp) function dists_gamma_sp(x, k, theta) result(val)
    ! Duplicate of dists_gamma, but for single precision values
    real(sp), intent(in) :: x
    real(sp), intent(in) :: k
    real(sp), intent(in) :: theta

    real(sp) :: numer
    real(sp) :: denom

    include "./dists_src/gamma.src"
  end function dists_gamma_sp

  pure real(dp) function dists_t_dp(t, n) result(val)
    ! Student's t distribution for double precision real values
    !
    ! t :: t value
    ! n :: degrees of freedom
    real(dp), intent(in) :: t
    integer(ip), intent(in) :: n

    real(dp) :: term1
    real(dp) :: term2
    real(dp) :: nu
    real(dp), parameter :: pi_fp = pi_dp

    nu = real(n - 1, kind=dp)

    include "./dists_src/t.src"
  end function dists_t_dp

  pure real(sp) function dists_t_sp(t, n) result(val)
    ! Duplicate of dists_t_sp, but for single precision values
    real(sp), intent(in) :: t
    integer(ip), intent(in) :: n

    real(sp) :: term1
    real(sp) :: term2
    real(sp) :: nu
    real(sp), parameter :: pi_fp = pi_sp

    nu = real(n - 1, kind=sp)

    include "./dists_src/t.src"
  end function dists_t_sp

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
