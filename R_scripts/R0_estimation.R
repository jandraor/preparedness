estimate_R0 <- function(par_beta, bio_params) {

  par_eta   <- bio_params$par_eta
  par_gamma <- bio_params$par_gamma
  inv_gamma <- 1 / par_gamma
  par_kappa <- bio_params$par_kappa
  inv_kappa <- 1 / par_kappa
  par_sigma <- bio_params$par_sigma
  par_omega <- bio_params$par_omega
  par_nu    <- bio_params$par_nu
  inv_nu    <- 1 / par_nu

  term_1 <- par_omega * (inv_gamma + inv_nu)
  term_2 <- (1 - par_omega) * par_eta * inv_kappa

  par_beta * (term_1 + term_2)
}

# R0 for testing & isolation model
estimate_R0_ti <- function(var_theta, par_beta, par_iota, bio_params) {

  par_eta   <- bio_params$par_eta
  par_gamma <- bio_params$par_gamma
  par_kappa <- bio_params$par_kappa
  par_sigma <- bio_params$par_sigma
  par_omega <- bio_params$par_omega
  par_nu    <- bio_params$par_nu

  term_1 <- par_gamma * par_kappa * par_omega

  term_2 <- (1 - var_theta * par_iota) * par_kappa * par_nu * par_omega

  term_3 <- par_gamma * par_eta * (par_nu - par_nu * par_omega)

  term_4 <- term_1 + term_2 + term_3

  term_5 <- par_gamma * par_kappa * par_nu

  par_beta * term_4 / term_5
}


