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

estimate_R0_ct <- function(par_zeta, par_upsilon, var_theta, par_iota,
                           var_zeta_k, bio_params) {

  par_eta   <- bio_params$par_eta
  par_gamma <- bio_params$par_gamma
  par_kappa <- bio_params$par_kappa
  par_sigma <- bio_params$par_sigma
  par_omega <- bio_params$par_omega
  par_nu    <- bio_params$par_nu

  term_1  <- 1 / (2 * par_gamma^3 * par_kappa^2 * par_nu)

  term_2  <- 1 - var_theta * par_iota

  term_3  <- par_gamma^2 * par_zeta * term_2 * par_kappa^2 * par_nu * par_upsilon * par_omega

  term_4  <- par_zeta * par_kappa * par_omega

  term_5  <- var_zeta_k * var_theta * par_kappa * par_omega

  term_6  <- par_zeta * par_eta * (par_nu - par_nu * par_omega)

  term_7  <- term_4 - term_5 + term_6

  term_8  <- par_gamma^3 * par_kappa * par_upsilon * term_7

  term_9  <- -1 + var_theta * par_iota

  term_10 <- par_zeta * term_9 * par_kappa * par_nu * par_omega

  term_11 <- par_zeta * par_eta * par_nu * (-1 + par_omega)

  term_12 <- par_zeta * par_kappa * par_omega

  term_13 <- var_zeta_k * var_theta * par_kappa * par_omega

  term_14 <- term_11 - term_12 + term_13

  term_15 <- par_gamma * term_14

  term_16 <- term_10 + term_15

  term_17 <- par_gamma * par_kappa * par_omega

  term_18 <- par_kappa * par_nu * par_omega

  term_19 <- par_gamma * par_eta * (par_nu - par_nu * par_omega)

  term_20  <- term_17 + term_18 + term_19

  term_21  <- 4 * par_gamma * par_zeta * var_zeta_k * var_theta * (-1 + par_iota) * par_kappa * par_omega * term_20

  term_22  <- term_16^2 - term_21

  term_23 <- par_gamma^4 * par_kappa^2 * par_upsilon^2 * term_22

  term_24  <- term_3 + term_8 + sqrt(term_23)

  term_1 * term_24
}
