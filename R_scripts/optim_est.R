estimate_rho <- function(data_df) {

  init_val <- data_df[1, 2]

  last_val <- tail(data_df, 1)[, 2]

  mdl <- read_xmile("./models/Logistic_model.stmx",
                    stock_list = list(Q         = init_val),
                    const_list = list(par_alpha = last_val))

  actual <- data_df |> slice(-1) |> pull(value)

  st <- tail(data_df, 1)[, 1]

  meas_times <- data_df[-1, ][, 1]

  optim_fun <- function(par) {

    mdl$deSolve_components$consts["par_rho"] <- par

    output <- sd_simulate(mdl$deSolve_components, start_time = 0,
                          stop_time =  st, integ_method = "euler",
                          timestep = 1 / 64)

    sim    <- output |> filter(time %in% meas_times) |> pull(Q)

    mse(actual, sim)
  }

  optim_result <- optim(0.5, optim_fun, method = "Brent",
                        lower = 0, upper = 1)

  mdl$deSolve_components$consts["par_rho"] <- optim_result$par

  output <- sd_simulate(mdl$deSolve_components, start_time = 0,
                        stop_time =  st, integ_method = "euler",
                        timestep = 1/32)

  list(estimate = optim_result$par,
       sim      = output)
}
