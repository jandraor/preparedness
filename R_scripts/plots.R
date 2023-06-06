library(ggplot2)
library(scales)
library(viridis)

source("./R_scripts/plots_paper.R")

plot_2d_sens <- function(df, x, y) {

  cols <- viridis(6, direction = -1)

  ggplot(df, aes(.data[[x]], .data[[y]])) +
    geom_point(aes(colour = c_inf), size = 2) +
    geom_vline(xintercept = 0.045, alpha = 0.5, colour = "grey50",
               linetype = "dashed") +
    scale_colour_gradientn(values = c(0, 0.19999, 0.2, 0.2001, 0.39999, 0.4,
                                      0.4001, 0.5999, 0.6, 0.6001, 0.7999, 0.8,
                                      0.8001, 1),
                           colors = c(cols[[1]], cols[[1]], cols[[2]], cols[[2]],
                                      cols[[3]], cols[[3]], cols[[4]], cols[[4]],
                                      cols[[5]], cols[[5]], cols[[6]], cols[[6]]),
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.2)) +
    theme_classic()
}

plot_2d_sens_faceted <- function(df, x, y, z, x_lab = "", y_lab = "",
                                 sub_txt = "") {

  cols <- viridis(6, direction = -1)

  ggplot(df, aes(.data[[x]], .data[[y]])) +
    geom_point(aes(colour = c_inf), size = 2) +
    # geom_vline(xintercept = 0.045, alpha = 0.5, colour = "grey50",
    #            linetype = "dashed") +
    labs(x = parse(text = x_lab), y = parse(text = y_lab),
         subtitle = parse(text = sub_txt)) +
    facet_wrap(vars(.data[[z]]), nrow = 1, labeller = label_parsed) +
    scale_colour_gradientn(values = c(0, 0.19999, 0.2, 0.2001, 0.39999, 0.4,
                                      0.4001, 0.5999, 0.6, 0.6001, 0.7999, 0.8,
                                      0.8001, 1),
                           colors = c(cols[[1]], cols[[1]], cols[[2]], cols[[2]],
                                      cols[[3]], cols[[3]], cols[[4]], cols[[4]],
                                      cols[[5]], cols[[5]], cols[[6]], cols[[6]]),
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.2)) +
    theme_classic()
}

plot_ct_sens <- function(df) {

  cols <- viridis::rocket(6, direction = -1)

  ggplot(df, aes(par_rho_k, Qk)) +
    geom_point(aes(colour = c_prime), size = 2) +
    facet_wrap(~time, ncol = 4) +
    geom_vline(xintercept = 0.045, alpha = 0.5, colour = "grey50",
               linetype = "dashed") +
    scale_colour_gradientn(values = c(0, 0.19999, 0.2, 0.2001, 0.39999, 0.4,
                                      0.4001, 0.5999, 0.6, 0.6001, 0.7999, 0.8,
                                      0.8001, 1),
                           colors = c(cols[[1]], cols[[1]], cols[[2]], cols[[2]],
                                      cols[[3]], cols[[3]], cols[[4]], cols[[4]],
                                      cols[[5]], cols[[5]], cols[[6]], cols[[6]]),
                           limits = c(0, 1.000001),
                           breaks = seq(0, 1, 0.2)) +
    theme_classic()
}


