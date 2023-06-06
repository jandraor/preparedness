plot_fig_02 <- function(df) {

  ggplot(df, aes(time, c)) +
    geom_line(aes(group = iter, colour = par_beta), alpha = 0.75) +
    facet_wrap(~model) +
    scale_y_continuous(labels = percent) +
    scale_colour_viridis(name  = parse(text = "beta"),direction = -1) +
    labs(y = parse(text = "'Attack rate' ~ (c[t])"), x = "Day") +
    theme_classic() +
    theme(
      axis.title   = element_text(colour = "grey65"),
      axis.line     = element_line(colour = "grey90"),
      axis.text = element_text(colour = "grey70", size = 11),
      axis.ticks    = element_line(colour = "grey90"))
}

plot_fig_04 <- function(df1, df2, df3, df4, df5) {

  cols <- viridis(6, direction = -1)

  tau_vals <- c(60, 30, 15)

  lbl_df <- data.frame(iter = 1:3,
                       time = c(78, 55, 28),
                       Vaccine_supply = c(1e5, 1.5e5, 2e5),
                       label = str_glue("tau^v~'= {tau_vals}'"))

  ggplot(df1, aes(time, Vaccine_supply)) +
    geom_line(aes(group = iter, colour = as.factor(iter))) +
    scale_y_continuous(labels = comma) +
    geom_text(data = lbl_df, aes(label = label, colour = as.factor(iter)),
              parse = TRUE, size = 2) +
    scale_colour_manual(values = c("#54B4EA", "#0363BB", "#023785")) +
    labs(y = "Vaccination supply",
         x = "Day") +
    theme_classic() +
    theme(axis.title      = element_text(colour = "grey65", size = 9),
          axis.line       = element_line(colour = "grey90"),
          axis.text       = element_text(colour = "grey70", size = 10),
          axis.ticks      = element_line(colour = "grey90"),
          legend.position = "none") -> g1

  ggplot(df2, aes(par_rho_v, par_tau_v)) +
    geom_point(aes(colour = c_star), size = 1, shape = 1, alpha = 0.75) +
    facet_wrap(~ R0_lbl, labeller = label_parsed) +
    geom_vline(xintercept = 0.15, alpha = 0.5, colour = "white",
               linetype = "dashed") +
    labs(x = parse(text = "'Vaccination capacity\\'s growth rate'~(rho^v)"),
         y = parse(text = "'Vaccine availability'~(tau^v)"),
         caption = "Vertical line: Estimated value") +
    scale_colour_gradientn(values = c(0, 0.19999, 0.2, 0.2001, 0.39999, 0.4,
                                      0.4001, 0.5999, 0.6, 0.6001, 0.7999, 0.8,
                                      0.8001, 1),
                           colors = c(cols[[1]], cols[[1]], cols[[2]], cols[[2]],
                                      cols[[3]], cols[[3]], cols[[4]], cols[[4]],
                                      cols[[5]], cols[[5]], cols[[6]], cols[[6]]),
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.2),
                           name = parse(text = "c[300]^'*'")) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.title   = element_text(colour = "grey65", size = 9),
      axis.line    = element_line(colour = "grey90"),
      axis.text.x  = element_text(colour = "grey70", size = 7),
      axis.text.y  = element_text(colour = "grey70", size = 9),
      axis.ticks   = element_line(colour = "grey90"),
      plot.caption = element_text(colour = "grey50"),
      text = element_text(family = "Arial Unicode MS")) -> g2


  ggplot(df3, aes(par_rho_v, par_tau_v)) +
    geom_point(aes(colour = c_star), size = 1, shape = 1, alpha = 0.75) +
    facet_wrap(~ iota_lbl, labeller = label_parsed) +
    geom_vline(xintercept = 0.15, alpha = 0.5, colour = "white",
               linetype = "dashed") +
    scale_colour_gradientn(values = c(0, 0.19999, 0.2, 0.2001, 0.39999, 0.4,
                                      0.4001, 0.5999, 0.6, 0.6001, 0.7999, 0.8,
                                      0.8001, 1),
                           colors = c(cols[[1]], cols[[1]], cols[[2]], cols[[2]],
                                      cols[[3]], cols[[3]], cols[[4]], cols[[4]],
                                      cols[[5]], cols[[5]], cols[[6]], cols[[6]]),
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.2),
                           name = parse(text = "c[300]^'*'")) +
    labs(x = parse(text = "'Vaccination capacity\\'s growth rate'~(rho^v)"),
         y = parse(text = "'Vaccine availability'~(tau^v)"),
         caption = "Vertical line: Estimated value") +
    theme_classic() +
    theme(
      axis.title  = element_text(colour = "grey65", size = 9),
      axis.line  = element_line(colour = "grey90"),
      axis.text.x  = element_text(colour = "grey70", size = 7),
      axis.text.y  = element_text(colour = "grey70", size = 9),
      axis.ticks = element_line(colour = "grey90"),
      plot.caption = element_text(colour = "grey50"),
      text = element_text(family = "Arial Unicode MS")) -> g3



  ggplot(df4, aes(time, value/pop_val)) +
    geom_line(aes(linetype = variable), colour = "#023785") +
    geom_vline(xintercept = 15, alpha = 0.4, colour = "grey60",
               linewidth = 0.25) +
    theme_classic() +
    labs(y = "Value", x = "Day",
         caption = "Vertical line: Vaccination starts") +
    theme(legend.position = c(0.85, 0.7),
          legend.title = element_text(size = 9, colour = "grey65"),
          legend.text = element_text(size = 7),
          axis.title  = element_text(colour = "grey65", size = 9),
          axis.line  = element_line(colour = "grey90"),
          axis.text  = element_text(colour = "grey70", size = 11),
          axis.ticks = element_line(colour = "grey90"),
          plot.caption = element_text(colour = "grey50"),) -> g4

  ggplot(df5, aes(time, ratio)) +
    geom_line(colour = "#023785") +
    scale_x_continuous(limits = c(15, 90), breaks = seq(15, 90, 15)) +
    theme_classic() +
    labs(y = "Ratio Demand/Supply", x = "Day") +
    theme(axis.title  = element_text(colour = "grey65", size = 9),
          axis.line  = element_line(colour = "grey90"),
          axis.text  = element_text(colour = "grey70", size = 11),
          axis.ticks = element_line(colour = "grey90"),
          plot.caption = element_text(colour = "grey50"),) -> g5

  g6 <- (g4|g5) + plot_layout(widths = c(5, 2))

  g1/(g2|g3)/ g6 + plot_annotation(tag_levels = 'A')
}



