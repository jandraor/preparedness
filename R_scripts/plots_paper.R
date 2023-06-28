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

plot_fig_06 <- function(df1, df2, df3) {

  cols <- rocket(7, direction = -1, )

  ggplot(df, aes(time, value)) +
    geom_line(aes(colour = model, linetype = model), alpha = 0.9) +
    scale_color_manual(values = c("grey50", "#0363BB")) +
    scale_linetype_manual(values = c("dotdash", "solid")) +
    scale_x_continuous(limits = c(1, 150)) +
    geom_hline(data = data.frame(name = "\u211c[t]", yint = 1),
               aes(yintercept = yint), linetype = "dotted") +
    facet_wrap(~name, ncol = 1, scales = "free", labeller = label_parsed) +
    labs(y = "Value", x = "Day") +
    theme_classic() +
    theme(axis.title      = element_text(colour = "grey65", size = 9),
          axis.line       = element_line(colour = "grey90"),
          axis.text       = element_text(colour = "grey70", size = 8),
          axis.ticks      = element_line(colour = "grey90"),
          legend.position  = "bottom",
          legend.margin    = margin(0, 0, 0, 0, "cm"),
          legend.title = element_text(size = 9, colour = "grey65"),
          legend.text = element_text(size = 7),
          strip.text = element_text(margin = margin( b = 0.05, t = 0.05,
                                                     unit = "cm")),
          strip.background = element_rect(colour = "grey80",
                                          linewidth = 0.5),
          text = element_text(family = "Arial Unicode MS")) -> g1

  df2 <- df2 |>
    mutate(lbl_iota  = str_glue("iota^d~' = {par_iota_d}'"),
           lbl_delta = str_glue("delta~' = {par_delta}'"))

  ats <- 6 # axis text size


  ggplot(df2, aes(par_rho_d, par_alpha_d)) +
    geom_point(aes(colour = time_star), size = 1, shape = 1, alpha = 0.5) +
    scale_y_continuous(breaks = c(10,40, 70, 100),
                       limits = c(10, 100)) +
    facet_grid(lbl_iota ~ lbl_delta, labeller = label_parsed) +
    scale_colour_gradientn(values = c(0, 0.19999,
                                      0.20, 0.39999,
                                      0.40, 0.59999,
                                      0.60, 0.7999,
                                      0.8, 1),
                           colors = c(cols[[1]], cols[[1]], cols[[2]], cols[[2]],
                                      cols[[3]], cols[[3]], cols[[4]], cols[[4]],
                                      cols[[5]], cols[[5]]),
                           limits = c(0, 30),
                           breaks = seq(0, 30, 6),
                           name = parse(text = "t^'*'"))  +
   geom_point(data = df2 |> filter(iter == 5001),
               colour = "#0363BB", shape = 1, size = 0.5) +
    labs(x = parse(text = "'Testing growth rate'~(rho^d)"),
         y = parse(text = "'Testing long-term capacity'~(alpha^d)"),
         title    = parse(text = "delta~': Willingness to take a test'"),
         subtitle = parse(text = "iota^d~': Willingness to isolate if positive'"),
         caption = "Blue dot: Estimated value") +
    theme_classic() +
    theme(axis.title  = element_text(colour = "grey65"),
          axis.line  = element_line(colour = "grey90"),
          axis.text  = element_text(colour = "grey70", size = ats),
          axis.ticks = element_line(colour = "grey90"),
          plot.title    = element_text(colour = "grey40", size = 9),
          plot.subtitle = element_text(colour = "grey40", size = 9),
          plot.caption = element_text(colour = "grey60", size = 5),
          strip.background = element_rect(colour = "grey80",
                                          linewidth = 0.5)) -> g2

  df3 <- df3 |>
    mutate(lbl_iota  = str_glue("iota^d~' = {par_iota_d}'"),
           lbl_delta = str_glue("delta~' = {par_delta}'"))


  ggplot(df3, aes(par_rho_d, par_alpha_d)) +
    geom_point(aes(colour = time_star), size = 1, shape = 1, alpha = 0.5) +
    facet_grid(lbl_iota ~ lbl_delta, labeller = label_parsed) +
    scale_y_continuous(breaks = c(10,40, 70, 100),
                       limits = c(10, 100)) +
    scale_colour_gradientn(values = c(0, 0.09999,
                                      0.10, 0.19999,
                                      0.2, 0.2999,
                                      0.3, 0.3999,
                                      0.4, 0.5,
                                      0.50001, 0.75, 1),
                           colors = c(cols[[1]], cols[[1]],
                                      cols[[2]], cols[[2]],
                                      cols[[3]], cols[[3]],
                                      cols[[4]], cols[[4]],
                                      cols[[5]], cols[[5]],
                                      cols[[6]], cols[[6]], cols[[7]]),
                           limits = c(0, 60.1),
                           breaks = c(seq(0, 30, 6), 60),
                           name = parse(text = "t^'*'"))  +
    geom_point(data = df3 |> filter(iter == 5001),
               colour = "#0363BB", shape = 1, size = 0.5) +
    labs(x       = parse(text = "rho^d"),
         y       = parse(text = "alpha^d"),
         caption = "Testing at the beginning of the pre-clinical phase") +
    theme_classic() +
    theme(axis.title  = element_text(colour = "grey65"),
          axis.line  = element_line(colour = "grey90"),
          axis.text  = element_text(colour = "grey70", size = ats),
          axis.ticks = element_line(colour = "grey90"),
          strip.background = element_rect(colour = "grey80",
                                          linewidth = 0.5),
          plot.caption = element_text(size = 6, colour = "grey40",
                                      hjust = 0))-> g3

  g4 <- (g2 / g3) + plot_layout(heights = c(5, 2))
  (g1 | g4) + plot_layout(widths = c(3, 4)) + plot_annotation(tag_levels = 'A')
}

plot_fig_08 <- function(df, df2, df3) {

  fig_8A <- draw_fig_8A(df)

  fig_8B <- draw_fig_8B(df2)

  fig_8C <- draw_fig_8C(df3)

  fig_8D <- draw_fig_8D(df3)

  (fig_8A | fig_8B) / fig_8C / fig_8D + plot_annotation(tag_levels = 'A')
}

draw_fig_8A <- function(df) {

  ggplot(df |> filter(time - trunc(time) == 0),
         aes(time, value)) +
    geom_line(aes(colour = model, linetype = model), alpha = 0.9) +
    facet_wrap(~name, scales = "free", ncol = 1) +
    scale_colour_manual(values = c("#0363BB", "grey50")) +
    scale_linetype_manual(values = c("solid", "dotdash")) +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(n.breaks = 3) +
    labs(x = "Day", y = "Value") +
    theme_classic() +
    theme(axis.title      = element_text(colour = "grey65", size = 9),
          axis.line       = element_line(colour = "grey90"),
          axis.text       = element_text(colour = "grey70", size = 7),
          axis.ticks      = element_line(colour = "grey90"),
          legend.position  = "bottom",
          legend.margin    = margin(0, 0, 0, 0, "cm"),
          legend.title = element_text(size = 9, colour = "grey65"),
          legend.text = element_text(size = 7),
          strip.background = element_rect(colour = "grey80", linewidth = 0.5),
          strip.text = element_text(size = 7,
                                    margin = margin(b = 0.1, t = 0.1,
                                                    unit = "cm")),
          text = element_text(family = "Arial Unicode MS"))
}

draw_fig_8B <- function(df) {

  ggplot(df, aes(time, Qd)) +
    geom_line(aes(linetype = as.factor(Scenario), group = Scenario), colour = "#0363BB",
              linewidth = 0.5) +
    scale_linetype_manual(values = c("solid", "dotted", "dashed", "dotdash"),
                          name = "Testing scenario") +
    labs(y = "Daily testing capacity \n per 1000 population", x = "Day") +
    theme_classic() +
    theme(axis.title      = element_text(colour = "grey65", size = 9),
          axis.line       = element_line(colour = "grey90"),
          axis.text       = element_text(colour = "grey70", size = 7),
          axis.ticks      = element_line(colour = "grey90"),
          legend.position  = "bottom",
          legend.margin    = margin(0, 0, 0, 0, "cm"),
          legend.title = element_text(size = 9, colour = "grey65"),
          legend.text = element_text(size = 7),
          text = element_text(family = "Arial Unicode MS"))
}

draw_fig_8C <- function(df) {

  cols <- rocket(7, direction = -1)

  df <- df |>
    mutate(label_ts = str_glue("'ts = {test_sce}'"),
           lbl_iota  = str_glue("iota^d~' = {par_iota_d}'"))

  ggplot(df, aes(par_rho_k, par_alpha_k)) +
    geom_point(aes(colour = time_star), size = 1, shape = 1, alpha = 0.5) +
    facet_grid(lbl_iota ~ label_ts, labeller = label_parsed) +
    scale_colour_gradientn(values = c(0, 0.19999,
                                      0.20, 0.39999,
                                      0.40, 0.59999,
                                      0.60, 0.7999,
                                      0.8, 1),
                           colors = c(cols[[1]], cols[[1]], cols[[2]], cols[[2]],
                                      cols[[3]], cols[[3]], cols[[4]], cols[[4]],
                                      cols[[5]], cols[[5]]),
                           limits = c(0, 30),
                           breaks = seq(0, 30, 6),
                           name = parse(text = "t^'*'"))  +
   labs(x = parse(text = "'CT growth rate'~(rho^k)"),
               y = parse(text = "'CT long-term capacity'~(alpha^k)"),
          title = "ts: Testing scenario",
         subtitle = parse(text = "iota^d~': Willingness to isolate if positive'")) +
    theme_classic() +
    theme(axis.title  = element_text(colour = "grey65"),
          axis.line  = element_line(colour = "grey90"),
          axis.text  = element_text(colour = "grey70", size = 6),
          axis.ticks = element_line(colour = "grey90"),
          plot.title    = element_text(colour = "grey40", size = 9),
          plot.subtitle = element_text(colour = "grey40", size = 9),
          plot.caption = element_text(colour = "grey60", size = 5),
          strip.background = element_rect(colour = "grey80",
                                          linewidth = 0.5),
          strip.text.y = element_text(size = 7))
}

draw_fig_8D <- function(df) {

  cols <- plasma(5, direction = -1, )

  df <- df |>
    mutate(c_dot_star = c_dot_max / ref_peak,
           label_ts = str_glue("'ts = {test_sce}'"),
           lbl_iota  = str_glue("iota^d~' = {par_iota_d}'"))

  ggplot(df, aes(par_rho_k, par_alpha_k)) +
    geom_point(aes(colour = c_dot_star), size = 1, shape = 1, alpha = 0.5) +
    facet_grid(lbl_iota ~ label_ts, labeller = label_parsed) +
    scale_colour_gradientn(values = c(0,    0.199999,
                                      0.2,  0.399999,
                                      0.40, 0.59999,
                                      0.60, 0.79999,
                                      0.80, 1),
                           colors = c(cols[[1]], cols[[1]],
                                      cols[[2]], cols[[2]],
                                      cols[[3]], cols[[3]],
                                      cols[[4]], cols[[4]],
                                      cols[[5]], cols[[5]]),
                           limits = c(0.5, 1),
                           breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
                           name = parse(text = "p^'*'"))  +
    labs(x = parse(text = "rho^k"),
         y = parse(text = "alpha^k")) +
    theme_classic() +
    theme(axis.title  = element_text(colour = "grey65"),
          axis.line  = element_line(colour = "grey90"),
          axis.text  = element_text(colour = "grey70", size = 6),
          axis.ticks = element_line(colour = "grey90"),
          plot.title    = element_text(colour = "grey40", size = 9),
          plot.subtitle = element_text(colour = "grey40", size = 9),
          plot.caption = element_text(colour = "grey60", size = 5),
          strip.background = element_rect(colour = "grey80",
                                          linewidth = 0.5),
          strip.text.y = element_text(size = 7))
}



