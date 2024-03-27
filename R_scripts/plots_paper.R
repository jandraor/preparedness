clrs <- c("#51BBE0", "#C7B9A2", "#0D294A", "#AD1F4D", "#ECE6DB") # Palette 818

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

plot_fig_04 <- function(tv_df, f_t_df, tv_ct_df, f_ct_df, re_df) {

  g1 <- draw_fig_4A(tv_df, f_t_df)
  g2 <- draw_fig_4B(tv_ct_df, f_ct_df)
  g3 <- draw_fig_4C(re_df)

  (g1 | g2) / (plot_spacer() + g3 + plot_spacer()) +
    plot_layout(heights = c(6, 1),
                guides = 'collect') +
    plot_annotation(
      tag_levels = 'A',
      subtitle = "Solid blue line: Time-varying fraction") &
    theme(legend.position = 'top',
          plot.subtitle     = element_text(colour = clrs[[1]]))

}

draw_fig_4A <- function(tv_df, f_t_df) {

  ggplot(tv_df, aes(time, value)) +
    facet_wrap(vars(name), scales = "free", ncol = 1) +
    geom_line(data = f_t_df, aes(linetype = par_theta_d, group = par_theta_d),
              colour = "grey60", alpha = 0.9) +
    geom_line(colour = clrs[[1]]) +
    scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
    scale_x_continuous(limits = c(0, 150)) +
    labs(linetype = "Constant fraction",
         x = "Day",
         y = "Value") +
    theme_classic() +
    theme(text = element_text(family = "Arial Unicode MS"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 7),
          axis.ticks       = element_line(colour = "grey90"),
          strip.background = element_rect(colour = "grey80"))
}

draw_fig_4B <- function(tv_ct_df, f_ct_df) {

  ggplot(tv_ct_df, aes(time, value)) +
    facet_wrap(vars(name), scales = "free", ncol = 1) +
    geom_line(data = f_ct_df, aes(linetype = par_theta_k, group = par_theta_k),
               colour = "grey60", alpha = 0.9) +
    geom_line(colour = clrs[[1]]) +
    scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
    scale_x_continuous(limits = c(0, 150)) +
    labs(linetype = "Constant fraction",
         x = "Day",
         y = "Value") +
    theme_classic() +
    theme(text = element_text(family = "Arial Unicode MS"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 7),
          axis.ticks       = element_line(colour = "grey90"),
          strip.background = element_rect(colour = "grey80"))
}

draw_fig_4C <- function(df) {

  df <- filter(df, time >=0.1 )

  ggplot(df, aes(time, Rt)) +
    geom_line(colour = clrs[[1]]) +
    labs(x = "Day",
         y = parse(text = "\u211c[t]"),
         caption =  parse(text = "'Dotdash line:'~\u211c[0]")) +
    geom_hline(yintercept = 3, linetype = "dotdash") +
    theme_classic() +
    theme(text = element_text(family = "Arial Unicode MS"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.title.y     = element_text(angle = 0, vjust = 0.5),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"))
}

plot_fig_06 <- function(cap_df, inc_df, sim_output) {

  ref_df <- sim_output |> filter(time <= 120)

  g1 <- draw_fig_6A(cap_df, ref_df)
  g2 <- draw_fig_6B(cap_df, ref_df)
  g3 <- draw_fig_6C(inc_df, ref_df)

  ((g1 | g2) / g3) +
    plot_layout(heights = c(2.5, 4.5)) +
    plot_annotation(tag_levels = 'A')
}

draw_fig_6A <- function(df, df2) {

  ggplot(df, aes(time, Qd)) +
    geom_line(data = df2, colour = "grey75", alpha = 0.5) +
    geom_line(aes(group = Testing_scenario, colour = Testing_scenario)) +
    scale_colour_manual(values = clrs[2:4]) +
    labs(subtitle = "Testing capacity",
         y = "Value",
         x = "Day") +
    theme_classic() +
    theme(legend.position = "none",
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"))
}

draw_fig_6B <- function(df, df2) {

  ggplot(df, aes(time, Qk)) +
    geom_line(data = df2, colour = "grey75",
              alpha = 0.5) +
    geom_line(aes(group = Tracing_scenario, linetype = Tracing_scenario)) +
    scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
    labs(subtitle = "Contact tracing capacity",
         y = "Value",
         x = "Day") +
    theme_classic() +
    theme(legend.position = "none",
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"))
}

draw_fig_6C <- function(df, df2) {

  ggplot(df, aes(time, 1000 * C_in / pop_val)) +
    geom_line(data = df2, colour = "grey75",
              alpha = 0.5) +
    geom_line(aes(group = iter, colour = Testing_scenario,
                  linetype = Tracing_scenario), alpha = 0.9) +
    scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
    scale_colour_manual(values = clrs[2:4]) +
    labs(subtitle = "Incidence rate [Cases per 1000 population]",
         x = "Day",
         y = "Value",
         linetype = "Tracing scenario",
         colour = "Testing scenario",
         caption = "Grey solid line: Ref TTI") +
    theme_classic() +
    theme(legend.position = "bottom",
          plot.caption = element_text(colour = "grey75"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"))
}

plot_fig_07 <- function(df, df2, ref_df, comp_df, inc_df) {

  g1 <- draw_fig_7A(df, ref_df, comp_df, inc_df)
  g2 <- draw_fig_7B(df2, ref_df)

  (g1/g2) +
    plot_annotation(tag_levels = 'A')

}

draw_fig_7A <- function(df, ref_df, comp_df, inc_df) {

  iota_pct  <- percent(comp_df$par_iota_d)
  delta_pct <- percent(comp_df$par_delta)

  text_df <- data.frame(
    iter = factor(1:3),
    x = 110,
    y = c(55, 65, 45),
    lbl = str_glue("iota^d~' = {iota_pct}, '~delta~'= {delta_pct}'"))

  ggplot(df |> filter(time <=150), aes(time, 1000 * C_in / pop_val)) +
    geom_line(aes(group = iter, colour = as.factor(iter))) +
    geom_line(data = ref_df |> filter(time <= 150), colour = "grey75") +
    geom_line(data = inc_df |> filter(iter == 9),
              colour = "grey75", linetype = "dashed") +
    scale_colour_manual(values = clrs[2:4]) +
    geom_text(data = text_df, aes(x = x, y = y, label = lbl, colour = iter),
              parse = TRUE) +
    annotate("text", x = 110, y = 75, colour = "grey75",
             label = "iota^d~' = 51%, '~delta~'= 60%'", parse = TRUE) +
    labs(x = "Day",
         y = "Incidence rate",
         title    = parse(text = "delta~': Willingness to take a test'"),
         subtitle = parse(text = "iota^d~': Willingness to isolate/quarantine'"),
         caption = "Grey solid line: Ref TTI\nGrey dashed line: Scenario t3 c3") +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(colour = "grey65", size = 10),
          plot.subtitle = element_text(colour = "grey65", size = 10),
          plot.caption = element_text(colour = "grey75"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90")) -> g1
}

draw_fig_7B <- function(df, ref_df) {

  ggplot(df |> filter(time <=150),
         aes(time, 1000 * C_in / pop_val)) +
    geom_line(aes(group = iter, colour = as.factor(iter)), linetype = "dotdash") +
    geom_line(data = ref_df |>
                filter(time <=150), colour = "grey75", alpha = 0.25) +
    scale_colour_manual(values = c("grey75", clrs[2:4])) +
    labs(x = "Day",
         y = "Incidence rate",
         caption = "Grey solid line: Ref TTI") +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(colour = "grey65", size = 10),
          plot.subtitle = element_text(colour = "grey65", size = 10),
          plot.caption = element_text(colour = "grey75"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"))
}

plot_fig_08 <- function(df, df_ref, sens_df) {

  df2 <- df |>
    filter(iter == 2) |>
    mutate(tracing_fraction = k/Dk * 100,
           testing_fraction = var_theta * 100) |>
    select(time, tracing_fraction, testing_fraction) |>
    pivot_longer(-time) |>
    mutate(name = case_when(name == "tracing_fraction" ~"Tracing fraction [%]",
                            name == "testing_fraction" ~"Testing fraction [%]"))

  df_ref2 <- df_ref |>
    mutate(tracing_fraction = k/Dk * 100,
           testing_fraction = var_theta * 100) |>
    select(time, tracing_fraction, testing_fraction) |>
    pivot_longer(-time) |>
    mutate(name = case_when(name == "tracing_fraction" ~"Tracing fraction [%]",
                            name == "testing_fraction" ~"Testing fraction [%]"))

  g1 <- draw_fig_8A(df, df_ref)
  g2 <- draw_fig_8B(df2, df_ref2)
  g3 <- (plot_spacer() + draw_fig_8C(sens_df) + plot_spacer()) +
    plot_layout(widths = c(1, 5, 1))

  (g1/g2/g3) +
    plot_layout(heights = c(2, 3, 2.5)) +
    plot_annotation(tag_levels = 'A')

}

draw_fig_8A <- function(df, df_ref) {

  ggplot(df, aes(time, 1000 * C_in / pop_val)) +
    geom_line(aes(linetype = itv, group = iter, colour = as.factor(iter))) +
    geom_line(data = df_ref |>
                filter(time <= 360), colour = "grey75", linetype = "dotted") +
    scale_linetype_manual(values = c("solid", "longdash"), guide = "none") +
    scale_colour_manual(values = c("grey50" ,clrs[c(4, 2)])) +
    annotate("text", label = "TTI", x = 77, y = 70, colour = "grey75") +
    annotate("text", label = "MR", x = 200, y = 65, colour = "grey50") +
    annotate("text", label = "TTI + MR (start day 0)", x = 280, y = 60,
             colour = clrs[[4]], size = 3) +
    annotate("text", label = "TTI + MR (start day 40)", x = 115, y = 20,
             colour = clrs[[2]], size = 3) +
    labs(x = "Day",
         y = "Incidence rate") +
    theme_classic() +
    theme(legend.position = "none",
          plot.caption = element_text(colour = "grey75"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"))
}

draw_fig_8B <- function(df, df_ref) {

  ggplot(df |> filter(time > 0), aes(time, value)) +
    facet_wrap(~name, ncol = 1) +
    geom_line(colour = clrs[[4]]) +
    geom_line(data = df_ref |>
                filter(time > 0 & time <= 300), colour = "grey75",
              linetype = "dotted") +
    labs(x = "Day", y = "Value",
         caption = "Dotted grey line: Ref TTI") +
    theme_classic() +
    theme(plot.caption = element_text(colour = "grey75"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"),
          strip.background = element_rect(colour = "grey80")) -> g2
}

draw_fig_8C <- function(df) {

  plot_df <- df|> filter(time %in% c(180, 270)) |>
    mutate(lbl_time = str_glue("At day {time}"))

  cols <- viridis(6, direction = -1)

  ggplot(plot_df, aes(x = par_xi, y = par_tau_m, fill =  100 * C/ pop_val)) +
    facet_wrap(~lbl_time, nrow = 1) +
    geom_raster() +
    scale_fill_gradientn(values  = seq(0, 1, 0.2),
                         colours = cols,
                         limits = c(0, 100),
                         breaks = seq(0, 1, 0.2) * 100,
                         name = "Attack rate [%]") +
    labs(x = parse(text = "'Stringency'~(xi)"),
         y = parse(text = "'Start of intervention'~(tau^m)")) +
    theme_classic() +
    theme(axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"),
          strip.background = element_rect(colour = "grey80"))
}


plot_fig_11 <- function(df, base_df, npi_df, vacc_df) {

  g1 <- draw_fig_11A(df, base_df, npi_df)
  g2 <- draw_fig_11B(df, base_df, npi_df)
  g3 <- draw_fig_11C(vacc_df)

  (g1/g2/g3) +
    plot_annotation(tag_levels = 'A')
}

draw_fig_11A <- function(df, base_df, npi_df) {

  ggplot(df, aes(time, 1000 * C_in/pop_val)) +
    geom_line(colour = clrs[[1]]) +
    geom_line(data = base_df, colour = "grey75", alpha = 0.75, linetype = "dotdash") +
    geom_line(data = npi_df, colour = "grey50", linetype = "dashed",
              alpha = 0.75) +
    annotate("text", label = "Base case", x = 70, y = 65, colour = "grey75",
             size = 2.5) +
    annotate("text", label = "TTI + MR", x = 258, y = 55, colour = "grey50",
             size = 2.5) +
    annotate("text", label = "TTI + MR + V", x = 270, y = 25, colour = clrs[[1]],
             size = 2.5) +
    geom_vline(xintercept = 180, colour = clrs[[1]], alpha = 0.15) +
    labs(x = "",
         y = "Incidence rate") +
    theme_classic() +
    theme(axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"))
}

draw_fig_11B <- function(df, base_df, npi_df) {

  df2 <- df |> select(time, Rv) |>
    mutate(Rv = 100 * Rv/ pop_val)

  ggplot(df, aes(time, 100 * C / pop_val)) +
    geom_line(colour = clrs[[1]]) +
    geom_line(data = base_df, colour = "grey75", linetype = "dotdash") +
    geom_line(data = npi_df, colour = "grey50", linetype = "dashed") +
    geom_line(data = df2, aes(y = Rv), colour = clrs[[3]],
              linetype = "dotted") +
    annotate("text", label = "Base case", x = 78, y = 88, colour = "grey75",
             size = 2.5) +
    annotate("text", label = "TTI + MR", x = 265, y = 80, colour = "grey50",
             size = 2.5) +
    annotate("text", label = "TTI + MR + V", x = 275, y = 55,
             colour = clrs[[1]], size = 2.5) +
    annotate("text", label = "Immune (vaccination)", x = 285, y = 15,
             colour = clrs[[3]], size = 2.5) +
    geom_vline(xintercept = 180, colour = clrs[[1]], alpha = 0.15) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "Day",
         y = "Attack rate [%]") +
    theme_classic() +
    theme(axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"))
}

draw_fig_11C <- function(df) {

  ggplot(df, aes(time, 100 * C / pop_val)) +
    geom_line(aes(colour = wln_scn, group = iter, linetype = cap_scn)) +
    scale_colour_manual(values = clrs[c(1, 2)]) +
    scale_linetype_manual(values = c("solid", "dotted")) +
    scale_y_continuous(limits = c(0, 100)) +
    geom_vline(xintercept = 180, colour = clrs[[1]], alpha = 0.15) +
    labs(colour   = "Vaccination willingness",
         linetype = "Capacity",
         y        = "Attack rate [%]",
         caption  = "Vertical line: Start of vaccination") +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"),
          plot.caption     = element_text(colour = clrs[[1]]))
}
