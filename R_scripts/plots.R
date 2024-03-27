library(ggplot2)
library(patchwork)
library(scales)
library(viridis)

source("./R_scripts/plots_paper.R")

plot_final_size <- function(df) {

  ggplot(df, aes(c)) +
    geom_histogram(aes(colour = model), fill = "white") +
    facet_wrap(~model, ncol = 1) +
    scale_colour_manual(values = c("#2E71AF", "#FFBE82")) +
    labs(x = parse(text = "'Attack rate at day 150'~(c[150])"),
         y = "Count") +
    theme_minimal()
}

plot_output_comparison <- function(df) {

  ggplot(df, aes(time, 1000 * C_in/pop_val)) +
    geom_line(aes(colour = model, linetype = model), alpha = 0.5,
              linewidth = 1.5) +
    facet_wrap(~itv, ncol = 1) +
    scale_linetype_manual(values = c("solid", "dotdash")) +
    scale_colour_manual(values = c("grey70", "#0363BB")) +
    scale_x_continuous(limits = c(0, 250)) +
    theme_classic() +
    labs(x = "Day", y = "Incidence rate")
}

plot_fit <- function(df_sim, df_data) {

  ggplot(df_sim, aes(time, Q)) +
    geom_line() +
    geom_point(data = df_data, aes(y = value), colour = clrs[[1]]) +
    labs(x = "Day", y = "Capacity",
         caption = "Points: Data") +
    theme_classic() +
    theme(axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"),
          plot.caption     = element_text(colour = clrs[[1]]))
}

plot_stringency_comparison <- function(df) {

  g1 <- ggplot(df |>
                 mutate(c = C / N,
                        Scenario = as.factor(iter)) , aes(time, Rt)) +
    geom_line(aes(group = Scenario, colour = Scenario)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_colour_manual(values = clrs[c(1, 2, 4)]) +
    labs(x = "Days",
         y = parse(text = "\u211c[t]"),
         caption = "Dashed line: Epidemic threshold") +
    theme_classic() +
    theme(text = element_text(family = "Arial Unicode MS"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.title.y     = element_text(angle = 0, vjust = 0.5),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"),
          strip.background = element_rect(colour = "grey80"))

  g2 <- ggplot(df |>
                 mutate(c = C / N,
                        Scenario = as.factor(iter)) , aes(time, s)) +
    scale_colour_manual(values = clrs[c(1, 2, 4)]) +
    geom_line(aes(group = Scenario, colour = Scenario)) +
    geom_hline(yintercept = 0.44, linetype = "dotted") +
    labs(x       = "Days",
         y       = "Susceptble fraction",
         caption = "Dotted line: Herd immunity fraction") +
    theme_classic() +
    theme(text = element_text(family = "Arial Unicode MS"),
          axis.title       = element_text(colour = "grey65", size = 9),
          axis.line        = element_line(colour = "grey90"),
          axis.text        = element_text(colour = "grey70", size = 8),
          axis.ticks       = element_line(colour = "grey90"),
          strip.background = element_rect(colour = "grey80"))

  (g1/g2) +
    plot_layout(guides = 'collect')
}



