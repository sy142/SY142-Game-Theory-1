library(ggplot2)
library(patchwork)

base_dir <- tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path), error = function(e) getwd())
fig_dir  <- file.path(dirname(base_dir), "figures")
data_dir <- file.path(dirname(base_dir), "data")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

tournament_df     <- read.csv(file.path(data_dir, "tournament_rankings.csv"))
mc_coop_df        <- read.csv(file.path(data_dir, "monte_carlo_coop_results.csv"))
mc_nash_df        <- read.csv(file.path(data_dir, "monte_carlo_nash_results.csv"))
sample_sim_df     <- read.csv(file.path(data_dir, "sample_simulation_monthly.csv"))
sens_discount_df  <- read.csv(file.path(data_dir, "sensitivity_discount_rate.csv"))
sens_shock_df     <- read.csv(file.path(data_dir, "sensitivity_shock_prob.csv"))
sens_demand_df    <- read.csv(file.path(data_dir, "sensitivity_base_demand.csv"))
sens_burnout_df   <- read.csv(file.path(data_dir, "sensitivity_burnout.csv"))

col_A <- "#2E86AB"
col_B <- "#A23B72"

theme_paper <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    plot.title       = element_text(size = 12, face = "bold", hjust = 0),
    plot.subtitle    = element_text(size = 9, color = "grey40", hjust = 0),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size = 9),
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    plot.margin      = margin(8, 12, 8, 8))

tournament_df$strategy_en <- tournament_df$strategy
tournament_df$strategy_en <- gsub("S1_Sabit", "S1 Fixed", tournament_df$strategy_en)
tournament_df$strategy_en <- gsub("S2_Sabit", "S2 Fixed", tournament_df$strategy_en)
tournament_df$strategy_en <- gsub("S3_Sabit", "S3 Fixed", tournament_df$strategy_en)
tournament_df$strategy_en <- gsub("S4_Sabit", "S4 Fixed", tournament_df$strategy_en)
tournament_df$strategy_en <- gsub("_", " ", tournament_df$strategy_en)
tournament_df$strategy_en <- gsub("Tit for Tat", "Tit-for-Tat", tournament_df$strategy_en)
tournament_df <- tournament_df[order(-tournament_df$average_npv), ]
tournament_df$color_group <- ifelse(
  tournament_df$strategy_en %in% c("Forgiving TfT", "Grim Trigger", "Tit-for-Tat"), "Reciprocity",
  ifelse(tournament_df$strategy_en %in% c("S2 Fixed", "S1 Fixed"), "Aggressive",
         "Context-dependent"))

fig2 <- ggplot(tournament_df,
               aes(x = reorder(strategy_en, average_npv),
                   y = average_npv / 1e6,
                   fill = color_group)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_text(aes(label = sprintf("#%d", rank)),
            hjust = -0.3, size = 3.2, fontface = "bold", family = "serif") +
  coord_flip() +
  scale_fill_manual(
    values = c("Reciprocity" = "#2E86AB",
               "Context-dependent" = "#7FB069",
               "Aggressive" = "#D73027"),
    name = "Strategy Type") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Average NPV (Million TL)") +
  theme_paper +
  theme(legend.position = "right", panel.grid.major.y = element_blank())

ggsave(file.path(fig_dir, "Figure2_Tournament.png"), fig2, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figure2_Tournament.pdf"), fig2, width = 10, height = 6)
cat("Figure 2 saved.\n")

mc_long <- data.frame(
  NPV = c(mc_coop_df$final_npv_A / 1e6, mc_coop_df$final_npv_B / 1e6,
           mc_nash_df$final_npv_A / 1e6, mc_nash_df$final_npv_B / 1e6),
  Center = rep(c("Center A", "Center B", "Center A", "Center B"),
               each = nrow(mc_coop_df)),
  Scenario = factor(
    rep(c("Cooperation (S3\u2013S3)", "Cooperation (S3\u2013S3)",
          "Nash Equilibrium (S2\u2013S2)", "Nash Equilibrium (S2\u2013S2)"),
        each = nrow(mc_coop_df)),
    levels = c("Cooperation (S3\u2013S3)", "Nash Equilibrium (S2\u2013S2)")))

fig3 <- ggplot(mc_long, aes(x = NPV, fill = Center)) +
  geom_histogram(alpha = 0.65, position = "identity", bins = 45,
                 color = "white", linewidth = 0.2) +
  facet_wrap(~ Scenario, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  labs(x = "Net Present Value (Million TL)", y = "Frequency") +
  theme_paper +
  theme(strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "grey95", color = NA))

ggsave(file.path(fig_dir, "Figure3_Monte_Carlo.png"), fig3, width = 10, height = 8, dpi = 600)
ggsave(file.path(fig_dir, "Figure3_Monte_Carlo.pdf"), fig3, width = 10, height = 8)
cat("Figure 3 saved.\n")

p4a <- ggplot() +
  geom_line(data = sample_sim_df, aes(x = month, y = npv_A / 1e6, color = "Center A"), linewidth = 1) +
  geom_line(data = sample_sim_df, aes(x = month, y = npv_B / 1e6, color = "Center B"), linewidth = 1) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B), name = NULL) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  labs(title = "(a) Cumulative NPV Evolution", x = "Month", y = "Cumulative NPV\n(Million TL)") +
  theme_paper +
  theme(legend.position = c(0.2, 0.85),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 11))

p4b <- ggplot(sample_sim_df) +
  geom_line(aes(x = month, y = market_share_A * 100), color = col_A, linewidth = 1) +
  geom_line(aes(x = month, y = market_share_B * 100), color = col_B, linewidth = 1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  scale_y_continuous(limits = c(35, 70)) +
  labs(title = "(b) Market Share Evolution", x = "Month", y = "Market Share (%)") +
  theme_paper +
  theme(plot.title = element_text(size = 11))

p4c <- ggplot(sample_sim_df) +
  geom_line(aes(x = month, y = satisfaction_A, color = "Center A"), linewidth = 1) +
  geom_line(aes(x = month, y = satisfaction_B, color = "Center B"), linewidth = 1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "#E74C3C", linewidth = 0.5) +
  geom_hline(yintercept = 70, linetype = "dotted", color = "#27AE60", linewidth = 0.5) +
  annotate("text", x = 35, y = 52, label = "Critical", size = 2.5,
           color = "#E74C3C", hjust = 1, fontface = "italic") +
  annotate("text", x = 35, y = 72, label = "Bonus", size = 2.5,
           color = "#27AE60", hjust = 1, fontface = "italic") +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B), name = NULL) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  scale_y_continuous(limits = c(15, 105)) +
  labs(title = "(c) Employee Satisfaction", x = "Month", y = "Satisfaction Score\n(0\u2013100)") +
  theme_paper +
  theme(legend.position = c(0.85, 0.3),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 11))

demand_decomp <- data.frame(
  Month = rep(sample_sim_df$month, 3),
  Clients = c(sample_sim_df$demand_new_A,
              sample_sim_df$demand_loyal_A,
              sample_sim_df$demand_referral_A),
  Type = factor(rep(c("New Clients", "Loyal Clients", "Referral Clients"),
                    each = nrow(sample_sim_df)),
                levels = c("Referral Clients", "Loyal Clients", "New Clients")))

p4d <- ggplot(demand_decomp, aes(x = Month, y = Clients, fill = Type)) +
  geom_area(alpha = 0.75) +
  scale_fill_manual(values = c("New Clients" = "#2E86AB",
                               "Loyal Clients" = "#27AE60",
                               "Referral Clients" = "#F39C12"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  labs(title = "(d) Demand Decomposition \u2013 Center A", x = "Month", y = "Number of Clients") +
  theme_paper +
  theme(legend.position = c(0.75, 0.85),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.35, "cm"),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 11))

fig4 <- (p4a | p4b) / (p4c | p4d) +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5)))

ggsave(file.path(fig_dir, "Figure4_Simulation_Dynamics.png"), fig4, width = 12, height = 9, dpi = 600)
ggsave(file.path(fig_dir, "Figure4_Simulation_Dynamics.pdf"), fig4, width = 12, height = 9)
cat("Figure 4 saved.\n")

sens_disc_long <- data.frame(
  Rate = rep(sens_discount_df$annual_rate * 100, 2),
  NPV = c(sens_discount_df$mean_npv_A / 1e6, sens_discount_df$mean_npv_B / 1e6),
  Center = rep(c("Center A", "Center B"), each = nrow(sens_discount_df)))

p5a <- ggplot(sens_disc_long, aes(x = Rate, y = NPV, color = Center)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B), name = NULL) +
  labs(title = "(a) Discount Rate", x = "Annual Discount Rate (%)", y = "Mean NPV\n(Million TL)") +
  theme_paper +
  theme(legend.position = c(0.8, 0.85),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 11))

sens_shock_long <- data.frame(
  Prob = rep(sens_shock_df$shock_probability * 100, 2),
  NPV = c(sens_shock_df$mean_npv_A / 1e6, sens_shock_df$mean_npv_B / 1e6),
  Center = rep(c("Center A", "Center B"), each = nrow(sens_shock_df)))

p5b <- ggplot(sens_shock_long, aes(x = Prob, y = NPV, color = Center)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B), name = NULL) +
  labs(title = "(b) Shock Probability", x = "Monthly Shock Probability (%)", y = "Mean NPV\n(Million TL)") +
  theme_paper +
  theme(legend.position = "none", plot.title = element_text(size = 11))

sens_dem_long <- data.frame(
  Demand = rep(sens_demand_df$base_demand, 2),
  NPV = c(sens_demand_df$mean_npv_A / 1e6, sens_demand_df$mean_npv_B / 1e6),
  Center = rep(c("Center A", "Center B"), each = nrow(sens_demand_df)))

p5c <- ggplot(sens_dem_long, aes(x = Demand, y = NPV, color = Center)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B), name = NULL) +
  labs(title = "(c) Base Demand", x = "Base Monthly Demand (clients)", y = "Mean NPV\n(Million TL)") +
  theme_paper +
  theme(legend.position = "none", plot.title = element_text(size = 11))

sens_burn_long <- data.frame(
  Threshold = rep(sens_burnout_df$burnout_threshold * 100, 2),
  Satisfaction = c(sens_burnout_df$mean_satisfaction_A, sens_burnout_df$mean_satisfaction_B),
  Center = rep(c("Center A", "Center B"), each = nrow(sens_burnout_df)))

p5d <- ggplot(sens_burn_long, aes(x = Threshold, y = Satisfaction, color = Center)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 70, linetype = "dotted", color = "#27AE60", linewidth = 0.4) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#E74C3C", linewidth = 0.4) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B), name = NULL) +
  scale_y_continuous(limits = c(15, 105)) +
  labs(title = "(d) Burnout Threshold", x = "Burnout Threshold (%)", y = "Mean Satisfaction\n(0\u2013100)") +
  theme_paper +
  theme(legend.position = "none", plot.title = element_text(size = 11))

fig5 <- (p5a | p5b) / (p5c | p5d) +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5)))

ggsave(file.path(fig_dir, "Figure5_Sensitivity_Panel.png"), fig5, width = 12, height = 9, dpi = 600)
ggsave(file.path(fig_dir, "Figure5_Sensitivity_Panel.pdf"), fig5, width = 12, height = 9)
cat("Figure 5 saved.\n")

cat("\nAll figures generated.\n")
