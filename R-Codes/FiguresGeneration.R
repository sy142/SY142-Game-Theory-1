rm(list = ls())
gc()

base_dir <- "C:/Users/Salim/Desktop/makaleler/Oyun Teorisi diyetisyen"
sim_dir <- file.path(base_dir, "simulation")
fig_dir <- file.path(base_dir, "Figures")

if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

setwd(sim_dir)
load("simulation_results.RData")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

payoff_df <- read.csv("results/payoff_matrix.csv")
tournament_df <- read.csv("results/tournament_rankings.csv")
mc_coop_df <- read.csv("results/monte_carlo_coop_results.csv")
mc_nash_df <- read.csv("results/monte_carlo_nash_results.csv")
sens_discount_df <- read.csv("results/sensitivity_discount_rate.csv")
sens_shock_df <- read.csv("results/sensitivity_shock_prob.csv")
sens_demand_df <- read.csv("results/sensitivity_base_demand.csv")
sens_burnout_df <- read.csv("results/sensitivity_burnout.csv")
sample_sim_df <- results$mc_coop$monthly_data[[1]]

theme_pub <- theme_minimal(base_size = 14, base_family = "serif") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey30"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 15, 10, 10))

col_A <- "#2E86AB"
col_B <- "#A23B72"

cat("Figure S6.1 - Payoff Matrix Heatmap...\n")

p1 <- ggplot(payoff_df, aes(x = factor(Strategy_B), y = factor(Strategy_A),
                            fill = Total_NPV / 1e6)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = paste0(round(NPV_A / 1e6, 1), " / ", round(NPV_B / 1e6, 1))),
            size = 5, fontface = "bold", family = "serif") +
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850",
                       midpoint = median(payoff_df$Total_NPV / 1e6),
                       name = "Total NPV\n(Million TL)") +
  scale_x_discrete(labels = c("S1\nDestructive", "S2\nAggressive",
                              "S3\nCooperative", "S4\nPremium")) +
  scale_y_discrete(labels = c("S1\nDestructive", "S2\nAggressive",
                              "S3\nCooperative", "S4\nPremium")) +
  labs(title = "Payoff Matrix (Center A NPV / Center B NPV)",
       subtitle = "36-Month Simulation, n = 100 per cell",
       x = "Center B Strategy", y = "Center A Strategy") +
  theme_pub +
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        panel.grid = element_blank())

ggsave(file.path(fig_dir, "Figure_S6_1_Payoff_Matrix.png"),
       p1, width = 10, height = 8, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_1_Payoff_Matrix.pdf"),
       p1, width = 10, height = 8, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.2 - Tournament Results...\n")

tournament_df <- tournament_df %>% arrange(rank)
tournament_df$strategy_en <- tournament_df$strategy
tournament_df$strategy_en <- gsub("S1_Sabit", "S1_Fixed", tournament_df$strategy_en)
tournament_df$strategy_en <- gsub("S2_Sabit", "S2_Fixed", tournament_df$strategy_en)
tournament_df$strategy_en <- gsub("S3_Sabit", "S3_Fixed", tournament_df$strategy_en)
tournament_df$strategy_en <- gsub("S4_Sabit", "S4_Fixed", tournament_df$strategy_en)

fill_colors <- ifelse(tournament_df$strategy_en %in%
                        c("Forgiving_TfT", "Grim_Trigger", "Tit_for_Tat"), "#2E86AB",
                      ifelse(tournament_df$strategy_en %in% c("S2_Fixed", "S1_Fixed"), "#D73027", "#7FB069"))

p2 <- ggplot(tournament_df, aes(x = reorder(strategy_en, -average_npv),
                                y = average_npv / 1e6)) +
  geom_bar(stat = "identity", fill = fill_colors[order(-tournament_df$average_npv)],
           alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("#%d", rank)), vjust = -0.5,
            size = 4, fontface = "bold", family = "serif") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Strategy Tournament Results",
       subtitle = "Average NPV across all opponent matchups (n = 100 per pair)",
       x = "Strategy", y = "Average NPV (Million TL)") +
  theme_pub +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))

ggsave(file.path(fig_dir, "Figure_S6_2_Tournament.png"),
       p2, width = 12, height = 7, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_2_Tournament.pdf"),
       p2, width = 12, height = 7, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.3 - Monte Carlo Distribution...\n")

mc_long <- data.frame(
  NPV = c(mc_coop_df$final_npv_A / 1e6, mc_coop_df$final_npv_B / 1e6,
          mc_nash_df$final_npv_A / 1e6, mc_nash_df$final_npv_B / 1e6),
  Center = rep(c("Center A", "Center B", "Center A", "Center B"),
               each = nrow(mc_coop_df)),
  Scenario = rep(c("Cooperation (S3-S3)", "Cooperation (S3-S3)",
                   "Nash Equilibrium (S2-S2)", "Nash Equilibrium (S2-S2)"),
                 each = nrow(mc_coop_df)))

p3 <- ggplot(mc_long, aes(x = NPV, fill = Center)) +
  geom_histogram(alpha = 0.65, position = "identity", bins = 45,
                 color = "white", linewidth = 0.2) +
  facet_wrap(~ Scenario, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  labs(title = "Monte Carlo Simulation NPV Distribution",
       subtitle = sprintf("n = %d simulations per scenario", nrow(mc_coop_df)),
       x = "Net Present Value (Million TL)", y = "Frequency") +
  theme_pub +
  theme(strip.text = element_text(size = 13, face = "bold"),
        strip.background = element_rect(fill = "grey95", color = NA))

ggsave(file.path(fig_dir, "Figure_S6_3_Monte_Carlo.png"),
       p3, width = 10, height = 8, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_3_Monte_Carlo.pdf"),
       p3, width = 10, height = 8, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.4 - NPV Evolution...\n")

npv_evol <- data.frame(
  Month = rep(sample_sim_df$month, 2),
  NPV = c(sample_sim_df$npv_A / 1e6, sample_sim_df$npv_B / 1e6),
  Center = rep(c("Center A", "Center B"), each = nrow(sample_sim_df)))

p4 <- ggplot(npv_evol, aes(x = Month, y = NPV, color = Center)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5, alpha = 0.6) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  labs(title = "Cumulative NPV Evolution",
       subtitle = "Sample simulation under cooperation (S3-S3)",
       x = "Month", y = "Cumulative NPV (Million TL)") +
  theme_pub

ggsave(file.path(fig_dir, "Figure_S6_4_NPV_Evolution.png"),
       p4, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_4_NPV_Evolution.pdf"),
       p4, width = 10, height = 6, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.5 - Market Share Evolution...\n")

market_evol <- data.frame(
  Month = rep(sample_sim_df$month, 2),
  Share = c(sample_sim_df$market_share_A * 100, sample_sim_df$market_share_B * 100),
  Center = rep(c("Center A", "Center B"), each = nrow(sample_sim_df)))

p5 <- ggplot(market_evol, aes(x = Month, y = Share, fill = Center)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Market Share Evolution",
       subtitle = "Sample simulation under cooperation (S3-S3)",
       x = "Month", y = "Market Share (%)") +
  theme_pub

ggsave(file.path(fig_dir, "Figure_S6_5_Market_Share.png"),
       p5, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_5_Market_Share.pdf"),
       p5, width = 10, height = 6, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.6 - Employee Satisfaction...\n")

sat_evol <- data.frame(
  Month = rep(sample_sim_df$month, 2),
  Satisfaction = c(sample_sim_df$satisfaction_A, sample_sim_df$satisfaction_B),
  Center = rep(c("Center A", "Center B"), each = nrow(sample_sim_df)))

p6 <- ggplot(sat_evol, aes(x = Month, y = Satisfaction, color = Center)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "#27AE60", alpha = 0.7) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#E74C3C", alpha = 0.7) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  scale_y_continuous(limits = c(0, 100)) +
  annotate("text", x = 34, y = 73, label = "Capacity bonus", color = "#27AE60",
           size = 3.5, hjust = 1, fontface = "italic", family = "serif") +
  annotate("text", x = 34, y = 17, label = "Capacity penalty", color = "#E74C3C",
           size = 3.5, hjust = 1, fontface = "italic", family = "serif") +
  labs(title = "Employee Satisfaction Evolution",
       subtitle = "Sample simulation under cooperation (S3-S3)",
       x = "Month", y = "Satisfaction Score (0-100)") +
  theme_pub

ggsave(file.path(fig_dir, "Figure_S6_6_Satisfaction.png"),
       p6, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_6_Satisfaction.pdf"),
       p6, width = 10, height = 6, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.7 - Sensitivity Shock Probability...\n")

sens_shock_long <- sens_shock_df %>%
  select(shock_probability, mean_npv_A, mean_npv_B) %>%
  pivot_longer(cols = c(mean_npv_A, mean_npv_B),
               names_to = "Center", values_to = "NPV") %>%
  mutate(Center = ifelse(Center == "mean_npv_A", "Center A", "Center B"))

p7 <- ggplot(sens_shock_long, aes(x = shock_probability * 100,
                                  y = NPV / 1e6, color = Center)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(data = sens_shock_df,
              aes(x = shock_probability * 100,
                  ymin = (mean_npv_A - sd_npv_A) / 1e6,
                  ymax = (mean_npv_A + sd_npv_A) / 1e6),
              fill = col_A, alpha = 0.15, inherit.aes = FALSE) +
  geom_ribbon(data = sens_shock_df,
              aes(x = shock_probability * 100,
                  ymin = (mean_npv_B - sd_npv_B) / 1e6,
                  ymax = (mean_npv_B + sd_npv_B) / 1e6),
              fill = col_B, alpha = 0.15, inherit.aes = FALSE) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  labs(title = "Sensitivity Analysis: Shock Probability",
       subtitle = "Cooperation scenario (S3-S3), n = 100 per value",
       x = "Monthly Shock Probability (%)",
       y = "Mean NPV (Million TL)") +
  theme_pub

ggsave(file.path(fig_dir, "Figure_S6_7_Sensitivity_Shock.png"),
       p7, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_7_Sensitivity_Shock.pdf"),
       p7, width = 10, height = 6, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.8 - Sensitivity Base Demand...\n")

sens_demand_long <- sens_demand_df %>%
  select(base_demand, mean_npv_A, mean_npv_B) %>%
  pivot_longer(cols = c(mean_npv_A, mean_npv_B),
               names_to = "Center", values_to = "NPV") %>%
  mutate(Center = ifelse(Center == "mean_npv_A", "Center A", "Center B"))

p8 <- ggplot(sens_demand_long, aes(x = base_demand, y = NPV / 1e6, color = Center)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(data = sens_demand_df,
              aes(x = base_demand,
                  ymin = (mean_npv_A - sd_npv_A) / 1e6,
                  ymax = (mean_npv_A + sd_npv_A) / 1e6),
              fill = col_A, alpha = 0.15, inherit.aes = FALSE) +
  geom_ribbon(data = sens_demand_df,
              aes(x = base_demand,
                  ymin = (mean_npv_B - sd_npv_B) / 1e6,
                  ymax = (mean_npv_B + sd_npv_B) / 1e6),
              fill = col_B, alpha = 0.15, inherit.aes = FALSE) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  labs(title = "Sensitivity Analysis: Base Demand",
       subtitle = "Cooperation scenario (S3-S3), n = 100 per value",
       x = "Monthly Base Demand (Number of Clients)",
       y = "Mean NPV (Million TL)") +
  theme_pub

ggsave(file.path(fig_dir, "Figure_S6_8_Sensitivity_Demand.png"),
       p8, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_8_Sensitivity_Demand.pdf"),
       p8, width = 10, height = 6, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.9 - Sensitivity Burnout Threshold...\n")

sens_burnout_long <- sens_burnout_df %>%
  select(burnout_threshold, mean_satisfaction_A, mean_satisfaction_B) %>%
  pivot_longer(cols = c(mean_satisfaction_A, mean_satisfaction_B),
               names_to = "Center", values_to = "Satisfaction") %>%
  mutate(Center = ifelse(Center == "mean_satisfaction_A", "Center A", "Center B"))

p9 <- ggplot(sens_burnout_long, aes(x = burnout_threshold * 100,
                                    y = Satisfaction, color = Center)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "#27AE60", alpha = 0.5) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#E74C3C", alpha = 0.5) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  scale_y_continuous(limits = c(0, 100)) +
  annotate("text", x = 62, y = 73, label = "Capacity bonus threshold",
           color = "#27AE60", size = 3.5, fontface = "italic", family = "serif") +
  annotate("text", x = 62, y = 17, label = "Capacity penalty threshold",
           color = "#E74C3C", size = 3.5, fontface = "italic", family = "serif") +
  labs(title = "Sensitivity Analysis: Burnout Threshold",
       subtitle = "Cooperation scenario (S3-S3), n = 100 per value",
       x = "Burnout Threshold (%)",
       y = "Mean Employee Satisfaction (0-100)") +
  theme_pub

ggsave(file.path(fig_dir, "Figure_S6_9_Sensitivity_Burnout.png"),
       p9, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_9_Sensitivity_Burnout.pdf"),
       p9, width = 10, height = 6, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.10 - Demand Decomposition...\n")

demand_decomp <- data.frame(
  Month = rep(sample_sim_df$month, 3),
  Clients = c(sample_sim_df$demand_new_A,
              sample_sim_df$demand_loyal_A,
              sample_sim_df$demand_referral_A),
  Type = factor(rep(c("New Clients", "Loyal Clients", "Referral Clients"),
                    each = nrow(sample_sim_df)),
                levels = c("Referral Clients", "Loyal Clients", "New Clients")))

p10 <- ggplot(demand_decomp, aes(x = Month, y = Clients, fill = Type)) +
  geom_area(alpha = 0.75) +
  scale_fill_manual(values = c("New Clients" = "#2E86AB",
                               "Loyal Clients" = "#27AE60",
                               "Referral Clients" = "#F39C12")) +
  scale_x_continuous(breaks = seq(0, 36, 6)) +
  labs(title = "Demand Decomposition: Center A",
       subtitle = "Sample simulation under cooperation (S3-S3)",
       x = "Month", y = "Number of Clients") +
  theme_pub

ggsave(file.path(fig_dir, "Figure_S6_10_Demand_Decomposition.png"),
       p10, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_10_Demand_Decomposition.pdf"),
       p10, width = 10, height = 6, dpi = 600, )
cat("  Done.\n")

cat("Figure S6.11 - Sensitivity Discount Rate...\n")

sens_disc_long <- sens_discount_df %>%
  select(annual_rate, mean_npv_A, mean_npv_B) %>%
  pivot_longer(cols = c(mean_npv_A, mean_npv_B),
               names_to = "Center", values_to = "NPV") %>%
  mutate(Center = ifelse(Center == "mean_npv_A", "Center A", "Center B"))

p11 <- ggplot(sens_disc_long, aes(x = annual_rate * 100,
                                  y = NPV / 1e6, color = Center)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  labs(title = "Sensitivity Analysis: Discount Rate",
       subtitle = "Cooperation scenario (S3-S3), n = 100 per value",
       x = "Annual Discount Rate (%)",
       y = "Mean NPV (Million TL)") +
  theme_pub

ggsave(file.path(fig_dir, "Figure_S6_11_Sensitivity_Discount.png"),
       p11, width = 10, height = 6, dpi = 600)
ggsave(file.path(fig_dir, "Figure_S6_11_Sensitivity_Discount.pdf"),
       p11, width = 10, height = 6, dpi = 600, )
cat("  Done.\n")

cat("\n==========================================\n")
cat("ALL FIGURES GENERATED\n")
cat(sprintf("Output directory: %s\n", fig_dir))
cat(sprintf("Total files: %d (PNG + PDF)\n", length(list.files(fig_dir))))
cat("==========================================\n")