rm(list = ls())
gc()

base_dir <- "C:/Users/Salim/Desktop/makaleler/Oyun Teorisi diyetisyen"
output_dir <- file.path(base_dir, "simulation")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(file.path(output_dir, "results"))) dir.create(file.path(output_dir, "results"), recursive = TRUE)
if (!dir.exists(file.path(output_dir, "plots"))) dir.create(file.path(output_dir, "plots"), recursive = TRUE)

setwd(output_dir)

cat("==========================================\n")
cat("SIMULASYON BASLATILIYOR\n")
cat("==========================================\n\n")

source(file.path(base_dir, "TamSimulasyon.R"))

cat("\n==========================================\n")
cat("ADIM 1/6: KAPSAMLI ANALIZ\n")
cat("   (Yaklasik 15-20 dakika)\n")
cat("==========================================\n\n")

results <- run_comprehensive_analysis(n_sims = 1000, n_tourn = 100)

cat("\n==========================================\n")
cat("ADIM 2/6: CSV KAYDI\n")
cat("==========================================\n\n")

export_results_to_csv(results, output_dir = "results")

cat("\n==========================================\n")
cat("ADIM 3/6: GENISLETILMIS TURNUVA\n")
cat("   (Yaklasik 60-75 dakika)\n")
cat("==========================================\n\n")

extended <- run_extended_strategy_analysis(n_sims = 200)

write.csv(extended$rankings,
          "results/extended_strategy_rankings.csv", row.names = FALSE)
write.csv(extended$results_matrix,
          "results/extended_strategy_matrix.csv")

cat("\nGenisletilmis Turnuva Siralamai:\n")
print(extended$rankings)

cat("\n==========================================\n")
cat("ADIM 4/6: OYUN TEORISI DOGRULAMA\n")
cat("==========================================\n\n")

verify_game_theory_properties(results$payoff_matrices$payoff_A,
                              results$payoff_matrices$payoff_B)

cat("\n==========================================\n")
cat("ADIM 5/6: MEMNUNIYET VE MEKANIZMA KONTROLU\n")
cat("==========================================\n\n")

test_sim <- run_single_simulation(
  create_fixed_strategy(3, 1),
  create_fixed_strategy(3, 1))

cat("Isbirligi Senaryosunda (S3 vs S3) Memnuniyet:\n")
cat(sprintf("  Merkez A: ortalama=%.1f, min=%.1f, max=%.1f\n",
            mean(test_sim$satisfaction_A), min(test_sim$satisfaction_A), max(test_sim$satisfaction_A)))
cat(sprintf("  Merkez B: ortalama=%.1f, min=%.1f, max=%.1f\n",
            mean(test_sim$satisfaction_B), min(test_sim$satisfaction_B), max(test_sim$satisfaction_B)))

cat("\nSadakat ve Referans Mekanizmasi:\n")
cat(sprintf("  Ortalama Sadik Danisan A: %.1f/ay\n", mean(test_sim$demand_loyal_A)))
cat(sprintf("  Ortalama Sadik Danisan B: %.1f/ay\n", mean(test_sim$demand_loyal_B)))
cat(sprintf("  Ortalama Referans Danisan A: %.1f/ay\n", mean(test_sim$demand_referral_A)))
cat(sprintf("  Ortalama Referans Danisan B: %.1f/ay\n", mean(test_sim$demand_referral_B)))

cat("\nKapasite Kullanimi:\n")
cat(sprintf("  Merkez A: ortalama=%.1f%%, max=%.1f%%\n",
            mean(test_sim$capacity_util_A)*100, max(test_sim$capacity_util_A)*100))
cat(sprintf("  Merkez B: ortalama=%.1f%%, max=%.1f%%\n",
            mean(test_sim$capacity_util_B)*100, max(test_sim$capacity_util_B)*100))

cat("\nItibar:\n")
cat(sprintf("  Merkez A: ortalama=%.1f, son ay=%.1f\n",
            mean(test_sim$reputation_A), test_sim$reputation_A[36]))
cat(sprintf("  Merkez B: ortalama=%.1f, son ay=%.1f\n",
            mean(test_sim$reputation_B), test_sim$reputation_B[36]))

cat("\nNakit Durumu:\n")
cat(sprintf("  Merkez A: son ay kasa=%.0f TL\n", test_sim$cash_A[36]))
cat(sprintf("  Merkez B: son ay kasa=%.0f TL\n", test_sim$cash_B[36]))

save(results, extended, test_sim, file = "simulation_results.RData")

cat("\n==========================================\n")
cat("ADIM 6/6: TABLOLAR VE GRAFIKLER\n")
cat("==========================================\n\n")

setwd("C:/Users/Salim/Desktop/makaleler/Oyun Teorisi diyetisyen/simulation")
dir.create("plots", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

write.csv(results$mc_coop$results, "results/monte_carlo_coop_results.csv", row.names = FALSE)
write.csv(results$mc_nash$results, "results/monte_carlo_nash_results.csv", row.names = FALSE)
write.csv(results$mc_coop$monthly_data[[1]], "results/sample_simulation_monthly.csv", row.names = FALSE)

cat("coop satir:", nrow(results$mc_coop$results), "\n")
cat("nash satir:", nrow(results$mc_nash$results), "\n")
cat("sample sutun:", ncol(results$mc_coop$monthly_data[[1]]), "\n")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

export_results_to_csv(results, output_dir = "results")
sample_sim_df <- results$mc_coop$monthly_data[[1]]
write.csv(sample_sim_df, "results/sample_simulation_monthly.csv", row.names = FALSE)

payoff_df <- read.csv("results/payoff_matrix.csv")
tournament_df <- read.csv("results/tournament_rankings.csv")
mc_coop_df <- read.csv("results/monte_carlo_coop_results.csv")
mc_nash_df <- read.csv("results/monte_carlo_nash_results.csv")
sens_discount_df <- read.csv("results/sensitivity_discount_rate.csv")
sens_shock_df <- read.csv("results/sensitivity_shock_prob.csv")
sens_demand_df <- read.csv("results/sensitivity_base_demand.csv")
sens_burnout_df <- read.csv("results/sensitivity_burnout.csv")
sample_sim_df <- read.csv("results/sample_simulation_monthly.csv")

cat("=====================================\n")
cat("TABLO 1: GETIRI MATRISI\n")
cat("=====================================\n\n")

for (i in 1:4) {
  for (j in 1:4) {
    row <- payoff_df %>% filter(Strategy_A == i, Strategy_B == j)
    cat(sprintf("S%d vs S%d: %.2f / %.2f (Toplam: %.2f M)\n",
                i, j, row$NPV_A/1e6, row$NPV_B/1e6, row$Total_NPV/1e6))
  }
  cat("\n")
}

cat("=====================================\n")
cat("TABLO 2: MAHKUM IKILEMI\n")
cat("=====================================\n\n")

T_val <- payoff_df %>% filter(Strategy_A == 2, Strategy_B == 3) %>% pull(NPV_A)
R_val <- payoff_df %>% filter(Strategy_A == 3, Strategy_B == 3) %>% pull(NPV_A)
P_val <- payoff_df %>% filter(Strategy_A == 2, Strategy_B == 2) %>% pull(NPV_A)
S_val <- payoff_df %>% filter(Strategy_A == 3, Strategy_B == 2) %>% pull(NPV_A)

cat(sprintf("T (Temptation) = %.2f M TL (S2 vs S3)\n", T_val/1e6))
cat(sprintf("R (Reward)     = %.2f M TL (S3 vs S3)\n", R_val/1e6))
cat(sprintf("P (Punishment) = %.2f M TL (S2 vs S2)\n", P_val/1e6))
cat(sprintf("S (Sucker)     = %.2f M TL (S3 vs S2)\n", S_val/1e6))

is_pd <- (T_val > R_val) && (R_val > P_val) && (P_val > S_val)
cat(sprintf("\nT>R>P>S: %s\n", ifelse(is_pd, "SAGLANIYOR", "SAGLANMIYOR")))
cat(sprintf("T>R farki: %.1f%%\n", (T_val - R_val) / R_val * 100))
cat(sprintf("2R(%.2f) > T+S(%.2f): %s\n",
            2*R_val/1e6, (T_val+S_val)/1e6,
            ifelse(2*R_val > T_val+S_val, "EVET", "HAYIR")))

cat("\n=====================================\n")
cat("TABLO 3: NASH DENGESI\n")
cat("=====================================\n\n")

cat("A'nin En Iyi Yanitlari:\n")
for (j in 1:4) {
  col_data <- payoff_df %>% filter(Strategy_B == j)
  best <- col_data[which.max(col_data$NPV_A), ]
  cat(sprintf("  B=S%d -> A=S%d (%.2fM)\n", j, best$Strategy_A, best$NPV_A/1e6))
}
cat("\nB'nin En Iyi Yanitlari:\n")
for (i in 1:4) {
  row_data <- payoff_df %>% filter(Strategy_A == i)
  best <- row_data[which.max(row_data$NPV_B), ]
  cat(sprintf("  A=S%d -> B=S%d (%.2fM)\n", i, best$Strategy_B, best$NPV_B/1e6))
}

cat("\n=====================================\n")
cat("TABLO 4: PARETO OPTIMALLIK\n")
cat("=====================================\n\n")

payoff_df$is_pareto <- TRUE
for (k in 1:nrow(payoff_df)) {
  for (l in 1:nrow(payoff_df)) {
    if (k != l &&
        payoff_df$NPV_A[l] >= payoff_df$NPV_A[k] &&
        payoff_df$NPV_B[l] >= payoff_df$NPV_B[k] &&
        (payoff_df$NPV_A[l] > payoff_df$NPV_A[k] || payoff_df$NPV_B[l] > payoff_df$NPV_B[k])) {
      payoff_df$is_pareto[k] <- FALSE
      break
    }
  }
}

pareto <- payoff_df %>% filter(is_pareto)
for (k in 1:nrow(pareto)) {
  cat(sprintf("  (S%d, S%d) -> A:%.2fM, B:%.2fM, Toplam:%.2fM\n",
              pareto$Strategy_A[k], pareto$Strategy_B[k],
              pareto$NPV_A[k]/1e6, pareto$NPV_B[k]/1e6, pareto$Total_NPV[k]/1e6))
}

max_total <- max(payoff_df$Total_NPV)
nash_s2 <- payoff_df %>% filter(Strategy_A == 2, Strategy_B == 2)
poa <- (max_total - nash_s2$Total_NPV) / max_total * 100
cat(sprintf("\nPrice of Anarchy: %.1f%%\n", poa))

cat("\n=====================================\n")
cat("TABLO 5: TURNUVA SONUCLARI\n")
cat("=====================================\n\n")

tournament_df <- tournament_df %>% arrange(rank)
for (k in 1:nrow(tournament_df)) {
  t <- tournament_df[k, ]
  cat(sprintf("  %2d. %-20s %.2f M TL\n", t$rank, t$strategy, t$average_npv/1e6))
}

cat("\n=====================================\n")
cat("TABLO 6: MONTE CARLO OZET\n")
cat("=====================================\n\n")

cat("Isbirligi (S3 vs S3):\n")
cat(sprintf("  A: %.2fM (SD:%.2fM) | B: %.2fM (SD:%.2fM)\n",
            mean(mc_coop_df$final_npv_A)/1e6, sd(mc_coop_df$final_npv_A)/1e6,
            mean(mc_coop_df$final_npv_B)/1e6, sd(mc_coop_df$final_npv_B)/1e6))
cat(sprintf("  A kazanma: %.1f%%\n", mean(mc_coop_df$winner == "A")*100))

cat("\nNash (S2 vs S2):\n")
cat(sprintf("  A: %.2fM (SD:%.2fM) | B: %.2fM (SD:%.2fM)\n",
            mean(mc_nash_df$final_npv_A)/1e6, sd(mc_nash_df$final_npv_A)/1e6,
            mean(mc_nash_df$final_npv_B)/1e6, sd(mc_nash_df$final_npv_B)/1e6))

cat("\nTfT vs TfT:\n")
cat(sprintf("  A: %.2fM (SD:%.2fM) | B: %.2fM (SD:%.2fM)\n",
            results$mc_tft$summary_stats$npv_A$mean/1e6, results$mc_tft$summary_stats$npv_A$sd/1e6,
            results$mc_tft$summary_stats$npv_B$mean/1e6, results$mc_tft$summary_stats$npv_B$sd/1e6))

coop_total <- mean(mc_coop_df$final_npv_A) + mean(mc_coop_df$final_npv_B)
nash_total <- mean(mc_nash_df$final_npv_A) + mean(mc_nash_df$final_npv_B)
cat(sprintf("\nIsbirligi Avantaji: %.1f%%\n", (coop_total - nash_total) / nash_total * 100))

cat("\nMemnuniyet Ortalamasi (S3 vs S3):\n")
cat(sprintf("  A: %.1f | B: %.1f\n",
            mean(mc_coop_df$avg_satisfaction_A), mean(mc_coop_df$avg_satisfaction_B)))

cat("\n=====================================\n")
cat("TABLO 7: DUYARLILIK OZETI\n")
cat("=====================================\n\n")

cat("Iskonto Orani:\n")
for (k in 1:nrow(sens_discount_df)) {
  r <- sens_discount_df[k, ]
  cat(sprintf("  r=%.3f (yillik %%%.1f): A=%.2fM, B=%.2fM\n",
              r$discount_rate, r$annual_rate*100, r$mean_npv_A/1e6, r$mean_npv_B/1e6))
}

cat("\nSok Olasiligi:\n")
for (k in 1:nrow(sens_shock_df)) {
  r <- sens_shock_df[k, ]
  cat(sprintf("  p=%.2f: A=%.2fM, B=%.2fM\n",
              r$shock_probability, r$mean_npv_A/1e6, r$mean_npv_B/1e6))
}

cat("\nBaz Talep:\n")
for (k in 1:nrow(sens_demand_df)) {
  r <- sens_demand_df[k, ]
  cat(sprintf("  D=%d: A=%.2fM, B=%.2fM\n",
              r$base_demand, r$mean_npv_A/1e6, r$mean_npv_B/1e6))
}

cat("\nTukenmislik Esigi:\n")
for (k in 1:nrow(sens_burnout_df)) {
  r <- sens_burnout_df[k, ]
  cat(sprintf("  esik=%.2f: A=%.2fM, B=%.2fM, memn_A=%.1f, memn_B=%.1f\n",
              r$burnout_threshold, r$mean_npv_A/1e6, r$mean_npv_B/1e6,
              r$mean_satisfaction_A, r$mean_satisfaction_B))
}


cat("\n=====================================\n")
cat("GRAFIKLER OLUSTURULUYOR\n")
cat("=====================================\n\n")

theme_sym <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "bottom")

renk_A <- "#2E86AB"
renk_B <- "#A23B72"

p1 <- ggplot(payoff_df, aes(x = factor(Strategy_B), y = factor(Strategy_A),
                            fill = Total_NPV/1e6)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(round(NPV_A/1e6, 1), "/", round(NPV_B/1e6, 1))),
            size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850",
                       midpoint = median(payoff_df$Total_NPV/1e6),
                       name = "Toplam NBD\n(Milyon TL)") +
  scale_x_discrete(labels = c("S1\nYikici", "S2\nAgresif", "S3\nIsbirlikci", "S4\nPremium")) +
  scale_y_discrete(labels = c("S1\nYikici", "S2\nAgresif", "S3\nIsbirlikci", "S4\nPremium")) +
  labs(title = "Getiri Matrisi (A / B, Milyon TL)", x = "Merkez B", y = "Merkez A") +
  theme_sym + theme(legend.position = "right", axis.text = element_text(size = 10))

ggsave("plots/Figure1_Payoff_Matrix.png", p1, width = 10, height = 8, dpi = 300)
cat("Figure1 kaydedildi.\n")

p2 <- ggplot(tournament_df, aes(x = reorder(strategy, -average_npv), y = average_npv/1e6)) +
  geom_bar(stat = "identity", fill = renk_A, alpha = 0.8) +
  geom_text(aes(label = sprintf("#%d", rank)), vjust = -0.5, size = 4, fontface = "bold") +
  labs(title = "Turnuva Siralamai", x = "Strateji", y = "Ortalama NBD (Milyon TL)") +
  theme_sym + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave("plots/Figure2_Tournament.png", p2, width = 12, height = 7, dpi = 300)
cat("Figure2 kaydedildi.\n")

mc_long <- data.frame(
  NPV = c(mc_coop_df$final_npv_A/1e6, mc_coop_df$final_npv_B/1e6,
          mc_nash_df$final_npv_A/1e6, mc_nash_df$final_npv_B/1e6),
  Center = rep(c("Merkez A", "Merkez B", "Merkez A", "Merkez B"),
               each = nrow(mc_coop_df)),
  Scenario = rep(c("Isbirligi (S3-S3)", "Isbirligi (S3-S3)",
                   "Nash (S2-S2)", "Nash (S2-S2)"), each = nrow(mc_coop_df)))

p3 <- ggplot(mc_long, aes(x = NPV, fill = Center)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40) +
  facet_wrap(~Scenario, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Merkez A" = renk_A, "Merkez B" = renk_B)) +
  labs(title = "Monte Carlo NBD Dagilimi",
       subtitle = sprintf("n = %d simulasyon", nrow(mc_coop_df)),
       x = "NBD (Milyon TL)", y = "Frekans") +
  theme_sym + theme(strip.text = element_text(size = 11, face = "bold"))

ggsave("plots/Figure3_Monte_Carlo.png", p3, width = 10, height = 8, dpi = 300)
cat("Figure3 kaydedildi.\n")

npv_evol <- data.frame(
  month = rep(sample_sim_df$month, 2),
  NPV = c(sample_sim_df$npv_A/1e6, sample_sim_df$npv_B/1e6),
  Center = rep(c("Merkez A", "Merkez B"), each = nrow(sample_sim_df)))

p4 <- ggplot(npv_evol, aes(x = month, y = NPV, color = Center)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Merkez A" = renk_A, "Merkez B" = renk_B)) +
  labs(title = "NBD Evrimi", x = "Ay", y = "Kumulatif NBD (Milyon TL)") +
  theme_sym

ggsave("plots/Figure4_NPV_Evolution.png", p4, width = 10, height = 6, dpi = 300)
cat("Figure4 kaydedildi.\n")

market_evol <- data.frame(
  month = rep(sample_sim_df$month, 2),
  Share = c(sample_sim_df$market_share_A * 100, sample_sim_df$market_share_B * 100),
  Center = rep(c("Merkez A", "Merkez B"), each = nrow(sample_sim_df)))

p5 <- ggplot(market_evol, aes(x = month, y = Share, fill = Center)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = c("Merkez A" = renk_A, "Merkez B" = renk_B)) +
  labs(title = "Pazar Payi Evrimi", x = "Ay", y = "Pazar Payi (%)") +
  theme_sym

ggsave("plots/Figure5_Market_Share.png", p5, width = 10, height = 6, dpi = 300)
cat("Figure5 kaydedildi.\n")

sens_shock_long <- sens_shock_df %>%
  select(shock_probability, mean_npv_A, mean_npv_B) %>%
  pivot_longer(cols = c(mean_npv_A, mean_npv_B), names_to = "Center", values_to = "NPV") %>%
  mutate(Center = ifelse(Center == "mean_npv_A", "Merkez A", "Merkez B"))

p6 <- ggplot(sens_shock_long, aes(x = shock_probability * 100, y = NPV/1e6, color = Center)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  scale_color_manual(values = c("Merkez A" = renk_A, "Merkez B" = renk_B)) +
  labs(title = "Duyarlilik - Sok Olasiligi", x = "Aylik Sok Olasiligi (%)", y = "Ortalama NBD (Milyon TL)") +
  theme_sym

ggsave("plots/Figure6_Sensitivity_Shock.png", p6, width = 10, height = 6, dpi = 300)
cat("Figure6 kaydedildi.\n")

sens_demand_long <- sens_demand_df %>%
  select(base_demand, mean_npv_A, mean_npv_B) %>%
  pivot_longer(cols = c(mean_npv_A, mean_npv_B), names_to = "Center", values_to = "NPV") %>%
  mutate(Center = ifelse(Center == "mean_npv_A", "Merkez A", "Merkez B"))

p7 <- ggplot(sens_demand_long, aes(x = base_demand, y = NPV/1e6, color = Center)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  scale_color_manual(values = c("Merkez A" = renk_A, "Merkez B" = renk_B)) +
  labs(title = "Duyarlilik - Baz Talep", x = "Aylik Baz Talep", y = "Ortalama NBD (Milyon TL)") +
  theme_sym

ggsave("plots/Figure7_Sensitivity_Demand.png", p7, width = 10, height = 6, dpi = 300)
cat("Figure7 kaydedildi.\n")

satisfaction_evol <- data.frame(
  month = rep(sample_sim_df$month, 2),
  Satisfaction = c(sample_sim_df$satisfaction_A, sample_sim_df$satisfaction_B),
  Center = rep(c("Merkez A", "Merkez B"), each = nrow(sample_sim_df)))

p8 <- ggplot(satisfaction_evol, aes(x = month, y = Satisfaction, color = Center)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_color_manual(values = c("Merkez A" = renk_A, "Merkez B" = renk_B)) +
  annotate("text", x = 5, y = 52, label = "Kritik Esik", color = "red", size = 3.5) +
  labs(title = "Calisan Memnuniyeti Evrimi", x = "Ay", y = "Memnuniyet (0-100)") +
  theme_sym

ggsave("plots/Figure8_Satisfaction.png", p8, width = 10, height = 6, dpi = 300)
cat("Figure8 kaydedildi.\n")

sens_burnout_long <- sens_burnout_df %>%
  select(burnout_threshold, mean_satisfaction_A, mean_satisfaction_B) %>%
  pivot_longer(cols = c(mean_satisfaction_A, mean_satisfaction_B),
               names_to = "Center", values_to = "Satisfaction") %>%
  mutate(Center = ifelse(Center == "mean_satisfaction_A", "Merkez A", "Merkez B"))

p9 <- ggplot(sens_burnout_long, aes(x = burnout_threshold * 100, y = Satisfaction, color = Center)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  scale_color_manual(values = c("Merkez A" = renk_A, "Merkez B" = renk_B)) +
  labs(title = "Duyarlilik - Tukenmislik Esigi", x = "Tukenmislik Esigi (%)", y = "Ortalama Memnuniyet") +
  theme_sym

ggsave("plots/Figure9_Sensitivity_Burnout.png", p9, width = 10, height = 6, dpi = 300)
cat("Figure9 kaydedildi.\n")

demand_decomp <- data.frame(
  month = rep(sample_sim_df$month, 3),
  Clients = c(sample_sim_df$demand_new_A, sample_sim_df$demand_loyal_A, sample_sim_df$demand_referral_A),
  Type = rep(c("Yeni Danisan", "Sadik Danisan", "Referans Danisan"), each = nrow(sample_sim_df)))

p10 <- ggplot(demand_decomp, aes(x = month, y = Clients, fill = Type)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(values = c("Yeni Danisan" = "#2E86AB", "Sadik Danisan" = "#27AE60", "Referans Danisan" = "#F39C12")) +
  labs(title = "Talep Bilesenleri - Merkez A", x = "Ay", y = "Danisan Sayisi") +
  theme_sym

ggsave("plots/Figure10_Demand_Decomposition.png", p10, width = 10, height = 6, dpi = 300)
cat("Figure10 kaydedildi.\n")

cat("\n=====================================\n")
cat("INGILIZCE FIGURELER (MAKALE ICIN)\n")
cat("=====================================\n\n")

p1_en <- ggplot(payoff_df, aes(x = factor(Strategy_B), y = factor(Strategy_A),
                               fill = Total_NPV/1e6)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(round(NPV_A/1e6, 1), "/", round(NPV_B/1e6, 1))),
            size = 4, fontface = "bold") +
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850",
                       midpoint = median(payoff_df$Total_NPV/1e6),
                       name = "Total NPV\n(Million TL)") +
  scale_x_discrete(labels = c("S1\nDestructive", "S2\nAggressive", "S3\nCooperative", "S4\nPremium")) +
  scale_y_discrete(labels = c("S1\nDestructive", "S2\nAggressive", "S3\nCooperative", "S4\nPremium")) +
  labs(title = "Payoff Matrix (Center A NPV / Center B NPV)",
       subtitle = "36-Month Simulation Results",
       x = "Center B Strategy", y = "Center A Strategy") +
  theme_sym + theme(legend.position = "right", axis.text = element_text(size = 10))

ggsave("plots/Figure1_Payoff_Matrix_EN.png", p1_en, width = 10, height = 8, dpi = 600)
cat("Figure1_EN kaydedildi.\n")

tournament_en <- tournament_df
tournament_en$strategy_en <- tournament_en$strategy
tournament_en$strategy_en <- gsub("S1_Sabit", "S1_Fixed", tournament_en$strategy_en)
tournament_en$strategy_en <- gsub("S2_Sabit", "S2_Fixed", tournament_en$strategy_en)
tournament_en$strategy_en <- gsub("S3_Sabit", "S3_Fixed", tournament_en$strategy_en)
tournament_en$strategy_en <- gsub("S4_Sabit", "S4_Fixed", tournament_en$strategy_en)

p2_en <- ggplot(tournament_en, aes(x = reorder(strategy_en, -average_npv), y = average_npv/1e6)) +
  geom_bar(stat = "identity", fill = renk_A, alpha = 0.8) +
  geom_text(aes(label = sprintf("#%d", rank)), vjust = -0.5, size = 4, fontface = "bold") +
  labs(title = "Strategy Tournament Results",
       subtitle = "Average NPV Against All Opponents",
       x = "Strategy", y = "Average NPV (Million TL)") +
  theme_sym + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

ggsave("plots/Figure2_Tournament_EN.png", p2_en, width = 12, height = 7, dpi = 600)
cat("Figure2_EN kaydedildi.\n")

mc_long_en <- data.frame(
  NPV = c(mc_coop_df$final_npv_A/1e6, mc_coop_df$final_npv_B/1e6,
          mc_nash_df$final_npv_A/1e6, mc_nash_df$final_npv_B/1e6),
  Center = rep(c("Center A", "Center B", "Center A", "Center B"),
               each = nrow(mc_coop_df)),
  Scenario = rep(c("Cooperation (S3-S3)", "Cooperation (S3-S3)",
                   "Nash Equilibrium (S2-S2)", "Nash Equilibrium (S2-S2)"), each = nrow(mc_coop_df)))

p3_en <- ggplot(mc_long_en, aes(x = NPV, fill = Center)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 40) +
  facet_wrap(~Scenario, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("Center A" = renk_A, "Center B" = renk_B)) +
  labs(title = "Monte Carlo Simulation NPV Distribution",
       subtitle = sprintf("n = %d simulations", nrow(mc_coop_df)),
       x = "Net Present Value (Million TL)", y = "Frequency") +
  theme_sym + theme(strip.text = element_text(size = 11, face = "bold"))

ggsave("plots/Figure3_Monte_Carlo_EN.png", p3_en, width = 10, height = 8, dpi = 600)
cat("Figure3_EN kaydedildi.\n")

p4_en <- ggplot(npv_evol %>% mutate(Center = gsub("Merkez", "Center", Center)),
                aes(x = month, y = NPV, color = Center)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Center A" = renk_A, "Center B" = renk_B)) +
  labs(title = "NPV Evolution - Sample Simulation", x = "Month", y = "Cumulative NPV (Million TL)") +
  theme_sym

ggsave("plots/Figure4_NPV_Evolution_EN.png", p4_en, width = 10, height = 6, dpi = 600)
cat("Figure4_EN kaydedildi.\n")

p8_en <- ggplot(satisfaction_evol %>% mutate(Center = gsub("Merkez", "Center", Center)),
                aes(x = month, y = Satisfaction, color = Center)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_color_manual(values = c("Center A" = renk_A, "Center B" = renk_B)) +
  annotate("text", x = 5, y = 52, label = "Critical Threshold", color = "red", size = 3.5) +
  labs(title = "Employee Satisfaction Evolution", x = "Month", y = "Satisfaction Score (0-100)") +
  theme_sym

ggsave("plots/Figure8_Satisfaction_EN.png", p8_en, width = 10, height = 6, dpi = 600)
cat("Figure8_EN kaydedildi.\n")

demand_decomp_en <- data.frame(
  month = rep(sample_sim_df$month, 3),
  Clients = c(sample_sim_df$demand_new_A, sample_sim_df$demand_loyal_A, sample_sim_df$demand_referral_A),
  Type = rep(c("New Clients", "Loyal Clients", "Referral Clients"), each = nrow(sample_sim_df)))

p10_en <- ggplot(demand_decomp_en, aes(x = month, y = Clients, fill = Type)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(values = c("New Clients" = "#2E86AB", "Loyal Clients" = "#27AE60", "Referral Clients" = "#F39C12")) +
  labs(title = "Demand Decomposition - Center A", x = "Month", y = "Number of Clients") +
  theme_sym

ggsave("plots/Figure10_Demand_Decomposition_EN.png", p10_en, width = 10, height = 6, dpi = 600)
cat("Figure10_EN kaydedildi.\n")

cat("\n=====================================\n")
cat("LATEX TABLOLARI\n")
cat("=====================================\n\n")

cat("\\begin{table}[htbp]\n")
cat("\\centering\n")
cat("\\caption{36-Month Net Present Value Payoff Matrix (Million TL)}\n")
cat("\\label{tab:payoff_matrix}\n")
cat("\\begin{tabular}{lcccc}\n")
cat("\\hline\n")
cat("A $\\backslash$ B & S1 & S2 & S3 & S4 \\\\\n")
cat("\\hline\n")

sn <- c("S1", "S2", "S3", "S4")
for (i in 1:4) {
  row_vals <- sapply(1:4, function(j) {
    row <- payoff_df %>% filter(Strategy_A == i, Strategy_B == j)
    sprintf("%.1f / %.1f", row$NPV_A/1e6, row$NPV_B/1e6)
  })
  cat(sprintf("%s & %s \\\\\n", sn[i], paste(row_vals, collapse = " & ")))
}
cat("\\hline\n\\end{tabular}\n\\end{table}\n\n")

cat("\\begin{table}[htbp]\n")
cat("\\centering\n")
cat("\\caption{Prisoner's Dilemma Parameter Values}\n")
cat("\\label{tab:pd_params}\n")
cat("\\begin{tabular}{lcc}\n")
cat("\\hline\n")
cat("Parameter & Value (Million TL) & Strategy Pair \\\\\n")
cat("\\hline\n")
cat(sprintf("T (Temptation) & %.2f & (S2, S3) \\\\\n", T_val/1e6))
cat(sprintf("R (Reward) & %.2f & (S3, S3) \\\\\n", R_val/1e6))
cat(sprintf("P (Punishment) & %.2f & (S2, S2) \\\\\n", P_val/1e6))
cat(sprintf("S (Sucker) & %.2f & (S3, S2) \\\\\n", S_val/1e6))
cat("\\hline\n")
cat(sprintf("\\multicolumn{3}{l}{T > R > P > S: %s} \\\\\n",
            ifelse(is_pd, "Satisfied", "Not Satisfied")))
cat(sprintf("\\multicolumn{3}{l}{2R (%.2f) > T+S (%.2f): %s} \\\\\n",
            2*R_val/1e6, (T_val+S_val)/1e6,
            ifelse(2*R_val > T_val+S_val, "Satisfied", "Not Satisfied")))
cat("\\hline\n\\end{tabular}\n\\end{table}\n\n")

cat("\\begin{table}[htbp]\n")
cat("\\centering\n")
cat("\\caption{Monte Carlo Simulation Summary Statistics (Million TL)}\n")
cat("\\label{tab:mc_summary}\n")
cat("\\begin{tabular}{llcccc}\n")
cat("\\hline\n")
cat("Scenario & Center & Mean & SD & Min & Max \\\\\n")
cat("\\hline\n")
for (scenario_data in list(
  list(df = mc_coop_df, name = "Cooperation (S3-S3)"),
  list(df = mc_nash_df, name = "Nash (S2-S2)"))) {
  for (center in c("A", "B")) {
    col <- paste0("final_npv_", center)
    vals <- scenario_data$df[[col]]
    cat(sprintf("%s & %s & %.2f & %.2f & %.2f & %.2f \\\\\n",
                scenario_data$name, center,
                mean(vals)/1e6, sd(vals)/1e6, min(vals)/1e6, max(vals)/1e6))
  }
}
cat("\\hline\n\\end{tabular}\n\\end{table}\n\n")

cat("\\begin{table}[htbp]\n")
cat("\\centering\n")
cat("\\caption{Strategy Tournament Rankings}\n")
cat("\\label{tab:tournament}\n")
cat("\\begin{tabular}{clc}\n")
cat("\\hline\n")
cat("Rank & Strategy & Average NPV (Million TL) \\\\\n")
cat("\\hline\n")
for (k in 1:nrow(tournament_df)) {
  t <- tournament_df[k, ]
  cat(sprintf("%d & %s & %.2f \\\\\n", t$rank, t$strategy, t$average_npv/1e6))
}
cat("\\hline\n\\end{tabular}\n\\end{table}\n")

cat("\n==========================================\n")
cat("TUM ISLEMLER TAMAMLANDI!\n")
cat("==========================================\n")
cat(sprintf("Calisma dizini: %s\n", getwd()))
cat("CSV dosyalari: results/\n")
cat("Grafikler: plots/\n")
cat("R verisi: simulation_results.RData\n")