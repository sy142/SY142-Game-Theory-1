# Standalone tournament runner. The canonical published pipeline is RunAll.R
# (extended tournament, 200 sims/matchup, 96,800 total runs). Parameter block
# here is duplicated from TamSimulasyon.R - keep in sync.
rm(list = ls())
gc()

base_dir <- tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path), error = function(e) getwd())
data_dir   <- file.path(base_dir, "Turnuva Sonuclari", "Datalar")
output_dir <- file.path(base_dir, "Turnuva Sonuclari", "Sonuclar")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(file.path(output_dir, "figures"))) dir.create(file.path(output_dir, "figures"))
if (!dir.exists(file.path(output_dir, "tables")))  dir.create(file.path(output_dir, "tables"))


library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

set.seed(2026)

sim_params <- list(
  n_months = 36,
  base_demand = 130,
  shock_probability = 0.08,
  burnout_threshold = 0.78,
  
  center_A = list(
    name = "Merkez A",
    n_dietitians = 4,
    monthly_capacity = 115,
    fixed_cost = 45000,
    brand_bonus = 0.04,
    discount_rate_annual = 0.16,
    discount_rate_monthly = 0.0133,
    initial_cash = 150000,
    initial_satisfaction = 70,
    initial_reputation = 72
  ),
  
  center_B = list(
    name = "Merkez B",
    n_dietitians = 3,
    monthly_capacity = 88,
    fixed_cost = 32000,
    brand_bonus = 0.00,
    discount_rate_annual = 0.19,
    discount_rate_monthly = 0.0158,
    initial_cash = 100000,
    initial_satisfaction = 75,
    initial_reputation = 65
  ),
  
  price_levels = data.frame(
    level = 1:4,
    strategy = c("Destructive", "Aggressive", "Cooperative", "Premium"),
    package_price = c(3200, 5500, 7200, 10500),
    variable_cost = c(280, 500, 1200, 1100),
    contribution_margin = c(2920, 5000, 6000, 9400),
    attractiveness = c(1.60, 5.50, 1.00, 0.25)
  ),
  
  marketing_levels = data.frame(
    level = 0:3,
    strategy = c("None", "Basic", "Active", "Intensive"),
    monthly_cost = c(0, 15000, 35000, 70000),
    market_share_effect = c(0.00, 0.02, 0.05, 0.08)
  ),
  
  seasonal_factors = data.frame(
    month_in_year = 1:12,
    season_name = c("Yilbasi", "Yilbasi", "Bahar", "Bahar", "Bahar",
                    "Yaz", "Yaz", "Yaz", "Sonbahar", "Sonbahar",
                    "YilSonu", "YilSonu"),
    coefficient = c(1.35, 1.35, 1.25, 1.25, 1.25,
                    0.65, 0.65, 0.65, 1.15, 1.15,
                    0.80, 0.80)
  ),
  
  investments = data.frame(
    code = c("T", "E", "D", "C"),
    name = c("Technology", "Training", "Decoration", "Equipment"),
    cost = c(60000, 40000, 50000, 80000),
    duration_months = c(12, 12, 12, 12),
    capacity_effect = c(0.15, 0.00, 0.00, 0.00),
    satisfaction_effect = c(0, 10, 0, 0),
    referral_effect = c(0.00, 0.15, 0.00, 0.10),
    market_share_bonus_s4 = c(0.00, 0.00, 0.03, 0.02)
  ),
  
  shocks = data.frame(
    shock_id = 1:7,
    shock_type = c("ekonomik_kriz", "rakip_skandal", "viral_basari",
                   "sgk_destegi", "yildiz_transfer", "calisan_hastaligi",
                   "influencer_isbirligi"),
    duration_months = c(3, 1, 2, 2, 3, 1, 2),
    demand_effect = c(-0.25, 0.00, 0.00, 0.20, 0.00, 0.00, 0.00),
    own_market_share_effect = c(0.00, 0.00, 0.12, 0.00, 0.08, 0.00, 0.00),
    competitor_market_share_effect = c(0.00, -0.15, 0.00, 0.00, 0.00, 0.00, 0.00),
    capacity_effect = c(0.00, 0.00, 0.00, 0.00, 0.00, -0.20, 0.00),
    s4_bonus = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.05)
  ),
  
  menu_cost_1_level = 4000,
  menu_cost_2_plus_levels = 10000,
  
  satisfaction_decay_rate = 0.8,
  satisfaction_recovery_rate = 3.0,
  premium_stress = 1.0,
  reputation_gain_consistent = 0.5,
  reputation_loss_change = 1.0,
  
  referral_base_rate = 0.01,
  referral_high_satisfaction_threshold = 70,
  referral_high_multiplier = 1.15,
  referral_low_satisfaction_threshold = 30,
  referral_low_multiplier = 0.30,
  
  loyalty_base_rate = 0.02,
  loyalty_no_change = 1.00,
  loyalty_one_level_change = 0.38,
  loyalty_multi_level_change = 0.08,
  loyalty_satisfaction_multiplier = c(0.80, 0.92, 1.00, 1.05),
  
  capacity_satisfaction_high_threshold = 70,
  capacity_satisfaction_high_multiplier = 1.10,
  capacity_satisfaction_low_threshold = 20,
  capacity_satisfaction_low_multiplier = 0.90
)

get_seasonal_coefficient <- function(month, params) {
  month_in_year <- ((month - 1) %% 12) + 1
  params$seasonal_factors$coefficient[month_in_year]
}

get_season_name <- function(month, params) {
  month_in_year <- ((month - 1) %% 12) + 1
  params$seasonal_factors$season_name[month_in_year]
}

calculate_market_share <- function(price_A, price_B, marketing_A, marketing_B,
                                   reputation_A, reputation_B,
                                   active_investments_A, active_investments_B,
                                   active_shocks_A, active_shocks_B,
                                   params) {
  attract_A <- params$price_levels$attractiveness[price_A]
  attract_B <- params$price_levels$attractiveness[price_B]
  base_share_A <- attract_A / (attract_A + attract_B)
  base_share_B <- attract_B / (attract_A + attract_B)
  share_A <- base_share_A + params$center_A$brand_bonus
  share_B <- base_share_B + params$center_B$brand_bonus
  mkt_effect_A <- params$marketing_levels$market_share_effect[marketing_A + 1]
  mkt_effect_B <- params$marketing_levels$market_share_effect[marketing_B + 1]
  net_mkt_A <- mkt_effect_A - 0.40 * mkt_effect_B
  net_mkt_B <- mkt_effect_B - 0.40 * mkt_effect_A
  share_A <- share_A + net_mkt_A
  share_B <- share_B + net_mkt_B
  rep_diff <- (reputation_A - reputation_B) / 100
  share_A <- share_A + rep_diff * 0.05
  share_B <- share_B - rep_diff * 0.05
  if (length(active_investments_A) > 0) {
    for (inv in active_investments_A) {
      inv_data <- params$investments[params$investments$code == inv, ]
      if (nrow(inv_data) > 0 && price_A == 4) {
        share_A <- share_A + inv_data$market_share_bonus_s4
      }
    }
  }
  if (length(active_investments_B) > 0) {
    for (inv in active_investments_B) {
      inv_data <- params$investments[params$investments$code == inv, ]
      if (nrow(inv_data) > 0 && price_B == 4) {
        share_B <- share_B + inv_data$market_share_bonus_s4
      }
    }
  }
  if (length(active_shocks_A) > 0) {
    for (shock in active_shocks_A) {
      shock_data <- params$shocks[params$shocks$shock_type == shock, ]
      if (nrow(shock_data) > 0) {
        share_A <- share_A + shock_data$own_market_share_effect
        share_B <- share_B + shock_data$competitor_market_share_effect
        if (shock_data$s4_bonus > 0 && price_A == 4) {
          share_A <- share_A + shock_data$s4_bonus
        }
      }
    }
  }
  if (length(active_shocks_B) > 0) {
    for (shock in active_shocks_B) {
      shock_data <- params$shocks[params$shocks$shock_type == shock, ]
      if (nrow(shock_data) > 0) {
        share_B <- share_B + shock_data$own_market_share_effect
        share_A <- share_A + shock_data$competitor_market_share_effect
        if (shock_data$s4_bonus > 0 && price_B == 4) {
          share_B <- share_B + shock_data$s4_bonus
        }
      }
    }
  }
  share_A <- max(0.05, min(0.95, share_A))
  share_B <- max(0.05, min(0.95, share_B))
  total <- share_A + share_B
  share_A <- share_A / total
  share_B <- share_B / total
  return(list(share_A = share_A, share_B = share_B))
}

calculate_demand <- function(month, market_share, active_shocks,
                             previous_clients, previous_price, current_price,
                             satisfaction, active_investments, params) {
  seasonal_coef <- get_seasonal_coefficient(month, params)
  demand_multiplier <- 1.0
  if (length(active_shocks) > 0) {
    for (shock in active_shocks) {
      shock_data <- params$shocks[params$shocks$shock_type == shock, ]
      if (nrow(shock_data) > 0) {
        demand_multiplier <- demand_multiplier + shock_data$demand_effect
      }
    }
  }
  new_clients <- round(params$base_demand * seasonal_coef * demand_multiplier * market_share)
  
  loyal_clients <- 0
  referral_clients <- 0
  if (month > 1 && previous_clients > 0) {
    price_diff <- abs(current_price - previous_price)
    if (is.na(price_diff)) price_diff <- 0
    loyalty_factor <- ifelse(price_diff == 0, params$loyalty_no_change,
                             ifelse(price_diff == 1, params$loyalty_one_level_change,
                                    params$loyalty_multi_level_change))
    sat_mult <- params$loyalty_satisfaction_multiplier[current_price]
    loyal_clients <- round(previous_clients * params$loyalty_base_rate * loyalty_factor * sat_mult)
    
    ref_rate <- params$referral_base_rate
    if (length(active_investments) > 0) {
      for (inv in active_investments) {
        inv_data <- params$investments[params$investments$code == inv, ]
        if (nrow(inv_data) > 0) ref_rate <- ref_rate + inv_data$referral_effect
      }
    }
    ref_mult <- 1.0
    if (satisfaction >= params$referral_high_satisfaction_threshold) {
      ref_mult <- params$referral_high_multiplier
    } else if (satisfaction < params$referral_low_satisfaction_threshold) {
      ref_mult <- params$referral_low_multiplier
    }
    referral_clients <- round(previous_clients * ref_rate * ref_mult)
  }
  total_demand <- new_clients + loyal_clients + referral_clients
  return(list(total = total_demand, new = new_clients,
              loyal = loyal_clients, referral = referral_clients))
}

calculate_effective_capacity <- function(base_capacity, satisfaction,
                                         active_investments, active_shocks, params) {
  capacity <- base_capacity
  if (length(active_investments) > 0) {
    for (inv in active_investments) {
      inv_data <- params$investments[params$investments$code == inv, ]
      if (nrow(inv_data) > 0) capacity <- capacity * (1 + inv_data$capacity_effect)
    }
  }
  if (length(active_shocks) > 0) {
    for (shock in active_shocks) {
      shock_data <- params$shocks[params$shocks$shock_type == shock, ]
      if (nrow(shock_data) > 0 && shock_data$capacity_effect != 0) {
        capacity <- capacity * (1 + shock_data$capacity_effect)
      }
    }
  }
  if (satisfaction >= params$capacity_satisfaction_high_threshold) {
    capacity <- capacity * params$capacity_satisfaction_high_multiplier
  } else if (satisfaction < params$capacity_satisfaction_low_threshold) {
    capacity <- capacity * params$capacity_satisfaction_low_multiplier
  }
  return(round(capacity))
}

calculate_monthly_profit <- function(n_clients, price_level, marketing_level,
                                     fixed_cost, investment_cost, menu_cost, params) {
  price_data <- params$price_levels[price_level, ]
  marketing_data <- params$marketing_levels[marketing_level + 1, ]
  revenue <- n_clients * price_data$package_price
  variable_costs <- n_clients * price_data$variable_cost
  contribution <- n_clients * price_data$contribution_margin
  marketing_cost <- marketing_data$monthly_cost
  profit <- contribution - fixed_cost - marketing_cost - investment_cost - menu_cost
  return(list(revenue = revenue, variable_costs = variable_costs,
              contribution = contribution, marketing_cost = marketing_cost,
              fixed_cost = fixed_cost, investment_cost = investment_cost,
              menu_cost = menu_cost, profit = profit))
}

calculate_npv <- function(monthly_profits, discount_rate) {
  npv <- 0
  for (t in seq_along(monthly_profits)) {
    npv <- npv + monthly_profits[t] / ((1 + discount_rate)^t)
  }
  return(npv)
}

update_satisfaction <- function(current_satisfaction, capacity_utilization,
                                price_level, active_investments, params) {
  new_sat <- current_satisfaction
  if (capacity_utilization > params$burnout_threshold) {
    overtime <- (capacity_utilization - params$burnout_threshold) / (1 - params$burnout_threshold)
    loss <- params$satisfaction_decay_rate * overtime * 10
    new_sat <- new_sat - loss
  } else {
    new_sat <- new_sat + params$satisfaction_recovery_rate
  }
  if (price_level == 4) new_sat <- new_sat - params$premium_stress
  if (length(active_investments) > 0) {
    for (inv in active_investments) {
      inv_data <- params$investments[params$investments$code == inv, ]
      if (nrow(inv_data) > 0 && inv_data$satisfaction_effect > 0) {
        new_sat <- new_sat + inv_data$satisfaction_effect / 12
      }
    }
  }
  return(max(0, min(100, new_sat)))
}

update_reputation <- function(current_reputation, price_changed, satisfaction, n_clients) {
  new_rep <- current_reputation
  if (price_changed) { new_rep <- new_rep - 1.0 } else { new_rep <- new_rep + 0.5 }
  if (satisfaction > 70) { new_rep <- new_rep + 0.3 }
  else if (satisfaction < 50) { new_rep <- new_rep - 0.5 }
  return(max(0, min(100, new_rep)))
}

calculate_menu_cost <- function(current_price, previous_price, params) {
  if (is.na(previous_price) || current_price == previous_price) return(0)
  price_diff <- abs(current_price - previous_price)
  if (price_diff == 1) return(params$menu_cost_1_level)
  return(params$menu_cost_2_plus_levels)
}

generate_shocks <- function(n_months, shock_probability, params) {
  shock_schedule <- vector('list', n_months)
  for (m in 1:n_months) shock_schedule[[m]] <- list(A = character(0), B = character(0))
  for (m in 1:n_months) {
    if (runif(1) < shock_probability) {
      shock_idx <- sample(1:nrow(params$shocks), 1)
      shock_type <- params$shocks$shock_type[shock_idx]
      duration <- params$shocks$duration_months[shock_idx]
      target <- sample(c("A", "B"), 1)
      for (d in 0:(duration - 1)) {
        affected_month <- m + d
        if (affected_month <= n_months) {
          if (target == "A") {
            shock_schedule[[affected_month]]$A <- c(shock_schedule[[affected_month]]$A, shock_type)
          } else {
            shock_schedule[[affected_month]]$B <- c(shock_schedule[[affected_month]]$B, shock_type)
          }
        }
      }
    }
  }
  return(shock_schedule)
}

evaluate_condition <- function(condition, state) {
  if (is.na(condition) || condition == "" || is.null(condition)) return(FALSE)
  condition <- as.character(condition)
  condition <- trimws(condition)
  if (nchar(condition) == 0) return(FALSE)
  
  allowed_vars <- c("competitor_last", "my_last", "my_npv", "competitor_npv",
                    "capacity_util", "calisan_memnuniyeti", "itibar",
                    "market_share", "cash", "season", "month")
  allowed_ops <- c("==", "!=", "<=", ">=", "<", ">", "AND", "OR", "&", "|",
                   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ".", " ", "-")
  test_str <- condition
  for (v in allowed_vars) test_str <- gsub(v, '', test_str)
  for (op in allowed_ops) test_str <- gsub(fixed = TRUE, op, '', test_str)
  if (nchar(trimws(test_str)) > 0) return(FALSE)
  
  condition <- gsub("market_share", as.character(round(state$market_share, 4)), condition)
  condition <- gsub("competitor_last", as.character(state$competitor_last), condition)
  condition <- gsub("competitor_npv", as.character(round(state$competitor_npv, 0)), condition)
  condition <- gsub("my_last", as.character(state$my_last), condition)
  condition <- gsub("my_npv", as.character(round(state$my_npv, 0)), condition)
  condition <- gsub("capacity_util", as.character(round(state$capacity_util, 4)), condition)
  condition <- gsub("calisan_memnuniyeti", as.character(round(state$calisan_memnuniyeti, 1)), condition)
  condition <- gsub("itibar", as.character(round(state$itibar, 1)), condition)
  condition <- gsub("cash", as.character(round(state$cash, 0)), condition)
  condition <- gsub("season", as.character(state$season), condition)
  condition <- gsub("month", as.character(state$month), condition)
  condition <- gsub(" AND ", " & ", condition, ignore.case = TRUE)
  condition <- gsub(" OR ", " | ", condition, ignore.case = TRUE)
  result <- tryCatch({ eval(parse(text = condition)) }, error = function(e) FALSE)
  return(isTRUE(result))
}

get_strategy_decision <- function(month, strategy_df, state) {
  row <- strategy_df[strategy_df$Ay == month, ]
  if (nrow(row) == 0) return(list(price = 3, marketing = 1))
  price <- row$Fiyat
  marketing <- row$Pazarlama
  if (!is.na(row$Kosul_1) && row$Kosul_1 != "") {
    if (evaluate_condition(row$Kosul_1, state)) {
      if (!is.na(row$Fiyat_1)) price <- row$Fiyat_1
      if (!is.na(row$Paz_1)) marketing <- row$Paz_1
      return(list(price = price, marketing = marketing))
    }
  }
  if (!is.na(row$Kosul_2) && row$Kosul_2 != "") {
    if (evaluate_condition(row$Kosul_2, state)) {
      if (!is.na(row$Fiyat_2)) price <- row$Fiyat_2
      if (!is.na(row$Paz_2)) marketing <- row$Paz_2
      return(list(price = price, marketing = marketing))
    }
  }
  return(list(price = price, marketing = marketing))
}

get_shock_response <- function(shock_type, shock_strategies) {
  if (is.null(shock_strategies) || nrow(shock_strategies) == 0) return(NULL)
  row <- shock_strategies[shock_strategies$sok_tipi == shock_type, ]
  if (nrow(row) == 0) return(NULL)
  if (!is.na(row$fiyat) && !is.na(row$pazarlama)) {
    return(list(price = row$fiyat, marketing = row$pazarlama))
  }
  return(NULL)
}

get_investment_decision <- function(month, investment_schedule) {
  if (is.null(investment_schedule)) return(NULL)
  row <- investment_schedule[investment_schedule$ay == month, ]
  if (nrow(row) == 0 || is.na(row$yatirim)) return(NULL)
  return(as.character(row$yatirim))
}
run_single_simulation <- function(strategy_A, strategy_B,
                                  shock_strategies_A = NULL, shock_strategies_B = NULL,
                                  investment_schedule_A = NULL, investment_schedule_B = NULL,
                                  shock_schedule = NULL, params = sim_params) {
  n_months <- params$n_months
  if (is.null(shock_schedule)) {
    shock_schedule <- generate_shocks(n_months, params$shock_probability, params)
  }
  results <- data.frame(
    month = 1:n_months, season = character(n_months), seasonal_coef = numeric(n_months),
    price_A = integer(n_months), price_B = integer(n_months),
    marketing_A = integer(n_months), marketing_B = integer(n_months),
    market_share_A = numeric(n_months), market_share_B = numeric(n_months),
    demand_A = integer(n_months), demand_B = integer(n_months),
    demand_new_A = integer(n_months), demand_new_B = integer(n_months),
    demand_loyal_A = integer(n_months), demand_loyal_B = integer(n_months),
    demand_referral_A = integer(n_months), demand_referral_B = integer(n_months),
    capacity_A = integer(n_months), capacity_B = integer(n_months),
    clients_A = integer(n_months), clients_B = integer(n_months),
    capacity_util_A = numeric(n_months), capacity_util_B = numeric(n_months),
    revenue_A = numeric(n_months), revenue_B = numeric(n_months),
    variable_cost_A = numeric(n_months), variable_cost_B = numeric(n_months),
    contribution_A = numeric(n_months), contribution_B = numeric(n_months),
    marketing_cost_A = numeric(n_months), marketing_cost_B = numeric(n_months),
    fixed_cost_A = numeric(n_months), fixed_cost_B = numeric(n_months),
    investment_cost_A = numeric(n_months), investment_cost_B = numeric(n_months),
    menu_cost_A = numeric(n_months), menu_cost_B = numeric(n_months),
    profit_A = numeric(n_months), profit_B = numeric(n_months),
    cumulative_profit_A = numeric(n_months), cumulative_profit_B = numeric(n_months),
    npv_A = numeric(n_months), npv_B = numeric(n_months),
    satisfaction_A = numeric(n_months), satisfaction_B = numeric(n_months),
    reputation_A = numeric(n_months), reputation_B = numeric(n_months),
    cash_A = numeric(n_months), cash_B = numeric(n_months),
    active_shocks_A = character(n_months), active_shocks_B = character(n_months),
    active_investments_A = character(n_months), active_investments_B = character(n_months),
    stringsAsFactors = FALSE
  )
  
  state_A <- list(satisfaction = params$center_A$initial_satisfaction,
                  reputation = params$center_A$initial_reputation,
                  cash = params$center_A$initial_cash,
                  previous_price = NA, previous_clients = 0,
                  cumulative_profit = 0, npv = 0, active_investments = list())
  state_B <- list(satisfaction = params$center_B$initial_satisfaction,
                  reputation = params$center_B$initial_reputation,
                  cash = params$center_B$initial_cash,
                  previous_price = NA, previous_clients = 0,
                  cumulative_profit = 0, npv = 0, active_investments = list())
  
  for (m in 1:n_months) {
    results$season[m] <- get_season_name(m, params)
    results$seasonal_coef[m] <- get_seasonal_coefficient(m, params)
    current_shocks_A <- shock_schedule[[m]]$A
    current_shocks_B <- shock_schedule[[m]]$B
    results$active_shocks_A[m] <- paste(current_shocks_A, collapse = ";")
    results$active_shocks_B[m] <- paste(current_shocks_B, collapse = ";")
    
    active_inv_A <- names(state_A$active_investments)[
      sapply(state_A$active_investments, function(x) x >= m)]
    active_inv_B <- names(state_B$active_investments)[
      sapply(state_B$active_investments, function(x) x >= m)]
    results$active_investments_A[m] <- paste(active_inv_A, collapse = ";")
    results$active_investments_B[m] <- paste(active_inv_B, collapse = ";")
    
    inv_decision_A <- get_investment_decision(m, investment_schedule_A)
    inv_decision_B <- get_investment_decision(m, investment_schedule_B)
    inv_cost_A <- 0
    inv_cost_B <- 0
    if (!is.null(inv_decision_A)) {
      inv_data <- params$investments[params$investments$code == inv_decision_A, ]
      if (nrow(inv_data) > 0 && state_A$cash >= inv_data$cost) {
        inv_cost_A <- inv_data$cost
        state_A$active_investments[[inv_decision_A]] <- m + inv_data$duration_months - 1
        active_inv_A <- c(active_inv_A, inv_decision_A)
      }
    }
    if (!is.null(inv_decision_B)) {
      inv_data <- params$investments[params$investments$code == inv_decision_B, ]
      if (nrow(inv_data) > 0 && state_B$cash >= inv_data$cost) {
        inv_cost_B <- inv_data$cost
        state_B$active_investments[[inv_decision_B]] <- m + inv_data$duration_months - 1
        active_inv_B <- c(active_inv_B, inv_decision_B)
      }
    }
    
    prev_share_A <- ifelse(m > 1, results$market_share_A[m-1], 0.54)
    prev_share_B <- ifelse(m > 1, results$market_share_B[m-1], 0.46)
    game_state_A <- list(month = m, season = results$seasonal_coef[m],
                         competitor_last = ifelse(m > 1, results$price_B[m-1], 3),
                         my_last = ifelse(m > 1, results$price_A[m-1], 3),
                         my_npv = state_A$npv, competitor_npv = state_B$npv,
                         capacity_util = ifelse(m > 1, results$capacity_util_A[m-1], 0.5),
                         calisan_memnuniyeti = state_A$satisfaction, itibar = state_A$reputation,
                         market_share = prev_share_A, cash = state_A$cash)
    game_state_B <- list(month = m, season = results$seasonal_coef[m],
                         competitor_last = ifelse(m > 1, results$price_A[m-1], 3),
                         my_last = ifelse(m > 1, results$price_B[m-1], 3),
                         my_npv = state_B$npv, competitor_npv = state_A$npv,
                         capacity_util = ifelse(m > 1, results$capacity_util_B[m-1], 0.5),
                         calisan_memnuniyeti = state_B$satisfaction, itibar = state_B$reputation,
                         market_share = prev_share_B, cash = state_B$cash)
    
    decision_A <- get_strategy_decision(m, strategy_A, game_state_A)
    decision_B <- get_strategy_decision(m, strategy_B, game_state_B)
    
    if (length(current_shocks_A) > 0) {
      for (shock in current_shocks_A) {
        shock_response <- get_shock_response(shock, shock_strategies_A)
        if (!is.null(shock_response)) { decision_A <- shock_response; break }
      }
    }
    if (length(current_shocks_B) > 0) {
      for (shock in current_shocks_B) {
        shock_response <- get_shock_response(shock, shock_strategies_B)
        if (!is.null(shock_response)) { decision_B <- shock_response; break }
      }
    }
    
    results$price_A[m] <- decision_A$price
    results$price_B[m] <- decision_B$price
    results$marketing_A[m] <- decision_A$marketing
    results$marketing_B[m] <- decision_B$marketing
    
    menu_cost_A <- calculate_menu_cost(decision_A$price, state_A$previous_price, params)
    menu_cost_B <- calculate_menu_cost(decision_B$price, state_B$previous_price, params)
    
    market_shares <- calculate_market_share(
      decision_A$price, decision_B$price, decision_A$marketing, decision_B$marketing,
      state_A$reputation, state_B$reputation, active_inv_A, active_inv_B,
      current_shocks_A, current_shocks_B, params)
    results$market_share_A[m] <- market_shares$share_A
    results$market_share_B[m] <- market_shares$share_B
    
    demand_result_A <- calculate_demand(m, market_shares$share_A, current_shocks_A,
                                        state_A$previous_clients, state_A$previous_price, decision_A$price,
                                        state_A$satisfaction, active_inv_A, params)
    demand_result_B <- calculate_demand(m, market_shares$share_B, current_shocks_B,
                                        state_B$previous_clients, state_B$previous_price, decision_B$price,
                                        state_B$satisfaction, active_inv_B, params)
    
    capacity_A <- calculate_effective_capacity(params$center_A$monthly_capacity,
                                               state_A$satisfaction, active_inv_A, current_shocks_A, params)
    capacity_B <- calculate_effective_capacity(params$center_B$monthly_capacity,
                                               state_B$satisfaction, active_inv_B, current_shocks_B, params)
    
    clients_A <- min(demand_result_A$total, capacity_A)
    clients_B <- min(demand_result_B$total, capacity_B)
    
    results$demand_A[m] <- demand_result_A$total
    results$demand_B[m] <- demand_result_B$total
    results$demand_new_A[m] <- demand_result_A$new
    results$demand_new_B[m] <- demand_result_B$new
    results$demand_loyal_A[m] <- demand_result_A$loyal
    results$demand_loyal_B[m] <- demand_result_B$loyal
    results$demand_referral_A[m] <- demand_result_A$referral
    results$demand_referral_B[m] <- demand_result_B$referral
    results$capacity_A[m] <- capacity_A
    results$capacity_B[m] <- capacity_B
    results$clients_A[m] <- clients_A
    results$clients_B[m] <- clients_B
    results$capacity_util_A[m] <- clients_A / capacity_A
    results$capacity_util_B[m] <- clients_B / capacity_B
    
    fin_A <- calculate_monthly_profit(clients_A, decision_A$price, decision_A$marketing,
                                      params$center_A$fixed_cost, inv_cost_A, menu_cost_A, params)
    fin_B <- calculate_monthly_profit(clients_B, decision_B$price, decision_B$marketing,
                                      params$center_B$fixed_cost, inv_cost_B, menu_cost_B, params)
    
    results$revenue_A[m] <- fin_A$revenue; results$revenue_B[m] <- fin_B$revenue
    results$variable_cost_A[m] <- fin_A$variable_costs; results$variable_cost_B[m] <- fin_B$variable_costs
    results$contribution_A[m] <- fin_A$contribution; results$contribution_B[m] <- fin_B$contribution
    results$marketing_cost_A[m] <- fin_A$marketing_cost; results$marketing_cost_B[m] <- fin_B$marketing_cost
    results$fixed_cost_A[m] <- fin_A$fixed_cost; results$fixed_cost_B[m] <- fin_B$fixed_cost
    results$investment_cost_A[m] <- fin_A$investment_cost; results$investment_cost_B[m] <- fin_B$investment_cost
    results$menu_cost_A[m] <- fin_A$menu_cost; results$menu_cost_B[m] <- fin_B$menu_cost
    results$profit_A[m] <- fin_A$profit; results$profit_B[m] <- fin_B$profit
    
    state_A$cumulative_profit <- state_A$cumulative_profit + fin_A$profit
    state_B$cumulative_profit <- state_B$cumulative_profit + fin_B$profit
    state_A$npv <- calculate_npv(results$profit_A[1:m], params$center_A$discount_rate_monthly)
    state_B$npv <- calculate_npv(results$profit_B[1:m], params$center_B$discount_rate_monthly)
    results$cumulative_profit_A[m] <- state_A$cumulative_profit
    results$cumulative_profit_B[m] <- state_B$cumulative_profit
    results$npv_A[m] <- state_A$npv; results$npv_B[m] <- state_B$npv
    
    state_A$satisfaction <- update_satisfaction(state_A$satisfaction,
                                                results$capacity_util_A[m], decision_A$price, active_inv_A, params)
    state_B$satisfaction <- update_satisfaction(state_B$satisfaction,
                                                results$capacity_util_B[m], decision_B$price, active_inv_B, params)
    
    price_changed_A <- !is.na(state_A$previous_price) && decision_A$price != state_A$previous_price
    price_changed_B <- !is.na(state_B$previous_price) && decision_B$price != state_B$previous_price
    state_A$reputation <- update_reputation(state_A$reputation, price_changed_A, state_A$satisfaction, clients_A)
    state_B$reputation <- update_reputation(state_B$reputation, price_changed_B, state_B$satisfaction, clients_B)
    
    results$satisfaction_A[m] <- state_A$satisfaction
    results$satisfaction_B[m] <- state_B$satisfaction
    results$reputation_A[m] <- state_A$reputation
    results$reputation_B[m] <- state_B$reputation
    
    state_A$cash <- state_A$cash + fin_A$profit
    state_B$cash <- state_B$cash + fin_B$profit
    results$cash_A[m] <- state_A$cash; results$cash_B[m] <- state_B$cash
    
    state_A$previous_price <- decision_A$price; state_B$previous_price <- decision_B$price
    state_A$previous_clients <- clients_A; state_B$previous_clients <- clients_B
  }
  return(results)
}


N_SIMS_PER_MATCH <- 200
VALID_PRICES     <- 1:4
VALID_MARKETING  <- 0:3
VALID_SHOCKS     <- c("ekonomik_kriz", "rakip_skandal", "viral_basari",
                      "sgk_destegi", "yildiz_transfer", "calisan_hastaligi",
                      "influencer_isbirligi")
VALID_INVESTMENTS <- c("T", "E", "D", "C")
REQUIRED_SHEETS   <- c("Merkez_A", "Merkez_B")
OPTIONAL_SHEETS_A <- "Ek_Durumlar_A"
OPTIONAL_SHEETS_B <- "Ek_Durumlar_B"
OPTIONAL_SHEET_LEGACY <- "Ek_Durumlar"
REQUIRED_COLS     <- c("Ay", "Fiyat", "Pazarlama")
OPTIONAL_COLS     <- c("Kosul_1", "Fiyat_1", "Paz_1", "Kosul_2", "Fiyat_2", "Paz_2")


parse_filename <- function(filepath) {
  fname <- tools::file_path_sans_extension(basename(filepath))
  parts <- strsplit(fname, "_")[[1]]
  if (length(parts) >= 4) {
    return(list(
      id = parts[1],
      first_name = parts[2],
      last_name = parts[3],
      university = paste(parts[4:length(parts)], collapse = "_"),
      label = paste(parts[2], parts[3], sep = " "),
      short = paste0(parts[2], " ", substr(parts[3], 1, 1), ".")
    ))
  } else {
    return(list(
      id = fname, first_name = fname, last_name = "",
      university = "Unknown", label = fname, short = fname
    ))
  }
}


validate_strategy_sheet <- function(df, sheet_name, issues) {
  if (is.null(df) || nrow(df) == 0) {
    issues <- c(issues, paste0(sheet_name, ": Empty or unreadable sheet"))
    return(list(valid = FALSE, issues = issues, df = NULL))
  }
  
  names(df) <- trimws(names(df))
  
  missing_cols <- setdiff(REQUIRED_COLS, names(df))
  if (length(missing_cols) > 0) {
    issues <- c(issues, paste0(sheet_name, ": Missing columns: ",
                               paste(missing_cols, collapse = ", ")))
    return(list(valid = FALSE, issues = issues, df = NULL))
  }
  
  for (col in OPTIONAL_COLS) {
    if (!(col %in% names(df))) df[[col]] <- NA
  }
  
  df$Ay <- suppressWarnings(as.integer(df$Ay))
  df$Fiyat <- suppressWarnings(as.integer(df$Fiyat))
  df$Pazarlama <- suppressWarnings(as.integer(df$Pazarlama))
  df$Fiyat_1 <- suppressWarnings(as.integer(df$Fiyat_1))
  df$Paz_1 <- suppressWarnings(as.integer(df$Paz_1))
  df$Fiyat_2 <- suppressWarnings(as.integer(df$Fiyat_2))
  df$Paz_2 <- suppressWarnings(as.integer(df$Paz_2))
  
  for (col in c("Kosul_1", "Kosul_2")) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
      df[[col]][is.na(df[[col]])] <- ""
      df[[col]] <- trimws(df[[col]])
    }
  }
  
  if (nrow(df) < 36) {
    issues <- c(issues, paste0(sheet_name, ": Only ", nrow(df),
                               " rows (36 required). Padding with last row values."))
    last_row <- df[nrow(df), ]
    while (nrow(df) < 36) {
      new_row <- last_row
      new_row$Ay <- nrow(df) + 1
      df <- rbind(df, new_row)
    }
  }
  if (nrow(df) > 36) {
    issues <- c(issues, paste0(sheet_name, ": ", nrow(df),
                               " rows found. Truncating to 36."))
    df <- df[1:36, ]
  }
  
  df$Ay <- 1:36
  
  na_fiyat <- which(is.na(df$Fiyat))
  if (length(na_fiyat) > 0) {
    issues <- c(issues, paste0(sheet_name, ": NA in Fiyat at months: ",
                               paste(na_fiyat, collapse = ", "), ". Defaulting to 3."))
    df$Fiyat[na_fiyat] <- 3
  }
  
  na_paz <- which(is.na(df$Pazarlama))
  if (length(na_paz) > 0) {
    issues <- c(issues, paste0(sheet_name, ": NA in Pazarlama at months: ",
                               paste(na_paz, collapse = ", "), ". Defaulting to 1."))
    df$Pazarlama[na_paz] <- 1
  }
  
  bad_price <- which(!(df$Fiyat %in% VALID_PRICES))
  if (length(bad_price) > 0) {
    issues <- c(issues, paste0(sheet_name, ": Invalid Fiyat values at months: ",
                               paste(bad_price, collapse = ", "),
                               " (values: ", paste(df$Fiyat[bad_price], collapse = ", "),
                               "). Clamping to [1,4]."))
    df$Fiyat[bad_price] <- pmin(4, pmax(1, df$Fiyat[bad_price]))
    df$Fiyat[is.na(df$Fiyat)] <- 3
  }
  
  bad_mkt <- which(!(df$Pazarlama %in% VALID_MARKETING))
  if (length(bad_mkt) > 0) {
    issues <- c(issues, paste0(sheet_name, ": Invalid Pazarlama values at months: ",
                               paste(bad_mkt, collapse = ", "),
                               " (values: ", paste(df$Pazarlama[bad_mkt], collapse = ", "),
                               "). Clamping to [0,3]."))
    df$Pazarlama[bad_mkt] <- pmin(3, pmax(0, df$Pazarlama[bad_mkt]))
    df$Pazarlama[is.na(df$Pazarlama)] <- 1
  }
  
  for (col in c("Kosul_1", "Kosul_2")) {
    vals <- df[[col]]
    for (m in which(vals != "")) {
      cond <- vals[m]
      test_ok <- tryCatch({
        dummy_state <- list(competitor_last = 3, my_last = 3, my_npv = 5000000,
                            competitor_npv = 5000000, capacity_util = 0.5,
                            calisan_memnuniyeti = 70, itibar = 70,
                            market_share = 0.5, cash = 150000,
                            season = 1.0, month = m)
        evaluate_condition(cond, dummy_state)
        TRUE
      }, error = function(e) FALSE)
      if (!test_ok) {
        issues <- c(issues, paste0(sheet_name, ": Unparseable condition at month ",
                                   m, " in ", col, ": '", cond, "'. Clearing."))
        df[[col]][m] <- ""
        fiyat_col <- gsub("Kosul", "Fiyat", col)
        paz_col   <- gsub("Kosul", "Paz", col)
        df[[fiyat_col]][m] <- NA
        df[[paz_col]][m]   <- NA
      }
    }
  }
  
  for (col in c("Fiyat_1", "Fiyat_2")) {
    vals <- df[[col]]
    bad <- which(!is.na(vals) & !(vals %in% VALID_PRICES))
    if (length(bad) > 0) {
      issues <- c(issues, paste0(sheet_name, ": Invalid ", col, " at months: ",
                                 paste(bad, collapse = ", "), ". Clamping."))
      df[[col]][bad] <- pmin(4, pmax(1, df[[col]][bad]))
    }
  }
  for (col in c("Paz_1", "Paz_2")) {
    vals <- df[[col]]
    bad <- which(!is.na(vals) & !(vals %in% VALID_MARKETING))
    if (length(bad) > 0) {
      issues <- c(issues, paste0(sheet_name, ": Invalid ", col, " at months: ",
                                 paste(bad, collapse = ", "), ". Clamping."))
      df[[col]][bad] <- pmin(3, pmax(0, df[[col]][bad]))
    }
  }
  
  df <- df[, c(REQUIRED_COLS, OPTIONAL_COLS)]
  df$stringsAsFactors <- FALSE
  
  return(list(valid = TRUE, issues = issues, df = df))
}


validate_ek_durumlar <- function(filepath, sheet_name, issues) {
  sheets <- tryCatch(excel_sheets(filepath), error = function(e) character(0))
  if (!(sheet_name %in% sheets)) {
    return(list(shock_strategies = NULL, investment_schedule = NULL, issues = issues))
  }
  
  ek <- tryCatch(as.data.frame(read_excel(filepath, sheet = sheet_name)),
                 error = function(e) NULL)
  if (is.null(ek) || nrow(ek) == 0) {
    return(list(shock_strategies = NULL, investment_schedule = NULL, issues = issues))
  }
  
  names(ek) <- trimws(names(ek))
  
  shock_df <- NULL
  if ("sok_tipi" %in% names(ek)) {
    shock_rows <- ek[!is.na(ek$sok_tipi) & ek$sok_tipi != "", ]
    if (nrow(shock_rows) > 0) {
      shock_rows$sok_tipi <- trimws(as.character(shock_rows$sok_tipi))
      shock_rows$sok_tipi <- tolower(shock_rows$sok_tipi)
      shock_rows$sok_tipi <- gsub("[^a-z_]", "", shock_rows$sok_tipi)
      
      bad_shocks <- which(!(shock_rows$sok_tipi %in% VALID_SHOCKS))
      if (length(bad_shocks) > 0) {
        issues <- c(issues, paste0(sheet_name, ": Unknown shock types: ",
                                   paste(shock_rows$sok_tipi[bad_shocks], collapse = ", ")))
        shock_rows <- shock_rows[-bad_shocks, ]
      }
      
      if (nrow(shock_rows) > 0 && "fiyat" %in% names(shock_rows) &&
          "pazarlama" %in% names(shock_rows)) {
        shock_rows$fiyat <- suppressWarnings(as.integer(shock_rows$fiyat))
        shock_rows$pazarlama <- suppressWarnings(as.integer(shock_rows$pazarlama))
        
        valid_shock_rows <- !is.na(shock_rows$fiyat) & !is.na(shock_rows$pazarlama) &
          shock_rows$fiyat %in% VALID_PRICES & shock_rows$pazarlama %in% VALID_MARKETING
        
        if (any(!valid_shock_rows)) {
          issues <- c(issues, paste0(sheet_name, ": Invalid fiyat/pazarlama in shock responses. Removing invalid rows."))
        }
        shock_rows <- shock_rows[valid_shock_rows, ]
        if (nrow(shock_rows) > 0) {
          shock_df <- shock_rows[, c("sok_tipi", "fiyat", "pazarlama")]
        }
      }
    }
  }
  
  inv_df <- NULL
  if ("ay" %in% names(ek) && "yatirim" %in% names(ek)) {
    inv_rows <- ek[!is.na(ek$ay) & !is.na(ek$yatirim) &
                     as.character(ek$yatirim) != "", ]
    if (nrow(inv_rows) > 0) {
      inv_rows$ay <- suppressWarnings(as.integer(inv_rows$ay))
      inv_rows$yatirim <- toupper(trimws(as.character(inv_rows$yatirim)))
      
      bad_months <- which(is.na(inv_rows$ay) | inv_rows$ay < 1 | inv_rows$ay > 36)
      if (length(bad_months) > 0) {
        issues <- c(issues, paste0(sheet_name, ": Invalid investment months. Removing."))
        inv_rows <- inv_rows[-bad_months, ]
      }
      
      bad_codes <- which(!(inv_rows$yatirim %in% VALID_INVESTMENTS))
      if (length(bad_codes) > 0) {
        issues <- c(issues, paste0(sheet_name, ": Invalid investment codes: ",
                                   paste(inv_rows$yatirim[bad_codes], collapse = ", ")))
        inv_rows <- inv_rows[-bad_codes, ]
      }
      
      dup_codes <- duplicated(inv_rows$yatirim)
      if (any(dup_codes)) {
        issues <- c(issues, paste0(sheet_name, ": Duplicate investment codes: ",
                                   paste(inv_rows$yatirim[dup_codes], collapse = ", "),
                                   ". Keeping first occurrence."))
        inv_rows <- inv_rows[!dup_codes, ]
      }
      
      if (nrow(inv_rows) > 0) {
        inv_df <- inv_rows[, c("ay", "yatirim")]
      }
    }
  }
  
  return(list(shock_strategies = shock_df, investment_schedule = inv_df, issues = issues))
}


load_and_validate_file <- function(filepath) {
  info <- parse_filename(filepath)
  issues <- character(0)
  
  sheets <- tryCatch(excel_sheets(filepath), error = function(e) {
    issues <<- c(issues, "Cannot read file: possibly corrupt or not a valid Excel file.")
    return(NULL)
  })
  if (is.null(sheets)) {
    return(list(valid = FALSE, info = info, issues = issues, data = NULL))
  }
  
  missing_sheets <- setdiff(REQUIRED_SHEETS, sheets)
  if (length(missing_sheets) > 0) {
    issues <- c(issues, paste0("Missing required sheets: ",
                               paste(missing_sheets, collapse = ", ")))
    return(list(valid = FALSE, info = info, issues = issues, data = NULL))
  }
  
  strat_A_raw <- tryCatch(as.data.frame(read_excel(filepath, sheet = "Merkez_A")),
                          error = function(e) NULL)
  result_A <- validate_strategy_sheet(strat_A_raw, "Merkez_A", issues)
  issues <- result_A$issues
  
  strat_B_raw <- tryCatch(as.data.frame(read_excel(filepath, sheet = "Merkez_B")),
                          error = function(e) NULL)
  result_B <- validate_strategy_sheet(strat_B_raw, "Merkez_B", issues)
  issues <- result_B$issues
  
  if (!result_A$valid || !result_B$valid) {
    return(list(valid = FALSE, info = info, issues = issues, data = NULL))
  }
  
  ek_A <- NULL; ek_B <- NULL
  shock_A <- NULL; shock_B <- NULL; inv_A <- NULL; inv_B <- NULL
  
  if (OPTIONAL_SHEETS_A %in% sheets) {
    ek_A <- validate_ek_durumlar(filepath, OPTIONAL_SHEETS_A, issues)
    issues <- ek_A$issues
    shock_A <- ek_A$shock_strategies
    inv_A   <- ek_A$investment_schedule
  }
  
  if (OPTIONAL_SHEETS_B %in% sheets) {
    ek_B <- validate_ek_durumlar(filepath, OPTIONAL_SHEETS_B, issues)
    issues <- ek_B$issues
    shock_B <- ek_B$shock_strategies
    inv_B   <- ek_B$investment_schedule
  }
  
  if (is.null(shock_A) && is.null(shock_B) && OPTIONAL_SHEET_LEGACY %in% sheets) {
    ek_legacy <- validate_ek_durumlar(filepath, OPTIONAL_SHEET_LEGACY, issues)
    issues <- ek_legacy$issues
    shock_A <- ek_legacy$shock_strategies
    shock_B <- ek_legacy$shock_strategies
    if (is.null(inv_A)) inv_A <- ek_legacy$investment_schedule
    if (is.null(inv_B)) inv_B <- ek_legacy$investment_schedule
    if (!is.null(shock_A) || !is.null(inv_A)) {
      issues <- c(issues, "Note: Using legacy single Ek_Durumlar sheet for both centers.")
    }
  }
  
  strat_profile <- list(
    dominant_price_A = as.integer(names(sort(table(result_A$df$Fiyat), decreasing = TRUE))[1]),
    dominant_price_B = as.integer(names(sort(table(result_B$df$Fiyat), decreasing = TRUE))[1]),
    dominant_mkt_A = as.integer(names(sort(table(result_A$df$Pazarlama), decreasing = TRUE))[1]),
    dominant_mkt_B = as.integer(names(sort(table(result_B$df$Pazarlama), decreasing = TRUE))[1]),
    n_conditions_A = sum(result_A$df$Kosul_1 != "" | result_A$df$Kosul_2 != ""),
    n_conditions_B = sum(result_B$df$Kosul_1 != "" | result_B$df$Kosul_2 != ""),
    n_shock_responses_A = ifelse(is.null(shock_A), 0, nrow(shock_A)),
    n_shock_responses_B = ifelse(is.null(shock_B), 0, nrow(shock_B)),
    n_investments_A = ifelse(is.null(inv_A), 0, nrow(inv_A)),
    n_investments_B = ifelse(is.null(inv_B), 0, nrow(inv_B)),
    n_price_changes_A = sum(diff(result_A$df$Fiyat) != 0),
    n_price_changes_B = sum(diff(result_B$df$Fiyat) != 0)
  )
  
  return(list(
    valid = TRUE,
    info = info,
    issues = issues,
    profile = strat_profile,
    data = list(
      strategy_A = result_A$df,
      strategy_B = result_B$df,
      shock_strategies_A = shock_A,
      shock_strategies_B = shock_B,
      investment_schedule_A = inv_A,
      investment_schedule_B = inv_B
    )
  ))
}


cat("==========================================\n")
cat("TOURNAMENT ANALYSIS\n")
cat("==========================================\n\n")

files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
files <- files[!grepl("^~\\$", basename(files))]

if (length(files) == 0) {
  stop(paste0("No .xlsx files found in: ", data_dir))
}

cat(sprintf("Found %d Excel files.\n\n", length(files)))

all_entries <- list()
validation_report <- data.frame(
  file = character(), student_id = character(), name = character(),
  university = character(), status = character(), n_issues = integer(),
  issues = character(), stringsAsFactors = FALSE
)

for (f in files) {
  cat(sprintf("  Validating: %s ... ", basename(f)))
  result <- load_and_validate_file(f)
  
  status <- ifelse(result$valid, "PASS", "FAIL")
  issue_text <- ifelse(length(result$issues) == 0, "None",
                       paste(result$issues, collapse = " | "))
  
  validation_report <- rbind(validation_report, data.frame(
    file = basename(f),
    student_id = result$info$id,
    name = result$info$label,
    university = result$info$university,
    status = status,
    n_issues = length(result$issues),
    issues = issue_text,
    stringsAsFactors = FALSE
  ))
  
  if (result$valid) {
    all_entries[[result$info$label]] <- result
    cat(sprintf("PASS (%d warnings)\n", length(result$issues)))
  } else {
    cat(sprintf("FAIL (%d errors)\n", length(result$issues)))
  }
}

write.csv(validation_report,
          file.path(output_dir, "tables", "validation_report.csv"),
          row.names = FALSE)

n_valid <- sum(validation_report$status == "PASS")
n_fail  <- sum(validation_report$status == "FAIL")
cat(sprintf("\nValidation complete: %d passed, %d failed, %d total.\n\n",
            n_valid, n_fail, nrow(validation_report)))

if (n_valid < 2) {
  stop("Need at least 2 valid strategies to run tournament.")
}

strat_names <- names(all_entries)
n_strats    <- length(strat_names)
n_matches   <- n_strats * n_strats

cat(sprintf("Running tournament: %d strategies, %d matchups, %d sims each.\n",
            n_strats, n_matches, N_SIMS_PER_MATCH))
cat(sprintf("Total simulations: %d (estimated time: %.0f minutes)\n\n",
            n_matches * N_SIMS_PER_MATCH,
            n_matches * N_SIMS_PER_MATCH * 0.08 / 60))

npv_as_A  <- matrix(0, nrow = n_strats, ncol = n_strats,
                    dimnames = list(strat_names, strat_names))
npv_as_B  <- matrix(0, nrow = n_strats, ncol = n_strats,
                    dimnames = list(strat_names, strat_names))
sd_as_A   <- matrix(0, nrow = n_strats, ncol = n_strats,
                    dimnames = list(strat_names, strat_names))
sd_as_B   <- matrix(0, nrow = n_strats, ncol = n_strats,
                    dimnames = list(strat_names, strat_names))
wins_A    <- matrix(0, nrow = n_strats, ncol = n_strats,
                    dimnames = list(strat_names, strat_names))

all_sim_results <- list()
counter <- 0
t_start <- Sys.time()

for (i in 1:n_strats) {
  for (j in 1:n_strats) {
    counter <- counter + 1
    if (counter %% 10 == 0 || counter == 1) {
      elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
      rate <- ifelse(counter > 1, elapsed / (counter - 1), 0)
      remaining <- rate * (n_matches - counter + 1)
      cat(sprintf("  Match %d/%d: %s vs %s (est. %.0f min remaining)\n",
                  counter, n_matches, strat_names[i], strat_names[j], remaining))
    }
    
    entry_i <- all_entries[[strat_names[i]]]
    entry_j <- all_entries[[strat_names[j]]]
    
    npv_a_vec <- numeric(N_SIMS_PER_MATCH)
    npv_b_vec <- numeric(N_SIMS_PER_MATCH)
    
    for (sim in 1:N_SIMS_PER_MATCH) {
      sim_result <- tryCatch({
        run_single_simulation(
          strategy_A = entry_i$data$strategy_A,
          strategy_B = entry_j$data$strategy_B,
          shock_strategies_A = entry_i$data$shock_strategies_A,
          shock_strategies_B = entry_j$data$shock_strategies_B,
          investment_schedule_A = entry_i$data$investment_schedule_A,
          investment_schedule_B = entry_j$data$investment_schedule_B
        )
      }, error = function(e) NULL)
      
      if (!is.null(sim_result)) {
        npv_a_vec[sim] <- sim_result$npv_A[36]
        npv_b_vec[sim] <- sim_result$npv_B[36]
      } else {
        npv_a_vec[sim] <- NA
        npv_b_vec[sim] <- NA
      }
    }
    
    npv_a_vec <- npv_a_vec[!is.na(npv_a_vec)]
    npv_b_vec <- npv_b_vec[!is.na(npv_b_vec)]
    
    if (length(npv_a_vec) > 0) {
      npv_as_A[i, j] <- mean(npv_a_vec)
      npv_as_B[i, j] <- mean(npv_b_vec)
      sd_as_A[i, j]  <- sd(npv_a_vec)
      sd_as_B[i, j]  <- sd(npv_b_vec)
      wins_A[i, j]   <- mean(npv_a_vec > npv_b_vec)
    }
    
    all_sim_results[[paste0(strat_names[i], "_vs_", strat_names[j])]] <- list(
      player_A = strat_names[i], player_B = strat_names[j],
      mean_npv_A = mean(npv_a_vec), mean_npv_B = mean(npv_b_vec),
      sd_npv_A = sd(npv_a_vec), sd_npv_B = sd(npv_b_vec),
      win_rate_A = mean(npv_a_vec > npv_b_vec),
      n_valid_sims = length(npv_a_vec)
    )
  }
}

elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
cat(sprintf("\nTournament complete in %.1f minutes.\n\n", elapsed_total))


cat("Computing rankings...\n")

avg_npv_A <- rowMeans(npv_as_A)
avg_npv_B <- colMeans(npv_as_B)
avg_npv_combined <- (avg_npv_A + avg_npv_B) / 2

rankings <- data.frame(
  rank = integer(n_strats),
  student = strat_names,
  student_id = sapply(strat_names, function(s) all_entries[[s]]$info$id),
  university = sapply(strat_names, function(s) all_entries[[s]]$info$university),
  avg_npv_total = avg_npv_combined,
  avg_npv_as_A = avg_npv_A,
  avg_npv_as_B = avg_npv_B,
  npv_advantage_A = avg_npv_A - avg_npv_B,
  dominant_price_A = sapply(strat_names, function(s) all_entries[[s]]$profile$dominant_price_A),
  dominant_price_B = sapply(strat_names, function(s) all_entries[[s]]$profile$dominant_price_B),
  n_conditions = sapply(strat_names, function(s)
    all_entries[[s]]$profile$n_conditions_A + all_entries[[s]]$profile$n_conditions_B),
  n_price_changes = sapply(strat_names, function(s)
    all_entries[[s]]$profile$n_price_changes_A + all_entries[[s]]$profile$n_price_changes_B),
  stringsAsFactors = FALSE
)

rankings <- rankings[order(-rankings$avg_npv_total), ]
rankings$rank <- 1:nrow(rankings)
rownames(rankings) <- NULL


head_to_head <- data.frame()
for (i in 1:n_strats) {
  for (j in 1:n_strats) {
    if (i != j) {
      match_AB <- all_sim_results[[paste0(strat_names[i], "_vs_", strat_names[j])]]
      match_BA <- all_sim_results[[paste0(strat_names[j], "_vs_", strat_names[i])]]
      i_total <- match_AB$mean_npv_A + match_BA$mean_npv_B
      j_total <- match_AB$mean_npv_B + match_BA$mean_npv_A
      head_to_head <- rbind(head_to_head, data.frame(
        player_1 = strat_names[i], player_2 = strat_names[j],
        p1_total_npv = i_total, p2_total_npv = j_total,
        p1_wins = ifelse(i_total > j_total, 1, 0),
        margin = i_total - j_total,
        stringsAsFactors = FALSE
      ))
    }
  }
}

win_counts <- head_to_head %>% group_by(player_1) %>%
  summarise(wins = sum(p1_wins), losses = n() - sum(p1_wins),
            total_matches = n(), win_rate = mean(p1_wins), .groups = "drop") %>%
  rename(student = player_1)

rankings <- merge(rankings, win_counts, by = "student", all.x = TRUE)
rankings <- rankings[order(rankings$rank), ]


profile_df <- data.frame(
  student = strat_names,
  stringsAsFactors = FALSE
)
for (s in strat_names) {
  e <- all_entries[[s]]
  profile_df[profile_df$student == s, "university"] <- e$info$university
  profile_df[profile_df$student == s, "dom_price_A"] <- e$profile$dominant_price_A
  profile_df[profile_df$student == s, "dom_price_B"] <- e$profile$dominant_price_B
  profile_df[profile_df$student == s, "dom_mkt_A"] <- e$profile$dominant_mkt_A
  profile_df[profile_df$student == s, "dom_mkt_B"] <- e$profile$dominant_mkt_B
  profile_df[profile_df$student == s, "n_cond_A"] <- e$profile$n_conditions_A
  profile_df[profile_df$student == s, "n_cond_B"] <- e$profile$n_conditions_B
  profile_df[profile_df$student == s, "n_shock_A"] <- e$profile$n_shock_responses_A
  profile_df[profile_df$student == s, "n_shock_B"] <- e$profile$n_shock_responses_B
  profile_df[profile_df$student == s, "n_inv_A"] <- e$profile$n_investments_A
  profile_df[profile_df$student == s, "n_inv_B"] <- e$profile$n_investments_B
  profile_df[profile_df$student == s, "price_changes_A"] <- e$profile$n_price_changes_A
  profile_df[profile_df$student == s, "price_changes_B"] <- e$profile$n_price_changes_B
}


cat("Saving tables...\n")

write.csv(rankings, file.path(output_dir, "tables", "tournament_rankings.csv"),
          row.names = FALSE)
write.csv(npv_as_A / 1e6, file.path(output_dir, "tables", "matchup_matrix_as_A.csv"))
write.csv(npv_as_B / 1e6, file.path(output_dir, "tables", "matchup_matrix_as_B.csv"))
write.csv((npv_as_A + t(npv_as_B)) / 2e6,
          file.path(output_dir, "tables", "matchup_matrix_combined.csv"))
write.csv(head_to_head, file.path(output_dir, "tables", "head_to_head_results.csv"),
          row.names = FALSE)
write.csv(profile_df, file.path(output_dir, "tables", "strategy_profiles.csv"),
          row.names = FALSE)

desc_stats <- data.frame(
  metric = c("Number of participants", "Number of valid strategies",
             "Total matchups", "Simulations per matchup",
             "Total simulations run", "Tournament duration (min)",
             "Mean NPV (M TL)", "SD NPV (M TL)",
             "Max NPV (M TL)", "Min NPV (M TL)",
             "NPV range (M TL)", "Mean win rate"),
  value = c(nrow(validation_report), n_valid,
            n_matches, N_SIMS_PER_MATCH,
            n_matches * N_SIMS_PER_MATCH,
            round(elapsed_total, 1),
            round(mean(rankings$avg_npv_total) / 1e6, 2),
            round(sd(rankings$avg_npv_total) / 1e6, 2),
            round(max(rankings$avg_npv_total) / 1e6, 2),
            round(min(rankings$avg_npv_total) / 1e6, 2),
            round((max(rankings$avg_npv_total) - min(rankings$avg_npv_total)) / 1e6, 2),
            round(mean(rankings$win_rate, na.rm = TRUE), 3)),
  stringsAsFactors = FALSE
)
write.csv(desc_stats, file.path(output_dir, "tables", "descriptive_statistics.csv"),
          row.names = FALSE)

save(all_entries, rankings, npv_as_A, npv_as_B, sd_as_A, sd_as_B,
     wins_A, head_to_head, profile_df, all_sim_results, validation_report,
     file = file.path(output_dir, "tournament_results.RData"))


cat("Generating figures...\n\n")

theme_pub <- theme_minimal(base_size = 14, base_family = "serif") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey30"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 15, 10, 10))

col_A <- "#2E86AB"
col_B <- "#A23B72"


cat("  Figure 1: Tournament Rankings\n")

rank_df <- rankings[order(rankings$rank), ]
rank_df$student <- factor(rank_df$student, levels = rev(rank_df$student))

fig1 <- ggplot(rank_df, aes(x = student, y = avg_npv_total / 1e6)) +
  geom_bar(stat = "identity", fill = col_A, alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("#%d", rank)), hjust = -0.3,
            size = 4, fontface = "bold", family = "serif") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Tournament Rankings",
       subtitle = sprintf("Average NPV across %d opponents, %d simulations per matchup",
                          n_strats, N_SIMS_PER_MATCH),
       x = "", y = "Average NPV (Million TL)") +
  theme_pub

ggsave(file.path(output_dir, "figures", "Fig1_Tournament_Rankings.png"),
       fig1, width = 12, height = max(6, 0.5 * n_strats), dpi = 600)
ggsave(file.path(output_dir, "figures", "Fig1_Tournament_Rankings.pdf"),
       fig1, width = 12, height = max(6, 0.5 * n_strats), dpi = 600)


cat("  Figure 2: NPV as Center A vs Center B\n")

fig2 <- ggplot(rankings, aes(x = avg_npv_as_A / 1e6, y = avg_npv_as_B / 1e6)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 4, color = col_A, alpha = 0.8) +
  geom_text(aes(label = gsub(" ", "\n", student)),
            size = 2.5, vjust = -1, family = "serif") +
  labs(title = "Performance Asymmetry: Center A vs Center B Role",
       subtitle = "Points above diagonal = better as Center B",
       x = "Average NPV as Center A (Million TL)",
       y = "Average NPV as Center B (Million TL)") +
  theme_pub

ggsave(file.path(output_dir, "figures", "Fig2_Role_Asymmetry.png"),
       fig2, width = 10, height = 8, dpi = 600)
ggsave(file.path(output_dir, "figures", "Fig2_Role_Asymmetry.pdf"),
       fig2, width = 10, height = 8, dpi = 600)


cat("  Figure 3: Matchup Heatmap\n")

combined_matrix <- (npv_as_A + t(npv_as_B)) / 2e6
ranked_order <- rankings$student[order(rankings$rank)]
combined_matrix <- combined_matrix[ranked_order, ranked_order]

heatmap_df <- expand.grid(
  Player = rownames(combined_matrix),
  Opponent = colnames(combined_matrix),
  stringsAsFactors = FALSE
)
heatmap_df$NPV <- as.vector(combined_matrix)
heatmap_df$Player <- factor(heatmap_df$Player, levels = rev(ranked_order))
heatmap_df$Opponent <- factor(heatmap_df$Opponent, levels = ranked_order)

fig3 <- ggplot(heatmap_df, aes(x = Opponent, y = Player, fill = NPV)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", NPV)), size = 2.5, family = "serif") +
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850",
                       midpoint = median(heatmap_df$NPV),
                       name = "NPV\n(M TL)") +
  labs(title = "Head-to-Head Results: Average NPV (Million TL)",
       subtitle = "Row player's combined NPV against column opponent",
       x = "Opponent", y = "Player") +
  theme_pub +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        legend.position = "right")

ggsave(file.path(output_dir, "figures", "Fig3_Matchup_Heatmap.png"),
       fig3, width = max(10, 0.6 * n_strats), height = max(8, 0.5 * n_strats), dpi = 600)
ggsave(file.path(output_dir, "figures", "Fig3_Matchup_Heatmap.pdf"),
       fig3, width = max(10, 0.6 * n_strats), height = max(8, 0.5 * n_strats), dpi = 600)


cat("  Figure 4: Win Rate\n")

win_df <- rankings[order(rankings$rank), ]
win_df$student <- factor(win_df$student, levels = rev(win_df$student))

fig4 <- ggplot(win_df, aes(x = student, y = win_rate * 100)) +
  geom_bar(stat = "identity", fill = "#27AE60", alpha = 0.85, width = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", win_rate * 100)), hjust = -0.2,
            size = 3.5, family = "serif") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  labs(title = "Head-to-Head Win Rate",
       subtitle = "Percentage of opponents defeated across all matchups",
       x = "", y = "Win Rate (%)") +
  theme_pub

ggsave(file.path(output_dir, "figures", "Fig4_Win_Rate.png"),
       fig4, width = 12, height = max(6, 0.5 * n_strats), dpi = 600)
ggsave(file.path(output_dir, "figures", "Fig4_Win_Rate.pdf"),
       fig4, width = 12, height = max(6, 0.5 * n_strats), dpi = 600)


cat("  Figure 5: Strategy Diversity\n")

price_labels <- c("S1 Destructive", "S2 Aggressive", "S3 Cooperative", "S4 Premium")

price_dist_A <- table(factor(profile_df$dom_price_A, levels = 1:4))
price_dist_B <- table(factor(profile_df$dom_price_B, levels = 1:4))
div_df <- data.frame(
  Strategy = rep(price_labels, 2),
  Count = c(as.integer(price_dist_A), as.integer(price_dist_B)),
  Role = rep(c("Center A", "Center B"), each = 4)
)

fig5 <- ggplot(div_df, aes(x = Strategy, y = Count, fill = Role)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           alpha = 0.85, width = 0.7) +
  scale_fill_manual(values = c("Center A" = col_A, "Center B" = col_B)) +
  labs(title = "Strategy Diversity: Dominant Pricing Choice",
       subtitle = "Most frequently selected price level across 36 months",
       x = "", y = "Number of Students") +
  theme_pub

ggsave(file.path(output_dir, "figures", "Fig5_Strategy_Diversity.png"),
       fig5, width = 10, height = 6, dpi = 600)
ggsave(file.path(output_dir, "figures", "Fig5_Strategy_Diversity.pdf"),
       fig5, width = 10, height = 6, dpi = 600)


cat("  Figure 6: Strategy Complexity vs Performance\n")

complexity_df <- rankings
complexity_df$complexity <- complexity_df$n_conditions + complexity_df$n_price_changes

fig6 <- ggplot(complexity_df, aes(x = complexity, y = avg_npv_total / 1e6)) +
  geom_point(size = 4, color = col_A, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = col_B,
              fill = col_B, alpha = 0.15, linewidth = 1) +
  geom_text(aes(label = student), size = 2.5, vjust = -1, family = "serif") +
  labs(title = "Strategy Complexity vs Tournament Performance",
       subtitle = "Complexity = number of conditions + number of price changes",
       x = "Strategy Complexity Score",
       y = "Average NPV (Million TL)") +
  theme_pub

ggsave(file.path(output_dir, "figures", "Fig6_Complexity_Performance.png"),
       fig6, width = 10, height = 7, dpi = 600)
ggsave(file.path(output_dir, "figures", "Fig6_Complexity_Performance.pdf"),
       fig6, width = 10, height = 7, dpi = 600)


cat("  Figure 7: University Comparison\n")

if (length(unique(rankings$university)) > 1) {
  uni_df <- rankings %>%
    group_by(university) %>%
    summarise(mean_npv = mean(avg_npv_total) / 1e6,
              sd_npv = sd(avg_npv_total) / 1e6,
              n = n(),
              best_rank = min(rank),
              .groups = "drop") %>%
    arrange(desc(mean_npv))
  
  fig7 <- ggplot(uni_df, aes(x = reorder(university, mean_npv), y = mean_npv)) +
    geom_bar(stat = "identity", fill = col_A, alpha = 0.85, width = 0.6) +
    geom_errorbar(aes(ymin = mean_npv - sd_npv, ymax = mean_npv + sd_npv),
                  width = 0.2) +
    geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.5,
              size = 4, family = "serif") +
    coord_flip() +
    labs(title = "University Comparison",
         subtitle = "Mean NPV by institution (error bars = 1 SD)",
         x = "", y = "Mean NPV (Million TL)") +
    theme_pub
  
  ggsave(file.path(output_dir, "figures", "Fig7_University_Comparison.png"),
         fig7, width = 10, height = 6, dpi = 600)
  ggsave(file.path(output_dir, "figures", "Fig7_University_Comparison.pdf"),
         fig7, width = 10, height = 6, dpi = 600)
  cat("    Saved.\n")
} else {
  cat("    Skipped (single university).\n")
}


cat("\nPrinting final rankings:\n\n")
cat(sprintf("  %-4s %-25s %-15s %12s %10s %8s\n",
            "Rank", "Student", "University", "Avg NPV (M)", "Win Rate", "Wins"))
cat(paste(rep("-", 80), collapse = ""), "\n")
for (k in 1:nrow(rankings)) {
  r <- rankings[k, ]
  cat(sprintf("  %-4d %-25s %-15s %12.2f %9.1f%% %5d/%d\n",
              r$rank, r$student, r$university,
              r$avg_npv_total / 1e6,
              r$win_rate * 100,
              r$wins, r$total_matches))
}

cat("TOURNAMENT ANALYSIS COMPLETE\n")

cat(sprintf("Results saved to: %s\n", output_dir))
cat(sprintf("  Tables: %d files\n", length(list.files(file.path(output_dir, "tables")))))
cat(sprintf("  Figures: %d files\n", length(list.files(file.path(output_dir, "figures")))))
cat(sprintf("  RData: tournament_results.RData\n"))
cat("==========================================\n")
