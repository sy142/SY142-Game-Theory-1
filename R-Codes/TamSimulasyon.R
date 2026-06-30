rm(list = ls())
gc()

library(ggplot2)
library(tidyr)
library(dplyr)

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

create_fixed_strategy <- function(price = 3, marketing = 1, n_months = 36) {
  data.frame(Ay = 1:n_months, Fiyat = rep(price, n_months),
             Pazarlama = rep(marketing, n_months),
             Kosul_1 = rep(NA, n_months), Fiyat_1 = rep(NA, n_months), Paz_1 = rep(NA, n_months),
             Kosul_2 = rep(NA, n_months), Fiyat_2 = rep(NA, n_months), Paz_2 = rep(NA, n_months),
             stringsAsFactors = FALSE)
}

create_tit_for_tat_strategy <- function(base_price = 3, base_marketing = 1,
                                        retaliation_price = 2, retaliation_marketing = 2,
                                        n_months = 36) {
  df <- create_fixed_strategy(base_price, base_marketing, n_months)
  for (m in 2:n_months) {
    df$Kosul_1[m] <- "competitor_last <= 2"
    df$Fiyat_1[m] <- retaliation_price
    df$Paz_1[m] <- retaliation_marketing
  }
  return(df)
}

create_seasonal_strategy <- function(n_months = 36) {
  df <- create_fixed_strategy(price = 3, marketing = 1, n_months = n_months)
  for (m in 1:n_months) {
    month_in_year <- ((m - 1) %% 12) + 1
    if (month_in_year %in% c(1, 2)) {
      df$Fiyat[m] <- 4; df$Pazarlama[m] <- 2
    } else if (month_in_year %in% c(6, 7, 8)) {
      df$Fiyat[m] <- 2; df$Pazarlama[m] <- 1
    }
    if (m > 1) {
      df$Kosul_1[m] <- "competitor_last <= 2"
      df$Fiyat_1[m] <- 2; df$Paz_1[m] <- 2
    }
  }
  return(df)
}

create_adaptive_strategy <- function(n_months = 36) {
  df <- create_seasonal_strategy(n_months)
  for (m in 2:n_months) {
    df$Kosul_2[m] <- "calisan_memnuniyeti < 50"
    df$Fiyat_2[m] <- 4; df$Paz_2[m] <- 0
  }
  return(df)
}

create_grim_trigger_strategy <- function(base_price = 3, base_marketing = 1,
                                         punishment_price = 2, punishment_marketing = 2,
                                         n_months = 36) {
  df <- create_fixed_strategy(base_price, base_marketing, n_months)
  for (m in 2:n_months) {
    cond_parts <- paste0('competitor_last <= 2')
    if (m >= 3) {
      cond_parts <- paste0('my_last <= 2 OR competitor_last <= 2')
    }
    df$Kosul_1[m] <- cond_parts
    df$Fiyat_1[m] <- punishment_price
    df$Paz_1[m] <- punishment_marketing
  }
  return(df)
}

create_alternating_strategy <- function(n_months = 36) {
  data.frame(Ay = 1:n_months, Fiyat = rep(c(3, 2), length.out = n_months),
             Pazarlama = rep(c(1, 2), length.out = n_months),
             Kosul_1 = rep(NA, n_months), Fiyat_1 = rep(NA, n_months), Paz_1 = rep(NA, n_months),
             Kosul_2 = rep(NA, n_months), Fiyat_2 = rep(NA, n_months), Paz_2 = rep(NA, n_months),
             stringsAsFactors = FALSE)
}

create_endgame_aggressive_strategy <- function(n_months = 36, aggressive_start = 30) {
  df <- create_fixed_strategy(price = 3, marketing = 1, n_months = n_months)
  for (m in aggressive_start:n_months) { df$Fiyat[m] <- 2; df$Pazarlama[m] <- 2 }
  return(df)
}

create_random_strategy <- function(n_months = 36, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  data.frame(Ay = 1:n_months, Fiyat = sample(1:4, n_months, replace = TRUE),
             Pazarlama = sample(0:3, n_months, replace = TRUE),
             Kosul_1 = rep(NA, n_months), Fiyat_1 = rep(NA, n_months), Paz_1 = rep(NA, n_months),
             Kosul_2 = rep(NA, n_months), Fiyat_2 = rep(NA, n_months), Paz_2 = rep(NA, n_months),
             stringsAsFactors = FALSE)
}

create_forgiving_tft_strategy <- function(n_months = 36) {
  df <- create_fixed_strategy(price = 3, marketing = 1, n_months = n_months)
  for (m in 2:n_months) {
    df$Kosul_1[m] <- "competitor_last == 1"
    df$Fiyat_1[m] <- 2; df$Paz_1[m] <- 2
    df$Kosul_2[m] <- "competitor_last == 2 AND my_npv < competitor_npv"
    df$Fiyat_2[m] <- 2; df$Paz_2[m] <- 1
  }
  return(df)
}

create_satisfaction_aware_strategy <- function(n_months = 36) {
  df <- create_seasonal_strategy(n_months)
  for (m in 2:n_months) {
    df$Kosul_2[m] <- "calisan_memnuniyeti < 45 OR capacity_util > 0.90"
    df$Fiyat_2[m] <- 4; df$Paz_2[m] <- 0
  }
  return(df)
}

read_student_strategy <- function(filepath) {
  if (!requireNamespace('readxl', quietly = TRUE)) {
    stop('readxl paketi gerekli: install.packages("readxl")')
  }
  strategy_A <- as.data.frame(readxl::read_excel(filepath, sheet = 'Merkez_A'))
  strategy_B <- as.data.frame(readxl::read_excel(filepath, sheet = 'Merkez_B'))
  for (col in c('Kosul_1', 'Kosul_2')) {
    if (col %in% names(strategy_A)) strategy_A[[col]][is.na(strategy_A[[col]])] <- ''
    if (col %in% names(strategy_B)) strategy_B[[col]][is.na(strategy_B[[col]])] <- ''
  }
  shock_A <- NULL; shock_B <- NULL; inv_A <- NULL; inv_B <- NULL
  sheets <- readxl::excel_sheets(filepath)
  if ('Ek_Durumlar_A' %in% sheets) {
    ek_a <- as.data.frame(readxl::read_excel(filepath, sheet = 'Ek_Durumlar_A'))
    if ('sok_tipi' %in% names(ek_a)) shock_A <- ek_a[, c('sok_tipi', 'fiyat', 'pazarlama')]
  }
  if ('Ek_Durumlar_B' %in% sheets) {
    ek_b <- as.data.frame(readxl::read_excel(filepath, sheet = 'Ek_Durumlar_B'))
    if ('sok_tipi' %in% names(ek_b)) shock_B <- ek_b[, c('sok_tipi', 'fiyat', 'pazarlama')]
  }
  if ('Ek_Durumlar' %in% sheets) {
    ek <- as.data.frame(readxl::read_excel(filepath, sheet = 'Ek_Durumlar'))
    if ('sok_tipi' %in% names(ek)) { shock_A <- ek[, c('sok_tipi', 'fiyat', 'pazarlama')]; shock_B <- shock_A }
  }
  for (sn in c('Ek_Durumlar_A', 'Ek_Durumlar_B', 'Ek_Durumlar')) {
    if (sn %in% sheets) {
      ek_raw <- as.data.frame(readxl::read_excel(filepath, sheet = sn))
      inv_cols <- ek_raw[, c('ay', 'yatirim')]
      inv_cols <- inv_cols[!is.na(inv_cols$ay) & !is.na(inv_cols$yatirim), ]
      if (nrow(inv_cols) > 0) {
        if (grepl('_A', sn)) inv_A <- inv_cols
        else if (grepl('_B', sn)) inv_B <- inv_cols
        else { inv_A <- inv_cols; inv_B <- inv_cols }
      }
    }
  }
  return(list(strategy_A = strategy_A, strategy_B = strategy_B,
              shock_strategies_A = shock_A, shock_strategies_B = shock_B,
              investment_schedule_A = inv_A, investment_schedule_B = inv_B))
}
calculate_payoff_matrix <- function(n_simulations = 100, params = sim_params) {
  n_prices <- 4
  payoff_A <- matrix(0, nrow = n_prices, ncol = n_prices)
  payoff_B <- matrix(0, nrow = n_prices, ncol = n_prices)
  payoff_A_sd <- matrix(0, nrow = n_prices, ncol = n_prices)
  payoff_B_sd <- matrix(0, nrow = n_prices, ncol = n_prices)
  nms <- paste0("S", 1:4)
  rownames(payoff_A) <- nms; colnames(payoff_A) <- nms
  rownames(payoff_B) <- nms; colnames(payoff_B) <- nms
  rownames(payoff_A_sd) <- nms; colnames(payoff_A_sd) <- nms
  rownames(payoff_B_sd) <- nms; colnames(payoff_B_sd) <- nms
  for (i in 1:n_prices) {
    for (j in 1:n_prices) {
      sa <- create_fixed_strategy(price = i, marketing = 1)
      sb <- create_fixed_strategy(price = j, marketing = 1)
      npv_a <- npv_b <- numeric(n_simulations)
      for (sim in 1:n_simulations) {
        r <- run_single_simulation(sa, sb, params = params)
        npv_a[sim] <- r$npv_A[params$n_months]
        npv_b[sim] <- r$npv_B[params$n_months]
      }
      payoff_A[i, j] <- mean(npv_a); payoff_B[i, j] <- mean(npv_b)
      payoff_A_sd[i, j] <- sd(npv_a); payoff_B_sd[i, j] <- sd(npv_b)
    }
  }
  return(list(payoff_A = payoff_A, payoff_B = payoff_B,
              payoff_A_sd = payoff_A_sd, payoff_B_sd = payoff_B_sd))
}

find_nash_equilibrium <- function(payoff_A, payoff_B) {
  nash <- list()
  for (i in 1:nrow(payoff_A)) {
    for (j in 1:ncol(payoff_A)) {
      best_A <- all(payoff_A[i, j] >= payoff_A[, j])
      best_B <- all(payoff_B[i, j] >= payoff_B[i, ])
      if (best_A && best_B) {
        nash[[length(nash) + 1]] <- list(strategy_A = i, strategy_B = j,
                                         payoff_A = payoff_A[i, j], payoff_B = payoff_B[i, j])
      }
    }
  }
  return(nash)
}

find_pareto_optimal <- function(payoff_A, payoff_B) {
  outcomes <- expand.grid(strategy_A = 1:nrow(payoff_A), strategy_B = 1:ncol(payoff_A))
  outcomes$payoff_A <- mapply(function(i,j) payoff_A[i,j], outcomes$strategy_A, outcomes$strategy_B)
  outcomes$payoff_B <- mapply(function(i,j) payoff_B[i,j], outcomes$strategy_A, outcomes$strategy_B)
  outcomes$is_pareto <- TRUE
  for (k in 1:nrow(outcomes)) {
    for (l in 1:nrow(outcomes)) {
      if (k != l && outcomes$payoff_A[l] >= outcomes$payoff_A[k] &&
          outcomes$payoff_B[l] >= outcomes$payoff_B[k] &&
          (outcomes$payoff_A[l] > outcomes$payoff_A[k] || outcomes$payoff_B[l] > outcomes$payoff_B[k])) {
        outcomes$is_pareto[k] <- FALSE; break
      }
    }
  }
  return(outcomes[outcomes$is_pareto, ])
}

check_prisoners_dilemma_condition <- function(payoff_A, payoff_B) {
  T_val <- payoff_A[2, 3]; R_val <- payoff_A[3, 3]
  P_val <- payoff_A[2, 2]; S_val <- payoff_A[3, 2]
  is_pd <- (T_val > R_val) && (R_val > P_val) && (P_val > S_val)
  coop_ok <- (2 * R_val) > (T_val + S_val)
  return(list(T = T_val, R = R_val, P = P_val, S = S_val,
              is_prisoners_dilemma = is_pd, cooperation_sustainable = coop_ok,
              condition_string = paste0("T=", round(T_val/1e6, 2), "M > R=", round(R_val/1e6, 2),
                                        "M > P=", round(P_val/1e6, 2), "M > S=", round(S_val/1e6, 2), "M"),
              cooperation_condition = paste0("2R=", round(2*R_val/1e6, 2), "M vs T+S=",
                                             round((T_val + S_val)/1e6, 2), "M")))
}

run_tournament <- function(strategies, strategy_names, n_simulations = 100, params = sim_params) {
  n <- length(strategies)
  rmat <- matrix(0, nrow = n, ncol = n)
  rownames(rmat) <- strategy_names; colnames(rmat) <- strategy_names
  for (i in 1:n) {
    for (j in 1:n) {
      npvs <- numeric(n_simulations)
      for (sim in 1:n_simulations) {
        r <- run_single_simulation(strategies[[i]], strategies[[j]], params = params)
        npvs[sim] <- r$npv_A[params$n_months]
      }
      rmat[i, j] <- mean(npvs)
    }
  }
  avg <- rowMeans(rmat)
  rankings <- data.frame(strategy = strategy_names, average_npv = avg, rank = rank(-avg))
  rankings <- rankings[order(rankings$rank), ]
  return(list(results_matrix = rmat, rankings = rankings))
}

run_monte_carlo_analysis <- function(strategy_A, strategy_B, n_simulations = 1000, params = sim_params) {
  results <- data.frame(simulation = 1:n_simulations,
                        final_npv_A = numeric(n_simulations), final_npv_B = numeric(n_simulations),
                        total_profit_A = numeric(n_simulations), total_profit_B = numeric(n_simulations),
                        avg_satisfaction_A = numeric(n_simulations), avg_satisfaction_B = numeric(n_simulations),
                        avg_market_share_A = numeric(n_simulations), avg_market_share_B = numeric(n_simulations),
                        n_shocks_A = integer(n_simulations), n_shocks_B = integer(n_simulations),
                        winner = character(n_simulations), stringsAsFactors = FALSE)
  monthly_data <- list()
  for (sim in 1:n_simulations) {
    sr <- run_single_simulation(strategy_A, strategy_B, params = params)
    results$final_npv_A[sim] <- sr$npv_A[params$n_months]
    results$final_npv_B[sim] <- sr$npv_B[params$n_months]
    results$total_profit_A[sim] <- sr$cumulative_profit_A[params$n_months]
    results$total_profit_B[sim] <- sr$cumulative_profit_B[params$n_months]
    results$avg_satisfaction_A[sim] <- mean(sr$satisfaction_A)
    results$avg_satisfaction_B[sim] <- mean(sr$satisfaction_B)
    results$avg_market_share_A[sim] <- mean(sr$market_share_A)
    results$avg_market_share_B[sim] <- mean(sr$market_share_B)
    results$n_shocks_A[sim] <- sum(sr$active_shocks_A != "")
    results$n_shocks_B[sim] <- sum(sr$active_shocks_B != "")
    results$winner[sim] <- ifelse(results$final_npv_A[sim] > results$final_npv_B[sim], "A",
                                  ifelse(results$final_npv_A[sim] < results$final_npv_B[sim], "B", "Tie"))
    if (sim <= 100) monthly_data[[sim]] <- sr
  }
  stats <- list(npv_A = list(mean = mean(results$final_npv_A), sd = sd(results$final_npv_A)),
                npv_B = list(mean = mean(results$final_npv_B), sd = sd(results$final_npv_B)),
                win_rate_A = mean(results$winner == "A"), win_rate_B = mean(results$winner == "B"))
  return(list(results = results, summary_stats = stats, monthly_data = monthly_data))
}

sensitivity_discount_rate <- function(strategy_A, strategy_B,
                                      values = seq(0.01, 0.03, by = 0.005), n_sims = 100, params = sim_params) {
  results <- data.frame()
  for (dr in values) {
    tp <- params
    tp$center_A$discount_rate_monthly <- dr; tp$center_B$discount_rate_monthly <- dr * 1.188
    npv_a <- npv_b <- numeric(n_sims)
    for (sim in 1:n_sims) {
      sr <- run_single_simulation(strategy_A, strategy_B, params = tp)
      npv_a[sim] <- sr$npv_A[tp$n_months]; npv_b[sim] <- sr$npv_B[tp$n_months]
    }
    results <- rbind(results, data.frame(discount_rate = dr, annual_rate = (1 + dr)^12 - 1,
                                         mean_npv_A = mean(npv_a), mean_npv_B = mean(npv_b),
                                         sd_npv_A = sd(npv_a), sd_npv_B = sd(npv_b)))
  }
  return(results)
}

sensitivity_shock_probability <- function(strategy_A, strategy_B,
                                          values = seq(0.00, 0.20, by = 0.02), n_sims = 100, params = sim_params) {
  results <- data.frame()
  for (sp in values) {
    tp <- params
    tp$shock_probability <- sp
    npv_a <- npv_b <- numeric(n_sims)
    for (sim in 1:n_sims) {
      sr <- run_single_simulation(strategy_A, strategy_B, params = tp)
      npv_a[sim] <- sr$npv_A[tp$n_months]; npv_b[sim] <- sr$npv_B[tp$n_months]
    }
    results <- rbind(results, data.frame(shock_probability = sp,
                                         mean_npv_A = mean(npv_a), mean_npv_B = mean(npv_b),
                                         sd_npv_A = sd(npv_a), sd_npv_B = sd(npv_b)))
  }
  return(results)
}

sensitivity_base_demand <- function(strategy_A, strategy_B,
                                    values = seq(80, 180, by = 20), n_sims = 100, params = sim_params) {
  results <- data.frame()
  for (bd in values) {
    tp <- params
    tp$base_demand <- bd
    npv_a <- npv_b <- numeric(n_sims)
    for (sim in 1:n_sims) {
      sr <- run_single_simulation(strategy_A, strategy_B, params = tp)
      npv_a[sim] <- sr$npv_A[tp$n_months]; npv_b[sim] <- sr$npv_B[tp$n_months]
    }
    results <- rbind(results, data.frame(base_demand = bd,
                                         mean_npv_A = mean(npv_a), mean_npv_B = mean(npv_b),
                                         sd_npv_A = sd(npv_a), sd_npv_B = sd(npv_b)))
  }
  return(results)
}

sensitivity_burnout <- function(strategy_A, strategy_B,
                                thresholds = seq(0.60, 0.95, by = 0.05), n_sims = 100, params = sim_params) {
  results <- data.frame()
  for (bt in thresholds) {
    tp <- params; tp$burnout_threshold <- bt
    npv_a <- npv_b <- sat_a <- sat_b <- numeric(n_sims)
    for (sim in 1:n_sims) {
      sr <- run_single_simulation(strategy_A, strategy_B, params = tp)
      npv_a[sim] <- sr$npv_A[tp$n_months]; npv_b[sim] <- sr$npv_B[tp$n_months]
      sat_a[sim] <- mean(sr$satisfaction_A); sat_b[sim] <- mean(sr$satisfaction_B)
    }
    results <- rbind(results, data.frame(burnout_threshold = bt,
                                         mean_npv_A = mean(npv_a), mean_npv_B = mean(npv_b),
                                         mean_satisfaction_A = mean(sat_a), mean_satisfaction_B = mean(sat_b)))
  }
  return(results)
}

verify_game_theory_properties <- function(payoff_A, payoff_B) {
  cat("\n== OYUN TEORISI DOGRULAMA ==\n\n")
  cat("1. Sifir toplamli mi?\n")
  total <- payoff_A + payoff_B
  cat(sprintf("   Hayir (isbirligi mumkun). Toplam refah aralik: %.2f - %.2fM TL\n",
              min(total)/1e6, max(total)/1e6))
  cat("\n2. Nash dengesi:\n")
  nash <- find_nash_equilibrium(payoff_A, payoff_B)
  for (ne in nash) {
    cat(sprintf("   (S%d, S%d) -> A=%.2fM, B=%.2fM TL\n",
                ne$strategy_A, ne$strategy_B, ne$payoff_A/1e6, ne$payoff_B/1e6))
  }
  cat("\n3. Mahkum Ikilemi:\n")
  pd <- check_prisoners_dilemma_condition(payoff_A, payoff_B)
  cat(sprintf("   %s\n", pd$condition_string))
  cat(sprintf("   Kosul saglaniyor mu: %s\n", ifelse(pd$is_prisoners_dilemma, "EVET", "HAYIR")))
  cat(sprintf("   Isbirligi surdurulebilir mi: %s (%s)\n",
              ifelse(pd$cooperation_sustainable, "EVET", "HAYIR"), pd$cooperation_condition))
  cat("\n4. Pareto optimal sonuclar:\n")
  pareto <- find_pareto_optimal(payoff_A, payoff_B)
  for (k in 1:nrow(pareto)) {
    cat(sprintf("   (S%d, S%d) -> A=%.2fM, B=%.2fM, Toplam=%.2fM TL\n",
                pareto$strategy_A[k], pareto$strategy_B[k],
                pareto$payoff_A[k]/1e6, pareto$payoff_B[k]/1e6,
                (pareto$payoff_A[k] + pareto$payoff_B[k])/1e6))
  }
  cat("\n5. En iyi yanit fonksiyonlari:\n")
  cat("   A icin:\n")
  for (j in 1:ncol(payoff_A)) {
    bi <- which.max(payoff_A[, j])
    cat(sprintf("      B=S%d -> A=S%d (NBD: %.2fM)\n", j, bi, payoff_A[bi, j]/1e6))
  }
  cat("   B icin:\n")
  for (i in 1:nrow(payoff_B)) {
    bj <- which.max(payoff_B[i, ])
    cat(sprintf("      A=S%d -> B=S%d (NBD: %.2fM)\n", i, bj, payoff_B[i, bj]/1e6))
  }
  cat("\n6. Verimlilik kaybi:\n")
  if (length(nash) > 0) {
    nash_total <- nash[[1]]$payoff_A + nash[[1]]$payoff_B
    max_total <- max(total)
    loss <- (max_total - nash_total) / max_total * 100
    cat(sprintf("   Price of Anarchy: %.1f%%\n", loss))
  }
  return(invisible(NULL))
}

export_results_to_csv <- function(analysis_results, output_dir = 'results') {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  payoff_df <- expand.grid(Strategy_A = 1:4, Strategy_B = 1:4)
  payoff_df$NPV_A <- as.vector(analysis_results$payoff_matrices$payoff_A)
  payoff_df$NPV_B <- as.vector(analysis_results$payoff_matrices$payoff_B)
  payoff_df$NPV_A_SD <- as.vector(analysis_results$payoff_matrices$payoff_A_sd)
  payoff_df$NPV_B_SD <- as.vector(analysis_results$payoff_matrices$payoff_B_sd)
  payoff_df$Total_NPV <- payoff_df$NPV_A + payoff_df$NPV_B
  write.csv(payoff_df, file.path(output_dir, 'payoff_matrix.csv'), row.names = FALSE)
  write.csv(analysis_results$tournament$rankings, file.path(output_dir, 'tournament_rankings.csv'), row.names = FALSE)
  write.csv(analysis_results$tournament$results_matrix, file.path(output_dir, 'tournament_full_matrix.csv'))
  write.csv(analysis_results$mc_coop$results, file.path(output_dir, 'monte_carlo_coop_results.csv'), row.names = FALSE)
  write.csv(analysis_results$mc_nash$results, file.path(output_dir, 'monte_carlo_nash_results.csv'), row.names = FALSE)
  write.csv(analysis_results$sens_discount, file.path(output_dir, 'sensitivity_discount_rate.csv'), row.names = FALSE)
  write.csv(analysis_results$sens_shock, file.path(output_dir, 'sensitivity_shock_prob.csv'), row.names = FALSE)
  write.csv(analysis_results$sens_demand, file.path(output_dir, 'sensitivity_base_demand.csv'), row.names = FALSE)
  write.csv(analysis_results$sens_burnout, file.path(output_dir, 'sensitivity_burnout.csv'), row.names = FALSE)
  if (length(analysis_results$mc_coop$monthly_data) > 0) {
    write.csv(analysis_results$mc_coop$monthly_data[[1]], file.path(output_dir, 'sample_simulation_monthly.csv'), row.names = FALSE)
  }
  cat(sprintf('CSV dosyalari %s klasorune kaydedildi.\n', output_dir))
}

run_extended_strategy_analysis <- function(n_sims = 200, params = sim_params) {
  all_strategies <- list(
    S1_Sabit = create_fixed_strategy(price = 1, marketing = 1),
    S2_Sabit = create_fixed_strategy(price = 2, marketing = 1),
    S3_Sabit = create_fixed_strategy(price = 3, marketing = 1),
    S4_Sabit = create_fixed_strategy(price = 4, marketing = 1),
    S1_Paz0 = create_fixed_strategy(price = 1, marketing = 0),
    S1_Paz2 = create_fixed_strategy(price = 1, marketing = 2),
    S2_Paz0 = create_fixed_strategy(price = 2, marketing = 0),
    S2_Paz2 = create_fixed_strategy(price = 2, marketing = 2),
    S3_Paz0 = create_fixed_strategy(price = 3, marketing = 0),
    S3_Paz2 = create_fixed_strategy(price = 3, marketing = 2),
    S4_Paz0 = create_fixed_strategy(price = 4, marketing = 0),
    S4_Paz2 = create_fixed_strategy(price = 4, marketing = 2),
    TitForTat_Std = create_tit_for_tat_strategy(),
    TitForTat_Agg = create_tit_for_tat_strategy(base_price = 2, retaliation_price = 1),
    TitForTat_Prm = create_tit_for_tat_strategy(base_price = 4, retaliation_price = 2),
    Forgiving_TfT = create_forgiving_tft_strategy(),
    Seasonal = create_seasonal_strategy(),
    Adaptive = create_adaptive_strategy(),
    SatAware = create_satisfaction_aware_strategy(),
    GrimTrigger = create_grim_trigger_strategy(),
    Alternating = create_alternating_strategy(),
    Endgame = create_endgame_aggressive_strategy())
  n <- length(all_strategies); nms <- names(all_strategies)
  rmat <- matrix(0, nrow = n, ncol = n); rownames(rmat) <- nms; colnames(rmat) <- nms
  total <- n * n; current <- 0
  for (i in 1:n) { for (j in 1:n) {
    current <- current + 1
    if (current %% 50 == 0) cat(sprintf('Ilerleme: %d/%d (%.0f%%)\n', current, total, current/total*100))
    npvs <- numeric(n_sims)
    for (sim in 1:n_sims) {
      r <- run_single_simulation(all_strategies[[i]], all_strategies[[j]], params = params)
      npvs[sim] <- r$npv_A[params$n_months]
    }
    rmat[i, j] <- mean(npvs)
  }}
  avg <- rowMeans(rmat)
  rankings <- data.frame(strategy = nms, average_npv = avg, rank = rank(-avg))
  rankings <- rankings[order(rankings$rank), ]
  return(list(results_matrix = rmat, rankings = rankings))
}

run_comprehensive_analysis <- function(n_sims = 1000, n_tourn = 100, params = sim_params) {
  cat("\n==== SAGLIKLI YASAM MERKEZI SIMULASYONU ====\n")
  cat("==== KAPSAMLI ANALIZ ====\n\n")
  
  cat("1/12 Getiri matrisi...\n")
  payoff <- calculate_payoff_matrix(n_simulations = n_tourn, params = params)
  cat("\nGetiri Matrisi (A / B, Milyon TL):\n")
  for (i in 1:4) { for (j in 1:4) {
    cat(sprintf('  S%d vs S%d: %.2f / %.2f\n', i, j, payoff$payoff_A[i,j]/1e6, payoff$payoff_B[i,j]/1e6))
  }}
  
  cat("\n2/12 Nash dengesi...\n")
  nash <- find_nash_equilibrium(payoff$payoff_A, payoff$payoff_B)
  for (ne in nash) cat(sprintf('  Nash: (S%d, S%d) -> A=%.2fM, B=%.2fM\n',
                               ne$strategy_A, ne$strategy_B, ne$payoff_A/1e6, ne$payoff_B/1e6))
  
  cat("\n3/12 Pareto...\n")
  pareto <- find_pareto_optimal(payoff$payoff_A, payoff$payoff_B)
  
  cat("\n4/12 Mahkum Ikilemi...\n")
  pd <- check_prisoners_dilemma_condition(payoff$payoff_A, payoff$payoff_B)
  cat(sprintf("  %s\n  PD: %s, Coop: %s\n", pd$condition_string,
              ifelse(pd$is_prisoners_dilemma, "EVET", "HAYIR"),
              ifelse(pd$cooperation_sustainable, "EVET", "HAYIR")))
  
  cat("\n5/12 Turnuva...\n")
  strats <- list(
    create_fixed_strategy(1, 1), create_fixed_strategy(2, 1),
    create_fixed_strategy(3, 1), create_fixed_strategy(4, 1),
    create_tit_for_tat_strategy(), create_seasonal_strategy(),
    create_adaptive_strategy(), create_grim_trigger_strategy(),
    create_forgiving_tft_strategy(), create_satisfaction_aware_strategy())
  snames <- c("S1_Sabit", "S2_Sabit", "S3_Sabit", "S4_Sabit",
              "Tit_for_Tat", "Seasonal", "Adaptive", "Grim_Trigger",
              "Forgiving_TfT", "SatAware")
  tournament <- run_tournament(strats, snames, n_simulations = n_tourn, params = params)
  cat('\nTurnuva siralamai:\n')
  print(tournament$rankings)
  
  cat("\n6/12 Monte Carlo (S3 vs S3)...\n")
  s_coop <- create_fixed_strategy(3, 1)
  mc_coop <- run_monte_carlo_analysis(s_coop, s_coop, n_simulations = n_sims, params = params)
  cat(sprintf('  A: %.2fM (SD: %.2fM), B: %.2fM (SD: %.2fM)\n',
              mc_coop$summary_stats$npv_A$mean/1e6, mc_coop$summary_stats$npv_A$sd/1e6,
              mc_coop$summary_stats$npv_B$mean/1e6, mc_coop$summary_stats$npv_B$sd/1e6))
  cat(sprintf('  Kazanma: A=%.1f%%, B=%.1f%%\n',
              mc_coop$summary_stats$win_rate_A*100, mc_coop$summary_stats$win_rate_B*100))
  
  cat("\n7/12 Monte Carlo (S2 vs S2)...\n")
  s_nash <- create_fixed_strategy(2, 1)
  mc_nash <- run_monte_carlo_analysis(s_nash, s_nash, n_simulations = n_sims, params = params)
  cat(sprintf('  A: %.2fM (SD: %.2fM), B: %.2fM (SD: %.2fM)\n',
              mc_nash$summary_stats$npv_A$mean/1e6, mc_nash$summary_stats$npv_A$sd/1e6,
              mc_nash$summary_stats$npv_B$mean/1e6, mc_nash$summary_stats$npv_B$sd/1e6))
  
  cat("\n8/12 Monte Carlo (TfT vs TfT)...\n")
  s_tft <- create_tit_for_tat_strategy()
  mc_tft <- run_monte_carlo_analysis(s_tft, s_tft, n_simulations = n_sims, params = params)
  cat(sprintf('  A: %.2fM (SD: %.2fM), B: %.2fM (SD: %.2fM)\n',
              mc_tft$summary_stats$npv_A$mean/1e6, mc_tft$summary_stats$npv_A$sd/1e6,
              mc_tft$summary_stats$npv_B$mean/1e6, mc_tft$summary_stats$npv_B$sd/1e6))
  
  cat("\n9/12 Duyarlilik: iskonto orani...\n")
  sens_disc <- sensitivity_discount_rate(s_coop, s_coop, params = params)
  cat("\n10/12 Duyarlilik: sok olasiligi...\n")
  sens_shk <- sensitivity_shock_probability(s_coop, s_coop, params = params)
  cat("\n11/12 Duyarlilik: baz talep...\n")
  sens_dem <- sensitivity_base_demand(s_coop, s_coop, params = params)
  cat("\n12/12 Duyarlilik: tukenmislik esigi...\n")
  sens_brn <- sensitivity_burnout(s_coop, s_coop, params = params)
  
  cat("\n==== ANALIZ TAMAMLANDI ====\n")
  return(list(payoff_matrices = payoff, nash_equilibria = nash,
              pareto_optimal = pareto, prisoners_dilemma = pd,
              tournament = tournament, mc_coop = mc_coop, mc_nash = mc_nash, mc_tft = mc_tft,
              sens_discount = sens_disc, sens_shock = sens_shk,
              sens_demand = sens_dem, sens_burnout = sens_brn))
}

cat("Simulasyon kodu yuklendi.\n")
cat("Analiz icin: results <- run_comprehensive_analysis()\n")
cat("CSV cikti: export_results_to_csv(results)\n")
cat("Genisletilmis: extended <- run_extended_strategy_analysis()\n")
cat("Dogrulama: verify_game_theory_properties(payoff_A, payoff_B)\n")
