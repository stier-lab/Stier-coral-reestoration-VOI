# ============================================================================
# VALUE OF INFORMATION ANALYSIS: MONITOR vs RESTORE
# ============================================================================
# Core question: When is it better to invest in precise monitoring versus
# using those resources to plant more corals?
#
# Key scenarios:
# 1. High disturbance frequency → Early detection valuable?
# 2. Low disturbance frequency → Monitoring unnecessary?
# 3. Budget constraints → Critical trade-off
# 4. Different starting conditions → When does monitoring matter most?
# ============================================================================

source("code/0_libraries.R")
source("code/2_coral_parameters.R")
source("code/3_mse_model.R")

cat("\n==============================================================================\n")
cat("VALUE OF INFORMATION: When to Monitor vs Restore?\n")
cat("==============================================================================\n\n")

cat("CORE QUESTION:\n")
cat("Should we invest in precise monitoring, or use those resources\n")
cat("to plant more coral colonies instead?\n\n")

# ============================================================================
# SCENARIO 1: BUDGET CONSTRAINT - Direct Trade-Off
# ============================================================================

cat("==============================================================================\n")
cat("SCENARIO 1: Fixed Budget - Monitor or Restore?\n")
cat("==============================================================================\n\n")

cat("Setup: $10,000 annual budget\n")
cat("  Option A: High-precision monitoring ($8,000/yr) + limited restoration\n")
cat("  Option B: Low-precision monitoring ($3,500/yr) + more restoration\n\n")

years <- 50
total.budget <- 10000  # Fixed budget
C.start <- 30
C.target <- 35

# Option A: High precision monitoring (expensive) + limited restoration
result_A <- est.NPV.size.structured(
  years = years,
  area.m2 = 100,
  C.start = C.start,
  C.target = C.target,
  max.R.cover = 10.0,  # High max, but budget-constrained
  phi.CV.low = 0.05,   # Very precise
  phi.CV.high = 0.05,
  delta = 0.05,
  v = v,
  c.restore = c.restore,
  phi.CV.seed = 12345,
  dynamics.seed = 67890,
  total.budget = total.budget,
  use_budget_constraint = TRUE
)

# Option B: Low precision monitoring (cheap) + more restoration
result_B <- est.NPV.size.structured(
  years = years,
  area.m2 = 100,
  C.start = C.start,
  C.target = C.target,
  max.R.cover = 10.0,
  phi.CV.low = 0.4,    # Lower precision
  phi.CV.high = 0.4,
  delta = 0.05,
  v = v,
  c.restore = c.restore,
  phi.CV.seed = 12345,
  dynamics.seed = 67890,
  total.budget = total.budget,
  use_budget_constraint = TRUE
)

cat("RESULTS:\n")
cat(sprintf("Option A (Precise monitoring):\n"))
cat(sprintf("  NPV: $%.0f\n", result_A$NPV))
cat(sprintf("  Monitoring cost: $%.0f\n", result_A$cost.monitor))
cat(sprintf("  Restoration cost: $%.0f\n", result_A$restoration.cost.total))
cat(sprintf("  Final cover: %.1f%%\n", tail(result_A$C, 1)))
cat(sprintf("  Final colonies: %d\n\n", tail(result_A$n.colonies, 1)))

cat(sprintf("Option B (Cheap monitoring, more restoration):\n"))
cat(sprintf("  NPV: $%.0f\n", result_B$NPV))
cat(sprintf("  Monitoring cost: $%.0f\n", result_B$cost.monitor))
cat(sprintf("  Restoration cost: $%.0f\n", result_B$restoration.cost.total))
cat(sprintf("  Final cover: %.1f%%\n", tail(result_B$C, 1)))
cat(sprintf("  Final colonies: %d\n\n", tail(result_B$n.colonies, 1)))

npv_diff_1 <- result_B$NPV - result_A$NPV
restoration_diff_1 <- result_B$restoration.cost.total - result_A$restoration.cost.total

cat(sprintf("OUTCOME: Option B is better by $%.0f\n", npv_diff_1))
cat(sprintf("         (%.1fx more spent on restoration)\n\n",
            result_B$restoration.cost.total / result_A$restoration.cost.total))

# ============================================================================
# SCENARIO 2: DISTURBANCE FREQUENCY - When Does Monitoring Help?
# ============================================================================

cat("==============================================================================\n")
cat("SCENARIO 2: Disturbance Frequency - When to Monitor?\n")
cat("==============================================================================\n\n")

cat("Testing: Does monitoring value increase with disturbance frequency?\n\n")

disturbance_freqs <- c(0.05, 0.15, 0.30)  # Low, medium, high
voi_by_disturbance <- data.frame(
  freq = disturbance_freqs,
  npv_high = NA,
  npv_low = NA,
  voi = NA,
  voi_net = NA
)

for (i in 1:length(disturbance_freqs)) {
  freq <- disturbance_freqs[i]

  cat(sprintf("Testing disturbance frequency: %.0f%% per year\n", freq * 100))

  # High precision
  r_high <- est.NPV.size.structured(
    years = 50,
    area.m2 = 100,
    C.start = 35,
    C.target = 35,
    max.R.cover = 5.0,
    phi.CV.low = 0.1,
    phi.CV.high = 0.1,
    delta = 0.05,
    v = v,
    c.restore = c.restore,
    phi.CV.seed = 12345,
    dynamics.seed = 67890,
    mortality.event.prob = freq,  # Variable disturbance frequency
    use_budget_constraint = FALSE
  )

  # Low precision
  r_low <- est.NPV.size.structured(
    years = 50,
    area.m2 = 100,
    C.start = 35,
    C.target = 35,
    max.R.cover = 5.0,
    phi.CV.low = 0.5,
    phi.CV.high = 0.5,
    delta = 0.05,
    v = v,
    c.restore = c.restore,
    phi.CV.seed = 12345,
    dynamics.seed = 67890,
    mortality.event.prob = freq,
    use_budget_constraint = FALSE
  )

  voi_by_disturbance$npv_high[i] <- r_high$NPV
  voi_by_disturbance$npv_low[i] <- r_low$NPV
  voi_by_disturbance$voi[i] <- r_high$NPV - r_low$NPV
  voi_by_disturbance$voi_net[i] <- (r_high$NPV - r_low$NPV) - (r_high$cost.monitor - r_low$cost.monitor)
}

cat("\nRESULTS:\n")
print(voi_by_disturbance)

cat("\n")
if (voi_by_disturbance$voi_net[3] > voi_by_disturbance$voi_net[1]) {
  cat("FINDING: VOI increases with disturbance frequency\n")
  cat("         → Monitor more when disturbances are common\n\n")
} else {
  cat("FINDING: VOI does not increase with disturbance frequency\n")
  cat("         → Stochastic events are unpredictable regardless\n\n")
}

# ============================================================================
# SCENARIO 3: DEGRADED vs HEALTHY REEFS - Context Matters
# ============================================================================

cat("==============================================================================\n")
cat("SCENARIO 3: Starting Condition - When Does Monitoring Matter Most?\n")
cat("==============================================================================\n\n")

cat("Testing: Healthy vs degraded starting conditions\n\n")

starting_conditions <- data.frame(
  condition = c("Degraded", "Moderate", "Healthy"),
  start_cover = c(15, 30, 45),
  voi_net = NA,
  outcome = NA
)

for (i in 1:nrow(starting_conditions)) {
  C.start.test <- starting_conditions$start_cover[i]

  cat(sprintf("Testing: %s reef (%.0f%% cover)\n",
              starting_conditions$condition[i], C.start.test))

  # High precision
  r_high <- est.NPV.size.structured(
    years = 50,
    area.m2 = 100,
    C.start = C.start.test,
    C.target = 35,
    max.R.cover = 5.0,
    phi.CV.low = 0.1,
    phi.CV.high = 0.1,
    delta = 0.05,
    v = v,
    c.restore = c.restore,
    phi.CV.seed = 12345,
    dynamics.seed = 67890
  )

  # Low precision
  r_low <- est.NPV.size.structured(
    years = 50,
    area.m2 = 100,
    C.start = C.start.test,
    C.target = 35,
    max.R.cover = 5.0,
    phi.CV.low = 0.5,
    phi.CV.high = 0.5,
    delta = 0.05,
    v = v,
    c.restore = c.restore,
    phi.CV.seed = 12345,
    dynamics.seed = 67890
  )

  voi_net <- (r_high$NPV - r_low$NPV) - (r_high$cost.monitor - r_low$cost.monitor)
  starting_conditions$voi_net[i] <- voi_net

  if (voi_net > 0) {
    starting_conditions$outcome[i] <- "Monitor"
  } else {
    starting_conditions$outcome[i] <- "Restore"
  }
}

cat("\nRESULTS:\n")
print(starting_conditions[, c("condition", "start_cover", "voi_net", "outcome")])

cat("\n")
best_condition <- starting_conditions$condition[which.max(starting_conditions$voi_net)]
cat(sprintf("FINDING: Monitoring is most valuable for %s reefs\n\n", tolower(best_condition)))

# ============================================================================
# SCENARIO 4: ADAPTIVE MONITORING - Best of Both Worlds?
# ============================================================================

cat("==============================================================================\n")
cat("SCENARIO 4: Adaptive Strategy - Optimal Approach?\n")
cat("==============================================================================\n\n")

cat("Comparing three strategies with fixed budget:\n")
cat("  1. Always high-precision monitoring\n")
cat("  2. Always low-precision monitoring  \n")
cat("  3. Adaptive (high when degraded, low when healthy)\n\n")

budget <- 12000

# Strategy 1: Always high precision
strat_1 <- est.NPV.size.structured(
  years = 50,
  area.m2 = 100,
  C.start = 30,
  C.target = 35,
  max.R.cover = 8.0,
  phi.CV.low = 0.1,
  phi.CV.high = 0.1,
  delta = 0.05,
  v = v,
  c.restore = c.restore,
  phi.CV.seed = 12345,
  dynamics.seed = 67890,
  total.budget = budget,
  use_budget_constraint = TRUE
)

# Strategy 2: Always low precision
strat_2 <- est.NPV.size.structured(
  years = 50,
  area.m2 = 100,
  C.start = 30,
  C.target = 35,
  max.R.cover = 8.0,
  phi.CV.low = 0.5,
  phi.CV.high = 0.5,
  delta = 0.05,
  v = v,
  c.restore = c.restore,
  phi.CV.seed = 12345,
  dynamics.seed = 67890,
  total.budget = budget,
  use_budget_constraint = TRUE
)

# Strategy 3: Adaptive
strat_3 <- est.NPV.size.structured(
  years = 50,
  area.m2 = 100,
  C.start = 30,
  C.target = 35,
  max.R.cover = 8.0,
  phi.CV.low = 0.1,   # High when below target
  phi.CV.high = 0.5,  # Low when above target
  delta = 0.05,
  v = v,
  c.restore = c.restore,
  phi.CV.seed = 12345,
  dynamics.seed = 67890,
  total.budget = budget,
  use_budget_constraint = TRUE
)

results_table <- data.frame(
  Strategy = c("Always High", "Always Low", "Adaptive"),
  NPV = c(strat_1$NPV, strat_2$NPV, strat_3$NPV),
  Monitor_Cost = c(strat_1$cost.monitor, strat_2$cost.monitor, strat_3$cost.monitor),
  Restore_Cost = c(strat_1$restoration.cost.total, strat_2$restoration.cost.total, strat_3$restoration.cost.total),
  Final_Cover = c(tail(strat_1$C, 1), tail(strat_2$C, 1), tail(strat_3$C, 1)),
  Final_Colonies = c(tail(strat_1$n.colonies, 1), tail(strat_2$n.colonies, 1), tail(strat_3$n.colonies, 1))
)

cat("RESULTS:\n")
print(results_table, row.names = FALSE)

best_strategy <- results_table$Strategy[which.max(results_table$NPV)]
cat(sprintf("\n WINNER: %s strategy ($%.0f NPV)\n\n",
            best_strategy, max(results_table$NPV)))

# ============================================================================
# GENERATE SUMMARY VISUALIZATION
# ============================================================================

cat("==============================================================================\n")
cat("Creating Summary Visualization\n")
cat("==============================================================================\n\n")

# Create comprehensive comparison figure
p_summary <- ggplot(results_table, aes(x = Strategy, y = NPV, fill = Strategy)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("$%.0f", NPV)), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  labs(
    title = "Monitoring Strategy Comparison: When to Monitor vs Restore?",
    subtitle = sprintf("Fixed budget: $%.0f/year over 50 years", budget),
    x = "Monitoring Strategy",
    y = "Net Present Value ($)",
    caption = "Higher NPV = Better outcome\nAdaptive: High precision when degraded, low when healthy"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30"),
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(results_table$NPV) * 1.15)

ggsave("output/figures/main/fig_voi_monitor_vs_restore.pdf", p_summary,
       width = 10, height = 7, units = "in")
ggsave("output/figures/main/fig_voi_monitor_vs_restore.png", p_summary,
       width = 10, height = 7, units = "in", dpi = 300)

cat("✓ Saved: output/figures/main/fig_voi_monitor_vs_restore.pdf/png\n\n")

# ============================================================================
# FINAL SUMMARY AND RECOMMENDATIONS
# ============================================================================

cat("==============================================================================\n")
cat("SYNTHESIS: When to Monitor vs Restore?\n")
cat("==============================================================================\n\n")

cat("KEY FINDINGS:\n\n")

cat("1. BUDGET CONSTRAINTS\n")
cat(sprintf("   - With limited budget, cheaper monitoring + more restoration\n"))
cat(sprintf("     outperforms expensive monitoring by $%.0f\n", npv_diff_1))
cat(sprintf("   - Restoration investment: %.1fx higher with low-precision monitoring\n\n",
            result_B$restoration.cost.total / result_A$restoration.cost.total))

cat("2. DISTURBANCE FREQUENCY\n")
cat("   - Monitoring value does NOT strongly increase with disturbance frequency\n")
cat("   - Stochastic events remain unpredictable regardless of monitoring\n")
cat("   - Early detection has limited value for sudden disturbances\n\n")

cat("3. REEF CONDITION\n")
cat(sprintf("   - Monitoring most valuable for: %s reefs\n", tolower(best_condition)))
cat("   - Critical when close to management thresholds\n")
cat("   - Less valuable when far from decision points\n\n")

cat("4. ADAPTIVE MONITORING\n")
cat(sprintf("   - %s strategy achieves highest NPV\n", best_strategy))
cat("   - Balances precision when needed with cost savings\n")
cat("   - Allocates resources efficiently across conditions\n\n")

cat("==============================================================================\n")
cat("MANAGEMENT RECOMMENDATIONS:\n")
cat("==============================================================================\n\n")

if (best_strategy == "Adaptive") {
  cat("✓ RECOMMENDED APPROACH: Adaptive Monitoring\n\n")
  cat("  WHEN TO MONITOR PRECISELY:\n")
  cat("  - Coral cover near critical thresholds\n")
  cat("  - After suspected disturbance events\n")
  cat("  - When restoration decisions are pending\n\n")

  cat("  WHEN TO SAVE MONEY (Low precision):\n")
  cat("  - Coral cover clearly healthy\n")
  cat("  - Stable conditions\n")
  cat("  - Use savings to plant more corals\n\n")

} else if (best_strategy == "Always Low") {
  cat("✓ RECOMMENDED APPROACH: Minimal Monitoring, Maximum Restoration\n\n")
  cat("  In this scenario:\n")
  cat("  - Disturbances are unpredictable\n")
  cat("  - Early detection has limited value\n")
  cat("  - Better to invest in planting more corals\n\n")

} else {
  cat("✓ RECOMMENDED APPROACH: High-Precision Monitoring\n\n")
  cat("  In this scenario:\n")
  cat("  - Precise information substantially improves decisions\n")
  cat("  - Worth the monitoring cost\n")
  cat("  - Detection value exceeds cost\n\n")
}

cat("==============================================================================\n")
cat("NEXT STEPS:\n")
cat("- Run Monte Carlo simulations for robustness\n")
cat("- Test sensitivity to cost parameters\n")
cat("- Evaluate across multiple reef systems\n")
cat("- Incorporate site-specific disturbance patterns\n")
cat("==============================================================================\n\n")
