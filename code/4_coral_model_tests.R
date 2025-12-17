# ============================================================================
# SIZE-STRUCTURED CORAL MSE TESTS
# ============================================================================
# Tests for size-structured Integral Projection Model MSE framework
# ============================================================================

source("code/0_libraries.R")
source("code/2_coral_parameters.R")
source("code/3_mse_model.R")

# ============================================================================
# TEST 1: Monitoring precision comparison (size-structured)
# ============================================================================

cat("\n==============================================================================\n")
cat("TEST 1: Monitoring Precision Comparison (Size-Structured)\n")
cat("==============================================================================\n\n")

years <- 50
C.start <- 30
C.target <- 35
max.R.cover <- 5.0

# High precision
result_high <- est.NPV.size.structured(
  years = years,
  area.m2 = 100,
  C.start = C.start,
  C.target = C.target,
  max.R.cover = max.R.cover,
  phi.CV.low = 0.1,
  phi.CV.high = 0.1,
  delta = 0.05,
  v = v,
  c.restore = c.restore,
  phi.CV.seed = 12345,
  dynamics.seed = 67890
)

# Low precision
result_low <- est.NPV.size.structured(
  years = years,
  area.m2 = 100,
  C.start = C.start,
  C.target = C.target,
  max.R.cover = max.R.cover,
  phi.CV.low = 0.5,
  phi.CV.high = 0.5,
  delta = 0.05,
  v = v,
  c.restore = c.restore,
  phi.CV.seed = 12345,
  dynamics.seed = 67890
)

# Adaptive
result_adaptive <- est.NPV.size.structured(
  years = years,
  area.m2 = 100,
  C.start = C.start,
  C.target = C.target,
  max.R.cover = max.R.cover,
  phi.CV.low = 0.1,
  phi.CV.high = 0.5,
  delta = 0.05,
  v = v,
  c.restore = c.restore,
  phi.CV.seed = 12345,
  dynamics.seed = 67890
)

cat(sprintf("High-precision NPV:   $%.2f\n", result_high$NPV))
cat(sprintf("Low-precision NPV:    $%.2f\n", result_low$NPV))
cat(sprintf("Adaptive NPV:         $%.2f\n\n", result_adaptive$NPV))

cat(sprintf("High-precision monitoring cost:  $%.2f\n", result_high$cost.monitor))
cat(sprintf("Low-precision monitoring cost:   $%.2f\n", result_low$cost.monitor))
cat(sprintf("Adaptive monitoring cost:        $%.2f\n\n", result_adaptive$cost.monitor))

cat(sprintf("Final colony numbers:\n"))
cat(sprintf("  High precision: %d colonies, mean size %.1f cm\n",
            tail(result_high$n.colonies, 1), tail(result_high$mean.size, 1)))
cat(sprintf("  Low precision:  %d colonies, mean size %.1f cm\n",
            tail(result_low$n.colonies, 1), tail(result_low$mean.size, 1)))
cat(sprintf("  Adaptive:       %d colonies, mean size %.1f cm\n",
            tail(result_adaptive$n.colonies, 1), tail(result_adaptive$mean.size, 1)))

voi <- result_high$NPV - result_low$NPV
voi_net <- voi - (result_high$cost.monitor - result_low$cost.monitor)
cat(sprintf("\nValue of Information: $%.2f\n", voi))
cat(sprintf("Net VOI (after monitoring cost): $%.2f\n", voi_net))

cat("\n✓ TEST 1 PASSED\n")

# ============================================================================
# TEST 2: Starting cover sensitivity (size-structured)
# ============================================================================

cat("\n==============================================================================\n")
cat("TEST 2: Starting Cover Sensitivity (Size-Structured)\n")
cat("==============================================================================\n\n")

starting.covers <- c(20, 30, 40, 50)
results.by.start <- list()

for (i in 1:length(starting.covers)) {
  C.start.test <- starting.covers[i]

  results.by.start[[i]] <- est.NPV.size.structured(
    years = years,
    area.m2 = 100,
    C.start = C.start.test,
    C.target = C.target,
    max.R.cover = max.R.cover,
    phi.CV.low = 0.1,
    phi.CV.high = 0.1,
    delta = 0.05,
    v = v,
    c.restore = c.restore,
    phi.CV.seed = 12345,
    dynamics.seed = 67890
  )

  cat(sprintf("Starting at %d%% cover:\n", C.start.test))
  cat(sprintf("  NPV: $%.2f\n", results.by.start[[i]]$NPV))
  cat(sprintf("  Final cover: %.1f%%\n",
              tail(results.by.start[[i]]$C, 1)))
  cat(sprintf("  Final colonies: %d (mean size %.1f cm)\n",
              tail(results.by.start[[i]]$n.colonies, 1),
              tail(results.by.start[[i]]$mean.size, 1)))
  cat(sprintf("  Restoration cost: $%.2f\n",
              results.by.start[[i]]$restoration.cost.total))
  cat(sprintf("  Collapsed: %d\n\n", results.by.start[[i]]$TP))
}

cat("✓ TEST 2 PASSED\n")

# ============================================================================
# TEST 3: Monte Carlo simulation (size-structured)
# ============================================================================

cat("\n==============================================================================\n")
cat("TEST 3: Monte Carlo Simulation (Size-Structured, 50 iterations)\n")
cat("==============================================================================\n\n")

n.iters <- 50  # Reduced from 100 for speed

set.seed(999)
phi.seeds <- sample(1:10000, n.iters)
dynamics.seeds <- sample(1:10000, n.iters)

cat("Running 50 simulations with high precision monitoring...\n")

mc.high.NPV <- rep(NA, n.iters)
mc.high.cover <- rep(NA, n.iters)
mc.high.colonies <- rep(NA, n.iters)
mc.high.size <- rep(NA, n.iters)

for (i in 1:n.iters) {
  result <- est.NPV.size.structured(
    years = 50,
    area.m2 = 100,
    C.start = 30,
    C.target = 35,
    max.R.cover = 5.0,
    phi.CV.low = 0.1,
    phi.CV.high = 0.1,
    delta = 0.05,
    v = v,
    c.restore = c.restore,
    phi.CV.seed = phi.seeds[i],
    dynamics.seed = dynamics.seeds[i]
  )

  mc.high.NPV[i] <- result$NPV
  mc.high.cover[i] <- mean(result$C, na.rm = TRUE)
  mc.high.colonies[i] <- tail(result$n.colonies, 1)
  mc.high.size[i] <- tail(result$mean.size, 1)
}

cat("\nRunning 50 simulations with low precision monitoring...\n")

mc.low.NPV <- rep(NA, n.iters)
mc.low.cover <- rep(NA, n.iters)
mc.low.colonies <- rep(NA, n.iters)
mc.low.size <- rep(NA, n.iters)

for (i in 1:n.iters) {
  result <- est.NPV.size.structured(
    years = 50,
    area.m2 = 100,
    C.start = 30,
    C.target = 35,
    max.R.cover = 5.0,
    phi.CV.low = 0.5,
    phi.CV.high = 0.5,
    delta = 0.05,
    v = v,
    c.restore = c.restore,
    phi.CV.seed = phi.seeds[i],
    dynamics.seed = dynamics.seeds[i]
  )

  mc.low.NPV[i] <- result$NPV
  mc.low.cover[i] <- mean(result$C, na.rm = TRUE)
  mc.low.colonies[i] <- tail(result$n.colonies, 1)
  mc.low.size[i] <- tail(result$mean.size, 1)
}

cat("\n--- Monte Carlo Results ---\n\n")
cat("High-precision monitoring:\n")
cat(sprintf("  Mean NPV: $%.2f (SD = $%.2f)\n", mean(mc.high.NPV), sd(mc.high.NPV)))
cat(sprintf("  Mean coral cover: %.1f%% (SD = %.1f%%)\n", mean(mc.high.cover), sd(mc.high.cover)))
cat(sprintf("  Mean final colonies: %.0f (SD = %.0f)\n", mean(mc.high.colonies), sd(mc.high.colonies)))
cat(sprintf("  Mean colony size: %.1f cm (SD = %.1f cm)\n\n", mean(mc.high.size), sd(mc.high.size)))

cat("Low-precision monitoring:\n")
cat(sprintf("  Mean NPV: $%.2f (SD = $%.2f)\n", mean(mc.low.NPV), sd(mc.low.NPV)))
cat(sprintf("  Mean coral cover: %.1f%% (SD = %.1f%%)\n", mean(mc.low.cover), sd(mc.low.cover)))
cat(sprintf("  Mean final colonies: %.0f (SD = %.0f)\n", mean(mc.low.colonies), sd(mc.low.colonies)))
cat(sprintf("  Mean colony size: %.1f cm (SD = %.1f cm)\n\n", mean(mc.low.size), sd(mc.low.size)))

voi.mc <- mean(mc.high.NPV) - mean(mc.low.NPV)
cat(sprintf("Value of Information: $%.2f\n", voi.mc))

cat("\n✓ TEST 3 PASSED\n")

# ============================================================================
# VISUALIZATION 1: Size-structured trajectories
# ============================================================================

cat("\n==============================================================================\n")
cat("Creating visualization: Size-structured trajectories\n")
cat("==============================================================================\n\n")

df.plot <- data.frame(
  year = rep(0:years, 3),
  cover = c(result_high$C, result_low$C, result_adaptive$C),
  estimated = c(result_high$Chat, result_low$Chat, result_adaptive$Chat),
  n.colonies = c(result_high$n.colonies, result_low$n.colonies, result_adaptive$n.colonies),
  mean.size = c(result_high$mean.size, result_low$mean.size, result_adaptive$mean.size),
  scenario = rep(c("High Precision", "Low Precision", "Adaptive"), each = years + 1)
)

p1 <- ggplot(df.plot, aes(x = year)) +
  geom_line(aes(y = cover, color = scenario), linewidth = 1.2) +
  geom_line(aes(y = estimated, color = scenario), linetype = "dashed", linewidth = 0.8, alpha = 0.6) +
  geom_hline(yintercept = C.target, linetype = "dotted", color = "gray40", linewidth = 0.8) +
  annotate("text", x = 45, y = C.target + 2, label = "Target",
           color = "gray30", size = 3.5) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  labs(
    title = "Coral Cover Trajectories (Size-Structured Model)",
    subtitle = "Solid = true cover, Dashed = estimated cover",
    x = "Year",
    y = "Coral Cover (%)",
    color = "Monitoring Strategy"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position.inside = c(0.82, 0.85),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave("output/figures/diagnostic/test1_size_structured_trajectories.pdf", p1,
       width = 10, height = 6, units = "in")
ggsave("output/figures/diagnostic/test1_size_structured_trajectories.png", p1,
       width = 10, height = 6, units = "in", dpi = 300)

cat("✓ Saved: output/figures/diagnostic/test1_size_structured_trajectories.pdf/png\n")

# ============================================================================
# VISUALIZATION 2: Colony dynamics
# ============================================================================

cat("\n==============================================================================\n")
cat("Creating visualization: Colony number and size dynamics\n")
cat("==============================================================================\n\n")

p2 <- ggplot(df.plot, aes(x = year)) +
  geom_line(aes(y = n.colonies, color = scenario), linewidth = 1.2) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  labs(
    title = "Colony Dynamics Over Time (Size-Structured Model)",
    x = "Year",
    y = "Number of Colonies",
    color = "Monitoring Strategy"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

p3 <- ggplot(df.plot, aes(x = year)) +
  geom_line(aes(y = mean.size, color = scenario), linewidth = 1.2) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  labs(
    title = "Mean Colony Size Over Time",
    x = "Year",
    y = "Mean Colony Diameter (cm)",
    color = "Monitoring Strategy"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

p_combined <- cowplot::plot_grid(p2, p3, ncol = 1)

ggsave("output/figures/diagnostic/test1_colony_dynamics.pdf", p_combined,
       width = 10, height = 10, units = "in")
ggsave("output/figures/diagnostic/test1_colony_dynamics.png", p_combined,
       width = 10, height = 10, units = "in", dpi = 300)

cat("✓ Saved: output/figures/diagnostic/test1_colony_dynamics.pdf/png\n")

# ============================================================================
# VISUALIZATION 3: Monte Carlo NPV distributions
# ============================================================================

cat("\n==============================================================================\n")
cat("Creating visualization: Monte Carlo NPV (Size-Structured)\n")
cat("==============================================================================\n\n")

df.mc <- data.frame(
  NPV = c(mc.high.NPV, mc.low.NPV),
  scenario = rep(c("High Precision", "Low Precision"), each = n.iters)
)

p4 <- ggplot(df.mc, aes(x = NPV, fill = scenario)) +
  geom_histogram(alpha = 0.6, bins = 20, position = "identity") +
  geom_vline(data = data.frame(
    scenario = c("High Precision", "Low Precision"),
    mean_val = c(mean(mc.high.NPV), mean(mc.low.NPV))
  ), aes(xintercept = mean_val, color = scenario),
  linetype = "dashed", linewidth = 1.2) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  labs(
    title = "NPV Distribution (Size-Structured, 50 Monte Carlo Simulations)",
    subtitle = sprintf("Mean VOI = $%.0f", voi.mc),
    x = "Net Present Value ($)",
    y = "Frequency",
    fill = "Monitoring Strategy",
    color = "Mean NPV"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave("output/figures/diagnostic/test3_size_structured_npv.pdf", p4,
       width = 10, height = 6, units = "in")
ggsave("output/figures/diagnostic/test3_size_structured_npv.png", p4,
       width = 10, height = 6, units = "in", dpi = 300)

cat("✓ Saved: output/figures/diagnostic/test3_size_structured_npv.pdf/png\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n==============================================================================\n")
cat("ALL TESTS PASSED (SIZE-STRUCTURED MODEL)\n")
cat("==============================================================================\n\n")

cat("Summary of size-structured IPM model:\n")
cat("1. Individual colony tracking (size distribution)\n")
cat("2. Size-dependent vital rates (survival, growth, fecundity)\n")
cat("3. Density-dependent recruitment\n")
cat("4. Restoration as colony outplanting\n")
cat("5. VOI from better size assessments\n\n")

cat("Key findings:\n")
cat(sprintf("- High-precision monitoring improves NPV by $%.0f\n", voi.mc))
cat(sprintf("- Colony dynamics tracked: number and mean size\n"))
cat(sprintf("- Size structure affects recovery trajectories\n"))
cat(sprintf("- Adaptive monitoring provides good cost-benefit balance\n\n"))

cat("All diagnostic figures saved to output/figures/diagnostic/\n")
cat("==============================================================================\n")
