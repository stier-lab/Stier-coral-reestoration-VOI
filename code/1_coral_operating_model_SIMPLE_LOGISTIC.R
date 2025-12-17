# ==============================================================================
# CORAL POPULATION DYNAMICS - BIOLOGICALLY REALISTIC OPERATING MODEL
# ==============================================================================
# Simple logistic growth with stochastic mortality
# NO ALLEE EFFECT (not realistic for coral cover)
#
# dC/dt = r*C*(1 - C/K) - m(t)*C + R
#
# Where:
#   C = Coral cover (%)
#   r = Intrinsic growth rate (recruitment + extension - baseline mortality)
#   K = Carrying capacity (space-limited maximum cover)
#   m(t) = Stochastic mortality rate (bleaching, COTS, storms)
#   R = Restoration effort (coral outplanting)
#
# KEY DIFFERENCE FROM FISHERIES MODEL:
#   - No Allee effect (corals recruit from regional larval pool)
#   - Mortality events are THE key source of uncertainty
#   - Monitoring value = early detection + targeted response
# ==============================================================================

source("code/0_libraries.R")

# ==============================================================================
# CORAL GROWTH FUNCTION
# ==============================================================================

coral.dynamics <- function(C, K, r, m, R){
  # Logistic growth
  growth <- r * C * (1 - C/K)

  # Mortality (baseline + events)
  mortality <- m * C

  # Restoration
  restoration <- R

  # Net change
  dC <- growth - mortality + restoration

  return(dC)
}

# ==============================================================================
# EQUILIBRIUM ANALYSIS
# ==============================================================================

# Equilibrium coral cover for given restoration effort and mortality
equilibrium.cover <- function(R, K, r, m){
  # At equilibrium: growth - mortality + restoration = 0
  # r*C*(1 - C/K) - m*C + R = 0
  # r*C - r*C^2/K - m*C + R = 0
  # -r/K * C^2 + (r - m)*C + R = 0

  # Quadratic formula
  a <- -r/K
  b <- r - m
  c <- R

  discriminant <- b^2 - 4*a*c

  if(discriminant < 0) {
    return(0)  # No equilibrium (extinction)
  }

  # Take positive root
  C.eq <- (-b + sqrt(discriminant)) / (2*a)

  # Bound by carrying capacity
  return(min(K, max(0, C.eq)))
}

# ==============================================================================
# PUBLICATION-QUALITY VISUALIZATION
# ==============================================================================

# Set publication theme
theme_pub <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.title = element_text(size = base_size + 2, face = "bold"),
      axis.text = element_text(size = base_size, color = "black"),
      plot.title = element_text(size = base_size + 4, face = "bold", hjust = 0),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      legend.position = "right",
      legend.key.size = unit(1.2, "lines"),
      strip.background = element_rect(fill = "gray95", color = "black"),
      strip.text = element_text(size = base_size, face = "bold")
    )
}

# Color palette (colorblind-friendly)
colors.pub <- c(
  "growth" = "#0072B2",      # Blue
  "mortality" = "#D55E00",   # Orange-red
  "restoration" = "#009E73", # Green
  "equilibrium" = "#CC79A7", # Purple
  "capacity" = "#999999"     # Gray
)

# ==============================================================================
# FIGURE 1: Coral Dynamics Under Different Mortality Scenarios
# ==============================================================================

create.dynamics.plot <- function(K = 60, r = 0.10, save = FALSE) {

  C.vec <- seq(0, K, length.out = 200)

  # Different mortality scenarios
  m.low <- 0.05      # Baseline only
  m.medium <- 0.15   # Baseline + occasional disturbance
  m.high <- 0.25     # Baseline + frequent disturbance

  # Calculate growth rate for each scenario
  df.dynamics <- data.frame(
    cover = rep(C.vec, 4),
    rate = c(
      sapply(C.vec, function(C) r * C * (1 - C/K)),  # Growth only
      sapply(C.vec, function(C) coral.dynamics(C, K, r, m.low, 0)),
      sapply(C.vec, function(C) coral.dynamics(C, K, r, m.medium, 0)),
      sapply(C.vec, function(C) coral.dynamics(C, K, r, m.high, 0))
    ),
    scenario = rep(c("Growth only (no mortality)",
                     "Low mortality (5%/yr)",
                     "Medium mortality (15%/yr)",
                     "High mortality (25%/yr)"),
                   each = length(C.vec))
  )

  df.dynamics$scenario <- factor(df.dynamics$scenario,
                                  levels = c("Growth only (no mortality)",
                                            "Low mortality (5%/yr)",
                                            "Medium mortality (15%/yr)",
                                            "High mortality (25%/yr)"))

  p <- ggplot(df.dynamics, aes(x = cover, y = rate, color = scenario, linetype = scenario)) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    scale_color_manual(values = c("#0072B2", "#009E73", "#E69F00", "#D55E00")) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "solid")) +
    labs(
      title = "Coral Population Dynamics",
      subtitle = "Logistic growth with varying mortality rates",
      x = "Coral Cover (%)",
      y = "Rate of Change (% per year)",
      color = "Scenario",
      linetype = "Scenario"
    ) +
    theme_pub(14) +
    theme(legend.position.inside = c(0.65, 0.80),
          legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5))

  if (save) {
    ggsave("output/figures/main/fig1_coral_dynamics.pdf", p,
           width = 8, height = 6, units = "in", dpi = 300)
    ggsave("output/figures/main/fig1_coral_dynamics.png", p,
           width = 8, height = 6, units = "in", dpi = 300)
  }

  return(p)
}

# ==============================================================================
# FIGURE 2: Equilibrium Cover vs. Restoration Effort
# ==============================================================================

create.equilibrium.plot <- function(K = 60, r = 0.10, save = FALSE) {

  R.vec <- seq(0, 5, length.out = 100)

  # Different mortality scenarios
  mortality.scenarios <- data.frame(
    scenario = c("Low (5%)", "Medium (15%)", "High (25%)"),
    m = c(0.05, 0.15, 0.25)
  )

  df.eq <- do.call(rbind, lapply(1:nrow(mortality.scenarios), function(i) {
    data.frame(
      R = R.vec,
      cover = sapply(R.vec, function(R) equilibrium.cover(R, K, r, mortality.scenarios$m[i])),
      scenario = mortality.scenarios$scenario[i]
    )
  }))

  df.eq$scenario <- factor(df.eq$scenario, levels = c("Low (5%)", "Medium (15%)", "High (25%)"))

  p <- ggplot(df.eq, aes(x = R, y = cover, color = scenario)) +
    geom_line(linewidth = 1.3) +
    geom_hline(yintercept = K, linetype = "dashed", color = "gray40",
               linewidth = 0.8, alpha = 0.7) +
    annotate("text", x = 4.5, y = K + 2, label = "Carrying capacity",
             color = "gray30", size = 4, fontface = "italic") +
    scale_color_manual(values = c("#009E73", "#E69F00", "#D55E00")) +
    labs(
      title = "Equilibrium Coral Cover vs. Restoration Effort",
      subtitle = "Higher mortality requires more restoration to maintain cover",
      x = "Restoration Effort (% cover added per year)",
      y = "Equilibrium Coral Cover (%)",
      color = "Mortality Scenario"
    ) +
    theme_pub(14) +
    theme(legend.position.inside = c(0.75, 0.25),
          legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5))

  if (save) {
    ggsave("output/figures/main/fig2_equilibrium_vs_restoration.pdf", p,
           width = 8, height = 6, units = "in", dpi = 300)
    ggsave("output/figures/main/fig2_equilibrium_vs_restoration.png", p,
           width = 8, height = 6, units = "in", dpi = 300)
  }

  return(p)
}

# ==============================================================================
# FIGURE 3: Economics - NPV vs. Restoration Effort
# ==============================================================================

create.economics.plot <- function(K = 60, r = 0.10, m = 0.15, v = 150,
                                   c.restore = 800, delta = 0.05, save = FALSE) {

  R.vec <- seq(0, 5, length.out = 100)

  # Calculate equilibrium cover for each R
  C.eq <- sapply(R.vec, function(R) equilibrium.cover(R, K, r, m))

  # Annual ecosystem value
  annual.value <- v * C.eq

  # Annual restoration cost
  annual.cost <- c.restore * R.vec

  # Net present value (perpetuity)
  NPV <- (annual.value - annual.cost) / delta

  # Find optimal
  optimal.idx <- which.max(NPV)
  R.optimal <- R.vec[optimal.idx]
  NPV.optimal <- NPV[optimal.idx]

  df.econ <- data.frame(
    R = rep(R.vec, 3),
    value = c(NPV, annual.value, annual.cost),
    type = rep(c("Net Present Value", "Annual Ecosystem Value", "Annual Restoration Cost"),
               each = length(R.vec))
  )

  df.econ$type <- factor(df.econ$type,
                          levels = c("Net Present Value", "Annual Ecosystem Value",
                                    "Annual Restoration Cost"))

  p <- ggplot(df.econ, aes(x = R, y = value, color = type)) +
    geom_line(linewidth = 1.3) +
    geom_vline(xintercept = R.optimal, linetype = "dashed", color = "black",
               linewidth = 0.8, alpha = 0.6) +
    annotate("text", x = R.optimal + 0.5, y = max(NPV) * 0.9,
             label = sprintf("Optimal R = %.2f", R.optimal),
             color = "black", size = 4, fontface = "bold") +
    scale_color_manual(values = c("#0072B2", "#009E73", "#D55E00")) +
    labs(
      title = "Restoration Economics",
      subtitle = sprintf("Optimal effort balances ecosystem value against restoration cost (m = %.0f%%)", m*100),
      x = "Restoration Effort (% cover per year)",
      y = "Value ($)",
      color = ""
    ) +
    theme_pub(14) +
    theme(legend.position.inside = c(0.70, 0.20),
          legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5))

  if (save) {
    ggsave("output/figures/main/fig3_restoration_economics.pdf", p,
           width = 8, height = 6, units = "in", dpi = 300)
    ggsave("output/figures/main/fig3_restoration_economics.png", p,
           width = 8, height = 6, units = "in", dpi = 300)
  }

  return(p)
}

# ==============================================================================
# RUN VISUALIZATIONS
# ==============================================================================

cat("\nGenerating publication-quality figures...\n\n")

# Generate all three main figures
p1 <- create.dynamics.plot(save = TRUE)
p2 <- create.equilibrium.plot(save = TRUE)
p3 <- create.economics.plot(save = TRUE)

print(p1)
print(p2)
print(p3)

cat("\nFigures saved to output/figures/main/\n")
cat("  - fig1_coral_dynamics.pdf/png\n")
cat("  - fig2_equilibrium_vs_restoration.pdf/png\n")
cat("  - fig3_restoration_economics.pdf/png\n\n")

# ==============================================================================
# KEY INSIGHTS:
# ==============================================================================
cat("==============================================================================\n")
cat("KEY INSIGHTS FROM CORAL DYNAMICS MODEL\n")
cat("==============================================================================\n\n")

cat("1. NO ALLEE EFFECT: Coral cover has simple logistic growth\n")
cat("   - Growth maximized at intermediate cover (~30% of K)\n")
cat("   - No critical threshold for collapse\n\n")

cat("2. MORTALITY EVENTS ARE KEY:\n")
cat("   - Stochastic mortality (bleaching, COTS, storms) drives uncertainty\n")
cat("   - This is WHERE MONITORING MATTERS\n")
cat("   - Early detection → targeted restoration response\n\n")

cat("3. RESTORATION CAN STABILIZE COVER:\n")
cat("   - Higher mortality → more restoration needed for equilibrium\n")
cat("   - Optimal effort balances ecosystem value vs. cost\n\n")

cat("4. VALUE OF INFORMATION:\n")
cat("   - High precision: Detect mortality events early\n")
cat("   - Low precision: Miss early warnings, inefficient restoration\n")
cat("   - VOI = Benefit of early detection + targeted response\n\n")

cat("==============================================================================\n")
