# ==============================================================================
# CORAL RESTORATION MODEL PARAMETERS - BIOLOGICALLY REALISTIC
# ==============================================================================
# Updated to reflect realistic coral biology without Allee effects
# Based on Indo-Pacific coral systems (e.g., Moorea, GBR)
# ==============================================================================

# ==============================================================================
# CORAL POPULATION DYNAMICS
# ==============================================================================

## Growth Parameters
r.coral <- 0.10          # Intrinsic growth rate (10% per year)
                         # Net of recruitment + extension - baseline mortality
                         # Source: Pratchett et al. (2015), Edmunds (2018)

K.coral <- 60            # Carrying capacity (60% maximum cover)
                         # Reduced from 100% due to:
                         # - Macroalgae competition
                         # - Sand/rubble patches
                         # - Other benthic organisms
                         # Source: Typical for Indo-Pacific reefs

## Mortality Parameters
m.baseline <- 0.05       # Baseline annual mortality (5% per year)
                         # From: disease, predation, minor disturbances

m.event.mean <- 0.35     # Mean mortality during disturbance events (35%)
                         # Can be bleaching, COTS, storms, disease

m.event.sd <- 0.15       # SD of disturbance mortality (15%)
                         # Creates variability in event severity

event.prob.annual <- 0.15  # Probability of disturbance per year (15%)
                           # Approximately once every 6-7 years
                           # Source: Hughes et al. (2018) - bleaching frequency

## Recruitment Threshold (Not an Allee effect!)
C.critical <- 10         # Below 10% cover, regional recruitment may fail
                         # This is a SOFT threshold, not hard Allee effect
                         # Coral can still grow, just slower recruitment

# ==============================================================================
# RESTORATION PARAMETERS
# ==============================================================================

R.max <- 5.0             # Maximum restoration rate (5% cover per year)
                         # Realistic for intensive coral outplanting
                         # Source: Boström-Einarsson et al. (2020)

max.R <- R.max           # Alias for backward compatibility

c.restore <- 800         # Cost per 1% cover restored ($ per year)
                         # Includes: coral nursery, outplanting, monitoring
                         # Source: Bayraktarov et al. (2016) - $100-400k per hectare

# ==============================================================================
# ECONOMIC PARAMETERS
# ==============================================================================

v <- 150                 # Ecosystem service value per % cover per year
                         # Tourism + fisheries + coastal protection
                         # Source: Cesar et al. (2003), Costanza et al. (2014)
                         # Conservative estimate for Indo-Pacific reefs

delta <- 0.05            # Discount rate (5% per year)
                         # Standard for long-term environmental projects

# ==============================================================================
# MONITORING PARAMETERS
# ==============================================================================

## Monitoring Precision Options
phi.CV.high <- 0.05      # High precision (5% CV) - intensive monitoring
                         # Quarterly surveys, multiple divers, many quadrats

phi.CV.low <- 0.30       # Low precision (30% CV) - minimal monitoring
                         # Annual surveys, single diver, few quadrats

## Monitoring Cost Function
# Cost = ci * exp(-cs * CV)
# Higher precision (lower CV) = exponentially more expensive

ci <- 500                    # Intercept for monitoring cost function
                             # Base cost when CV = 0
cs <- 2.0                    # Slope for monitoring cost vs. CV
                             # Controls how fast cost decreases with lower precision

monitoring.base.cost <- 500  # Base monitoring cost ($ per year)
                             # Minimum for any monitoring program

## Adaptive Monitoring Threshold
adaptive.threshold <- 0.6    # Switch to high precision when C < 60% of K
                            # = 36% cover for K=60%
                            # Increased monitoring when reef declining

# ==============================================================================
# SIMULATION PARAMETERS
# ==============================================================================

years <- 50              # Simulation length (50 years)
                         # Long enough for multiple disturbance events

process.noise.sd <- 0.02 # Process noise SD (2% per year)
                         # Natural variability in growth/mortality

C.start.default <- 40    # Default starting coral cover (40%)
                         # Moderate condition (typical for many reefs)

# ==============================================================================
# MANAGEMENT SCENARIOS
# ==============================================================================

## Scenario 1: Passive (No Restoration)
scenario.passive <- list(
  R.max = 0,
  monitoring.freq = "low",
  description = "No restoration, minimal monitoring"
)

## Scenario 2: Reactive Restoration
scenario.reactive <- list(
  R.max = 3.0,
  monitoring.freq = "low",
  restore.threshold = 30,  # Only restore when C < 30%
  description = "Low monitoring, restore only when very degraded"
)

## Scenario 3: Proactive Restoration
scenario.proactive <- list(
  R.max = 5.0,
  monitoring.freq = "high",
  restore.threshold = 45,  # Restore when C < 45%
  description = "High monitoring, early intervention"
)

## Scenario 4: Adaptive Management
scenario.adaptive <- list(
  R.max = 5.0,
  monitoring.freq = "adaptive",
  restore.threshold = 0.6 * K.coral,  # Restore when below 60% of K
  description = "Adaptive monitoring, targeted restoration"
)

# ==============================================================================
# DERIVED PARAMETERS
# ==============================================================================

# Equilibrium coral cover with no restoration
C.equilibrium.no.restoration <- K.coral * (1 - (m.baseline + event.prob.annual * m.event.mean) / r.coral)
C.equilibrium.no.restoration <- max(0, C.equilibrium.no.restoration)

# Restoration effort needed to maintain equilibrium at target
restoration.for.equilibrium <- function(C.target) {
  growth <- r.coral * C.target * (1 - C.target/K.coral)
  mortality <- (m.baseline + event.prob.annual * m.event.mean) * C.target
  R.needed <- mortality - growth
  max(0, R.needed)
}

# ==============================================================================
# PRINT PARAMETER SUMMARY
# ==============================================================================

cat("==============================================================================\n")
cat("CORAL RESTORATION MODEL PARAMETERS\n")
cat("==============================================================================\n\n")

cat("POPULATION DYNAMICS:\n")
cat(sprintf("  Growth rate (r):               %.3f per year\n", r.coral))
cat(sprintf("  Carrying capacity (K):         %.1f%% cover\n", K.coral))
cat(sprintf("  Baseline mortality:            %.1f%% per year\n", m.baseline * 100))
cat(sprintf("  Disturbance mortality:         %.1f%% (SD=%.1f%%)\n",
            m.event.mean * 100, m.event.sd * 100))
cat(sprintf("  Disturbance frequency:         %.1f%% chance per year\n\n",
            event.prob.annual * 100))

cat("ECONOMICS:\n")
cat(sprintf("  Ecosystem value:               $%.0f per %% cover per year\n", v))
cat(sprintf("  Restoration cost:              $%.0f per %% cover restored\n", c.restore))
cat(sprintf("  Discount rate:                 %.1f%% per year\n", delta * 100))
cat(sprintf("  Max restoration rate:          %.1f%% cover per year\n\n", R.max))

cat("MONITORING:\n")
cat(sprintf("  High precision (CV):           %.1f%%\n", phi.CV.high * 100))
cat(sprintf("  Low precision (CV):            %.1f%%\n", phi.CV.low * 100))
cat(sprintf("  Base monitoring cost:          $%.0f per year\n\n", monitoring.base.cost))

cat("EQUILIBRIA:\n")
cat(sprintf("  Equilibrium without restoration: %.1f%% cover\n", C.equilibrium.no.restoration))
cat(sprintf("  Restoration to maintain 40%%:    %.2f%% per year\n",
            restoration.for.equilibrium(40)))
cat(sprintf("  Restoration to maintain 50%%:    %.2f%% per year\n\n",
            restoration.for.equilibrium(50)))

cat("==============================================================================\n")
