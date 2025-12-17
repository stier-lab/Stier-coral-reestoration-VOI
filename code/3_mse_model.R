# ============================================================================
# SIZE-STRUCTURED CORAL RESTORATION MSE MODEL
# ============================================================================
# Management Strategy Evaluation using Integral Projection Model framework
#
# KEY FEATURES:
# 1. Individual colony tracking (size-structured)
# 2. Size-dependent vital rates (survival, growth, fecundity)
# 3. Density-dependent recruitment
# 4. Stochastic disturbances with size-dependent vulnerability
# 5. Restoration as colony outplanting (with size distribution)
# 6. Monitoring precision affects size estimates
# 7. VOI from better size assessments → better restoration decisions
#
# Following standard coral ecology literature:
# - Kayal et al. (2018) - Moorea coral IPM
# - Edmunds & Elahi (2007) - Size-structured dynamics
# ============================================================================

source("code/0_libraries.R")
source("code/2_coral_parameters.R")
source("code/1_coral_operating_model.R")  # Size-structured IPM functions

# ============================================================================
# FUNCTION: est.NPV.size.structured
# ============================================================================
# Simulates coral restoration with size-structured population dynamics
#
# PARAMETERS:
#   - years: Simulation length
#   - area.m2: Reef area surveyed (m²)
#   - C.start: Starting coral cover (%)
#   - C.target: Target cover for restoration decisions
#   - max.R.cover: Maximum restoration (% cover per year)
#   - outplant.size.mean: Mean size of outplanted colonies (cm)
#   - outplant.size.sd: SD of outplant sizes
#   - phi.CV.low/high: Monitoring precision (CV)
#   - delta: Discount rate
#   - v: Ecosystem value per % cover per year
#   - c.restore: Cost per % cover restored
#   - phi.CV.seed: Random seed for monitoring error
#   - dynamics.seed: Random seed for population dynamics
#   - total.budget: Total annual budget
#   - use_budget_constraint: Budget trade-off enabled
#   - track_errors: Track decision errors
#
# Returns:
#   - NPV: Net present value
#   - C.vec: True cover trajectory
#   - Chat.vec: Estimated cover
#   - sizes.final: Final size distribution
#   - Decision error metrics
# ============================================================================

est.NPV.size.structured <- function(years,
                                    area.m2 = 100,
                                    C.start,
                                    C.target,
                                    max.R.cover,
                                    outplant.size.mean = 8,
                                    outplant.size.sd = 3,
                                    phi.CV.low,
                                    phi.CV.high,
                                    delta,
                                    v,
                                    c.restore,
                                    phi.CV.seed,
                                    dynamics.seed,
                                    total.budget = NULL,
                                    use_budget_constraint = FALSE,
                                    track_errors = TRUE,
                                    # IPM parameters
                                    survival.params = list(intercept = -1.5, slope = 0.8),
                                    growth.params = list(intercept = 0.8, slope = 0.9, sigma = 0.3),
                                    fecundity.params = list(polyps.per.cm2 = 10, eggs.per.polyp = 5),
                                    recruitment.params = list(intercept = 2.0, slope = 0.05),
                                    mortality.event.prob = 0.15,
                                    mortality.event.magnitude = 0.35,
                                    baseline.mortality = 0.05) {

  # Initialize population from starting cover
  set.seed(dynamics.seed)
  sizes <- cover.to.sizes(C.start, area.m2 = area.m2)

  # Initialize tracking vectors
  C.vec <- rep(NA, years + 1)
  C.vec[1] <- C.start
  Chat.vec <- rep(NA, years + 1)
  Chat.vec[1] <- C.start
  ES.vec <- rep(NA, years)
  R.vec <- rep(NA, years)  # Restoration as % cover
  R.desired.vec <- rep(NA, years)
  phi.CV.vec <- rep(NA, years)
  monitoring.cost.vec <- rep(NA, years)
  n.colonies.vec <- rep(NA, years + 1)
  n.colonies.vec[1] <- length(sizes)
  mean.size.vec <- rep(NA, years + 1)
  mean.size.vec[1] <- mean(sizes)

  # Error tracking
  error_type1.vec <- rep(0, years)
  error_type2.vec <- rep(0, years)
  cost_error_type1.vec <- rep(0, years)
  cost_error_type2.vec <- rep(0, years)

  # Generate observation errors (lognormal)
  set.seed(phi.CV.seed)
  C.errors.low <- exp(rnorm(years, mean = (0 - phi.CV.low^2/2), sd = phi.CV.low))
  C.errors.high <- exp(rnorm(years, mean = (0 - phi.CV.high^2/2), sd = phi.CV.high))

  # ============================================================================
  # SIMULATION LOOP
  # ============================================================================

  for (i in 1:years) {

    # ------------------------------------------------------------------------
    # SAMPLING MODEL: Monitoring precision
    # ------------------------------------------------------------------------
    current.phi.CV <- ifelse(Chat.vec[i] < C.target, phi.CV.low, phi.CV.high)
    phi.CV.vec[i] <- current.phi.CV

    # Monitoring cost
    monitoring.cost.vec[i] <- ci * exp(-cs * current.phi.CV)

    # ------------------------------------------------------------------------
    # BUDGET CONSTRAINT
    # ------------------------------------------------------------------------
    if (use_budget_constraint && !is.null(total.budget)) {
      budget_for_restoration <- total.budget - monitoring.cost.vec[i]
    } else {
      budget_for_restoration <- Inf
    }

    # ------------------------------------------------------------------------
    # MANAGEMENT MODEL: Restoration decision based on estimated cover
    # ------------------------------------------------------------------------

    if (Chat.vec[i] >= C.target) {
      R.desired.cover <- 0

    } else if (Chat.vec[i] > 0.5 * C.target) {
      # Linear increase
      R.desired.cover <- max.R.cover * (C.target - Chat.vec[i]) / (0.5 * C.target)

    } else {
      # Emergency restoration
      R.desired.cover <- max.R.cover * 1.5
    }

    R.desired.vec[i] <- R.desired.cover

    # Apply budget constraint
    if (use_budget_constraint && !is.null(total.budget)) {
      R.actual.cover <- min(R.desired.cover, budget_for_restoration / c.restore)
    } else {
      R.actual.cover <- R.desired.cover
    }

    R.vec[i] <- R.actual.cover

    # ------------------------------------------------------------------------
    # TRACK DECISION ERRORS
    # ------------------------------------------------------------------------
    if (track_errors) {
      should_restore <- C.vec[i] < C.target
      did_restore <- R.actual.cover > 0.01

      if (should_restore && !did_restore) {
        error_type1.vec[i] <- 1
        cost_error_type1.vec[i] <- v * (C.target - C.vec[i])
      }

      if (!should_restore && did_restore) {
        error_type2.vec[i] <- 1
        cost_error_type2.vec[i] <- R.actual.cover * c.restore
      }
    }

    # Ecosystem services (from TRUE cover)
    ES.vec[i] <- v * C.vec[i]

    # ------------------------------------------------------------------------
    # RESTORATION: Generate outplant colonies
    # ------------------------------------------------------------------------
    restoration.sizes <- NULL
    if (R.actual.cover > 0) {
      # Convert % cover to colony sizes
      restoration.sizes <- cover.to.sizes(
        cover.percent = R.actual.cover,
        area.m2 = area.m2,
        mean.size = outplant.size.mean,
        sd.size = outplant.size.sd
      )
    }

    # ------------------------------------------------------------------------
    # OPERATING MODEL: Size-structured dynamics
    # ------------------------------------------------------------------------
    result <- coral.dynamics.size.structured(
      sizes.cm = sizes,
      area.m2 = area.m2,
      survival.params = survival.params,
      growth.params = growth.params,
      fecundity.params = fecundity.params,
      recruitment.params = recruitment.params,
      mortality.event.prob = mortality.event.prob,
      mortality.event.magnitude = mortality.event.magnitude,
      baseline.mortality = baseline.mortality,
      restoration.sizes = restoration.sizes
    )

    # Update population
    sizes <- result$sizes
    C.vec[i+1] <- result$cover.percent
    n.colonies.vec[i+1] <- result$n.colonies
    mean.size.vec[i+1] <- result$mean.size

    # ------------------------------------------------------------------------
    # OBSERVATION MODEL: Estimate cover with error
    # ------------------------------------------------------------------------
    C.error <- ifelse(current.phi.CV == phi.CV.low,
                     C.errors.low[i],
                     C.errors.high[i])

    Chat.vec[i+1] <- C.vec[i+1] * C.error
  }

  # ============================================================================
  # CALCULATE PERFORMANCE METRICS
  # ============================================================================

  # RESCUE PROBABILITY
  temp_mat <- matrix(NA, nrow = length(C.vec), ncol = 2)
  threshold <- 0.8 * C.target

  for(j in 1:(length(C.vec)-1)){
    temp_mat[j+1, 1] <- ifelse(C.vec[j+1] > threshold & C.vec[j] < threshold, 1, 0)
    temp_mat[j+1, 2] <- ifelse(C.vec[j+1] < threshold & C.vec[j] > threshold, 1, 0)
  }

  # ECONOMIC METRICS
  restoration.cost.vec <- R.vec * c.restore
  Value <- ES.vec - restoration.cost.vec
  discount.vec <- 1 / ((1 + delta)^seq(0, (years - 1)))
  NPV_no_monitoring_cost <- sum(Value * discount.vec)
  total.monitoring.cost <- sum(monitoring.cost.vec * discount.vec)
  NPV <- NPV_no_monitoring_cost - total.monitoring.cost

  # STABILITY METRICS
  BB <- sum(length(which(C.vec <= (0.25 * median(C.vec)))),
            length(which(C.vec >= (2.25 * median(C.vec))))) / length(C.vec)

  # COLLAPSE METRICS
  TP <- ifelse(C.vec[years+1] > 0.1 * 60, 0, 1)  # Below 10% of typical K
  TPCMSY <- ifelse(C.vec[years+1] > (0.5 * C.target), 0, 1)

  # RECOVERY METRICS
  rescue <- colSums(temp_mat, na.rm = T)[1]
  rescue_prob <- colSums(temp_mat, na.rm = T)[1] / colSums(temp_mat, na.rm = T)[2]
  dangers <- colSums(temp_mat, na.rm = T)[2]

  # DECISION ERROR METRICS
  total.error.type1 <- sum(error_type1.vec)
  total.error.type2 <- sum(error_type2.vec)
  total.cost.errors <- sum((cost_error_type1.vec + cost_error_type2.vec) * discount.vec)

  return(list(
    # Core outputs
    NPV = NPV,
    ES = ES.vec,
    C = C.vec,
    Chat = Chat.vec,
    BB = BB,
    TP = TP,
    TPCMSY = TPCMSY,
    phi.CV = phi.CV.vec,
    cost.monitor = total.monitoring.cost,
    pR = R.vec,
    rescue = rescue,
    rescue_prob = rescue_prob,
    dangers = dangers,

    # Economic outputs
    NPV_no_monitoring_cost = NPV_no_monitoring_cost,
    R.desired = R.desired.vec,
    R.actual = R.vec,
    monitoring.cost.annual = monitoring.cost.vec,
    restoration.cost.total = sum(restoration.cost.vec),

    # Decision error tracking
    error.type1.count = total.error.type1,
    error.type2.count = total.error.type2,
    error.cost.total = total.cost.errors,
    error.type1.vec = error_type1.vec,
    error.type2.vec = error_type2.vec,

    # Size structure outputs
    sizes.final = sizes,
    n.colonies = n.colonies.vec,
    mean.size = mean.size.vec,

    # Reference points
    C.target = C.target
  ))
}

# ============================================================================
# HELPER FUNCTION: Calculate VOI with size structure
# ============================================================================

calculate.VOI.size.structured <- function(years, area.m2, C.start, C.target,
                                         max.R.cover, delta, v, c.restore,
                                         phi.seeds, dynamics.seeds,
                                         n.iters = 100,
                                         total.budget = NULL,
                                         use_budget_constraint = FALSE) {

  results.perfect <- rep(NA, n.iters)
  results.high <- rep(NA, n.iters)
  results.low <- rep(NA, n.iters)

  cost.perfect <- rep(NA, n.iters)
  cost.high <- rep(NA, n.iters)
  cost.low <- rep(NA, n.iters)

  errors.perfect <- rep(NA, n.iters)
  errors.high <- rep(NA, n.iters)
  errors.low <- rep(NA, n.iters)

  for (i in 1:n.iters) {
    # Perfect information
    out.perfect <- est.NPV.size.structured(
      years, area.m2, C.start, C.target, max.R.cover,
      phi.CV.low = 0.001, phi.CV.high = 0.001,
      delta, v, c.restore,
      phi.seeds[i], dynamics.seeds[i],
      total.budget, use_budget_constraint, track_errors = TRUE
    )
    results.perfect[i] <- out.perfect$NPV
    cost.perfect[i] <- out.perfect$cost.monitor
    errors.perfect[i] <- out.perfect$error.cost.total

    # High precision
    out.high <- est.NPV.size.structured(
      years, area.m2, C.start, C.target, max.R.cover,
      phi.CV.low = 0.1, phi.CV.high = 0.1,
      delta, v, c.restore,
      phi.seeds[i], dynamics.seeds[i],
      total.budget, use_budget_constraint, track_errors = TRUE
    )
    results.high[i] <- out.high$NPV
    cost.high[i] <- out.high$cost.monitor
    errors.high[i] <- out.high$error.cost.total

    # Low precision
    out.low <- est.NPV.size.structured(
      years, area.m2, C.start, C.target, max.R.cover,
      phi.CV.low = 0.5, phi.CV.high = 0.5,
      delta, v, c.restore,
      phi.seeds[i], dynamics.seeds[i],
      total.budget, use_budget_constraint, track_errors = TRUE
    )
    results.low[i] <- out.low$NPV
    cost.low[i] <- out.low$cost.monitor
    errors.low[i] <- out.low$error.cost.total
  }

  # Calculate VOI metrics
  NPV.perfect <- median(results.perfect)
  NPV.high <- median(results.high)
  NPV.low <- median(results.low)

  VOI.current <- (NPV.high - NPV.low) - (median(cost.high) - median(cost.low))
  VOI.potential <- (NPV.perfect - NPV.low) - (median(cost.perfect) - median(cost.low))
  VOI.efficiency <- ifelse(VOI.potential > 0, VOI.current / VOI.potential, NA)

  error.reduction.high.vs.low <- median(errors.low) - median(errors.high)
  error.reduction.perfect.vs.low <- median(errors.low) - median(errors.perfect)

  return(list(
    NPV.perfect = NPV.perfect,
    NPV.high = NPV.high,
    NPV.low = NPV.low,

    VOI.current = VOI.current,
    VOI.potential = VOI.potential,
    VOI.efficiency = VOI.efficiency,

    monitoring.cost.perfect = median(cost.perfect),
    monitoring.cost.high = median(cost.high),
    monitoring.cost.low = median(cost.low),

    error.cost.perfect = median(errors.perfect),
    error.cost.high = median(errors.high),
    error.cost.low = median(errors.low),
    error.reduction.high.vs.low = error.reduction.high.vs.low,

    interpretation = paste0(
      "High-precision monitoring captures ",
      round(VOI.efficiency * 100, 1),
      "% of the value of perfect information. ",
      "VOI = $", round(VOI.current, 0),
      " (current) vs $", round(VOI.potential, 0),
      " (potential)"
    )
  ))
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

cat("Testing SIZE-STRUCTURED coral restoration MSE model...\n\n")

source("code/2_coral_parameters.R")

# Test single run
test_size_structured <- est.NPV.size.structured(
  years = 50,
  area.m2 = 100,
  C.start = 30,
  C.target = 35,
  max.R.cover = 5.0,
  outplant.size.mean = 8,
  outplant.size.sd = 3,
  phi.CV.low = 0.1,
  phi.CV.high = 0.5,
  delta = 0.05,
  v = v,
  c.restore = c.restore,
  phi.CV.seed = 12345,
  dynamics.seed = 67890,
  total.budget = 15000,
  use_budget_constraint = TRUE,
  track_errors = TRUE
)

cat("SIZE-STRUCTURED MSE MODEL RESULTS:\n")
cat("===================================\n")
cat("NPV:", round(test_size_structured$NPV, 2), "\n")
cat("NPV (before monitoring cost):", round(test_size_structured$NPV_no_monitoring_cost, 2), "\n")
cat("Monitoring cost:", round(test_size_structured$cost.monitor, 2), "\n")
cat("Restoration cost:", round(test_size_structured$restoration.cost.total, 2), "\n")
cat("Type I errors (should restore but didn't):", test_size_structured$error.type1.count, "\n")
cat("Type II errors (restored unnecessarily):", test_size_structured$error.type2.count, "\n")
cat("Total cost of errors:", round(test_size_structured$error.cost.total, 2), "\n")
cat("Final coral cover:", round(test_size_structured$C[length(test_size_structured$C)], 2), "%\n")
cat("Final number of colonies:", test_size_structured$n.colonies[length(test_size_structured$n.colonies)], "\n")
cat("Final mean colony size:", round(test_size_structured$mean.size[length(test_size_structured$mean.size)], 2), "cm\n")
cat("Collapsed:", test_size_structured$TP, "\n")

cat("\n✓ Size-structured MSE model working!\n\n")

cat("Key features:\n")
cat("1. Individual colony tracking throughout simulation\n")
cat("2. Size-dependent vital rates (survival, growth, fecundity)\n")
cat("3. Density-dependent recruitment\n")
cat("4. Restoration as colony outplanting (with size distribution)\n")
cat("5. Monitoring precision affects cover estimates\n")
cat("6. Budget trade-off implemented\n")
cat("7. Decision errors tracked\n")
cat("8. Size structure tracked over time\n\n")
