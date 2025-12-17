# ============================================================================
# CORAL RESTORATION MSE MODEL - BIOLOGICALLY REALISTIC VERSION
# ============================================================================
# Management Strategy Evaluation for coral restoration monitoring
#
# BIOLOGICALLY REALISTIC CORAL DYNAMICS:
# - Simple logistic growth (NO Allee effect)
# - Stochastic mortality events (bleaching, COTS, storms)
# - Restoration as direct cover addition
#
# KEY IMPROVEMENTS:
# 1. Biologically realistic coral dynamics (removed Allee effect)
# 2. Stochastic disturbance events drive uncertainty
# 3. Restoration control rule based on deviation from target
# 4. Budget trade-off between monitoring and restoration
# 5. Decision error tracking (Type I and Type II)
# 6. VOI comes from early detection of mortality events
#
# Key question: Is precise monitoring worth the cost for early
# detection of disturbances, or should we invest in restoration?
# ============================================================================

source("code/0_libraries.R")
source("code/2b_risk_zone_functions.R")
source("code/2_coral_parameters.R")
source("code/1_coral_operating_model.R")  # Get coral.dynamics() function

# ============================================================================
# FUNCTION: est.NPV
# ============================================================================
# Simulates biologically realistic coral restoration with stochastic mortality
#
# NEW BIOLOGICALLY REALISTIC VERSION:
# 1. Simple logistic growth (NO Allee effect)
# 2. Stochastic mortality events (bleaching, COTS, storms)
# 3. Restoration control based on equilibrium cover
# 4. Budget trade-off between monitoring and restoration
# 5. Decision error tracking (Type I and Type II errors)
# 6. VOI from early detection of disturbances
#
# PARAMETERS:
#   - years: Simulation length
#   - K: Carrying capacity (%)
#   - r: Growth rate (per year)
#   - m.baseline: Baseline mortality (per year)
#   - m.event.mean: Mean mortality during disturbance
#   - m.event.sd: SD of disturbance mortality
#   - event.prob.annual: Probability of disturbance per year
#   - phi.CV.low/high: Monitoring precision (CV)
#   - delta: Discount rate
#   - v: Ecosystem value per % cover
#   - C.start: Starting coral cover (%)
#   - C.target: Target cover for restoration decisions
#   - max.R: Maximum restoration effort
#   - total.budget: Total budget for monitoring + restoration
#   - use_budget_constraint: If TRUE, monitoring costs reduce restoration budget
#   - track_errors: If TRUE, track Type I and Type II decision errors
#
# Returns:
#   - NPV: Net present value (ecosystem services - restoration - monitoring)
#   - C: True coral cover trajectory
#   - Chat: Estimated coral cover
#   - Decision error metrics
# ============================================================================

est.NPV <- function(years, K, r, m.baseline, m.event.mean, m.event.sd,
                    event.prob.annual, phi.CV.low, phi.CV.high, delta,
                    v, C.start, C.target, max.R,
                    phi.CV.seed, mortality.seed, c.restore,
                    total.budget = NULL,
                    use_budget_constraint = FALSE,
                    track_errors = TRUE) {

  # Calculate equilibrium cover with no restoration
  # At equilibrium: r*C*(1 - C/K) - m*C = 0
  # Solving: C_eq = K * (r - m) / r
  C.eq.no.restoration <- K * (r - m.baseline) / r
  C.eq.no.restoration <- max(0, C.eq.no.restoration)

  # Initialize vectors
  C.vec <- rep(NA, years)        # True coral cover
  C.vec[1] <- C.start
  Chat.vec <- rep(NA, years)     # Estimated coral cover
  Chat.vec[1] <- C.start
  ES.vec <- rep(NA, years)       # Ecosystem service value
  R.vec <- rep(NA, years)        # Restoration effort (actual)
  R.desired.vec <- rep(NA, years)  # Restoration effort (desired, before budget limit)
  phi.CV <- rep(NA, years)       # Monitoring precision
  monitoring.cost.vec <- rep(NA, years)  # Annual monitoring cost
  mortality.vec <- rep(NA, years)  # Annual mortality rate

  # NEW: Error tracking
  error_type1.vec <- rep(0, years)  # Should restore but don't (underestimate)
  error_type2.vec <- rep(0, years)  # Restore when unnecessary (overestimate)
  cost_error_type1.vec <- rep(0, years)
  cost_error_type2.vec <- rep(0, years)

  # Generate observation errors (lognormal)
  set.seed(phi.CV.seed)
  C.errors.low <- exp(rnorm(years, mean = (0 - phi.CV.low^2/2), sd = phi.CV.low))
  C.errors.high <- exp(rnorm(years, mean = (0 - phi.CV.high^2/2), sd = phi.CV.high))

  # Generate stochastic mortality events
  set.seed(mortality.seed)
  mortality.events <- runif(years) < event.prob.annual
  mortality.magnitudes <- pmax(0, rnorm(years, mean = m.event.mean, sd = m.event.sd))

  # ============================================================================
  # SIMULATION LOOP
  # ============================================================================

  for (i in 1:years){

    # ------------------------------------------------------------------------
    # SAMPLING MODEL: Determine monitoring precision for this year
    # ------------------------------------------------------------------------
    # Adaptive monitoring: high precision when below target
    current.phi.CV <- ifelse(Chat.vec[i] < C.target, phi.CV.low, phi.CV.high)
    phi.CV[i] <- current.phi.CV

    # Calculate monitoring cost for this year
    monitoring.cost.vec[i] <- ci * exp(-cs * current.phi.CV)

    # ------------------------------------------------------------------------
    # BUDGET CONSTRAINT (if enabled)
    # ------------------------------------------------------------------------
    if (use_budget_constraint && !is.null(total.budget)) {
      budget_for_restoration <- total.budget - monitoring.cost.vec[i]
    } else {
      budget_for_restoration <- Inf  # No budget constraint
    }

    # ------------------------------------------------------------------------
    # MANAGEMENT MODEL: Determine restoration effort based on estimated cover
    # NEW: Based on deviation from target, not Allee threshold
    # ------------------------------------------------------------------------

    # RESTORATION CONTROL RULE:
    # - No restoration if above target
    # - Increasing restoration as cover decreases below target
    # - Maximum restoration if critically low (< 50% of target)

    if (Chat.vec[i] >= C.target) {
      R.desired <- 0  # No restoration needed when above target

    } else if (Chat.vec[i] > 0.5 * C.target) {
      # Linear increase in restoration as cover decreases
      # R increases from 0 at C.target to max.R at 0.5*C.target
      R.desired <- max.R * (C.target - Chat.vec[i]) / (0.5 * C.target)

    } else {
      # Emergency restoration when critically low
      R.desired <- max.R * 1.5  # 150% of max for critical situations
    }

    R.desired.vec[i] <- R.desired

    # Apply budget constraint
    if (use_budget_constraint && !is.null(total.budget)) {
      R.actual <- min(R.desired, budget_for_restoration / c.restore)
    } else {
      R.actual <- R.desired
    }

    R.vec[i] <- R.actual

    # ------------------------------------------------------------------------
    # TRACK DECISION ERRORS (if enabled)
    # ------------------------------------------------------------------------
    if (track_errors) {
      # What SHOULD we do based on TRUE coral cover?
      should_restore <- C.vec[i] < C.target
      did_restore <- R.actual > 0.01  # Small threshold to account for rounding

      # Type I error: Should restore but don't (dangerous!)
      if (should_restore && !did_restore) {
        error_type1.vec[i] <- 1
        # Cost: Lost ecosystem services from degraded state
        cost_error_type1.vec[i] <- v * (C.target - C.vec[i])
      }

      # Type II error: Restore when unnecessary (wasteful!)
      if (!should_restore && did_restore) {
        error_type2.vec[i] <- 1
        # Cost: Wasted restoration funds
        cost_error_type2.vec[i] <- R.actual * c.restore
      }
    }

    # Ecosystem service value (proportional to TRUE coral cover)
    ES.vec[i] <- v * C.vec[i]

    # ------------------------------------------------------------------------
    # OPERATING MODEL: True coral population dynamics
    # NEW: Biologically realistic dynamics with stochastic mortality
    # ------------------------------------------------------------------------

    # Determine mortality for this year
    if (mortality.events[i]) {
      # Disturbance event
      mortality.vec[i] <- m.baseline + mortality.magnitudes[i]
    } else {
      # Baseline mortality only
      mortality.vec[i] <- m.baseline
    }

    # Use coral.dynamics() function: dC/dt = r*C*(1-C/K) - m*C + R
    dC <- coral.dynamics(C.vec[i], K, r, mortality.vec[i], R.actual)

    # Update coral cover
    C.vec[i+1] <- C.vec[i] + dC

    # Bound by [0, K]
    C.vec[i+1] <- max(0.1, min(K, C.vec[i+1]))

    # ------------------------------------------------------------------------
    # OBSERVATION MODEL: Get new estimate for next year
    # ------------------------------------------------------------------------
    # Determine which error to use based on CURRENT monitoring precision
    C.error <- ifelse(current.phi.CV == phi.CV.low,
                      C.errors.low[i],
                      C.errors.high[i])

    Chat.vec[i+1] <- C.vec[i+1] * C.error
  }

  # ============================================================================
  # CALCULATE PERFORMANCE METRICS
  # ============================================================================

  # RESCUE PROBABILITY: Recovery from decline events
  temp_mat <- matrix(NA, nrow = length(C.vec) + 1, ncol = 2)
  threshold <- 0.8 * C.target

  for(j in 1:length(C.vec)){
    # Number of times recovered above threshold
    temp_mat[j+1, 1] <- ifelse(C.vec[j+1] > threshold & C.vec[j] < threshold, 1, 0)
    # Number of times dipped below threshold
    temp_mat[j+1, 2] <- ifelse(C.vec[j+1] < threshold & C.vec[j] > threshold, 1, 0)
  }

  # ECONOMIC METRICS
  # Cost of restoration
  restoration.cost.vec <- R.vec * c.restore

  # Net value each year: Ecosystem services - Restoration costs
  Value <- ES.vec - restoration.cost.vec

  # Discount to present value
  discount.vec <- 1 / ((1 + delta)^seq(0, (years - 1)))
  NPV_no_monitoring_cost <- sum(Value * discount.vec)

  # Total monitoring cost (discounted)
  total.monitoring.cost <- sum(monitoring.cost.vec * discount.vec)

  # Final NPV (ecosystem services - restoration costs - monitoring costs)
  NPV <- NPV_no_monitoring_cost - total.monitoring.cost

  # STABILITY METRICS
  BB <- sum(length(which(C.vec <= (0.25 * median(C.vec)))),
            length(which(C.vec >= (2.25 * median(C.vec))))) / length(C.vec)

  # COLLAPSE METRICS (based on biological thresholds, not Allee)
  TP <- ifelse(C.vec[years] > 0.1 * K, 0, 1)  # Below 10% of K is collapse
  TPCMSY <- ifelse(C.vec[years] > (0.5 * C.target), 0, 1)  # Below 50% of target

  # RECOVERY METRICS
  rescue <- colSums(temp_mat, na.rm = T)[1]  # Number of recoveries
  rescue_prob <- colSums(temp_mat, na.rm = T)[1] / colSums(temp_mat, na.rm = T)[2]
  dangers <- colSums(temp_mat, na.rm = T)[2]  # Number of danger events

  # DECISION ERROR METRICS (discounted)
  total.error.type1 <- sum(error_type1.vec)
  total.error.type2 <- sum(error_type2.vec)
  total.cost.errors <- sum((cost_error_type1.vec + cost_error_type2.vec) * discount.vec)

  # DISTURBANCE TRACKING
  n.disturbances <- sum(mortality.events)
  avg.mortality <- mean(mortality.vec)

  return(list(
    # Core outputs
    NPV = NPV,
    ES = ES.vec,
    C = C.vec,
    Chat = Chat.vec,
    BB = BB,
    TP = TP,
    TPCMSY = TPCMSY,
    phi.CV = phi.CV,
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

    # Disturbance tracking
    mortality.vec = mortality.vec,
    n.disturbances = n.disturbances,
    avg.mortality = avg.mortality,

    # Reference points
    C.target = C.target,
    C.eq.no.restoration = C.eq.no.restoration
  ))
}

# ============================================================================
# HELPER FUNCTION: Calculate VOI with perfect information benchmark
# ============================================================================

calculate.VOI <- function(years, K, r, m.baseline, m.event.mean, m.event.sd,
                          event.prob.annual, delta, v, C.start,
                          C.target, max.R, phi.seeds, mortality.seeds,
                          c.restore, n.iters = 100,
                          total.budget = NULL,
                          use_budget_constraint = FALSE) {

  # Run three scenarios:
  # 1. Perfect information (CV = 0.001, very small to avoid division by zero)
  # 2. High precision (CV = 0.1)
  # 3. Low precision (CV = 0.5)

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
    out.perfect <- est.NPV(
      years, K, r, m.baseline, m.event.mean, m.event.sd, event.prob.annual,
      phi.CV.low = 0.001, phi.CV.high = 0.001,
      delta, v, C.start, C.target, max.R,
      phi.seeds[i], mortality.seeds[i], c.restore,
      total.budget, use_budget_constraint, track_errors = TRUE
    )
    results.perfect[i] <- out.perfect$NPV
    cost.perfect[i] <- out.perfect$cost.monitor
    errors.perfect[i] <- out.perfect$error.cost.total

    # High precision
    out.high <- est.NPV(
      years, K, r, m.baseline, m.event.mean, m.event.sd, event.prob.annual,
      phi.CV.low = 0.1, phi.CV.high = 0.1,
      delta, v, C.start, C.target, max.R,
      phi.seeds[i], mortality.seeds[i], c.restore,
      total.budget, use_budget_constraint, track_errors = TRUE
    )
    results.high[i] <- out.high$NPV
    cost.high[i] <- out.high$cost.monitor
    errors.high[i] <- out.high$error.cost.total

    # Low precision
    out.low <- est.NPV(
      years, K, r, m.baseline, m.event.mean, m.event.sd, event.prob.annual,
      phi.CV.low = 0.5, phi.CV.high = 0.5,
      delta, v, C.start, C.target, max.R,
      phi.seeds[i], mortality.seeds[i], c.restore,
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

  # Value of Information calculations
  VOI.current <- (NPV.high - NPV.low) - (median(cost.high) - median(cost.low))
  VOI.potential <- (NPV.perfect - NPV.low) - (median(cost.perfect) - median(cost.low))
  VOI.efficiency <- ifelse(VOI.potential > 0, VOI.current / VOI.potential, NA)

  # Cost of errors
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

cat("Testing BIOLOGICALLY REALISTIC coral restoration model...\n\n")

source("code/2_coral_parameters.R")

# Test single run with new parameters
test_realistic <- est.NPV(
  years = 50,
  K = K.coral,
  r = r.coral,
  m.baseline = m.baseline,
  m.event.mean = m.event.mean,
  m.event.sd = m.event.sd,
  event.prob.annual = event.prob.annual,
  phi.CV.low = 0.1,
  phi.CV.high = 0.5,
  delta = 0.05,
  v = v,
  C.start = 45,
  C.target = 40,  # Target equilibrium with moderate restoration
  max.R = max.R,
  phi.CV.seed = 12345,
  mortality.seed = 67890,
  c.restore = c.restore,
  total.budget = 15000,
  use_budget_constraint = TRUE,
  track_errors = TRUE
)

cat("BIOLOGICALLY REALISTIC MODEL RESULTS:\n")
cat("=====================================\n")
cat("NPV:", round(test_realistic$NPV, 2), "\n")
cat("NPV (before monitoring cost):", round(test_realistic$NPV_no_monitoring_cost, 2), "\n")
cat("Monitoring cost:", round(test_realistic$cost.monitor, 2), "\n")
cat("Restoration cost:", round(test_realistic$restoration.cost.total, 2), "\n")
cat("Type I errors (should restore but didn't):", test_realistic$error.type1.count, "\n")
cat("Type II errors (restored unnecessarily):", test_realistic$error.type2.count, "\n")
cat("Total cost of errors:", round(test_realistic$error.cost.total, 2), "\n")
cat("Number of disturbances:", test_realistic$n.disturbances, "\n")
cat("Average mortality rate:", round(test_realistic$avg.mortality * 100, 2), "%\n")
cat("Final coral cover:", round(test_realistic$C[length(test_realistic$C)], 2), "%\n")
cat("Collapsed:", test_realistic$TP, "\n")

cat("\n✓ Biologically realistic model is working!\n")
cat("\nKey features:\n")
cat("1. NO Allee effect - simple logistic growth\n")
cat("2. Stochastic mortality events (bleaching, COTS, storms)\n")
cat("3. Restoration control based on target cover, not thresholds\n")
cat("4. Budget trade-off implemented\n")
cat("5. Decision errors tracked\n")
cat("6. VOI from early detection of disturbances\n")

# ============================================================================
# FUNCTION: repeat.model2
# ============================================================================
# Runs Monte Carlo simulations with est.NPV
# Updated for biologically realistic model (no Allee parameters)
# ============================================================================

repeat.model2 <- function(n.iters, C.start, C.target, years, K, r,
                          m.baseline, m.event.mean, m.event.sd, event.prob.annual,
                          phi.CV, delta, v, max.R, phi.seeds, mortality.seeds,
                          c.restore = 800){

  # Store input phi.CV parameter before creating result vectors
  phi.CV.input <- phi.CV

  # Initialize result vectors
  value <- rep(NA, n.iters)
  BB <- rep(NA, n.iters)
  TP <- rep(NA, n.iters)
  TPCMSY <- rep(NA, n.iters)
  dC <- rep(NA, n.iters)
  C <- rep(NA, n.iters)
  ES <- rep(NA, n.iters)
  phi.CV.result <- rep(NA, n.iters)
  cost.monitor <- rep(NA, n.iters)
  NPV_minusCM <- rep(NA, n.iters)
  pRmax <- rep(NA, n.iters)

  phi.CV.seed.save <- rep(NA, n.iters)
  thresh2 <- thresh1 <- rep(NA, n.iters)
  rescue <- rep(NA, n.iters)
  rescue_prob <- rep(NA, n.iters)
  dangers <- rep(NA, n.iters)
  n.disturbances <- rep(NA, n.iters)

  # Run simulation n.iters times
  for (i in 1:n.iters){

    phi.CV.seed <- phi.seeds[i]
    mortality.seed <- mortality.seeds[i]
    phi.CV.seed.save[i] <- phi.CV.seed

    # Determine phi.CV values (handle both single value and separate low/high)
    if (length(phi.CV.input) == 1) {
      phi.CV.low <- phi.CV.high <- phi.CV.input
    } else {
      phi.CV.low <- phi.CV.input[1]
      phi.CV.high <- phi.CV.input[2]
    }

    # Run single simulation
    model.output <- est.NPV(
      years, K, r, m.baseline, m.event.mean, m.event.sd, event.prob.annual,
      phi.CV.low, phi.CV.high, delta, v, C.start, C.target, max.R,
      phi.CV.seed, mortality.seed, c.restore,
      use_budget_constraint = FALSE, track_errors = FALSE
    )

    # Extract results
    value[i] <- model.output$NPV
    BB[i] <- model.output$BB
    TP[i] <- model.output$TP
    TPCMSY[i] <- model.output$TPCMSY
    dC[i] <- median(abs(model.output$C / model.output$Chat))
    C[i] <- mean(model.output$C)

    # Use C.target instead of Allee threshold for danger zone calculations
    thresh1[i] <- sum(model.output$C < 0.8 * C.target) / length(model.output$C)
    thresh2[i] <- sum(model.output$C < 0.5 * C.target) / length(model.output$C)
    rescue[i] <- length(which(model.output$C < 0.8 * C.target & model.output$C > 0.1 * K))
    rescue_prob[i] <- model.output$rescue_prob
    dangers[i] <- length(which(model.output$C > 0.8 * C.target))

    ES[i] <- median(model.output$ES)
    phi.CV.result[i] <- mean(model.output$phi.CV, na.rm = T)
    cost.monitor[i] <- model.output$cost.monitor
    NPV_minusCM[i] <- model.output$NPV - model.output$cost.monitor
    pRmax[i] <- max(model.output$pR)
    n.disturbances[i] <- model.output$n.disturbances
  }

  return(list(
    value = value,
    BB = BB,
    TP = TP,
    TPCMSY = TPCMSY,
    dC = dC,
    C = C,
    ES = ES,
    phi.CV = phi.CV.result,
    cost.monitor = cost.monitor,
    NPV_minusCM = NPV_minusCM,
    pRmax = pRmax,
    thresh1 = thresh1,
    thresh2 = thresh2,
    rescue = rescue,
    rescue_prob = rescue_prob,
    dangers = dangers,
    n.disturbances = n.disturbances
  ))
}
