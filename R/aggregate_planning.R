
library(shiny)

calculate_chase_plan <- function(data, whole_workers = FALSE) {
  
  df <- data$periods
  k <- data$productivity
  n <- nrow(df)
  
  workers <- numeric(n)
  hired <- numeric(n)
  fired <- numeric(n)
  production <- numeric(n)
  inventory <- numeric(n)
  
  previous_workers <- data$starting_workforce
  previous_inventory <- data$starting_inventory
  
  for (i in seq_len(n)) {
    
    target <- if (i == n) data$ending_inventory else 0
    
    needed <- max(
      0,
      df$Demand[i] + target - previous_inventory
    )
    
    required_workers <- needed / (k * df$WorkingDays[i])
    
    if (whole_workers) {
      workers[i] <- ceiling(round(required_workers, 10))
      production[i] <- workers[i] * k * df$WorkingDays[i]
    } else {
      workers[i] <- required_workers
      production[i] <- needed
    }
    
    inventory[i] <- previous_inventory +
      production[i] - df$Demand[i]
    
    hired[i] <- max(0, workers[i] - previous_workers)
    fired[i] <- max(0, previous_workers - workers[i])
    
    previous_workers <- workers[i]
    previous_inventory <- inventory[i]
  }
  
  costs <- data$costs
  
  plan <- data.frame(
    Period = df$Period,
    Demand = df$Demand,
    Workers = round(workers, 3),
    Hired = round(hired, 3),
    Fired = round(fired, 3),
    Production = round(production, 3),
    Inventory = round(inventory, 3)
  )
  
  breakdown <- data.frame(
    Item = c(
      "Hiring",
      "Firing",
      "Regular production",
      "Inventory holding"
    ),
    Cost = c(
      sum(hired) * costs$hiring,
      sum(fired) * costs$firing,
      sum(production) * costs$regular,
      sum(inventory) * costs$holding
    )
  )
  
  list(
    plan = plan,
    costs = breakdown,
    total = sum(breakdown$Cost),
    cumulative_production = cumsum(production),
    cumulative_net_demand =
      cumsum(df$Demand) - data$starting_inventory
  )
}


`%||%` <- function(x, y) if (is.null(x)) y else x

# Level and mixed planning helpers -----------------------------------------
# Full regular-time capacity is used in both fractional and whole-worker plans.
# Negative inventory represents unmet demand, not available stock.

build_heuristic_result <- function(data, workers, production) {
  df <- data$periods
  n <- nrow(df)
  previous_workers <- data$starting_workforce
  previous_inventory <- data$starting_inventory
  hired <- fired <- inventory <- numeric(n)

  for (i in seq_len(n)) {
    hired[i] <- max(0, workers[i] - previous_workers)
    fired[i] <- max(0, previous_workers - workers[i])
    inventory[i] <- previous_inventory + production[i] - df$Demand[i]
    previous_workers <- workers[i]
    previous_inventory <- inventory[i]
  }

  costs <- data$costs
  breakdown <- data.frame(
    Item = c("Hiring", "Firing", "Regular production", "Inventory holding"),
    Cost = c(sum(hired) * costs$hiring,
             sum(fired) * costs$firing,
             sum(production) * costs$regular,
             sum(pmax(inventory, 0)) * costs$holding)
  )
  plan <- data.frame(
    Period = df$Period,
    Demand = df$Demand,
    Workers = round(workers, 3),
    Hired = round(hired, 3),
    Fired = round(fired, 3),
    Production = round(production, 3),
    Inventory = round(inventory, 3),
    UnmetDemand = round(pmax(-inventory, 0), 3)
  )
  list(
    plan = plan,
    costs = breakdown,
    total = sum(breakdown$Cost),
    cumulative_production = cumsum(production),
    cumulative_net_demand = cumsum(df$Demand) - data$starting_inventory,
    feasible = all(inventory >= -1e-7) &&
      tail(inventory, 1) >= data$ending_inventory - 1e-7,
    ending_inventory = tail(inventory, 1),
    required_ending_inventory = data$ending_inventory
  )
}

calculate_level_plan <- function(data, whole_workers = FALSE) {
  df <- data$periods
  required_production <- sum(df$Demand) + data$ending_inventory -
    data$starting_inventory
  workforce <- max(0, required_production) /
    (data$productivity * sum(df$WorkingDays))
  if (whole_workers) workforce <- ceiling(round(workforce, 10))
  workers <- rep(workforce, nrow(df))
  production <- workers * data$productivity * df$WorkingDays
  build_heuristic_result(data, workers, production)
}

calculate_mixed_plan <- function(data, change_periods = 1L,
                                 whole_workers = FALSE) {
  df <- data$periods
  n <- nrow(df)
  starts <- sort(unique(c(1L, as.integer(change_periods))))
  starts <- starts[is.finite(starts) & starts >= 1 & starts <= n]
  ends <- c(starts[-1L] - 1L, n)
  workers <- production <- numeric(n)
  previous_inventory <- data$starting_inventory

  for (j in seq_along(starts)) {
    idx <- seq.int(starts[j], ends[j])
    target <- if (j == length(starts)) data$ending_inventory else 0
    required <- max(0, sum(df$Demand[idx]) + target - previous_inventory)
    workforce <- required / (data$productivity * sum(df$WorkingDays[idx]))
    if (whole_workers) workforce <- ceiling(round(workforce, 10))
    workers[idx] <- workforce
    production[idx] <- workforce * data$productivity * df$WorkingDays[idx]
    previous_inventory <- previous_inventory +
      sum(production[idx]) - sum(df$Demand[idx])
  }
  build_heuristic_result(data, workers, production)
}

heuristic_status <- function(result) {
  if (result$feasible) {
    paste0("Feasible for all periods. Ending inventory: ",
           round(result$ending_inventory, 3), ".")
  } else {
    paste0("WARNING: This plan does not meet all demand on time or the ",
           "required ending inventory. Negative inventory represents ",
           "unmet demand. Total cost excludes shortage penalties.")
  }
}

plot_heuristic_comparison <- function(original, whole, title) {
  n <- nrow(original$plan)
  x <- seq_len(n)
  has_whole <- !is.null(whole) && isTRUE(whole$success %||% TRUE)
  y <- c(0, original$cumulative_production,
         if (has_whole) whole$cumulative_production, original$cumulative_net_demand)
  plot(x, original$cumulative_net_demand, type = "o", pch = 16,
       col = "black", lwd = 2,
       xlim = if (n == 1) c(0.5, 1.5) else range(x),
       ylim = range(y), xaxt = "n", xlab = "Period",
       ylab = "Cumulative Units", main = title)
  axis(1, at = x, labels = original$plan$Period, las = 2,
       cex.axis = if (n > 10) 0.65 else 0.85)
  lines(x, original$cumulative_production, type = "o", pch = 17,
        col = "blue", lwd = 2)
  if (has_whole) {
    lines(x, whole$cumulative_production, type = "o", pch = 15,
          col = "red", lwd = 2)
  }
  keep <- c(TRUE, TRUE, has_whole)
  legend("topleft", c("Cumulative Net Demand", "Original Production",
                       "Whole-Worker Production")[keep],
         col = c("black", "blue", "red")[keep], lty = 1,
         pch = c(16, 17, 15)[keep], bty = "n", cex = 0.8)
}

# Transportation method helpers ---------------------------------------------

solve_transportation_plan <- function(data, capacities, method = "northwest") {
  periods <- data$periods
  n <- nrow(periods)
  costs <- data$costs
  tol <- 1e-8

  fail <- function(message) {
    list(success = FALSE, message = message, method = method)
  }

  capacity_columns <- c("Regular", "Overtime", "Subcontract")
  if (nrow(capacities) != n || !all(capacity_columns %in% names(capacities))) {
    return(fail("Capacity data must be completed for every planning period."))
  }
  capacity_values <- as.matrix(capacities[, capacity_columns, drop = FALSE])
  if (!all(is.finite(capacity_values)) || any(capacity_values < 0)) {
    return(fail("All production capacities must be finite, non-negative values."))
  }
  cost_values <- c(costs$regular, costs$overtime, costs$subcontract, costs$holding)
  if (!all(is.finite(cost_values)) || any(cost_values < 0)) {
    return(fail("Transportation costs must be finite and non-negative."))
  }
  if (!is.finite(data$starting_inventory) || data$starting_inventory < 0 ||
      !is.finite(data$ending_inventory) || data$ending_inventory < 0) {
    return(fail("Starting and required ending inventory must be finite and non-negative."))
  }

  # As in the lecture example, beginning inventory reduces the first period's
  # net demand and the required ending inventory is added to the final period.
  demand <- as.numeric(periods$Demand)
  if (data$starting_inventory > demand[1] + tol) {
    return(fail(paste0(
      "Starting inventory (", data$starting_inventory,
      ") exceeds first-period demand (", demand[1],
      "). This transportation layout follows the lecture example and requires starting inventory not to exceed first-period demand."
    )))
  }
  demand[1] <- demand[1] - data$starting_inventory
  demand[n] <- demand[n] + data$ending_inventory

  source <- do.call(rbind, lapply(seq_len(n), function(i) {
    data.frame(
      Source = paste(periods$Period[i], c("Regular time", "Overtime", "Subcontracting")),
      PeriodIndex = i,
      Type = c("Regular time", "Overtime", "Subcontracting"),
      Supply = as.numeric(capacities[i, capacity_columns]),
      UnitCost = c(costs$regular, costs$overtime, costs$subcontract),
      stringsAsFactors = FALSE
    )
  }))
  rownames(source) <- NULL

  total_supply <- sum(source$Supply)
  total_demand <- sum(demand)
  if (total_supply + tol < total_demand) {
    return(fail(paste0(
      "Insufficient total capacity: ", round(total_supply, 3),
      " units available, but ", round(total_demand, 3), " units are required."
    )))
  }

  # A period's demand may only be supplied by production from that period
  # or an earlier period. Check cumulative feasibility before allocating.
  cumulative_supply <- vapply(seq_len(n), function(j) {
    sum(source$Supply[source$PeriodIndex <= j])
  }, numeric(1))
  cumulative_demand <- cumsum(demand)
  if (any(cumulative_supply + tol < cumulative_demand)) {
    first_bad <- which(cumulative_supply + tol < cumulative_demand)[1]
    return(fail(paste0(
      "The plan is not feasible by period ", periods$Period[first_bad],
      ": cumulative available capacity is ", round(cumulative_supply[first_bad], 3),
      " units, below cumulative net demand of ", round(cumulative_demand[first_bad], 3),
      " units. Later production cannot satisfy earlier demand."
    )))
  }

  excess <- max(0, total_supply - total_demand)
  allocation <- matrix(
    0, nrow = nrow(source), ncol = n + 1,
    dimnames = list(source$Source, c(as.character(periods$Period), "Excess"))
  )
  unit_cost_matrix <- matrix(Inf, nrow = nrow(source), ncol = n,
                             dimnames = list(source$Source, as.character(periods$Period)))
  for (i in seq_len(nrow(source))) {
    for (j in seq_len(n)) {
      if (j >= source$PeriodIndex[i]) {
        unit_cost_matrix[i, j] <- source$UnitCost[i] +
          costs$holding * (j - source$PeriodIndex[i])
      }
    }
  }

  remaining_supply <- source$Supply
  remaining_demand <- demand

  if (identical(method, "northwest")) {
    # North-West Corner: process demand periods from left to right and use
    # the earliest remaining production source first.
    for (j in seq_len(n)) {
      while (remaining_demand[j] > tol) {
        eligible <- which(remaining_supply > tol & source$PeriodIndex <= j)
        if (!length(eligible)) {
          return(fail(paste0("No eligible production capacity remains for ", periods$Period[j], ".")))
        }
        i <- eligible[1]
        qty <- min(remaining_supply[i], remaining_demand[j])
        allocation[i, j] <- allocation[i, j] + qty
        remaining_supply[i] <- remaining_supply[i] - qty
        remaining_demand[j] <- remaining_demand[j] - qty
      }
    }
  } else if (identical(method, "least_cost")) {
    # Least-Cost Method: repeatedly allocate to the cheapest currently
    # feasible source/demand cell. This is a feasible heuristic, not a proof
    # of global optimality.
    while (any(remaining_demand > tol)) {
      candidates <- which(
        outer(remaining_supply > tol, remaining_demand > tol, `&`) &
          is.finite(unit_cost_matrix),
        arr.ind = TRUE
      )
      if (!nrow(candidates)) {
        return(fail("No feasible source/demand cell remains. Check the period capacities."))
      }
      candidate_costs <- unit_cost_matrix[candidates]
      chosen <- candidates[order(candidate_costs, candidates[, 1], candidates[, 2])[1], ]
      i <- chosen[1]
      j <- chosen[2]
      qty <- min(remaining_supply[i], remaining_demand[j])
      allocation[i, j] <- allocation[i, j] + qty
      remaining_supply[i] <- remaining_supply[i] - qty
      remaining_demand[j] <- remaining_demand[j] - qty
    }
  } else {
    return(fail("Unknown transportation method."))
  }

  # Any unused source capacity is assigned to the dummy Excess destination.
  allocation[, n + 1] <- pmax(remaining_supply, 0)

  holding_unit_periods <- 0
  for (i in seq_len(nrow(source))) {
    for (j in seq_len(n)) {
      holding_unit_periods <- holding_unit_periods +
        allocation[i, j] * max(0, j - source$PeriodIndex[i])
    }
  }

  production_by_type <- vapply(
    c("Regular time", "Overtime", "Subcontracting"),
    function(type) sum(allocation[source$Type == type, seq_len(n), drop = FALSE]),
    numeric(1)
  )
  cost_breakdown <- data.frame(
    Item = c("Regular production", "Overtime production", "Subcontracting", "Inventory holding"),
    `Units / unit-periods` = c(production_by_type, holding_unit_periods),
    Cost = c(
      production_by_type[1] * costs$regular,
      production_by_type[2] * costs$overtime,
      production_by_type[3] * costs$subcontract,
      holding_unit_periods * costs$holding
    ),
    check.names = FALSE
  )

  allocation_table <- data.frame(
    Source = source$Source,
    as.data.frame(allocation, check.names = FALSE),
    check.names = FALSE
  )
  list(
    success = TRUE,
    allocation = allocation_table,
    costs = cost_breakdown,
    total = sum(cost_breakdown$Cost),
    adjusted_demand = data.frame(Period = periods$Period, Demand = demand),
    period_production = vapply(seq_len(n), function(period_index) {
      sum(allocation[source$PeriodIndex == period_index, seq_len(n), drop = FALSE])
    }, numeric(1)),
    total_capacity = total_supply,
    excess_capacity = sum(allocation[, n + 1]),
    method = method
  )
}

# Disaggregation helper ------------------------------------------------------

calculate_disaggregation <- function(total_quantity, products, tolerance = 1e-8) {
  fail <- function(message) {
    list(success = FALSE, message = message)
  }

  required_columns <- c("Product", "AnnualUsage", "SetupCost", "MinQty", "MaxQty")
  if (!all(required_columns %in% names(products))) {
    return(fail("Product data is incomplete."))
  }

  if (!is.finite(total_quantity) || total_quantity <= 0) {
    return(fail("Aggregate production quantity must be a positive number."))
  }

  numeric_data <- products[, c("AnnualUsage", "SetupCost", "MinQty", "MaxQty")]
  if (any(!is.finite(as.matrix(numeric_data)))) {
    return(fail("All product parameters must be finite numbers."))
  }
  if (any(products$AnnualUsage <= 0) || any(products$SetupCost <= 0)) {
    return(fail("Annual usage and setup cost must be greater than zero for every product."))
  }
  if (any(products$MinQty < 0) || any(products$MaxQty < products$MinQty)) {
    return(fail("Minimum quantities must be non-negative and maximum quantities must not be below minimum quantities."))
  }
  if (any(!nzchar(trimws(as.character(products$Product))))) {
    return(fail("Product names cannot be empty."))
  }

  minimum_total <- sum(products$MinQty)
  maximum_total <- sum(products$MaxQty)

  if (total_quantity < minimum_total - tolerance) {
    return(fail(paste0(
      "The aggregate quantity is below total minimum production. Required: ",
      round(minimum_total, 4), " units; available: ",
      round(total_quantity, 4), " units."
    )))
  }
  if (total_quantity > maximum_total + tolerance) {
    return(fail(paste0(
      "The aggregate quantity exceeds total maximum production. Required: ",
      round(total_quantity, 4), " units; maximum capacity: ",
      round(maximum_total, 4), " units."
    )))
  }

  # KKT conditions give q_i = clamp(scale * sqrt(K_i * lambda_i), min_i, max_i).
  # Solve for the common scale so products at a bound can re-enter as it changes.
  weights <- sqrt(products$SetupCost * products$AnnualUsage)
  lower <- products$MinQty
  upper <- products$MaxQty

  if (total_quantity <= minimum_total) {
    allocation <- lower
  } else if (total_quantity >= maximum_total) {
    allocation <- upper
  } else {
    low_scale <- 0
    high_scale <- max(upper / weights)
    repeat {
      scale <- low_scale + (high_scale - low_scale) / 2
      allocation <- pmin(upper, pmax(lower, scale * weights))
      difference <- sum(allocation) - total_quantity
      if (abs(difference) <= tolerance ||
          scale == low_scale || scale == high_scale) {
        break
      }
      if (difference < 0) {
        low_scale <- scale
      } else {
        high_scale <- scale
      }
    }
  }

  objective_cost <- sum(products$SetupCost * products$AnnualUsage / allocation)
  result <- data.frame(
    Product = as.character(products$Product),
    AnnualUsage = products$AnnualUsage,
    SetupCost = products$SetupCost,
    Weight = weights,
    MinQty = products$MinQty,
    AllocatedQty = allocation,
    MaxQty = products$MaxQty,
    AllocationPercent = allocation / total_quantity * 100,
    stringsAsFactors = FALSE
  )

  list(
    success = TRUE,
    allocation = result,
    total_quantity = total_quantity,
    allocated_total = sum(allocation),
    objective_cost = objective_cost,
    minimum_total = minimum_total,
    maximum_total = maximum_total
  )
}


# Linear programming helpers ------------------------------------------------
# Variables per period: W workforce, H hired, F fired, P regular production,
# I inventory, O overtime, S subcontracting.
# Options: overtime (limited to a fraction of regular capacity), subcontracting
# (unlimited) and idle time. When idle time is NOT allowed, regular production
# must use the full regular capacity of the workforce (P = K * days * W).
# fixed_workers: if given, the workforce is fixed to these (whole) numbers and
# the rest of the plan is re-optimized.

solve_aggregate_lp <- function(values, fixed_workers = NULL) {
  d <- values$data
  df <- d$periods
  n <- nrow(df)
  k <- d$productivity
  cst <- d$costs
  idle_allowed <- is.null(values$allow_idle) || isTRUE(values$allow_idle)

  vars <- c("W", "H", "F", "P", "I", "O", "S")
  m <- 7 * n
  idx <- function(v, t) (match(v, vars) - 1) * n + t

  objective <- numeric(m)
  for (t in seq_len(n)) {
    objective[idx("H", t)] <- cst$hiring
    objective[idx("F", t)] <- cst$firing
    objective[idx("P", t)] <- cst$regular
    objective[idx("I", t)] <- cst$holding
    objective[idx("O", t)] <- cst$overtime
    objective[idx("S", t)] <- cst$subcontract
  }

  rows <- list()
  directions <- character()
  rhs <- numeric()
  add_constraint <- function(coeff, direction, value) {
    rows[[length(rows) + 1]] <<- coeff
    directions <<- c(directions, direction)
    rhs <<- c(rhs, value)
  }

  for (t in seq_len(n)) {
    capacity <- k * df$WorkingDays[t]

    # Workforce balance: W_t = W_{t-1} + H_t - F_t
    a <- numeric(m)
    a[idx("W", t)] <- 1
    a[idx("H", t)] <- -1
    a[idx("F", t)] <- 1
    if (t > 1) {
      a[idx("W", t - 1)] <- -1
      b <- 0
    } else {
      b <- d$starting_workforce
    }
    add_constraint(a, "=", b)

    # Inventory balance: I_t = I_{t-1} + P_t + O_t + S_t - D_t
    a <- numeric(m)
    a[idx("I", t)] <- 1
    a[idx("P", t)] <- -1
    a[idx("O", t)] <- -1
    a[idx("S", t)] <- -1
    if (t > 1) {
      a[idx("I", t - 1)] <- -1
      b <- -df$Demand[t]
    } else {
      b <- d$starting_inventory - df$Demand[t]
    }
    add_constraint(a, "=", b)

    # Regular production capacity (equality when idle time is not allowed)
    a <- numeric(m)
    a[idx("P", t)] <- 1
    a[idx("W", t)] <- -capacity
    add_constraint(a, if (idle_allowed) "<=" else "=", 0)

    # Overtime capacity
    a <- numeric(m)
    a[idx("O", t)] <- 1
    if (isTRUE(values$allow_overtime)) {
      a[idx("W", t)] <- -capacity * values$overtime_ratio
    }
    add_constraint(a, "<=", 0)

    # Subcontracting availability
    if (!isTRUE(values$allow_subcontract)) {
      a <- numeric(m)
      a[idx("S", t)] <- 1
      add_constraint(a, "=", 0)
    }

    # Whole-worker plan: the workforce is fixed
    if (!is.null(fixed_workers)) {
      a <- numeric(m)
      a[idx("W", t)] <- 1
      add_constraint(a, "=", fixed_workers[t])
    }
  }

  # Required ending inventory
  a <- numeric(m)
  a[idx("I", n)] <- 1
  add_constraint(a, ">=", d$ending_inventory)

  result <- lpSolve::lp(
    direction = "min",
    objective.in = objective,
    const.mat = do.call(rbind, rows),
    const.dir = directions,
    const.rhs = rhs
  )
  if (result$status != 0) {
    return(list(success = FALSE, status = result$status))
  }

  sol <- result$solution
  get <- function(v) sol[vapply(seq_len(n), function(t) idx(v, t), numeric(1))]
  capacity_all <- k * df$WorkingDays
  plan <- data.frame(
    Period = df$Period,
    Demand = df$Demand,
    Workers = get("W"),
    Hired = get("H"),
    Fired = get("F"),
    Regular = get("P"),
    Overtime = get("O"),
    Subcontract = get("S"),
    Idle = pmax(0, capacity_all * get("W") - get("P")),
    Inventory = get("I")
  )
  # Remove solver noise such as 1e-13
  plan[, -1][abs(plan[, -1]) < 1e-9] <- 0

  costs <- data.frame(
    Item = c("Hiring", "Firing", "Regular production",
             "Overtime", "Subcontracting", "Inventory holding"),
    Cost = c(
      sum(plan$Hired) * cst$hiring,
      sum(plan$Fired) * cst$firing,
      sum(plan$Regular) * cst$regular,
      sum(plan$Overtime) * cst$overtime,
      sum(plan$Subcontract) * cst$subcontract,
      sum(plan$Inventory) * cst$holding
    )
  )
  total_production <- plan$Regular + plan$Overtime + plan$Subcontract
  list(
    success = TRUE,
    plan = plan,
    costs = costs,
    total = result$objval,
    cumulative_production = cumsum(total_production),
    cumulative_net_demand = cumsum(df$Demand) - d$starting_inventory
  )
}

# Original LP plan + whole-worker plan (workers rounded up, then re-optimized)
calculate_lp_plans <- function(values) {
  d <- values$data
  validate(
    need(requireNamespace("lpSolve", quietly = TRUE),
         "Install lpSolve first."),
    need(all(is.finite(unlist(d$costs))) && all(unlist(d$costs) >= 0),
         "All costs must be non-negative and finite."),
    need(all(is.finite(c(d$starting_workforce, d$starting_inventory,
                         d$ending_inventory))) &&
           all(c(d$starting_workforce, d$starting_inventory,
                 d$ending_inventory) >= 0),
         "Starting workforce and inventory values must be non-negative."),
    need(!isTRUE(values$allow_overtime) ||
           (is.finite(values$overtime_ratio) && values$overtime_ratio >= 0),
         "Overtime ratio must be non-negative.")
  )
  original <- solve_aggregate_lp(values)
  if (!isTRUE(original$success)) return(original)
  original$whole <- solve_aggregate_lp(
    values, fixed_workers = ceiling(round(original$plan$Workers, 6)))
  original
}


aggregate_planning_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Aggregate Planning"),
    p("Enter planning data, then select a planning method."),
    
    tabsetPanel(
      id = ns("planning_tab"),
      
      tabPanel(
        "Data Input",
        br(),
        
        fluidRow(
          column(
            5,
            wellPanel(
              h4("Demand Data"),
              
              radioButtons(
                ns("data_source"),
                "Data source",
                choices = c(
                  "Upload Excel / CSV" = "upload",
                  "Enter manually" = "manual"
                ),
                selected = "manual"
              ),
              
              conditionalPanel(
                condition = "input.data_source == 'upload'",
                ns = ns,
                fileInput(
                  ns("demand_file"),
                  "Upload demand data",
                  accept = c(".csv", ".xlsx", ".xls")
                )
              ),
              
              conditionalPanel(
                condition = "input.data_source == 'manual'",
                ns = ns,
                numericInput(
                  ns("n_periods"),
                  "Number of periods",
                  value = 4,
                  min = 1,
                  max = 60,
                  step = 1
                )
              ),
              
              p(
                "Required columns: Period, Demand, WorkingDays."
              ),
              
              hr(),
              
              h4("Plant Data"),
              
              numericInput(
                ns("starting_workforce"),
                "Starting workforce",
                value = 10,
                min = 0,
                step = 1
              ),
              
              numericInput(
                ns("starting_inventory"),
                "Starting inventory",
                value = 0,
                min = 0
              ),
              
              numericInput(
                ns("ending_inventory"),
                "Required ending inventory",
                value = 0,
                min = 0
              ),
              
              hr(),
              
              h4("Productivity"),
              
              radioButtons(
                ns("k_method"),
                "Productivity calculation",
                choices = c(
                  "Enter K directly" = "direct",
                  "Calculate from historical data" = "historical"
                ),
                selected = "direct"
              ),
              
              conditionalPanel(
                condition = "input.k_method == 'direct'",
                ns = ns,
                numericInput(
                  ns("k_direct"),
                  "K (units / worker / day)",
                  value = 2,
                  min = 0.0001
                )
              ),
              
              conditionalPanel(
                condition = "input.k_method == 'historical'",
                ns = ns,
                
                numericInput(
                  ns("historical_units"),
                  "Historical units produced",
                  value = 1000,
                  min = 0
                ),
                
                numericInput(
                  ns("historical_workers"),
                  "Historical number of workers",
                  value = 10,
                  min = 1
                ),
                
                numericInput(
                  ns("historical_days"),
                  "Historical working days",
                  value = 20,
                  min = 1
                )
              ),
              
              strong(textOutput(ns("k_result")))
            )
          ),
          
          column(
            7,
            wellPanel(
              h4("Cost Parameters"),
              
              fluidRow(
                column(
                  6,
                  numericInput(
                    ns("hiring_cost"),
                    "Hiring cost / worker",
                    value = 500,
                    min = 0
                  ),
                  
                  numericInput(
                    ns("firing_cost"),
                    "Firing cost / worker",
                    value = 700,
                    min = 0
                  ),
                  
                  numericInput(
                    ns("holding_cost"),
                    "Inventory holding cost / unit / period",
                    value = 5,
                    min = 0
                  )
                ),
                
                column(
                  6,
                  numericInput(
                    ns("regular_cost"),
                    "Regular production cost / unit",
                    value = 20,
                    min = 0
                  ),
                  
                  numericInput(
                    ns("overtime_cost"),
                    "Overtime production cost / unit",
                    value = 30,
                    min = 0
                  ),
                  
                  numericInput(
                    ns("subcontract_cost"),
                    "Subcontracting cost / unit",
                    value = 40,
                    min = 0
                  )
                )
              ),
              
              p(
                "All cost values must use the same currency."
              )
            ),
            
            wellPanel(
              h4("Period Data"),
              
              p(
                "Edit period names, demand forecasts and working days."
              ),
              
              uiOutput(ns("period_inputs")),
              
              hr(),
              
              actionButton(
                ns("apply_data"),
                "Apply Planning Data",
                class = "btn-primary"
              ),
              
              br(),
              br(),
              
              textOutput(ns("data_status"))
            )
          )
        ),
        
        hr(),
        
        h4("Planning Data Preview"),
        tableOutput(ns("data_preview"))
      ),
      
      
      
      tabPanel(
        "Heuristic Plans",
        
        h3("Heuristic Aggregate Planning"),
        
        p("Select a strategy to view its production plan and costs."),
        textOutput(ns("heuristic_data_status")),
        
        tabsetPanel(
          id = ns("heuristic_strategy"),
          
          # ---------------------------------
          # Chase Strategy
          # ---------------------------------
          
          tabPanel(
            "Chase Strategy",
            
            br(),
            
            h3("Chase Strategy - Zero Inventory Plan"),
            
            p("Workforce is adjusted to match demand in each period."),
            
            h4("Original Chase Plan"),
            tableOutput(ns("chase_original_table")),
            
            h4("Original Plan Costs"),
            tableOutput(ns("chase_original_costs")),
            
            strong(textOutput(ns("chase_original_total"))),
            
            hr(),
            
            h4("Whole-Worker Chase Plan"),
            tableOutput(ns("chase_whole_table")),
            
            h4("Whole-Worker Plan Costs"),
            tableOutput(ns("chase_whole_costs")),
            
            strong(textOutput(ns("chase_whole_total"))),
            
            hr(),
            
            h4("Cumulative Production vs. Net Demand"),
            plotOutput(ns("chase_plot"), height = "350px")
          ),
          
          # ---------------------------------
          # Level Strategy
          # ---------------------------------
          tabPanel(
            "Level Strategy", br(),
            h3("Level Strategy - Constant Workforce Plan"),
            p("One constant workforce is used across the planning horizon."),
            h4("Original Level Plan"),
            tableOutput(ns("level_original_table")),
            h4("Original Plan Costs"),
            tableOutput(ns("level_original_costs")),
            strong(textOutput(ns("level_original_total"))),
            textOutput(ns("level_original_status")),
            hr(),
            h4("Whole-Worker Level Plan"),
            tableOutput(ns("level_whole_table")),
            h4("Whole-Worker Plan Costs"),
            tableOutput(ns("level_whole_costs")),
            strong(textOutput(ns("level_whole_total"))),
            textOutput(ns("level_whole_status")),
            hr(),
            h4("Cumulative Production vs. Net Demand"),
            plotOutput(ns("level_plot"), height = "390px")
          ),

          # ---------------------------------
          # Mixed Strategy
          # ---------------------------------
          tabPanel(
            "Mixed Strategy", br(),
            h3("Mixed Strategy - Selected Workforce Changes"),
            p("Choose periods when the workforce may change. The workforce ",
              "remains constant between selected periods."),
            uiOutput(ns("mixed_change_controls")),
            p("The first period is always a workforce-setting period. ",
              "This is a segment-based heuristic, not a cost-optimal LP solution."),
            h4("Original Mixed Plan"),
            tableOutput(ns("mixed_original_table")),
            h4("Original Plan Costs"),
            tableOutput(ns("mixed_original_costs")),
            strong(textOutput(ns("mixed_original_total"))),
            textOutput(ns("mixed_original_status")),
            hr(),
            h4("Whole-Worker Mixed Plan"),
            tableOutput(ns("mixed_whole_table")),
            h4("Whole-Worker Plan Costs"),
            tableOutput(ns("mixed_whole_costs")),
            strong(textOutput(ns("mixed_whole_total"))),
            textOutput(ns("mixed_whole_status")),
            hr(),
            h4("Cumulative Production vs. Net Demand"),
            plotOutput(ns("mixed_plot"), height = "390px")
          )
        )
      ),
      
      
      tabPanel(
        "Linear Programming",
        h3("Linear Programming Optimization"),
        p("Minimize total aggregate planning cost using the applied planning data."),
        
        checkboxInput(
          ns("lp_allow_overtime"),
          "Allow overtime production",
          value = TRUE
        ),
        conditionalPanel(
          condition = "input.lp_allow_overtime",
          ns = ns,
          numericInput(
            ns("lp_overtime_ratio"),
            "Maximum overtime as a fraction of regular capacity",
            value = 0.25, min = 0, max = 1, step = 0.05
          )
        ),
        checkboxInput(
          ns("lp_allow_idle"),
          "Allow idle time (regular capacity may be left unused)",
          value = TRUE
        ),
        checkboxInput(
          ns("lp_allow_subcontract"),
          "Allow subcontracting",
          value = TRUE
        ),
        p(class = "text-muted",
          "Whole workers: the optimal number of workers is rounded up in each ",
          "period and the plan is re-optimized with that workforce. Both plans ",
          "are shown below."),
        
        actionButton(ns("lp_solve"), "Run Optimization", class = "btn-primary"),
        hr(),
        
        h4("Solver Status"),
        verbatimTextOutput(ns("lp_status")),
        
        tabsetPanel(
          tabPanel("Production Plan", br(),
                   h4("Original LP Plan"), tableOutput(ns("lp_plan")),
                   h4("Whole-Worker LP Plan"), tableOutput(ns("lp_plan_whole"))),
          tabPanel("Cost Breakdown", br(),
                   h4("Original Plan Costs"), tableOutput(ns("lp_costs")),
                   h4("Whole-Worker Plan Costs"), tableOutput(ns("lp_costs_whole"))),
          tabPanel("Production Graph", br(),
                   h4("Cumulative Production vs. Net Demand"),
                   plotOutput(ns("lp_plot"), height = "390px"))
        )
      ),
      tabPanel(
        "Transportation",
        h3("Transportation Method"),
        p("Apply planning data first. Then enter the available production capacity for each source and period."),
        p("Beginning inventory is deducted from the first period's demand. Required ending inventory is added to the final period's demand."),
        uiOutput(ns("transport_capacity_inputs")),
        fluidRow(
          column(6, actionButton(ns("transport_nw_solve"), "Calculate: North-West Corner", class = "btn-primary")),
          column(6, actionButton(ns("transport_lc_solve"), "Calculate: Least-Cost", class = "btn-primary"))
        ),
        p(class = "text-muted", "Both methods produce feasible allocations when the entered capacities permit. Least-Cost is a heuristic and is not guaranteed to be globally optimal."),
        hr(),
        h4("Calculation Status"),
        verbatimTextOutput(ns("transport_status")),
        tabsetPanel(
          tabPanel("Allocation Matrix", tableOutput(ns("transport_allocation"))),
          tabPanel("Cost Breakdown", tableOutput(ns("transport_costs"))),
          tabPanel("Production Graph", plotOutput(ns("transport_plot"), height = "390px"))
        )
      ),
      
      tabPanel(
        "Disaggregation",
        h3("Disaggregation - Square-Root Rule"),
        p("Allocate the aggregate production quantity among individual product families."),
        p("The allocation minimizes the annual setup-cost expression while satisfying the total quantity and product-specific minimum and maximum limits."),
        fluidRow(
          column(
            4,
            wellPanel(
              h4("Aggregate Production"),
              numericInput(
                ns("disagg_total_quantity"),
                "Aggregate production quantity (X*)",
                value = 4800,
                min = 0.0001
              ),
              numericInput(
                ns("disagg_product_count"),
                "Number of product families",
                value = 3,
                min = 1,
                max = 20,
                step = 1
              ),
              actionButton(
                ns("disagg_calculate"),
                "Calculate Allocation",
                class = "btn-primary"
              )
            )
          ),
          column(
            8,
            wellPanel(
              h4("Product Family Parameters"),
              p("Enter annual usage, setup cost, and the permitted production range for each product."),
              uiOutput(ns("disagg_product_inputs"))
            )
          )
        ),
        hr(),
        h4("Calculation Status"),
        verbatimTextOutput(ns("disagg_status")),
        tabsetPanel(
          tabPanel("Allocation Results", tableOutput(ns("disagg_allocation"))),
          tabPanel("Allocation Graph", plotOutput(ns("disagg_plot"), height = "360px")),
          tabPanel("Calculation Details", tableOutput(ns("disagg_summary")))
        )
      )
    )
  )
}


aggregate_planning_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # -------------------------------------
    # 1. Data source
    # -------------------------------------
    
    uploaded_data <- reactive({
      req(input$demand_file)
      
      file_path <- input$demand_file$datapath
      file_name <- input$demand_file$name
      
      if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
        df <- read.csv(
          file_path,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        df <- as.data.frame(
          readxl::read_excel(file_path)
        )
      }
      
      validate(
        need(
          all(c("Period", "Demand", "WorkingDays") %in% names(df)),
          "File must contain Period, Demand and WorkingDays columns."
        )
      )
      
      df <- df[, c("Period", "Demand", "WorkingDays")]
      
      df
    })
    
    base_data <- reactive({
      if (input$data_source == "upload") {
        uploaded_data()
      } else {
        n <- input$n_periods
        
        data.frame(
          Period = paste0("Period ", seq_len(n)),
          Demand = rep(100, n),
          WorkingDays = rep(20, n),
          stringsAsFactors = FALSE
        )
      }
    })
    
    # -------------------------------------
    # 2. Editable period inputs
    # -------------------------------------
    
    output$period_inputs <- renderUI({
      df <- base_data()
      
      tagList(
        fluidRow(
          column(4, strong("Period")),
          column(4, strong("Demand")),
          column(4, strong("Working days"))
        ),
        
        lapply(seq_len(nrow(df)), function(i) {
          fluidRow(
            column(
              4,
              textInput(
                ns(paste0("period_", i)),
                label = NULL,
                value = as.character(df$Period[i])
              )
            ),
            
            column(
              4,
              numericInput(
                ns(paste0("demand_", i)),
                label = NULL,
                value = suppressWarnings(
                  as.numeric(df$Demand[i])
                ),
                min = 0
              )
            ),
            
            column(
              4,
              numericInput(
                ns(paste0("days_", i)),
                label = NULL,
                value = suppressWarnings(
                  as.numeric(df$WorkingDays[i])
                ),
                min = 1
              )
            )
          )
        })
      )
    })
    
    edited_data <- reactive({
      df <- base_data()
      n <- nrow(df)
      
      period <- sapply(seq_len(n), function(i) {
        input[[paste0("period_", i)]]
      })
      
      demand <- sapply(seq_len(n), function(i) {
        input[[paste0("demand_", i)]]
      })
      
      days <- sapply(seq_len(n), function(i) {
        input[[paste0("days_", i)]]
      })
      
      req(
        length(period) == n,
        length(demand) == n,
        length(days) == n
      )
      
      data.frame(
        Period = as.character(period),
        Demand = as.numeric(demand),
        WorkingDays = as.numeric(days),
        stringsAsFactors = FALSE
      )
    })
    
    # -------------------------------------
    # 3. Productivity
    # -------------------------------------
    
    productivity <- reactive({
      if (input$k_method == "direct") {
        input$k_direct
      } else {
        req(
          input$historical_workers > 0,
          input$historical_days > 0
        )
        
        input$historical_units /
          (
            input$historical_workers *
              input$historical_days
          )
      }
    })
    
    output$k_result <- renderText({
      paste(
        "Productivity K =",
        round(productivity(), 4),
        "units / worker / day"
      )
    })
    
    # -------------------------------------
    # 4. Apply and validate data
    # -------------------------------------
    
    planning_draft <- reactive({
      df <- edited_data()
      k <- productivity()
      data <- list(
        periods = df,
        productivity = k,
        
        starting_workforce =
          input$starting_workforce,
        
        starting_inventory =
          input$starting_inventory,
        
        ending_inventory =
          input$ending_inventory,
        
        costs = list(
          hiring = input$hiring_cost,
          firing = input$firing_cost,
          holding = input$holding_cost,
          regular = input$regular_cost,
          overtime = input$overtime_cost,
          subcontract = input$subcontract_cost
        )
      )
      list(
        source = input$data_source,
        file = if (identical(input$data_source, "upload")) input$demand_file,
        data = data
      )
    })

    # Capture on the click even when the result tab is hidden.
    capture_on_click <- function(event, calculate, priority = 0) {
      saved <- reactiveVal(NULL)
      observeEvent(event(), {
        if (event() != 0) {
          saved(tryCatch(list(value = calculate()), error = function(e) list(error = e)))
        }
      }, priority = priority)
      reactive({
        state <- saved()
        if (!is.null(state$error)) stop(state$error)
        state$value
      })
    }

    applied_draft <- capture_on_click(reactive(input$apply_data), planning_draft,
                                      priority = 10)

    planning_applied <- reactive({
      draft <- applied_draft()
      req(!is.null(draft))
      df <- draft$data$periods
      k <- draft$data$productivity
      validate(
        need(
          nrow(df) > 0,
          "At least one period is required."
        ),
        need(
          all(!is.na(df$Demand)) &&
            all(df$Demand >= 0),
          "Demand must be non-negative."
        ),
        need(
          all(!is.na(df$WorkingDays)) &&
            all(df$WorkingDays > 0),
          "Working days must be positive."
        ),
        need(
          all(nzchar(trimws(df$Period))),
          "Period names cannot be empty."
        ),
        need(
          is.finite(k) && k > 0,
          "Productivity K must be positive."
        )
      )
      draft
    })

    # Missing or temporarily invalid form fields cannot match a saved result.
    inputs_match <- function(saved, current) {
      tryCatch(identical(saved, current()), error = function(e) FALSE)
    }

    planning_message <- reactive({
      if (is.null(input$apply_data) || input$apply_data == 0) {
        return("Apply Planning Data to calculate plans.")
      }
      applied <- tryCatch(applied_draft(), error = function(e) NULL)
      if (is.null(applied) || !inputs_match(applied, planning_draft)) {
        return("Planning inputs changed. Click Apply Planning Data to update plans.")
      }
      planning_applied()
      NULL
    })

    planning_data <- reactive({
      req(is.null(planning_message()))
      planning_applied()$data
    })

    # Keep the inputs used for each run alongside its result. All outputs
    # consume the same guarded result, including the status and cost summary.
    run_on_click <- function(event, inputs, calculate, rerun_message,
                             uses_planning_data = TRUE) {
      run_event <- function() {
        click <- event()
        if (uses_planning_data && !isolate(inputs_match(NULL, planning_message))) {
          return(NULL)
        }
        click
      }
      run <- capture_on_click(run_event, function() {
        values <- inputs()
        list(inputs = values,
             outcome = tryCatch(list(result = calculate(values)),
                                error = function(e) list(error = e)))
      })
      reactive({
        if (uses_planning_data) {
          message <- planning_message()
          if (!is.null(message)) {
            return(list(success = FALSE, stale_message = message))
          }
        }
        if (is.null(event()) || event() == 0) {
          return(list(success = FALSE, stale_message = rerun_message))
        }
        saved <- tryCatch(run(), error = function(e) NULL)
        if (is.null(saved) || !inputs_match(saved$inputs, inputs)) {
          return(list(success = FALSE, stale_message = rerun_message))
        }
        if (!is.null(saved$outcome$error)) stop(saved$outcome$error)
        saved$outcome$result
      })
    }

    output$data_preview <- renderTable(planning_data()$periods)

    output$data_status <- renderText({
      message <- planning_message()
      if (!is.null(message)) return(message)
      paste("Planning data applied successfully.",
            nrow(planning_data()$periods), "periods are ready.")
    })

    output$heuristic_data_status <- renderText(planning_message())

    # -------------------------------------
    # 5. Chase Strategy
    # -------------------------------------
    
    chase_original <- reactive({
      req(planning_data())
      calculate_chase_plan(planning_data(), FALSE)
    })
    
    chase_whole <- reactive({
      req(planning_data())
      calculate_chase_plan(planning_data(), TRUE)
    })
    
    output$chase_original_table <- renderTable({
      chase_original()$plan
    })
    
    output$chase_whole_table <- renderTable({
      chase_whole()$plan
    })
    
    output$chase_original_costs <- renderTable({
      chase_original()$costs
    }, digits = 2)
    
    output$chase_whole_costs <- renderTable({
      chase_whole()$costs
    }, digits = 2)
    
    output$chase_original_total <- renderText({
      paste(
        "Total Cost:",
        round(chase_original()$total, 2)
      )
    })
    
    output$chase_whole_total <- renderText({
      paste(
        "Total Cost:",
        round(chase_whole()$total, 2)
      )
    })
    
    output$chase_plot <- renderPlot({
      
      original <- chase_original()
      whole <- chase_whole()
      
      n <- nrow(original$plan)
      x <- seq_len(n)
      
      y_values <- c(
        original$cumulative_production,
        whole$cumulative_production,
        original$cumulative_net_demand
      )
      
      plot(
        x,
        original$cumulative_net_demand,
        type = "o",
        pch = 16,
        col = "black",
        lwd = 2,
        ylim = range(c(0, y_values)),
        xaxt = "n",
        xlab = "Period",
        ylab = "Cumulative Units",
        main = "Cumulative Production vs. Net Demand"
      )
      
      axis(
        1,
        at = x,
        labels = original$plan$Period
      )
      
      lines(
        x,
        original$cumulative_production,
        type = "o",
        pch = 17,
        col = "blue",
        lwd = 2
      )
      
      lines(
        x,
        whole$cumulative_production,
        type = "o",
        pch = 15,
        col = "red",
        lwd = 2
      )
      
      legend(
        "topleft",
        legend = c(
          "Cumulative Net Demand",
          "Original Production",
          "Whole-Worker Production"
        ),
        col = c("black", "blue", "red"),
        lty = 1,
        pch = c(16, 17, 15),
        bty = "n"
      )
    })
    
    # -------------------------------------
    # 6. Level Strategy
    # -------------------------------------
    level_original <- reactive({
      req(planning_data())
      calculate_level_plan(planning_data(), FALSE)
    })
    level_whole <- reactive({
      req(planning_data())
      calculate_level_plan(planning_data(), TRUE)
    })
    output$level_original_table <- renderTable(level_original()$plan)
    output$level_whole_table <- renderTable(level_whole()$plan)
    output$level_original_costs <- renderTable(level_original()$costs, digits = 2)
    output$level_whole_costs <- renderTable(level_whole()$costs, digits = 2)
    output$level_original_total <- renderText({
      paste("Total Cost:", round(level_original()$total, 2))
    })
    output$level_whole_total <- renderText({
      paste("Total Cost:", round(level_whole()$total, 2))
    })
    output$level_original_status <- renderText(heuristic_status(level_original()))
    output$level_whole_status <- renderText(heuristic_status(level_whole()))
    output$level_plot <- renderPlot({
      plot_heuristic_comparison(level_original(), level_whole(),
                                "Level Plan: Cumulative Production vs. Net Demand")
    })

    # -------------------------------------
    # 7. Mixed Strategy
    # -------------------------------------
    output$mixed_change_controls <- renderUI({
      req(planning_applied())
      df <- planning_applied()$data$periods
      if (nrow(df) <= 1) return(p("Only one period is available."))
      checkboxGroupInput(
        ns("mixed_change_periods"),
        "Additional periods where workforce changes are allowed",
        choices = setNames(as.character(seq.int(2, nrow(df))),
                           df$Period[-1]),
        selected = character(0)
      )
    })
    mixed_starts <- reactive({
      req(planning_data())
      c(1L, as.integer(input$mixed_change_periods))
    })
    mixed_original <- reactive({
      req(planning_data())
      calculate_mixed_plan(planning_data(), mixed_starts(), FALSE)
    })
    mixed_whole <- reactive({
      req(planning_data())
      calculate_mixed_plan(planning_data(), mixed_starts(), TRUE)
    })
    output$mixed_original_table <- renderTable(mixed_original()$plan)
    output$mixed_whole_table <- renderTable(mixed_whole()$plan)
    output$mixed_original_costs <- renderTable(mixed_original()$costs, digits = 2)
    output$mixed_whole_costs <- renderTable(mixed_whole()$costs, digits = 2)
    output$mixed_original_total <- renderText({
      paste("Total Cost:", round(mixed_original()$total, 2))
    })
    output$mixed_whole_total <- renderText({
      paste("Total Cost:", round(mixed_whole()$total, 2))
    })
    output$mixed_original_status <- renderText(heuristic_status(mixed_original()))
    output$mixed_whole_status <- renderText(heuristic_status(mixed_whole()))
    output$mixed_plot <- renderPlot({
      plot_heuristic_comparison(mixed_original(), mixed_whole(),
                                "Mixed Plan: Cumulative Production vs. Net Demand")
    })

    # -------------------------------------
    # 8. Linear Programming Optimization
    # -------------------------------------
    
    lp_inputs <- reactive({
      list(data = planning_applied()$data,
           allow_overtime = input$lp_allow_overtime,
           overtime_ratio = if (isTRUE(input$lp_allow_overtime)) input$lp_overtime_ratio,
           allow_idle = input$lp_allow_idle,
           allow_subcontract = input$lp_allow_subcontract)
    })

    lp_result <- run_on_click(
      reactive(input$lp_solve), lp_inputs, calculate_lp_plans,
      "Click Run Optimization to calculate a result for the current inputs."
    )

    output$lp_status <- renderText({
      r <- lp_result()
      if (!is.null(r$stale_message)) return(r$stale_message)
      
      if (!r$success) {
        return(paste("Optimization failed. Solver status:", r$status,
                     "(the plan may be infeasible with the selected options)."))
      }
      
      whole_text <- if (isTRUE(r$whole$success)) {
        paste("Whole-worker plan total cost:", round(r$whole$total, 2))
      } else {
        "Whole-worker plan could not be solved."
      }
      paste0("Optimal solution found. Minimum total cost: ", round(r$total, 2),
             ". ", whole_text)
    })
    
    output$lp_plan <- renderTable({
      req(lp_result()$success)
      lp_result()$plan
    }, digits = 2)

    output$lp_plan_whole <- renderTable({
      req(lp_result()$success, lp_result()$whole$success)
      lp_result()$whole$plan
    }, digits = 2)
    
    lp_cost_table <- function(r) {
      rbind(r$costs, data.frame(Item = "TOTAL", Cost = sum(r$costs$Cost)))
    }
    output$lp_costs <- renderTable({
      req(lp_result()$success)
      lp_cost_table(lp_result())
    }, digits = 2)

    output$lp_costs_whole <- renderTable({
      req(lp_result()$success, lp_result()$whole$success)
      lp_cost_table(lp_result()$whole)
    }, digits = 2)
    
    output$lp_plot <- renderPlot({
      req(lp_result()$success)
      r <- lp_result()
      plot_heuristic_comparison(r, r$whole,
                                "LP Plan: Cumulative Production vs. Net Demand")
    })

    # -------------------------------------
    # 9. Transportation Method
    # -------------------------------------
    output$transport_capacity_inputs <- renderUI({
      req(planning_applied())
      d <- planning_applied()$data
      tagList(
        h4("Production Capacity by Period"),
        p("Enter the maximum units available from each production source in every period."),
        lapply(seq_len(nrow(d$periods)), function(i) {
          fluidRow(
            column(3, strong(d$periods$Period[i])),
            column(3, numericInput(ns(paste0("transport_regular_", i)),
                                   "Regular time capacity",
                                   value = d$starting_workforce * d$productivity * d$periods$WorkingDays[i], min = 0)),
            column(3, numericInput(ns(paste0("transport_overtime_", i)),
                                   "Overtime capacity", value = 0, min = 0)),
            column(3, numericInput(ns(paste0("transport_subcontract_", i)),
                                   "Subcontracting capacity", value = 0, min = 0))
          )
        })
      )
    })

    transportation_capacities <- reactive({
      n <- nrow(planning_applied()$data$periods)
      values <- lapply(seq_len(n), function(i) {
        c(input[[paste0("transport_regular_", i)]],
          input[[paste0("transport_overtime_", i)]],
          input[[paste0("transport_subcontract_", i)]])
      })
      req(all(lengths(values) == 3), all(vapply(values, function(x) all(is.finite(x)), logical(1))))
      out <- as.data.frame(do.call(rbind, values))
      names(out) <- c("Regular", "Overtime", "Subcontract")
      out
    })

    transport_inputs <- reactive({
      d <- planning_applied()$data
      d$periods <- d$periods[, c("Period", "Demand"), drop = FALSE]
      d$costs <- d$costs[c("regular", "overtime", "subcontract", "holding")]
      list(data = d[c("periods", "starting_inventory", "ending_inventory", "costs")],
           capacities = transportation_capacities())
    })
    transport_nw_result <- run_on_click(
      reactive(input$transport_nw_solve), transport_inputs,
      function(values) solve_transportation_plan(values$data, values$capacities, "northwest"),
      "Click Calculate: North-West Corner to calculate a result for the current inputs."
    )
    transport_lc_result <- run_on_click(
      reactive(input$transport_lc_solve), transport_inputs,
      function(values) solve_transportation_plan(values$data, values$capacities, "least_cost"),
      "Click Calculate: Least-Cost to calculate a result for the current inputs."
    )
    transport_method <- reactiveVal("northwest")
    observeEvent(input$transport_nw_solve, { transport_method("northwest") })
    observeEvent(input$transport_lc_solve, { transport_method("least_cost") })
    active_transport_result <- reactive({
      if (identical(transport_method(), "least_cost")) {
        transport_lc_result()
      } else {
        transport_nw_result()
      }
    })

    output$transport_status <- renderText({
      r <- active_transport_result()
      if (!is.null(r$stale_message)) return(r$stale_message)
      if (!isTRUE(r$success)) return(paste("Calculation could not be completed:", r$message))
      paste0(if (r$method == "northwest") "North-West Corner" else "Least-Cost",
             " allocation completed. Total cost: ", round(r$total, 2),
             ". Unused capacity: ", round(r$excess_capacity, 2), " units.")
    })
    output$transport_allocation <- renderTable({
      r <- active_transport_result()
      req(r, isTRUE(r$success))
      r$allocation
    }, digits = 2)
    output$transport_costs <- renderTable({
      r <- active_transport_result()
      req(r, isTRUE(r$success))
      total_row <- as.data.frame(as.list(rep(NA, ncol(r$costs))), check.names = FALSE)
      names(total_row) <- names(r$costs)
      total_row$Item <- "TOTAL"
      total_row$Cost <- r$total
      rbind(r$costs, total_row)
    }, digits = 2)
    output$transport_plot <- renderPlot({
      r <- active_transport_result()
      req(r, isTRUE(r$success))
      d <- r$adjusted_demand
      x <- seq_len(nrow(d))
      cumulative_demand <- cumsum(d$Demand)
      cumulative_production <- cumsum(r$period_production)
      plot(x, cumulative_demand, type = "o", pch = 16, col = "red",
           xaxt = "n", xlab = "Period", ylab = "Cumulative Units",
           main = "Cumulative Production vs. Adjusted Demand",
           ylim = range(c(0, cumulative_demand, cumulative_production)))
      lines(x, cumulative_production, type = "o", pch = 17, col = "blue")
      axis(1, at = x, labels = d$Period)
      legend("topleft", c("Cumulative Adjusted Demand", "Cumulative Production"),
             col = c("red", "blue"), lty = 1, pch = c(16, 17), bty = "n")
    })

    # -------------------------------------
    # 10. Disaggregation - Square-Root Rule
    # -------------------------------------
    output$disagg_product_inputs <- renderUI({
      n <- input$disagg_product_count
      req(n, n >= 1, n <= 20)
      tagList(
        fluidRow(
          column(3, strong("Product")),
          column(2, strong("Annual usage")),
          column(2, strong("Setup cost")),
          column(2, strong("Minimum")),
          column(3, strong("Maximum"))
        ),
        lapply(seq_len(n), function(i) {
          fluidRow(
            column(3, textInput(ns(paste0("disagg_name_", i)), NULL,
                                value = paste("Product", LETTERS[(i - 1) %% 26 + 1]))),
            column(2, numericInput(ns(paste0("disagg_usage_", i)), NULL,
                                   value = c(1000, 2000, 1800)[(i - 1) %% 3 + 1],
                                   min = 0.0001)),
            column(2, numericInput(ns(paste0("disagg_setup_", i)), NULL,
                                   value = c(500, 800, 600)[(i - 1) %% 3 + 1],
                                   min = 0.0001)),
            column(2, numericInput(ns(paste0("disagg_min_", i)), NULL,
                                   value = 0, min = 0)),
            column(3, numericInput(ns(paste0("disagg_max_", i)), NULL,
                                   value = 100000, min = 0))
          )
        })
      )
    })

    disagg_products <- reactive({
      n <- input$disagg_product_count
      req(n, n >= 1, n <= 20)
      rows <- lapply(seq_len(n), function(i) {
        data.frame(
          Product = input[[paste0("disagg_name_", i)]],
          AnnualUsage = input[[paste0("disagg_usage_", i)]],
          SetupCost = input[[paste0("disagg_setup_", i)]],
          MinQty = input[[paste0("disagg_min_", i)]],
          MaxQty = input[[paste0("disagg_max_", i)]],
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    })

    disagg_inputs <- reactive({
      list(total_quantity = input$disagg_total_quantity, products = disagg_products())
    })
    disagg_result <- run_on_click(
      reactive(input$disagg_calculate), disagg_inputs,
      function(values) calculate_disaggregation(values$total_quantity, values$products),
      "Click Calculate Allocation to calculate a result for the current inputs.",
      uses_planning_data = FALSE
    )

    output$disagg_status <- renderText({
      r <- disagg_result()
      if (!is.null(r$stale_message)) return(r$stale_message)
      if (!isTRUE(r$success)) {
        return(paste("Allocation could not be completed:", r$message))
      }
      paste0(
        "Allocation completed. Total allocated: ",
        round(r$allocated_total, 4), " / ",
        round(r$total_quantity, 4),
        " units. Annual setup-cost objective: ",
        round(r$objective_cost, 4), "."
      )
    })

    output$disagg_allocation <- renderTable({
      r <- disagg_result()
      req(r, isTRUE(r$success))
      out <- r$allocation
      out$AnnualUsage <- round(out$AnnualUsage, 3)
      out$SetupCost <- round(out$SetupCost, 3)
      out$Weight <- round(out$Weight, 3)
      out$AllocatedQty <- round(out$AllocatedQty, 3)
      out$AllocationPercent <- round(out$AllocationPercent, 2)
      out
    }, digits = 3)

    output$disagg_plot <- renderPlot({
      r <- disagg_result()
      req(r, isTRUE(r$success))
      barplot(
        r$allocation$AllocatedQty,
        names.arg = r$allocation$Product,
        las = 2,
        ylab = "Allocated Production Quantity",
        main = "Product-Level Production Allocation"
      )
    })

    output$disagg_summary <- renderTable({
      r <- disagg_result()
      req(r, isTRUE(r$success))
      data.frame(
        Metric = c(
          "Aggregate production quantity (X*)",
          "Total allocated quantity",
          "Total minimum quantity",
          "Total maximum quantity",
          "Annual setup-cost objective"
        ),
        Value = c(
          r$total_quantity,
          r$allocated_total,
          r$minimum_total,
          r$maximum_total,
          r$objective_cost
        )
      )
    }, digits = 4)
  })
}
