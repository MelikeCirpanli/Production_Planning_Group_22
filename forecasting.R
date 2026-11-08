# =====================================================================
# Topic: Forecasting  (Assignment 2)
# Nahmias, Chapter 2
#
# Forecasting
#   ├── Stationary Series
#   │     Moving Average        F_t = (D_{t-1} + ... + D_{t-N}) / N
#   │     WMA                   F_t = w1*D_{t-1} + ... + wN*D_{t-N}
#   │     Exponential Smoothing F_t = a*D_{t-1} + (1-a)*F_{t-1}
#   │     Quick and Dirty Est.  c_j from each cycle's own average
#   │     Seasonal Factors      c_j = mean of season j / overall mean
#   │     Comparison
#   ├── Trend   (no regression forecasting module, as asked in class)
#   │     Holt's Method
#   │     Seasonal Decomposition using MA
#   │     Winters Method
#   │     Comparison
#   └── Box-Jenkins
#         Identification (time series plot, ACF, PACF, differencing)
#         AR, MA, ARMA, ARIMA (manual orders or best orders by AICc)
#         auto.arima (best ARIMA model)
#
# Parameters:      every smoothing method can use values typed by the user
#                  or the best values found by the app (grid search).
# Initial values:  ES, Holt and Winters: regression fitted by the app, or
#                  level / slope entered manually.
#
# Forecast error:  e_t = F_t - D_t
# MAD  = mean of |e_t|
# MSE  = mean of e_t^2
# MAPE = mean of |e_t / D_t| x 100
#
# Packages: shiny, readxl, forecast  (forecast is NEW -> update manifest.json)
# All helper names start with "fc_" so they do not clash with other modules.
# =====================================================================


# ---------------------------------------------------------------------
# 1) HELPER FUNCTIONS (the calculations; independent of the screen)
# ---------------------------------------------------------------------

# Sample data: 5 years of quarterly demand (trend + seasonality)
fc_sample <- c(112, 136, 158, 121, 128, 150, 177, 134,
               141, 168, 195, 149, 155, 182, 214, 163,
               170, 199, 231, 178)

# Text -> numbers. Separators: space, new line, comma or semicolon.
# Use a dot for decimals (12.5).
fc_parse_numbers <- function(txt) {
  parts <- unlist(strsplit(txt, "[,;[:space:]]+"))
  x <- suppressWarnings(as.numeric(parts[parts != ""]))
  x[!is.na(x)]
}

# Read a .csv (comma or semicolon) or .xlsx file
fc_read_file <- function(path, name) {
  if (tolower(tools::file_ext(name)) %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(path)))
  }
  first <- readLines(path, n = 1, warn = FALSE)
  if (grepl(";", first, fixed = TRUE)) read.csv2(path) else read.csv(path)
}

# Season position of period t: 1, 2, ..., N
fc_season_pos <- function(t, N) ((t - 1) %% N) + 1

# MAD, MSE, MAPE over the periods that have a forecast
fc_errors <- function(d, F) {
  t <- which(!is.na(F) & !is.na(d))
  if (length(t) == 0) return(c(MAD = NA, MSE = NA, MAPE = NA, Periods = 0))
  e <- F[t] - d[t]
  nz <- d[t] != 0
  c(MAD = mean(abs(e)), MSE = mean(e^2),
    MAPE = mean(abs(e[nz] / d[t][nz])) * 100, Periods = length(t))
}

fc_error_table <- function(d, F) {
  m <- fc_errors(d, F)
  t <- which(!is.na(F))
  data.frame(
    "Measure" = c("MAD", "MSE", "MAPE (%)"),
    "Value" = round(unname(m[1:3]), 3),
    "Periods evaluated" = if (length(t)) paste0(min(t), "–", length(d)) else "-",
    check.names = FALSE
  )
}

# Plot: actual demand, fitted forecasts and future forecasts
fc_plot <- function(d, res, title) {
  n <- length(d); h <- length(res$future)
  all_F <- c(res$fitted, res$future)
  plot(1:n, d, type = "o", pch = 19, col = "grey30",
       xlim = c(1, n + max(h, 1)), ylim = range(c(d, all_F), na.rm = TRUE),
       xlab = "Period", ylab = "Demand", main = title)
  lines(1:n, res$fitted, type = "o", pch = 17, lty = 2, col = "#2F6FB0")
  if (h > 0) {
    lines(n:(n + h), c(d[n], res$future), lty = 2, col = "#B4501E")
    points((n + 1):(n + h), res$future, pch = 17, cex = 1.6, col = "#B4501E")
    abline(v = n + 0.5, lty = 3, col = "grey60")
  }
  legend("topleft", bty = "n",
         legend = c("Actual demand", "Forecast", "Future forecast"),
         col = c("grey30", "#2F6FB0", "#B4501E"), pch = c(19, 17, 17), lty = c(1, 2, 2))
}

# Period table: D, F, e = F - D (future periods appended)
fc_period_table <- function(d, res, extra = NULL) {
  n <- length(d); h <- length(res$future)
  D <- c(d, rep(NA, h))
  F <- c(res$fitted, res$future)
  out <- data.frame("Period" = seq_len(n + h), "Demand (D)" = D, check.names = FALSE)
  if (!is.null(extra)) {
    extra <- extra[c(seq_len(n), rep(NA, h)), , drop = FALSE]
    out <- cbind(out, extra)
  }
  out[["Forecast (F)"]] <- round(F, 2)
  out[["Error (e = F - D)"]] <- round(F - D, 2)
  rownames(out) <- NULL
  out
}

# Seasonal factors table
fc_factor_table <- function(cj) {
  data.frame("Season (j)" = seq_along(cj), "Factor c(j)" = round(as.numeric(cj), 4),
             check.names = FALSE)
}

# ---------------- Stationary series methods ----------------

# Moving average MA(N)
fc_ma <- function(d, N, h) {
  n <- length(d); F <- rep(NA_real_, n)
  if (n > N) for (t in (N + 1):n) F[t] <- mean(d[(t - N):(t - 1)])
  list(fitted = F, future = rep(mean(d[(n - N + 1):n]), h))
}

# Weighted moving average. w: from the most recent period to the oldest.
fc_wma <- function(d, w, h) {
  w <- w / sum(w); N <- length(w); n <- length(d); F <- rep(NA_real_, n)
  if (n > N) for (t in (N + 1):n) F[t] <- sum(w * d[(t - 1):(t - N)])
  list(fitted = F, future = rep(sum(w * d[n:(n - N + 1)]), h), w = w)
}

# Exponential smoothing  F_t = a*D_{t-1} + (1-a)*F_{t-1}
# Initial value (the first forecast):
#   init = "avg"    : F_{k+1} = average of D_1 ... D_k
#   init = "reg"    : straight line fitted to D_1 ... D_k, F_{k+1} = value of the line at period k
#   init = "manual" : F_1 = level typed by the user
fc_es <- function(d, alpha, h, k = 1, init = "avg", level = NULL) {
  n <- length(d); F <- rep(NA_real_, n)
  if (init == "manual") {
    s <- 1; f0 <- level
  } else {
    k <- max(1, round(k))
    if (init == "reg") {
      k <- max(2, k)
      m <- lm(d[1:k] ~ seq_len(k))
      f0 <- unname(coef(m)[1] + coef(m)[2] * k)
    } else {
      f0 <- mean(d[1:k])
    }
    s <- k + 1
  }
  F[s] <- f0
  if (n > s) for (t in (s + 1):n) F[t] <- alpha * d[t - 1] + (1 - alpha) * F[t - 1]
  nxt <- alpha * d[n] + (1 - alpha) * F[n]
  list(fitted = F[1:n], future = rep(nxt, h), f0 = f0, s = s)
}

# Seasonal factors for a stationary (no trend) series
fc_seasonal_factors <- function(d, N, h) {
  n <- length(d); pos <- fc_season_pos(1:n, N)
  avg <- mean(d)
  cj <- tapply(d, pos, mean) / avg
  cj <- cj * N / sum(cj)                       # factors sum to N
  fpos <- fc_season_pos((n + 1):(n + h), N)
  list(fitted = as.numeric(avg * cj[pos]),
       future = if (h > 0) as.numeric(avg * cj[fpos]) else numeric(0),
       factors = cj, avg = avg)
}

# Quick and dirty estimation of seasonal factors:
# each demand is divided by the average of ITS OWN cycle (e.g. its own year),
# then the ratios of the same season are averaged. Only full cycles are used.
fc_quick_dirty <- function(d, N, h) {
  n <- length(d); K <- n %/% N
  pos <- fc_season_pos(1:n, N)
  cyc <- (seq_len(K * N) - 1) %/% N + 1
  cycle_avg <- tapply(d[1:(K * N)], cyc, mean)
  ratio <- d[1:(K * N)] / cycle_avg[cyc]
  cj <- tapply(ratio, pos[1:(K * N)], mean)
  cj <- cj * N / sum(cj)
  avg <- mean(d)
  fpos <- fc_season_pos((n + 1):(n + h), N)
  list(fitted = as.numeric(avg * cj[pos]),
       future = if (h > 0) as.numeric(avg * cj[fpos]) else numeric(0),
       factors = cj, avg = avg, cycle_avg = cycle_avg, K = K)
}

# ---------------- Trend methods ----------------

# Holt's method
#   S_t = a*D_t + (1-a)*(S_{t-1} + G_{t-1})
#   G_t = b*(S_t - S_{t-1}) + (1-b)*G_{t-1}
#   F_{t,t+tau} = S_t + tau*G_t
# start = "reg"    : straight line fitted to the first k periods (S_k = line at k, G_k = slope)
# start = "d0"     : S = D_2,  G = D_2 - D_1  (first two observations)
# start = "manual" : S0 and G0 typed by the user (values at period 0; forecasts start in period 1)
fc_holt <- function(d, alpha, beta, h, start = "reg", k = 4, S0 = NULL, G0 = NULL) {
  n <- length(d); S <- G <- F <- rep(NA_real_, n)
  if (start == "manual") {
    s <- 0; Sp <- S0; Gp <- G0
  } else if (start == "d0") {
    s <- 2; Sp <- d[2]; Gp <- d[2] - d[1]
  } else {
    k <- min(max(2, round(k)), n - 1); s <- k
    m <- lm(d[1:k] ~ seq_len(k))
    Sp <- unname(coef(m)[1] + coef(m)[2] * k); Gp <- unname(coef(m)[2])
  }
  Sstart <- Sp; Gstart <- Gp
  if (s >= 1) { S[s] <- Sp; G[s] <- Gp }
  if (n > s) for (t in (s + 1):n) {
    F[t] <- Sp + Gp
    St <- alpha * d[t] + (1 - alpha) * (Sp + Gp)
    Gt <- beta * (St - Sp) + (1 - beta) * Gp
    S[t] <- St; G[t] <- Gt; Sp <- St; Gp <- Gt
  }
  list(fitted = F, future = Sp + seq_len(h) * Gp,
       extra = data.frame("S" = round(S, 2), "G" = round(G, 2), check.names = FALSE),
       S0 = Sstart, G0 = Gstart, s = s)
}

# Seasonal decomposition using moving averages
#   1) centered MA of length N   2) ratios D / MA   3) seasonal factors
#   4) deseasonalized series -> straight line a + b*t
#   5) F_t = (a + b*t) * c_j
fc_decomposition <- function(d, N, h) {
  n <- length(d); pos <- fc_season_pos(1:n, N)
  wts <- if (N %% 2 == 1) rep(1 / N, N) else c(0.5, rep(1, N - 1), 0.5) / N
  cma <- as.numeric(stats::filter(d, wts, sides = 2))
  ratio <- d / cma
  cj <- tapply(ratio, pos, mean, na.rm = TRUE)
  cj <- cj * N / sum(cj)
  deseason <- d / cj[pos]
  t <- 1:n
  m <- lm(deseason ~ t)
  a <- unname(coef(m)[1]); b <- unname(coef(m)[2])
  ft <- (n + 1):(n + h)
  list(fitted = as.numeric((a + b * t) * cj[pos]),
       future = if (h > 0) as.numeric((a + b * ft) * cj[fc_season_pos(ft, N)]) else numeric(0),
       factors = cj, a = a, b = b,
       extra = data.frame("Season" = pos, "Centered MA" = round(cma, 2),
                          "D / MA" = round(ratio, 3),
                          "Deseasonalized D" = round(as.numeric(deseason), 2),
                          check.names = FALSE))
}

# Winters method (multiplicative)
#   S_t = a*D_t/c_{t-N} + (1-a)*(S_{t-1} + G_{t-1})
#   G_t = b*(S_t - S_{t-1}) + (1-b)*G_{t-1}
#   c_t = g*D_t/S_t + (1-g)*c_{t-N}
#   F_{t,t+tau} = (S_t + tau*G_t) * c_{t+tau-N}
# Starting values:
# start = "reg"    : straight line D = a + b*t on the first two seasons (S = line at 2N, G = b)
# start = "two"    : the book's two-season method (V1, V2)
# start = "manual" : level S0 and slope G0 typed by the user (values at period 0).
#                    Starting seasonal factors = average of D_t / (S0 + G0*t) for each
#                    season position (first two seasons), scaled to sum N.
#                    Forecasts start in period 1.
fc_winters <- function(d, N, alpha, beta, gamma, h, start = "reg", S0 = NULL, G0 = NULL) {
  n <- length(d); S <- G <- F <- rep(NA_real_, n); cc <- rep(NA_real_, n + h)
  if (start == "manual") {
    s <- 0; Sp <- S0; Gp <- G0
    tt <- seq_len(min(n, 2 * N))
    cj <- as.numeric(tapply(d[tt] / (S0 + G0 * tt), fc_season_pos(tt, N), mean))
    cj <- cj * N / sum(cj)
  } else if (start == "reg") {
    s <- 2 * N
    t0 <- seq_len(s)
    m <- lm(d[t0] ~ t0)
    a <- unname(coef(m)[1]); b <- unname(coef(m)[2])
    cj <- tapply(d[t0] / (a + b * t0), fc_season_pos(t0, N), mean)
    cj <- as.numeric(cj * N / sum(cj))
    cc[(N + 1):(2 * N)] <- cj
    Sp <- a + b * s; Gp <- b
  } else {
    s <- 2 * N
    V1 <- mean(d[1:N]); V2 <- mean(d[(N + 1):(2 * N)])
    Gp <- (V2 - V1) / N
    Sp <- V2 + Gp * (N - 1) / 2
    ct <- sapply(1:(2 * N), function(t) {
      Vi <- if (t <= N) V1 else V2
      d[t] / (Vi - ((N + 1) / 2 - fc_season_pos(t, N)) * Gp)
    })
    cj <- (ct[1:N] + ct[(N + 1):(2 * N)]) / 2
    cj <- cj * N / sum(cj)
    cc[(N + 1):(2 * N)] <- cj
  }
  Sstart <- Sp; Gstart <- Gp
  if (s >= 1) { S[s] <- Sp; G[s] <- Gp }
  # factor of the same season one cycle ago (the starting factor in the first cycle)
  c_prev <- function(t) if (t > N && !is.na(cc[t - N])) cc[t - N] else cj[fc_season_pos(t, N)]
  if (n > s) for (t in (s + 1):n) {
    cp <- c_prev(t)
    F[t] <- (Sp + Gp) * cp
    St <- alpha * d[t] / cp + (1 - alpha) * (Sp + Gp)
    Gt <- beta * (St - Sp) + (1 - beta) * Gp
    cc[t] <- gamma * d[t] / St + (1 - gamma) * cp
    S[t] <- St; G[t] <- Gt; Sp <- St; Gp <- Gt
  }
  tau <- seq_len(h)
  future <- if (h > 0) (Sp + tau * Gp) * cc[n + tau - N * ceiling(tau / N)] else numeric(0)
  list(fitted = F, future = future, start_factors = cj, S0 = Sstart, G0 = Gstart, s = s,
       extra = data.frame("S" = round(S, 2), "G" = round(G, 2),
                          "c" = round(cc[1:n], 3), check.names = FALSE))
}

# ---------------- Searching for the best parameters ----------------

# Value of one error measure ("MAD", "MSE" or "MAPE") for fitted forecasts F.
# Periods before `start` are ignored. Returns Inf if it cannot be computed.
fc_crit_value <- function(d, F, crit, start = 1) {
  if (start > 1) F[seq_len(min(start - 1, length(F)))] <- NA
  v <- unname(fc_errors(d, F)[crit])
  if (is.na(v) || !is.finite(v)) Inf else v
}

# Best N for the moving average. All N are compared over the SAME periods
# (Nmax+1 ... n), so the comparison is fair.
fc_best_ma <- function(d, crit, Nmax = 12) {
  Nmax <- max(1, min(Nmax, length(d) - 2))
  v <- vapply(1:Nmax, function(N) fc_crit_value(d, fc_ma(d, N, 0)$fitted, crit, Nmax + 1), numeric(1))
  which.min(v)
}

# Best k weights for the weighted moving average (weights >= 0, sum = 1).
# The weights are written as exp(x)/sum(exp(x)) and x is searched with optim().
fc_best_wma <- function(d, k, crit) {
  if (k <= 1) return(1)
  obj <- function(x) {
    w <- exp(c(0, x)); w <- w / sum(w)
    fc_crit_value(d, fc_wma(d, w, 0)$fitted, crit, k + 1)
  }
  starts <- list(rep(0, k - 1), seq(0.5, -0.5, length.out = k - 1)[seq_len(k - 1)],
                 rep(-1, k - 1), rep(1, k - 1))
  best <- NULL
  for (s0 in starts) {
    r <- if (k == 2) {
      o <- optimize(function(x) obj(x), c(-8, 8)); list(par = o$minimum, value = o$objective)
    } else {
      suppressWarnings(optim(s0, obj, method = "Nelder-Mead", control = list(maxit = 500)))
    }
    if (is.null(best) || r$value < best$value) best <- r
  }
  w <- exp(c(0, best$par)); w / sum(w)
}

# Grid search for parameters in (0, 1]: a coarse grid first, then a fine grid
# around the best point. fn() receives a named numeric vector and returns the
# error to be minimized. Returns the best named vector.
fc_search_unit <- function(fn, pars, coarse = 0.1, fine = 0.01) {
  best_of <- function(grids) {
    g <- expand.grid(grids, KEEP.OUT.ATTRS = FALSE)
    vals <- vapply(seq_len(nrow(g)), function(i) {
      v <- tryCatch(fn(unlist(as.list(g[i, , drop = FALSE]))), error = function(e) Inf)
      if (is.na(v)) Inf else v
    }, numeric(1))
    unlist(as.list(g[which.min(vals), , drop = FALSE]))
  }
  grids <- setNames(rep(list(round(seq(coarse, 1, by = coarse), 4)), length(pars)), pars)
  b <- best_of(grids)
  fine_grids <- lapply(b, function(x) round(seq(max(0.01, x - coarse), min(1, x + coarse), by = fine), 4))
  best_of(fine_grids)
}

# ---------------- Box-Jenkins helpers ----------------

# Model name of an auto.arima fit, e.g. "ARIMA(0,1,1)(0,1,0)[4]"
fc_arima_name <- function(fit) forecast:::arima.string(fit, padding = FALSE)

# Fit ARIMA(p, d, q) (no seasonal part). Returns NULL if the model cannot be fitted.
fc_bj_fit <- function(d, p, dd, q, mean_term = TRUE) {
  tryCatch(
    suppressWarnings(forecast::Arima(ts(d), order = c(p, dd, q),
                                     include.mean = mean_term && dd == 0)),
    error = function(e) NULL)
}

# Searches p in prange and q in qrange (d fixed) and returns the fit with the lowest AICc
# (AIC corrected for small samples, so short series do not get over-fitted).
fc_bj_search <- function(d, prange, dd, qrange, mean_term = TRUE) {
  best <- NULL
  for (p in prange) for (q in qrange) {
    if (length(d) <= p + dd + q + 3) next
    fit <- fc_bj_fit(d, p, dd, q, mean_term)
    if (is.null(fit) || !is.finite(fit$aicc)) next
    if (is.null(best) || fit$aicc < best$fit$aicc) best <- list(fit = fit, p = p, q = q)
  }
  best
}

# Parameter estimates: estimate, standard error, t ratio
fc_bj_coef_table <- function(fit) {
  est <- fit$coef
  if (length(est) == 0) return(data.frame(Parameter = "(no parameters in this model)", check.names = FALSE))
  se <- rep(NA_real_, length(est)); names(se) <- names(est)
  v <- sqrt(diag(fit$var.coef)); se[names(v)] <- v
  data.frame("Parameter" = names(est), "Estimate" = round(est, 4),
             "Std. error" = round(se, 4), "t ratio" = round(est / se, 2),
             check.names = FALSE, row.names = NULL)
}

fc_bj_stats_table <- function(fit) {
  data.frame(
    "Statistic" = c("sigma^2 (residual variance)", "Log likelihood", "AIC", "AICc", "BIC", "Observations"),
    "Value" = round(c(fit$sigma2, fit$loglik, fit$aic, fit$aicc, fit$bic, fit$nobs), 3),
    check.names = FALSE)
}


# ---------------------------------------------------------------------
# 2) SCREEN (UI)
# ---------------------------------------------------------------------

# "Enter manually / find the best" switch placed at the top of a method's inputs
fc_mode_radio <- function(ns, key, what) {
  radioButtons(ns(paste0(key, "_mode")), "Parameters",
               choices = setNames(c("manual", "auto"),
                                  c(paste("Enter", what, "manually"), paste("Find the best", what))))
}

# Text shown when "find the best" is selected
fc_auto_help <- function(ns, key, txt) {
  conditionalPanel(sprintf("input.%s_mode == 'auto'", key), ns = ns, helpText(txt))
}

# One method sub-tab: inputs on the left, results on the right
fc_method_tab <- function(ns, key, title, formula, inputs) {
  tabPanel(title,
    br(),
    sidebarLayout(
      sidebarPanel(width = 3, inputs),
      mainPanel(width = 9,
        div(style = "background:#f6f8fb;border-left:4px solid #2F6FB0;padding:8px 12px;margin-bottom:10px;",
            formula),
        h4(textOutput(ns(paste0(key, "_title")))),
        plotOutput(ns(paste0(key, "_plot")), height = "350px"),
        h4("Error measures"),
        tableOutput(ns(paste0(key, "_errors"))),
        uiOutput(ns(paste0(key, "_extra"))),
        h4("Forecasts by period"),
        tableOutput(ns(paste0(key, "_table")))
      )
    )
  )
}

# Box-Jenkins model sub-tab (AR, MA, ARMA, ARIMA): inputs on the left, results on the right
fc_bj_tab <- function(ns, key, title, formula, p = FALSE, d = FALSE, q = FALSE) {
  id <- function(x) paste0("bj_", key, "_", x)
  orders <- tagList(
    if (p) numericInput(ns(id("p")), "AR order p", if (key == "arima") 1 else 1, min = if (key == "arima") 0 else 1, max = 8, step = 1),
    if (q) numericInput(ns(id("q")), "MA order q", 1, min = if (key == "arima") 0 else 1, max = 8, step = 1)
  )
  tabPanel(title,
    br(),
    sidebarLayout(
      sidebarPanel(width = 3,
        radioButtons(ns(id("mode")), "Orders",
                     choices = c("Enter the orders manually" = "manual",
                                 "Find the best orders (lowest AICc)" = "auto")),
        if (d) numericInput(ns(id("d")), "Differencing d", 1, min = 0, max = 2, step = 1),
        conditionalPanel(sprintf("input.%s == 'manual'", id("mode")), ns = ns, orders),
        conditionalPanel(sprintf("input.%s == 'auto'", id("mode")), ns = ns,
                         helpText(switch(key,
                           ar = "Tries p = 1, ..., 5.",
                           ma = "Tries q = 1, ..., 5.",
                           arma = "Tries p = 1, ..., 3 and q = 1, ..., 3.",
                           arima = "Tries p = 0, ..., 3 and q = 0, ..., 3 for the chosen d."))),
        checkboxInput(ns(id("mean")), "Include a constant (mean) when d = 0", TRUE)
      ),
      mainPanel(width = 9,
        div(style = "background:#f6f8fb;border-left:4px solid #2F6FB0;padding:8px 12px;margin-bottom:10px;",
            formula),
        h4(textOutput(ns(id("title")))),
        fluidRow(column(6, plotOutput(ns(id("acf")), height = "240px")),
                 column(6, plotOutput(ns(id("pacf")), height = "240px"))),
        h4("Parameter estimates"),
        tableOutput(ns(id("coef"))),
        tableOutput(ns(id("stats"))),
        plotOutput(ns(id("plot")), height = "350px"),
        h4("Error measures"),
        helpText("The first p + d periods are skipped (the model needs them as starting values)."),
        tableOutput(ns(id("errors"))),
        h4("Forecasts"),
        tableOutput(ns(id("table")))
      )
    )
  )
}

# Box-Jenkins tab: identification, AR, MA, ARMA, ARIMA and auto.arima
fc_box_jenkins_tab <- function(ns) {
  tabPanel("Box-Jenkins",
    br(),
    tabsetPanel(
      tabPanel("Identification",
        br(),
        fluidRow(
          column(4, selectInput(ns("ts_d"), "Differencing (d)", 0:2, 0)),
          column(4, selectInput(ns("ts_D"), "Seasonal differencing (D)", 0:1, 0)),
          column(4, numericInput(ns("ts_lag"), "Max lag", 16, min = 4, step = 1))
        ),
        plotOutput(ns("ts_plot"), height = "280px"),
        fluidRow(column(6, plotOutput(ns("ts_acf"), height = "270px")),
                 column(6, plotOutput(ns("ts_pacf"), height = "270px"))),
        verbatimTextOutput(ns("ts_stationarity")),
        helpText("Reading the plots: ACF cuts off after lag q and PACF dies out -> MA(q).",
                 "PACF cuts off after lag p and ACF dies out -> AR(p).",
                 "Both die out -> ARMA(p, q).",
                 "ACF dies out very slowly -> difference the series (ARIMA).")
      ),
      fc_bj_tab(ns, "ar", "AR",
                "AR(p):  D(t) = c + φ1·D(t-1) + ... + φp·D(t-p) + e(t)", p = TRUE),
      fc_bj_tab(ns, "ma", "MA",
                "MA(q):  D(t) = c + e(t) + θ1·e(t-1) + ... + θq·e(t-q)", q = TRUE),
      fc_bj_tab(ns, "arma", "ARMA",
                "ARMA(p, q):  AR(p) and MA(q) parts together", p = TRUE, q = TRUE),
      fc_bj_tab(ns, "arima", "ARIMA",
                "ARIMA(p, d, q):  ARMA(p, q) applied to the series differenced d times",
                p = TRUE, d = TRUE, q = TRUE),
      tabPanel("auto.arima",
        br(),
        checkboxInput(ns("ts_seasonal"), "auto.arima: try seasonal models", TRUE),
        h4(textOutput(ns("ts_ar_title"))),
        verbatimTextOutput(ns("ts_ar_summary")),
        plotOutput(ns("ts_ar_plot"), height = "340px"),
        h4("auto.arima forecasts"),
        tableOutput(ns("ts_ar_table"))
      )
    )
  )
}

# Comparison sub-tab
fc_compare_tab <- function(ns, p) {
  tabPanel("Comparison",
    br(),
    helpText("Every method uses the parameters chosen in its own tab. Sorted by MSE."),
    textOutput(ns(paste0(p, "_cmp_note"))),
    tableOutput(ns(paste0(p, "_cmp_table"))),
    plotOutput(ns(paste0(p, "_cmp_plot")), height = "300px")
  )
}

forecasting_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    # ---------- left: data ----------
    column(3,
      wellPanel(
        h4("Data"),
        radioButtons(ns("source"), NULL,
                     choices = c("Use the sample data" = "sample",
                                 "Upload my own file (.xlsx / .csv)" = "upload",
                                 "Type the values" = "manual")),
        conditionalPanel(
          condition = "input.source == 'upload'", ns = ns,
          fileInput(ns("file"), NULL, accept = c(".xlsx", ".xls", ".csv")),
          selectInput(ns("column"), "Demand column", choices = NULL)
        ),
        conditionalPanel(
          condition = "input.source == 'manual'", ns = ns,
          textAreaInput(ns("manual"), "Demand values (in order)",
                        "112 136 158 121 128 150 177 134 141 168 195 149", rows = 4)
        ),
        numericInput(ns("N"), "Season length N (quarterly 4, monthly 12)", 4, min = 2, step = 1),
        numericInput(ns("h"), "Periods to forecast ahead", 4, min = 0, max = 60, step = 1),
        selectInput(ns("crit"), "\"Find the best\" minimizes", c("MSE", "MAD", "MAPE"), "MSE"),
        textOutput(ns("data_info"))
      )
    ),
    # ---------- right: Stationary Series / Trend / Box-Jenkins ----------
    column(9,
      tabsetPanel(
        # ===================== STATIONARY SERIES =====================
        tabPanel("Stationary Series",
          br(),
          tabsetPanel(
            fc_method_tab(ns, "ma", "Moving Average",
              "F(t) = (D(t-1) + D(t-2) + ... + D(t-N)) / N",
              tagList(
                fc_mode_radio(ns, "ma", "N"),
                conditionalPanel("input.ma_mode == 'manual'", ns = ns,
                  sliderInput(ns("ma_N"), "N (number of past periods)", 1, 12, 3, step = 1)),
                fc_auto_help(ns, "ma", "Tries N = 1, 2, ..., 12 and keeps the N with the smallest error. All N are compared over the same periods."))),
            fc_method_tab(ns, "wma", "WMA",
              "F(t) = w1·D(t-1) + w2·D(t-2) + ... + wN·D(t-N)   (weights are scaled to sum to 1)",
              tagList(
                fc_mode_radio(ns, "wma", "weights"),
                conditionalPanel("input.wma_mode == 'manual'", ns = ns,
                  textInput(ns("wma_w"), "Weights (most recent first)", "0.5, 0.3, 0.2"),
                  helpText("Example: 3, 2, 1 is used as 3/6, 2/6, 1/6.")),
                conditionalPanel("input.wma_mode == 'auto'", ns = ns,
                  numericInput(ns("wma_k"), "Number of weights", 3, min = 1, max = 12, step = 1),
                  helpText("The app finds the weights (all >= 0, sum = 1) with the smallest error.")))),
            fc_method_tab(ns, "es", "Exponential Smoothing",
              "F(t) = α·D(t-1) + (1-α)·F(t-1).  The first forecast is the initial value.",
              tagList(
                fc_mode_radio(ns, "es", "α"),
                conditionalPanel("input.es_mode == 'manual'", ns = ns,
                  sliderInput(ns("es_alpha"), "α", 0.01, 1, 0.2, step = 0.01)),
                fc_auto_help(ns, "es", "Tries α = 0.01, 0.02, ..., 1 and keeps the best."),
                hr(),
                radioButtons(ns("es_init"), "Initial value",
                             choices = c("Regression on the first k periods" = "reg",
                                         "Average of the first k periods" = "avg",
                                         "Enter the initial level manually" = "manual")),
                conditionalPanel("input.es_init != 'manual'", ns = ns,
                  numericInput(ns("es_k"), "k (periods used for the initial value)", 3, min = 1, step = 1)),
                conditionalPanel("input.es_init == 'manual'", ns = ns,
                  numericInput(ns("es_level"), "Initial level (forecast for period 1)", 120)))),
            fc_method_tab(ns, "qd", "Quick and Dirty Est.",
              "Quick and dirty estimation of seasonal factors: divide every demand by the average of its own cycle (e.g. its own year), then average the ratios of the same season.  F(t) = overall average × c(j).",
              helpText("The season length is the N on the left. Only full cycles are used.")),
            fc_method_tab(ns, "sf", "Seasonal Factors",
              "c(j) = average demand of season j / overall average (factors sum to N).  F(t) = overall average × c(j).",
              helpText("The season length is the N on the left.")),
            fc_compare_tab(ns, "st")
          )
        ),
        # ============================ TREND ===========================
        tabPanel("Trend",
          br(),
          tabsetPanel(
            fc_method_tab(ns, "holt", "Holt's Method",
              "S(t) = α·D(t) + (1-α)·(S(t-1) + G(t-1));   G(t) = β·(S(t) - S(t-1)) + (1-β)·G(t-1);   F(t, t+τ) = S(t) + τ·G(t)",
              tagList(
                fc_mode_radio(ns, "holt", "α and β"),
                conditionalPanel("input.holt_mode == 'manual'", ns = ns,
                  sliderInput(ns("holt_alpha"), "α", 0.01, 1, 0.2, step = 0.01),
                  sliderInput(ns("holt_beta"), "β", 0.01, 1, 0.1, step = 0.01)),
                fc_auto_help(ns, "holt", "Searches α and β in (0, 1] and keeps the pair with the smallest error."),
                hr(),
                radioButtons(ns("holt_init"), "Initial level and slope",
                             choices = c("Regression on the first k periods" = "reg",
                                         "Enter level and slope manually" = "manual",
                                         "S = D2,  G = D2 - D1" = "d0")),
                conditionalPanel("input.holt_init == 'reg'", ns = ns,
                  numericInput(ns("holt_k"), "k", 4, min = 2, step = 1)),
                conditionalPanel("input.holt_init == 'manual'", ns = ns,
                  numericInput(ns("holt_S0"), "Initial level S0", 110),
                  numericInput(ns("holt_G0"), "Initial slope G0", 4)))),
            fc_method_tab(ns, "dec", "Seasonal Decomposition MA",
              "1) Centered MA of length N   2) D / MA   3) Seasonal factors   4) Straight line on deseasonalized data   5) F(t) = (a + b·t) × c(j)",
              helpText("The season length is the N on the left. At least 2 full seasons are needed.")),
            fc_method_tab(ns, "win", "Winters Method",
              "S(t) = α·D(t)/c(t-N) + (1-α)·(S(t-1)+G(t-1));   G(t) = β·(S(t)-S(t-1)) + (1-β)·G(t-1);   c(t) = γ·D(t)/S(t) + (1-γ)·c(t-N);   F(t, t+τ) = (S(t) + τ·G(t))·c(t+τ-N)",
              tagList(
                fc_mode_radio(ns, "win", "α, β and γ"),
                conditionalPanel("input.win_mode == 'manual'", ns = ns,
                  sliderInput(ns("win_alpha"), "α", 0.01, 1, 0.2, step = 0.01),
                  sliderInput(ns("win_beta"), "β", 0.01, 1, 0.1, step = 0.01),
                  sliderInput(ns("win_gamma"), "γ", 0.01, 1, 0.1, step = 0.01)),
                fc_auto_help(ns, "win", "Searches α, β and γ in (0, 1] and keeps the best triple (this can take a few seconds)."),
                hr(),
                radioButtons(ns("win_init"), "Initial level, slope and factors",
                             choices = c("Regression on the first two seasons" = "reg",
                                         "Enter level and slope manually" = "manual",
                                         "Book: first two seasons (V1, V2)" = "two")),
                conditionalPanel("input.win_init == 'manual'", ns = ns,
                  numericInput(ns("win_S0"), "Initial level S0", 110),
                  numericInput(ns("win_G0"), "Initial slope G0", 4),
                  helpText("The starting seasonal factors are computed from the first two seasons and S0, G0.")))),
            fc_compare_tab(ns, "tr")
          )
        ),
        # ========================= BOX-JENKINS ========================
        fc_box_jenkins_tab(ns)
      )
    )
  )
}


# ---------------------------------------------------------------------
# 3) CALCULATIONS (SERVER)
# ---------------------------------------------------------------------
forecasting_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- Data ---------------------------------------------------------
    file_data <- reactive({
      req(input$file)
      fc_read_file(input$file$datapath, input$file$name)
    })

    observeEvent(file_data(), {
      cols <- names(file_data())[sapply(file_data(), is.numeric)]
      updateSelectInput(session, "column", choices = cols, selected = tail(cols, 1))
    })

    demand <- reactive({
      d <- switch(input$source,
                  sample = fc_sample,
                  manual = fc_parse_numbers(input$manual),
                  upload = { req(input$column); as.numeric(file_data()[[input$column]]) })
      d <- d[!is.na(d)]
      validate(need(length(d) >= 4, "At least 4 observations are needed."))
      d
    })

    N <- reactive({ req(input$N); max(2, round(input$N)) })
    h <- reactive({ if (is.na(input$h)) 0 else max(0, round(input$h)) })
    crit <- reactive({ if (is.null(input$crit)) "MSE" else input$crit })
    is_auto <- function(key) identical(input[[paste0(key, "_mode")]], "auto")
    note_auto <- function(key) if (is_auto(key)) paste0(" — best found by ", crit()) else ""

    output$data_info <- renderText({
      sprintf("%d observations, mean %.2f, %.1f seasons (N = %d)",
              length(demand()), mean(demand()), length(demand()) / N(), N())
    })

    # --- Box-Jenkins: identification ------------------------------------
    series <- reactive({
      y <- ts(demand(), frequency = N())
      D <- as.integer(input$ts_D); d <- as.integer(input$ts_d)
      if (D > 0) {
        validate(need(length(y) > N() + 2, "Not enough data for seasonal differencing."))
        y <- diff(y, lag = N(), differences = D)
      }
      if (d > 0) {
        validate(need(length(y) > d + 2, "Not enough data for differencing."))
        y <- diff(y, differences = d)
      }
      as.numeric(y)
    })

    output$ts_plot <- renderPlot({
      y <- series()
      plot(y, type = "o", pch = 19, col = "#2F6FB0", xlab = "Period", ylab = "Value",
           main = paste0("Time series plot  (d = ", input$ts_d, ", D = ", input$ts_D, ")"))
      abline(h = mean(y), lty = 3, col = "grey50")
    })
    output$ts_acf  <- renderPlot(acf(series(),  lag.max = input$ts_lag, main = "ACF (autocorrelation function)"))
    output$ts_pacf <- renderPlot(pacf(series(), lag.max = input$ts_lag, main = "PACF (partial autocorrelation function)"))
    output$ts_stationarity <- renderText({
      y <- ts(demand(), frequency = N())
      nd <- tryCatch(forecast::ndiffs(y), error = function(e) NA)
      nsd <- if (length(y) >= 2 * N() + 1)
        tryCatch(forecast::nsdiffs(y), error = function(e) NA) else NA
      paste0("Suggested differencing (ndiffs): d = ", nd,
             "\nSuggested seasonal differencing (nsdiffs): D = ",
             ifelse(is.na(nsd), "not available (at least 2 full seasons needed)", nsd),
             "\nIf the ACF dies out slowly the series is not stationary; spikes at lags N, 2N, ... mean seasonality.")
    })

    # --- Box-Jenkins: AR, MA, ARMA, ARIMA --------------------------------
    bj_specs <- list(
      ar    = list(name = "AR",    p = TRUE,  d = FALSE, q = FALSE, prange = 1:5, qrange = 0),
      ma    = list(name = "MA",    p = FALSE, d = FALSE, q = TRUE,  prange = 0,   qrange = 1:5),
      arma  = list(name = "ARMA",  p = TRUE,  d = FALSE, q = TRUE,  prange = 1:3, qrange = 1:3),
      arima = list(name = "ARIMA", p = TRUE,  d = TRUE,  q = TRUE,  prange = 0:3, qrange = 0:3)
    )

    lapply(names(bj_specs), function(key) {
      spec <- bj_specs[[key]]
      id <- function(x) paste0("bj_", key, "_", x)
      int_input <- function(x, default = 0) {
        v <- input[[id(x)]]
        if (is.null(v) || is.na(v)) default else max(0, round(v))
      }

      # The fitted model: orders typed by the user or the best orders by AICc
      model <- reactive({
        y <- demand()
        dd <- if (spec$d) int_input("d", 1) else 0
        mean_term <- isTRUE(input[[id("mean")]])
        if (identical(input[[id("mode")]], "auto")) {
          best <- fc_bj_search(y, spec$prange, dd, spec$qrange, mean_term)
          validate(need(!is.null(best), "No model could be fitted to this data."))
          p <- best$p; q <- best$q; fit <- best$fit
        } else {
          p <- if (spec$p) int_input("p", 1) else 0
          q <- if (spec$q) int_input("q", 1) else 0
          validate(need(length(y) > p + dd + q + 3, "Not enough observations for these orders."))
          fit <- fc_bj_fit(y, p, dd, q, mean_term)
          validate(need(!is.null(fit), "The model could not be fitted. Try other orders."))
        }
        label <- switch(key,
          ar = sprintf("AR(%d)", p), ma = sprintf("MA(%d)", q),
          arma = sprintf("ARMA(%d,%d)", p, q), arima = sprintf("ARIMA(%d,%d,%d)", p, dd, q))
        fitted <- as.numeric(fitted(fit))
        if (p + dd > 0) fitted[seq_len(min(p + dd, length(fitted)))] <- NA
        fc <- forecast::forecast(fit, h = max(1, h()))
        list(fit = fit, p = p, d = dd, q = q, label = label, fitted = fitted, fc = fc,
             auto = identical(input[[id("mode")]], "auto"))
      })

      output[[id("title")]] <- renderText({
        m <- model()
        paste0(m$label, if (m$auto) " — best orders by AICc" else "",
               "  —  AICc = ", round(m$fit$aicc, 2))
      })
      acf_series <- reactive({
        y <- demand(); m <- model()
        if (m$d > 0) y <- diff(y, differences = m$d)
        y
      })
      output[[id("acf")]]  <- renderPlot(acf(acf_series(),  lag.max = input$ts_lag, main = "ACF of the series used"))
      output[[id("pacf")]] <- renderPlot(pacf(acf_series(), lag.max = input$ts_lag, main = "PACF of the series used"))
      output[[id("coef")]]  <- renderTable(fc_bj_coef_table(model()$fit))
      output[[id("stats")]] <- renderTable(fc_bj_stats_table(model()$fit))
      output[[id("plot")]] <- renderPlot({
        m <- model()
        fc_plot(demand(), list(fitted = m$fitted, future = if (h() > 0) as.numeric(m$fc$mean) else numeric(0)),
                m$label)
      })
      output[[id("errors")]] <- renderTable(fc_error_table(demand(), model()$fitted))
      output[[id("table")]] <- renderTable({
        m <- model(); f <- m$fc
        data.frame("Period" = as.integer(length(demand()) + seq_along(f$mean)),
                   "Forecast" = round(as.numeric(f$mean), 2),
                   "Lower 95%" = round(as.numeric(f$lower[, 2]), 2),
                   "Upper 95%" = round(as.numeric(f$upper[, 2]), 2), check.names = FALSE)
      })
    })

    # --- Box-Jenkins: auto.arima finds the best ARIMA model by itself ----
    arima_fit <- reactive({
      forecast::auto.arima(ts(demand(), frequency = N()), seasonal = isTRUE(input$ts_seasonal))
    })
    arima_fc <- reactive(forecast::forecast(arima_fit(), h = max(1, h())))

    output$ts_ar_title <- renderText(paste("auto.arima — best model:", fc_arima_name(arima_fit())))
    output$ts_ar_summary <- renderPrint(summary(arima_fit()))
    output$ts_ar_plot <- renderPlot({
      plot(arima_fc(), main = paste("Forecast with", fc_arima_name(arima_fit())),
           xlab = "Time (seasons)", ylab = "Demand")
      lines(fitted(arima_fit()), col = "#B4501E", lty = 2)
      legend("topleft", bty = "n", legend = c("Actual demand", "Fitted", "Forecast"),
             col = c("black", "#B4501E", "#2F6FB0"), lty = c(1, 2, 1))
    })
    output$ts_ar_table <- renderTable({
      f <- arima_fc()
      data.frame("Period" = as.integer(length(demand()) + seq_along(f$mean)),
                 "Forecast" = round(as.numeric(f$mean), 2),
                 "Lower 95%" = round(as.numeric(f$lower[, 2]), 2),
                 "Upper 95%" = round(as.numeric(f$upper[, 2]), 2), check.names = FALSE)
    })

    # --- Methods --------------------------------------------------------
    # Every run() returns the fitted forecasts, the future forecasts and a label
    # (the label contains the parameters that were actually used).
    methods <- list(
      ma = list(
        run = reactive({
          d <- demand()
          if (is_auto("ma")) {
            Nn <- fc_best_ma(d, crit())
          } else {
            validate(need(!is.na(input$ma_N) && input$ma_N < length(d),
                          "N must be smaller than the number of observations."))
            Nn <- input$ma_N
          }
          r <- fc_ma(d, Nn, h())
          r$label <- paste0("Moving Average MA(", Nn, ")", note_auto("ma"))
          r
        })),
      wma = list(
        run = reactive({
          d <- demand()
          if (is_auto("wma")) {
            k <- if (is.na(input$wma_k)) 3 else max(1, round(input$wma_k))
            validate(need(k < length(d), "Too many weights for this data."))
            w <- fc_best_wma(d, k, crit())
          } else {
            w <- fc_parse_numbers(input$wma_w)
            validate(need(length(w) >= 1 && all(w >= 0) && sum(w) > 0, "Enter valid weights."),
                     need(length(w) < length(d), "Too many weights for this data."))
          }
          r <- fc_wma(d, w, h())
          r$label <- paste0("Weighted Moving Average", note_auto("wma"))
          r
        })),
      es = list(
        run = reactive({
          d <- demand(); init <- input$es_init
          k <- if (is.null(input$es_k) || is.na(input$es_k)) 3 else max(1, round(input$es_k))
          if (init == "manual") {
            validate(need(!is.null(input$es_level) && !is.na(input$es_level), "Enter the initial level."))
          } else {
            validate(need(k < length(d) - 1, "k is too large."),
                     need(init != "reg" || k >= 2, "At least 2 periods are needed for the regression."))
          }
          lev <- if (init == "manual") input$es_level else NULL
          if (is_auto("es")) {
            alphas <- seq(0.01, 1, by = 0.01)
            v <- vapply(alphas, function(a) fc_crit_value(d, fc_es(d, a, 0, k, init, lev)$fitted, crit()), numeric(1))
            alpha <- alphas[which.min(v)]
          } else {
            alpha <- input$es_alpha
          }
          r <- fc_es(d, alpha, h(), k, init, lev)
          r$label <- paste0("Exponential Smoothing (α = ", alpha, ")", note_auto("es"))
          r$alpha <- alpha
          r
        })),
      qd = list(
        run = reactive({
          validate(need(length(demand()) >= N(), "At least one full cycle is needed."))
          r <- fc_quick_dirty(demand(), N(), h())
          r$label <- "Quick and Dirty Estimation"
          r
        })),
      sf = list(
        run = reactive({
          validate(need(length(demand()) >= N(), "At least one full season is needed."))
          r <- fc_seasonal_factors(demand(), N(), h())
          r$label <- "Seasonal Factors"
          r
        })),
      holt = list(
        run = reactive({
          d <- demand(); init <- input$holt_init
          validate(need(length(d) >= 3, "At least 3 observations are needed."))
          k <- if (is.null(input$holt_k) || is.na(input$holt_k)) 4 else input$holt_k
          S0 <- G0 <- NULL
          if (init == "manual") {
            validate(need(!is.null(input$holt_S0) && !is.na(input$holt_S0) &&
                            !is.null(input$holt_G0) && !is.na(input$holt_G0),
                          "Enter the initial level and slope."))
            S0 <- input$holt_S0; G0 <- input$holt_G0
          }
          if (is_auto("holt")) {
            best <- fc_search_unit(function(p)
              fc_crit_value(d, fc_holt(d, p[["alpha"]], p[["beta"]], 0, init, k, S0, G0)$fitted, crit()),
              c("alpha", "beta"), coarse = 0.1, fine = 0.01)
            alpha <- best[["alpha"]]; beta <- best[["beta"]]
          } else {
            alpha <- input$holt_alpha; beta <- input$holt_beta
          }
          r <- fc_holt(d, alpha, beta, h(), init, k, S0, G0)
          r$label <- paste0("Holt's Method (α = ", alpha, ", β = ", beta, ")", note_auto("holt"))
          r
        })),
      dec = list(
        run = reactive({
          validate(need(length(demand()) >= 2 * N(), "At least 2 full seasons are needed."))
          r <- fc_decomposition(demand(), N(), h())
          r$label <- "Seasonal Decomposition using MA"
          r
        })),
      win = list(
        run = reactive({
          d <- demand(); init <- input$win_init
          need_n <- if (init == "manual") N() + 1 else 2 * N() + 1
          validate(need(length(d) >= need_n, paste("Winters needs at least", need_n, "observations.")))
          S0 <- G0 <- NULL
          if (init == "manual") {
            validate(need(!is.null(input$win_S0) && !is.na(input$win_S0) &&
                            !is.null(input$win_G0) && !is.na(input$win_G0),
                          "Enter the initial level and slope."))
            S0 <- input$win_S0; G0 <- input$win_G0
          }
          if (is_auto("win")) {
            best <- withProgress(message = "Searching the best α, β, γ ...", value = 0.5,
              fc_search_unit(function(p)
                fc_crit_value(d, fc_winters(d, N(), p[["alpha"]], p[["beta"]], p[["gamma"]], 0,
                                            init, S0, G0)$fitted, crit()),
                c("alpha", "beta", "gamma"), coarse = 0.1, fine = 0.02))
            alpha <- best[["alpha"]]; beta <- best[["beta"]]; gamma <- best[["gamma"]]
          } else {
            alpha <- input$win_alpha; beta <- input$win_beta; gamma <- input$win_gamma
          }
          r <- fc_winters(d, N(), alpha, beta, gamma, h(), init, S0, G0)
          r$label <- paste0("Winters Method (α = ", alpha, ", β = ", beta, ", γ = ", gamma, ")",
                            note_auto("win"))
          r
        }))
    )

    # Title, plot, error table and period table for every method
    lapply(names(methods), function(k) {
      m <- methods[[k]]
      output[[paste0(k, "_title")]] <- renderText({
        r <- m$run()
        nxt <- if (length(r$future)) sprintf("  —  next-period forecast: %.2f", r$future[1]) else ""
        paste0(r$label, nxt)
      })
      output[[paste0(k, "_plot")]] <- renderPlot({
        r <- m$run()
        fc_plot(demand(), r, r$label)
      })
      output[[paste0(k, "_errors")]] <- renderTable(fc_error_table(demand(), m$run()$fitted))
      output[[paste0(k, "_table")]] <- renderTable(
        fc_period_table(demand(), m$run(), m$run()$extra), na = "")
    })

    # Extra information for some methods
    output$ma_extra  <- renderUI(NULL)
    output$es_extra  <- renderUI({
      r <- methods$es$run()
      helpText(sprintf("Initial value: the forecast for period %d is %.2f.", r$s, r$f0))
    })
    output$wma_extra <- renderUI({
      w <- methods$wma$run()$w
      helpText("Weights used: ", paste(sprintf("w%d = %.3f", seq_along(w), w), collapse = ", "))
    })
    output$qd_extra <- renderUI({
      r <- methods$qd$run()
      tagList(h4("Seasonal factors (quick and dirty)"),
              helpText(sprintf("%d full cycles used. Cycle averages: %s. Overall average = %.2f",
                               r$K, paste(round(r$cycle_avg, 2), collapse = ", "), r$avg)),
              renderTable(fc_factor_table(r$factors)))
    })
    output$sf_extra <- renderUI({
      r <- methods$sf$run()
      tagList(h4("Seasonal factors"),
              helpText(sprintf("Overall average = %.2f", r$avg)),
              renderTable(fc_factor_table(r$factors)))
    })
    output$holt_extra <- renderUI({
      r <- methods$holt$run()
      helpText(sprintf("Starting values (period %d): S0 = %.2f, G0 = %.2f. Forecasts start in period %d.",
                       r$s, r$S0, r$G0, r$s + 1))
    })
    output$dec_extra <- renderUI({
      r <- methods$dec$run()
      tagList(h4("Seasonal factors and trend line"),
              helpText(sprintf("Deseasonalized demand = %.3f + %.3f·t   →   F(t) = (%.3f + %.3f·t) × c(j)",
                               r$a, r$b, r$a, r$b)),
              renderTable(fc_factor_table(r$factors)))
    })
    output$win_extra <- renderUI({
      r <- methods$win$run()
      tagList(h4("Starting values"),
              helpText(sprintf("Period %d: S0 = %.2f, G0 = %.2f. Forecasts start in period %d.",
                               r$s, r$S0, r$G0, r$s + 1)),
              renderTable(fc_factor_table(r$start_factors)))
    })

    # --- Comparison -------------------------------------------------------
    # All methods are compared over the SAME periods (from the first period
    # for which every method has a forecast), so the comparison is fair.
    compare_outputs <- function(p, keys) {
      tab <- reactive({
        runs <- lapply(keys, function(k) tryCatch(methods[[k]]$run(), error = function(e) NULL))
        runs <- runs[!vapply(runs, is.null, logical(1))]
        validate(need(length(runs) > 0, "No method could be computed with the current settings."))
        start <- max(vapply(runs, function(r) which(!is.na(r$fitted))[1], numeric(1)))
        rows <- lapply(runs, function(r) {
          F <- r$fitted; F[seq_len(start - 1)] <- NA
          data.frame("Method" = r$label, t(fc_errors(demand(), F)), check.names = FALSE)
        })
        out <- do.call(rbind, rows)
        out <- out[order(out$MSE), ]
        rownames(out) <- NULL
        attr(out, "start") <- start
        out
      })
      output[[paste0(p, "_cmp_note")]] <- renderText({
        paste0("All methods are compared over the same periods: ", attr(tab(), "start"),
               "–", length(demand()), ".")
      })
      output[[paste0(p, "_cmp_table")]] <- renderTable({
        out <- tab()
        out$MAD <- round(out$MAD, 3); out$MSE <- round(out$MSE, 3)
        out$MAPE <- round(out$MAPE, 3); out$Periods <- as.integer(out$Periods)
        names(out)[names(out) == "MAPE"] <- "MAPE (%)"
        out[[" "]] <- c("<-- best (MSE)", rep("", nrow(out) - 1))
        out
      }, striped = TRUE)
      output[[paste0(p, "_cmp_plot")]] <- renderPlot({
        out <- tab()
        op <- par(mar = c(4, 15, 2, 1)); on.exit(par(op))
        barplot(rev(out$MSE), names.arg = rev(out$Method), horiz = TRUE, las = 1,
                col = rev(c("#B4501E", rep("#9DB4CF", nrow(out) - 1))), border = NA,
                main = "MSE (smaller is better)", xlab = "MSE")
      })
    }
    compare_outputs("st", c("ma", "wma", "es", "qd", "sf"))
    compare_outputs("tr", c("holt", "dec", "win"))
  })
}
