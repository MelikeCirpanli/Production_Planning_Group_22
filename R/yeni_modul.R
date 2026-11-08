# =====================================================================
# Topic: Finding the best N for Moving Average — MA(N), N = 1, ..., 10
#
# MA(N) forecast:  F_t = (D_{t-1} + D_{t-2} + ... + D_{t-N}) / N
# Forecast error:  e_t = F_t - D_t
# MSE  = mean of e_t^2
# MAPE = mean of |e_t / D_t| x 100
#
# Uses only packages already in manifest.json (shiny, readxl).
# All names start with "ma_n_" so they do not clash with R/moving_average.R.
# =====================================================================


# ---------------------------------------------------------------------
# 1) CALCULATIONS (independent of the screen)
# ---------------------------------------------------------------------

# MA(N) forecasts for demand vector d. Length n + 1: the last element is
# the next-period forecast. The first N periods have no forecast (NA).
ma_n_forecast <- function(d, N) {
  n <- length(d)
  F <- rep(NA_real_, n + 1)
  for (t in (N + 1):(n + 1)) F[t] <- mean(d[(t - N):(t - 1)])
  F
}

# MSE over periods start ... n
ma_n_mse <- function(d, F, start) {
  t <- start:length(d)
  mean((F[t] - d[t])^2)
}

# MAPE (%) over periods start ... n (periods with D = 0 are skipped)
ma_n_mape <- function(d, F, start) {
  t <- start:length(d)
  t <- t[d[t] != 0]
  mean(abs((F[t] - d[t]) / d[t])) * 100
}

# NORMAL METHOD: try N = 1, ..., Nmax on the real data.
# All N are evaluated over the SAME periods (Nmax+1 ... n), the same fair-
# comparison convention used in R/moving_average.R.
ma_n_search <- function(d, Nmax = 10) {
  rows <- lapply(1:Nmax, function(N) {
    F <- ma_n_forecast(d, N)
    data.frame(N = N,
               MSE  = ma_n_mse(d, F, Nmax + 1),
               MAPE = ma_n_mape(d, F, Nmax + 1))
  })
  do.call(rbind, rows)
}

# Best N = smallest value of the chosen criterion ("MSE" or "MAPE")
ma_n_best <- function(tab, criterion = "MSE") {
  tab$N[which.min(tab[[criterion]])]
}

# ---------------------------------------------------------------------
# SIMULATION METHOD  -->  OUR OWN ASSUMPTION, NOT THE INSTRUCTOR'S METHOD
# The assignment's exact simulation method was not available, so this is a
# simple, standard Monte Carlo (residual bootstrap). If the instructor
# defines a different method, only this function needs to change.
#
#   1. Fit a straight-line trend to the real demand:  D_t ~ a + b*t
#   2. Take the residuals (what the trend does not explain = "noise")
#   3. Build `reps` artificial demand series: trend + noise resampled
#      (with replacement) from the residuals
#   4. On every artificial series, run MA(N) for N = 1..Nmax and compute
#      MSE and MAPE (same periods Nmax+1 ... n as the normal method)
#   5. Average the errors over all series -> best N = smallest average
#
# Why: one real series is just one possible "history". Averaging over many
# plausible histories shows which N is good in general, not by luck.
# ---------------------------------------------------------------------
ma_n_simulation <- function(d, Nmax = 10, reps = 500, seed = 123) {
  set.seed(seed)
  n <- length(d)
  t <- seq_len(n)
  fit <- lm(d ~ t)
  trend <- unname(fitted(fit))
  noise <- unname(residuals(fit))

  mse  <- matrix(NA_real_, reps, Nmax)
  mape <- matrix(NA_real_, reps, Nmax)
  for (r in seq_len(reps)) {
    y <- trend + sample(noise, n, replace = TRUE)
    for (N in 1:Nmax) {
      F <- ma_n_forecast(y, N)
      mse[r, N]  <- ma_n_mse(y, F, Nmax + 1)
      mape[r, N] <- ma_n_mape(y, F, Nmax + 1)
    }
  }
  list(
    tab = data.frame(N = 1:Nmax, MSE = colMeans(mse), MAPE = colMeans(mape)),
    mse = mse, mape = mape, reps = reps
  )
}

# Share (%) of simulated series in which each N was the best
ma_n_win_share <- function(sim, criterion = "MSE") {
  m <- if (criterion == "MSE") sim$mse else sim$mape
  wins <- tabulate(apply(m, 1, which.min), nbins = ncol(m))
  100 * wins / nrow(m)
}


# ---------------------------------------------------------------------
# 2) SAMPLE DATA + FILE READING
# ---------------------------------------------------------------------

# 36 months of demand (artificial, for trying the module out)
ma_n_sample <- data.frame(
  Month = 1:36,
  Demand = c(96, 93, 83, 96, 106, 103, 101, 111, 112, 104, 114, 126,
             127, 109, 126, 118, 116, 129, 119, 124, 120, 132, 139, 129,
             133, 132, 138, 149, 131, 143, 134, 135, 143, 145, 144, 145)
)

# Read a .csv (comma or semicolon) or .xlsx file
ma_n_read <- function(path, name) {
  if (tolower(tools::file_ext(name)) %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(path)))
  }
  first <- readLines(path, n = 1, warn = FALSE)
  if (grepl(";", first, fixed = TRUE)) read.csv2(path) else read.csv(path)
}


# ---------------------------------------------------------------------
# 3) SCREEN (UI)
# ---------------------------------------------------------------------
ma_n_css <- "
.man { --blue:#2F6FB0; --orange:#B4501E; --ink:#1f2a37; --mute:#6b7785; --line:#e3e8ef; --bg:#f6f8fb; }
.man-hero { background:linear-gradient(120deg,#1f4f86,#2F6FB0 60%,#5b95cf); color:#fff;
            padding:22px 26px; border-radius:14px; margin-bottom:18px; }
.man-hero h2 { margin:0 0 4px; font-weight:700; }
.man-hero p  { margin:0; opacity:.9; }
.man-card { background:#fff; border:1px solid var(--line); border-radius:14px; padding:16px 18px;
            margin-bottom:16px; box-shadow:0 1px 3px rgba(20,40,80,.06); }
.man-step { font-size:12px; font-weight:700; letter-spacing:.08em; text-transform:uppercase;
            color:var(--blue); margin:4px 0 8px; }
.man-card .radio, .man-card .shiny-options-group { margin-top:2px; }
.man .btn-run { width:100%; background:var(--orange); border:none; color:#fff; font-weight:700;
                padding:11px; border-radius:10px; margin-top:6px; }
.man .btn-run:hover { background:#963f14; color:#fff; }
.man-kpis { display:grid; grid-template-columns:repeat(4,1fr); gap:12px; margin-bottom:16px; }
@media (max-width:900px){ .man-kpis { grid-template-columns:repeat(2,1fr); } }
.man-kpi { background:#fff; border:1px solid var(--line); border-radius:14px; padding:14px 16px; }
.man-kpi .lab { font-size:12px; color:var(--mute); text-transform:uppercase; letter-spacing:.06em; }
.man-kpi .val { font-size:28px; font-weight:700; color:var(--ink); line-height:1.2; }
.man-kpi.best { background:var(--orange); border-color:var(--orange); }
.man-kpi.best .lab, .man-kpi.best .val { color:#fff; }
.man-note { background:#fff7ef; border-left:4px solid var(--orange); border-radius:8px;
            padding:10px 14px; margin-bottom:14px; color:#5a3a22; font-size:14px; }
.man-err  { background:#fdecea; border-left:4px solid #c0392b; border-radius:8px;
            padding:12px 14px; color:#7a1f16; }
.man-badge { display:inline-block; font-size:12px; font-weight:700; padding:3px 10px;
             border-radius:20px; background:#e6effa; color:var(--blue); margin-bottom:10px; }
.man-badge.sim { background:#fbe9de; color:var(--orange); }
.man .nav-tabs > li > a { color:var(--mute); font-weight:600; }
.man .nav-tabs > li.active > a { color:var(--blue); }
"

ma_n_ui <- function(id) {
  ns <- NS(id)
  div(class = "man",
    tags$style(HTML(ma_n_css)),
    div(class = "man-hero",
        h2("Best N for Moving Average"),
        p("Tries N = 1 to 10, compares MSE and MAPE, and shows the best N.")),
    fluidRow(
      # ---------- left: controls ----------
      column(4,
        div(class = "man-card",
          div(class = "man-step", "1 · Data"),
          radioButtons(ns("source"), NULL,
                       choices = c("Use the sample data" = "sample",
                                   "Upload my own file (.csv / .xlsx)" = "upload")),
          conditionalPanel(
            condition = "input.source == 'upload'", ns = ns,
            fileInput(ns("file"), NULL, accept = c(".csv", ".xlsx", ".xls"))
          ),
          selectInput(ns("column"), "Demand column", choices = NULL),
          downloadLink(ns("dl_sample"), "Download the sample CSV")
        ),
        div(class = "man-card",
          div(class = "man-step", "2 · Method"),
          radioButtons(ns("method"), NULL,
                       choices = c("Normal Moving Average" = "normal",
                                   "With simulation" = "sim")),
          conditionalPanel(
            condition = "input.method == 'sim'", ns = ns,
            sliderInput(ns("reps"), "Number of simulated series",
                        min = 100, max = 2000, value = 500, step = 100),
            numericInput(ns("seed"), "Random seed (same seed = same result)", value = 123)
          )
        ),
        div(class = "man-card",
          div(class = "man-step", "3 · Choose the best N by"),
          radioButtons(ns("criterion"), NULL, choices = c("MSE", "MAPE"), inline = TRUE),
          actionButton(ns("run"), "Run analysis", class = "btn-run")
        )
      ),
      # ---------- right: results ----------
      column(8,
        uiOutput(ns("kpis")),
        tabsetPanel(
          tabPanel("Results",
                   br(), plotOutput(ns("result_plot"), height = "320px"),
                   h4("N = 1, ..., 10"), tableOutput(ns("result_table"))),
          tabPanel("Demand & forecast",
                   br(), plotOutput(ns("forecast_plot"), height = "380px")),
          tabPanel("How it works", br(), uiOutput(ns("explain")))
        )
      )
    )
  )
}


# ---------------------------------------------------------------------
# 4) SERVER
# ---------------------------------------------------------------------
ma_n_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- Data -----------------------------------------------------------
    data <- reactive({
      if (input$source == "sample") {
        ma_n_sample
      } else {
        req(input$file)
        ma_n_read(input$file$datapath, input$file$name)
      }
    })

    numeric_cols <- reactive(names(data())[sapply(data(), is.numeric)])

    observeEvent(data(), {
      cols <- numeric_cols()
      updateSelectInput(session, "column", choices = cols, selected = tail(cols, 1))
    })

    output$dl_sample <- downloadHandler(
      filename = function() "sample_demand.csv",
      content  = function(file) write.csv(ma_n_sample, file, row.names = FALSE)
    )

    # --- Analysis: runs once at start and every time "Run analysis" is pressed
    analysis <- eventReactive(input$run, ignoreNULL = FALSE, {
      tryCatch({
        dat <- data()
        cols <- numeric_cols()
        validate(need(length(cols) > 0, "The file has no numeric column."))
        col <- if (!is.null(input$column) && input$column %in% cols) input$column else tail(cols, 1)
        d <- dat[[col]]
        d <- d[!is.na(d)]
        Nmax <- 10
        if (length(d) <= Nmax + 1) {
          stop("At least ", Nmax + 2, " observations are needed to compare N = 1 to ",
               Nmax, " (your data has ", length(d), ").")
        }

        sim <- NULL
        if (input$method == "sim") {
          reps <- if (is.null(input$reps)) 500 else input$reps
          seed <- if (is.null(input$seed) || is.na(input$seed)) 123 else input$seed
          withProgress(message = "Running simulation...", value = 0.5, {
            sim <- ma_n_simulation(d, Nmax, reps = reps, seed = seed)
          })
          tab <- sim$tab
        } else {
          tab <- ma_n_search(d, Nmax)
        }

        best <- ma_n_best(tab, input$criterion)
        list(d = d, col = col, tab = tab, sim = sim, best = best,
             F = ma_n_forecast(d, best), Nmax = Nmax,
             criterion = input$criterion, method = input$method, error = NULL)
      }, error = function(e) list(error = conditionMessage(e)))
    })

    # --- KPI cards + notes ---------------------------------------------
    output$kpis <- renderUI({
      a <- analysis()
      if (!is.null(a$error)) return(div(class = "man-err", a$error))
      row <- a$tab[a$tab$N == a$best, ]
      other <- if (a$criterion == "MSE") "MAPE" else "MSE"
      other_best <- ma_n_best(a$tab, other)
      tagList(
        span(class = paste("man-badge", if (a$method == "sim") "sim"),
             if (a$method == "sim") sprintf("Simulation · %d series", a$sim$reps) else "Normal method"),
        div(class = "man-kpis",
          div(class = "man-kpi best", div(class = "lab", paste("Best N by", a$criterion)),
              div(class = "val", a$best)),
          div(class = "man-kpi", div(class = "lab", "MSE"), div(class = "val", sprintf("%.1f", row$MSE))),
          div(class = "man-kpi", div(class = "lab", "MAPE"), div(class = "val", sprintf("%.2f%%", row$MAPE))),
          div(class = "man-kpi", div(class = "lab", sprintf("Next period (N=%d)", a$best)),
              div(class = "val", sprintf("%.1f", tail(a$F, 1))))
        ),
        if (other_best != a$best)
          div(class = "man-note", sprintf("By %s the best N would be %d instead.", other, other_best))
      )
    })

    # --- Results chart ---------------------------------------------------
    output$result_plot <- renderPlot({
      a <- analysis()
      validate(need(is.null(a$error), ""))
      k <- a$criterion
      col <- ifelse(a$tab$N == a$best, "#B4501E", "#9DB4CF")
      if (a$method == "sim") {
        op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1)); on.exit(par(op))
      }
      barplot(a$tab[[k]], names.arg = a$tab$N, col = col, border = NA,
              xlab = "N", ylab = paste(k, if (a$method == "sim") "(average)" else ""),
              main = paste0(k, " by N  —  best N = ", a$best))
      if (a$method == "sim") {
        share <- ma_n_win_share(a$sim, k)
        barplot(share, names.arg = 1:a$Nmax, col = ifelse(1:a$Nmax == a$best, "#B4501E", "#9DB4CF"),
                border = NA, xlab = "N", ylab = "% of simulated series",
                main = paste0("How often each N was best (", k, ")"))
      }
    })

    output$result_table <- renderTable({
      a <- analysis()
      validate(need(is.null(a$error), ""))
      out <- data.frame(
        "N" = as.integer(a$tab$N),
        "MSE" = round(a$tab$MSE, 2),
        "MAPE (%)" = round(a$tab$MAPE, 2),
        check.names = FALSE
      )
      if (a$method == "sim") {
        out[["Best in (% of series)"]] <- round(ma_n_win_share(a$sim, a$criterion), 1)
      }
      out[[" "]] <- ifelse(a$tab$N == a$best, "<-- best N", "")
      out
    }, striped = TRUE)

    # --- Demand & forecast chart -----------------------------------------
    output$forecast_plot <- renderPlot({
      a <- analysis()
      validate(need(is.null(a$error), ""))
      n <- length(a$d)
      plot(1:n, a$d, type = "o", pch = 19, col = "grey30",
           xlim = c(1, n + 1), ylim = range(c(a$d, a$F), na.rm = TRUE),
           xlab = "Period", ylab = a$col,
           main = paste0("Actual demand and MA(", a$best, ") forecast"))
      lines(1:(n + 1), a$F, type = "o", pch = 17, lty = 2, col = "#2F6FB0")
      points(n + 1, tail(a$F, 1), pch = 17, cex = 2, col = "#B4501E")
      legend("topleft", bty = "n",
             legend = c("Actual demand", paste0("MA(", a$best, ") forecast"), "Next-period forecast"),
             col = c("grey30", "#2F6FB0", "#B4501E"), pch = c(19, 17, 17), lty = c(1, 2, NA))
    })

    # --- Explanation -------------------------------------------------------
    output$explain <- renderUI({
      tagList(
        h4("Normal method"),
        tags$ol(
          tags$li("For each N = 1, ..., 10: forecast = average of the last N demands."),
          tags$li("Compute the error of each forecast: e = F - D."),
          tags$li("MSE = average of e²;  MAPE = average of |e / D| × 100."),
          tags$li("All N are compared over the same periods (11 to the last one)."),
          tags$li("The best N is the one with the smallest MSE (or MAPE).")
        ),
        h4("With simulation"),
        p("Instead of using the one real history only, the module builds many artificial histories:"),
        tags$ol(
          tags$li("Fit a straight-line trend to the demand; the leftover part is the noise."),
          tags$li("Create many artificial series = trend + noise drawn at random from the real noise."),
          tags$li("Run the normal method on each artificial series."),
          tags$li("Average MSE and MAPE over all series; the best N has the smallest average.")
        ),
        div(class = "man-note",
            "Note: this simulation is a simple assumption made by our group (residual bootstrap). ",
            "If the course defines a different simulation, only ma_n_simulation() in R/yeni_modul.R changes.")
      )
    })
  })
}
