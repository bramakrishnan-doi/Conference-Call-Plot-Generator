# This R code uses a custom function (get_hdb_data) to pull data from the public API
# Plots for conference calls are generated including:
# Mohave/Havasu elevation, Davis Releases, and 2008-YYYY average release
# for the current month for Davis/Parker

### Everything below here is automatic ###
library(ggplot2)
library(tidyverse)
library(scales)
library(httr2)
library(zoo)
library(magick)

source("scripts/config.R")

# Validate config inputs early
stopifnot(
  "endmonth_2 must be after endmonth_1" = as.Date(endmonth_2) > as.Date(endmonth_1),
  "monthflag must be 0 or 1" = monthflag %in% c(0, 1)
)

# SDI-to-name lookup vectors (single source of truth)
sdi_dam_hist  <- c("1874" = "Hoover", "2096" = "Davis",  "2097" = "Parker")
sdi_dam_model <- c("1863" = "Hoover", "2166" = "Davis",  "2146" = "Parker")
sdi_res_map   <- c("1930" = "Mead",   "2100" = "Mohave", "2101" = "Havasu")

# ------------------------------------------------------------------------------
# Fetch and process data from the USBR HDB database
#
# Example 1: Get the last 24 hours of data for two sites.
#   get_hdb_data(sdi = c(1930, 2100))
#
# Example 2: Get daily data for a site over a longer period.
#   get_hdb_data(sdi = 1934, tstp = "DY", t1 = -30, t2 = 0)
#   get_hdb_data(c(1930, 2101), t1 = "2025-06-01", t2 = "2025-06-25", tstp = "DY")
# ------------------------------------------------------------------------------
get_hdb_data <- function(
  sdi,
  svr   = "lchdb",
  tstp  = "HR",
  t1    = -24,
  t2    = 0,
  table = "R",
  mrid  = 0
) {
  hdb_base_url <- "https://www.usbr.gov/pn-bin/hdb/hdb.pl"
  sdi_string   <- paste(sdi, collapse = ",")

  resp <- request(hdb_base_url) |>
    req_url_query(
      svr    = svr,
      sdi    = sdi_string,
      tstp   = tstp,
      t1     = t1,
      t2     = t2,
      table  = table,
      mrid   = mrid,
      format = "json"
    ) |>
    req_perform()

  # The HDB API returns an HTML error page (not JSON) when a query fails.
  # Detect that early and surface a useful message instead of a JSON parse error.
  body <- resp_body_string(resp)
  if (grepl("Software error|<html|<h1>", body, ignore.case = TRUE)) {
    stop(
      "HDB API returned an error page instead of JSON.\n",
      "  URL: ", resp$url, "\n",
      "  Response: ", substr(body, 1, 300)
    )
  }

  jsonlite::fromJSON(body, simplifyVector = FALSE) |>
    pluck("Series") |>
    map(\(x) {
      data_points <- x |> pluck("Data")

      # Guard: skip series with no data so empty results don't error or
      # produce mismatched-length columns.
      if (length(data_points) == 0) {
        return(tibble(
          sdi       = integer(),
          time_step = as.POSIXct(character(), tz = "MST"),
          mrid      = character(),
          value     = numeric(),
          units     = character()
        ))
      }

      tibble(
        sdi       = x |> pluck("SDI"),
        time_step = data_points |>
          map("t") |>
          unlist() |>
          parse_date_time2("%m/%d/%Y %I:%M:%S %p", tz = "MST"),
        mrid  = x |> pluck("MRID", .default = NA_character_) |> as.character(),
        value = data_points |> map("v") |> unlist() |> as.numeric(),
        units = x |> pluck("DataTypeUnit")
      )
    }) |>
    list_rbind()
}

# ------------------------------------------------------------------------------
# Helper: add month and shortdate columns, map SDI to a label column.
# `sdi_map` is a named character vector like c("1874" = "Hoover", ...).
# ------------------------------------------------------------------------------
add_time_cols <- function(df, sdi_map, label_col) {
  df |>
    mutate(
      month     = month(time_step),
      shortdate = as_date(time_step),
      {{ label_col }} := unname(sdi_map[as.character(sdi)])
    )
}

# ------------------------------------------------------------------------------
# Date setup
# ------------------------------------------------------------------------------
currentday       <- format(Sys.Date(), "%Y-%m-%d")
currentdaysplit  <- strsplit(currentday, "-")[[1]]
yesterday        <- format(as.Date(currentday) - 1, "%Y-%m-%d")
start_date       <- "2008-01-01"
start_firstmonth <- format(as.Date(endmonth_1), "%Y-%m-01")
enddate          <- format(as.Date(endmonth_2) %m+% months(1), "%Y-%m-%d")

startplot <- as.Date(paste0(
  format(as.Date(endmonth_1), "%Y-%m"),
  "-",
  ifelse(as.numeric(currentdaysplit[3]) > 10, "01", "15")
))

# ------------------------------------------------------------------------------
# Build elevation targets tibble
# ------------------------------------------------------------------------------
targets <- tibble(
  shortdate = c(
    as.Date(endmonth_1), as.Date(endmonth_2),
    as.Date(endmonth_1), as.Date(endmonth_2)
  ),
  value     = c(mohave_elev_1, mohave_elev_2, havasu_elev_1, havasu_elev_2),
  Reservoir = c("Mohave", "Mohave", "Havasu", "Havasu"),
  Type      = "Target"
)

# ------------------------------------------------------------------------------
# Fetch data
# ------------------------------------------------------------------------------
hist_releases  <- get_hdb_data(
  sdi  = c(1874, 2096, 2097),
  tstp = "DY", t1 = start_date, t2 = yesterday
)
model_releases <- get_hdb_data(
  sdi   = c(1863, 2166, 2146),
  tstp  = "DY", t1 = currentday, t2 = enddate,
  table = "M", mrid = 4
)
hist_elevation  <- get_hdb_data(
  sdi  = c(1930, 2100, 2101),
  tstp = "DY", t1 = start_firstmonth, t2 = yesterday
)
model_elevation <- get_hdb_data(
  sdi   = c(1930, 2100, 2101),
  tstp  = "DY", t1 = currentday, t2 = enddate,
  table = "M", mrid = 4
)

# ------------------------------------------------------------------------------
# Add derived columns
# ------------------------------------------------------------------------------
hist_releases  <- hist_releases  |> add_time_cols(sdi_dam_hist,  Dam)       |> mutate(Type = "Observed")
model_releases <- model_releases |> add_time_cols(sdi_dam_model, Dam)       |> mutate(Type = "Modeled")
hist_elevation  <- hist_elevation  |> add_time_cols(sdi_res_map, Reservoir) |> mutate(Type = "Observed")
model_elevation <- model_elevation |> add_time_cols(sdi_res_map, Reservoir) |> mutate(Type = "Modeled")

# ------------------------------------------------------------------------------
# Combine observed + modeled, add a bridge point so there's no gap in lines.
# Takes the first (and second) modeled day, relabels it "Observed", and appends
# it so the observed line extends forward to meet the modeled line.
# ------------------------------------------------------------------------------
add_bridge <- function(obs_df, mod_df) {
  combined  <- bind_rows(obs_df, mod_df)
  mod_start <- min(mod_df$shortdate)

  bridge <- combined |>
    filter(shortdate == mod_start | shortdate == mod_start + 1) |>
    mutate(Type = "Observed")

  bind_rows(combined, bridge) |>
    mutate(Type = factor(Type, levels = c("Observed", "Modeled")))
}

relcomb <- add_bridge(hist_releases,  model_releases)  |> mutate(datatype = "Releases")
elcomb  <- add_bridge(hist_elevation, model_elevation) |> mutate(datatype = "Elevation")
comb    <- bind_rows(elcomb, relcomb)

# Pre-split to avoid repeated full-data filter() calls inside loops
comb_releases  <- split(
  filter(comb, datatype == "Releases"),
  filter(comb, datatype == "Releases")$Dam
)
comb_elevation <- split(
  filter(comb, datatype == "Elevation"),
  filter(comb, datatype == "Elevation")$Reservoir
)

# ------------------------------------------------------------------------------
# Shared theme for conference call plots
# ------------------------------------------------------------------------------
theme_cc <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.y    = element_text(face = "bold", size = 10),
      axis.text.x     = element_text(size = 9),
      axis.text.y     = element_text(size = 9),
      legend.title    = element_blank(),
      legend.position = "bottom"
    )
}

shared_x_date_scale <- scale_x_date(
  limits            = c(startplot, as.Date(endmonth_2)),
  date_breaks       = "week",
  date_minor_breaks = "days",
  labels            = \(x) paste(month(x), day(x), sep = "/"),
  expand            = c(0.01, 0.01)
)

# ==============================================================================
# Laughlin charts
# ==============================================================================
if (plot_Laughlin_charts) {

  file.create("AveragesforPptSlides.txt")
  avgs <- "AveragesforPptSlides.txt"

  plot_months <- c(
    ifelse(monthflag == 0, endmonth_1, endmonth_2),
    ifelse(monthflag == 0, endmonth_2, enddate)
  )

  for (n in plot_months) {
    n_date   <- as.Date(n)
    n_month  <- month(n_date)
    n_year   <- year(n_date)

    # Average release by Dam and year for this calendar month
    monthonly_avgrelease <- hist_releases |>
      filter(month == n_month) |>
      mutate(year = year(shortdate)) |>
      summarise(value = mean(value), .by = c(Dam, year)) |>
      mutate(
        Dam    = factor(Dam, levels = c("Hoover", "Davis", "Parker")),
        symbol = if_else(year == n_year, "open", "closed")
      )

    # --- Per-dam average release plots ---
    for (name in levels(monthonly_avgrelease$Dam)) {
      dam_data <- filter(monthonly_avgrelease, Dam == name)
      min_val  <- floor(min(dam_data$value) / 1000) * 1000 - 1000
      max_val  <- ceiling(max(dam_data$value) / 1000) * 1000 + 1000

      ggplot(dam_data, aes(x = year, y = value)) +
        geom_line(color = "#003E51", linewidth = 1.5) +
        geom_point(
          aes(fill = as.factor(symbol)),
          color = "#003E51", size = 2.5, shape = 21, stroke = 2
        ) +
        scale_fill_manual(values = c("closed" = "#003E51", "open" = "white")) +
        scale_y_continuous(
          limits = c(min_val, max_val),
          breaks = seq(min_val, max_val, 1000),
          labels = label_comma(),
          expand = c(0, 0)
        ) +
        scale_x_continuous(
          breaks = seq(min(dam_data$year), max(dam_data$year), 1),
          expand = c(0.025, 0.025)
        ) +
        labs(
          y = "Power Release (cfs)",
          x = "Year"
        ) +
        theme_bw(base_size = 8) +
        theme(
          plot.title        = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.grid.minor.x = element_blank(),
          legend.position   = "none",
          axis.title.y      = element_text(size = 8, face = "bold"),
          axis.title.x      = element_text(size = 8, face = "bold"),
          axis.text.x       = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )

      ggsave(
        filename = paste0(name, "AverageReleasesin", month.name[n_month], ".png"),
        width = 6.25, height = 3.1
      )
    }

    # --- Write averages to text file ---
    historic_mean <- monthonly_avgrelease |>
      filter(year != n_year) |>
      summarise(value = round(mean(value), -2), .by = Dam) |>
      mutate(year = paste0("2008-", n_year - 1))

    this_last_year <- monthonly_avgrelease |>
      filter(year >= n_year - 1) |>
      select(-symbol) |>
      mutate(value = round(value, -2), year = as.character(year))

    all_hist <- bind_rows(this_last_year, historic_mean)

    cat(
      paste0("Average releases for ", month.name[n_month], ":"),
      file = avgs, append = TRUE, sep = "\n"
    )
    write.table(
      all_hist[order(all_hist$Dam, all_hist$year), ],
      file = avgs, append = TRUE, row.names = FALSE, col.names = FALSE
    )
  }

  # --- Release plots per dam ---
  for (dam in names(comb_releases)) {
    dam_data <- comb_releases[[dam]]
    y_range  <- range(filter(dam_data, shortdate >= startplot)$value)

    p <- ggplot(dam_data, aes(x = shortdate, y = value, color = Type)) +
      geom_line(linewidth = 2, lineend = "round") +
      shared_x_date_scale +
      scale_y_continuous(
        limits = c(y_range[1] - 2000, y_range[2] + 2000),
        labels = \(x) format(x, big.mark = ",")
      ) +
      scale_color_manual(
        values = c("Observed" = "#4472C4", "Modeled" = "#ED7D31"),
        labels = c(
          paste0("Historical ", dam, " Releases"),
          paste0("Projected ",  dam, " Releases")
        )
      ) +
      labs(
        x     = "",
        y     = "Power Release (cfs)",
        title = paste0(dam, " Dam Releases")
      ) +
      theme_cc()

    ggsave(p, filename = paste0(dam, "_releases.png"), width = 5.7, height = 4.1)
  }

  # --- Elevation plots per reservoir ---
  for (reservoir in names(comb_elevation)) {
    res_data    <- comb_elevation[[reservoir]]
    res_targets <- filter(targets, Reservoir == reservoir)

    visible_vals <- filter(res_data, shortdate >= startplot)$value
    all_vals     <- c(visible_vals, res_targets$value)
    y_lims       <- c(min(all_vals) - 1.5, max(all_vals) + 1.5)

    p <- ggplot(res_data, aes(x = shortdate, y = value, color = Type)) +
      geom_line(linewidth = 2, lineend = "round") +
      shared_x_date_scale +
      scale_y_continuous(
        limits = y_lims,
        labels = \(x) format(x, big.mark = ",")
      ) +
      labs(
        x     = "",
        y     = "Surface Water Elevation (feet)",
        title = paste0("Lake ", reservoir, " Elevation")
      ) +
      theme_cc()

    if (reservoir == "Mead") {
      p <- p + scale_color_manual(
        values = c("Observed" = "#4472C4", "Modeled" = "#ED7D31"),
        labels = c(
          paste0("Historical ", reservoir, " Elevation"),
          paste0("Projected ", reservoir, " Elevation")
        )
      )
    } else {
      p <- p +
        geom_point(
          data    = res_targets,
          mapping = aes(x = shortdate, y = value, color = Type),
          size    = 2.75
        ) +
        ggrepel::geom_label_repel(
          data    = res_targets,
          mapping = aes(x = shortdate, y = value, label = sprintf("%0.2f", value)),
          color   = "black",
          size    = 3
        ) +
        scale_color_manual(
          values = c("Observed" = "#4472C4", "Modeled" = "#ED7D31", "Target" = "#FF0000"),
          labels = c(
            paste0("Historical\n", reservoir, " Elevation"),
            paste0("Projected\n", reservoir, " Elevation"),
            paste0(reservoir, " Target\nElevation")
          ),
          guide = guide_legend(
            override.aes = list(
              linetype = c("solid", "solid", "blank"),
              shape    = c(NA, NA, 16)
            )
          )
        )
    }

    ggsave(p, filename = paste0(reservoir, "_elevation.png"), width = 5.7, height = 4.1)
  }
}

# ==============================================================================
# NPS charts
# ==============================================================================
if (plot_NPS_charts) {

  maxelevs <- list(
    "Mead"   = "Maximum Elevation = 1,229 ft",
    "Mohave" = "Maximum Elevation = 647 ft",
    "Havasu" = "Maximum Elevation = 450 ft"
  )

  for (reservoir in names(comb_elevation)) {
    res_data    <- comb_elevation[[reservoir]]
    res_targets <- filter(targets, Reservoir == reservoir)

    visible_vals <- filter(res_data, shortdate >= start_firstmonth)$value
    y_min <- min(c(visible_vals, res_targets$value)) - 2
    y_max <- max(c(visible_vals, res_targets$value)) + 2
    y_max <- ifelse(reservoir == "Havasu", 450, y_max)

    p <- ggplot(res_data, aes(x = shortdate, y = value, fill = Type)) +
      geom_ribbon(aes(ymin = y_min, ymax = value)) +
      geom_line(linewidth = 0.2) +
      scale_x_date(
        limits            = c(startplot, as.Date(endmonth_2)),
        date_breaks       = "1 week",
        date_minor_breaks = "days",
        date_labels       = "%m/%d",
        expand            = c(0, 0)
      ) +
      scale_y_continuous(
        limits = c(y_min, y_max),
        expand = c(0, 0),
        labels = \(x) format(x, big.mark = ",")
      ) +
      scale_fill_manual(values = c("Observed" = "#1F497D", "Modeled" = "#B7DEE8")) +
      labs(
        x        = ifelse(
          month(as.Date(endmonth_2)) == 1,
          paste0(year(as.Date(endmonth_1)), "/", year(as.Date(endmonth_2))),
          year(as.Date(endmonth_2))
        ),
        y        = "Elevation (ft)",
        title    = paste0("Lake ", reservoir, " End of Day Elevation"),
        subtitle = maxelevs[[reservoir]]
      ) +
      theme_bw(base_size = 6) +
      theme(
        plot.title    = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 5),
        axis.title.y  = element_text(face = "bold"),
        axis.title.x  = element_text(face = "bold"),
        legend.title  = element_blank(),
        legend.position = "none"
      )

    ggsave(
      p,
      filename = paste0(reservoir, "_filledelevation_NPS.png"),
      width = 4, height = 2.1
    )
  }

  # --- NPS Lake Mead EOM Elevation Projections ---
  month_heading <- paste(month.name[month(ym(run_date))], year(ym(run_date)))
  subtitleb     <- paste0(
    "Projections from ", month_heading, " 24-Month Study Most Probable Inflow Scenario"
  )

  sdis         <- c("Mead.Pool Elevation" = 1930)
  mrid_to_trace <- setNames(c("24MS Most"), as.character(most_mrid))
  lab_names     <- c(
    "24MS Most" = paste0(month_heading, " Probable 24-Month Study"),
    "Historical" = "Historical"
  )

  hist_nMons     <- 12
  end_date       <- format(ym(run_date) + months(23), "%Y-%m")
  start_date_nps <- format(ym(run_date) - months(1),  "%Y-%m")
  histStart_date <- format(ym(run_date) - months(hist_nMons), "%Y-%m")
  textStart_date <- format(ym(run_date) - months(4), "%Y-%m")

  df_hdb <- get_hdb_data(
    sdi = sdis["Mead.Pool Elevation"], tstp = "MN", svr = "lchdb",
    t1 = run_date, t2 = end_date, table = "M", mrid = most_mrid
  ) |>
    mutate(
      slot  = names(sdis)[match(sdi, sdis)],
      run   = run_date,
      Trace = mrid_to_trace[as.character(mrid)],
      Date  = as.yearmon(time_step)
    ) |>
    select(-sdi, -mrid, -time_step)

  df_hist <- get_hdb_data(
    sdi = sdis["Mead.Pool Elevation"], tstp = "MN", svr = "lchdb",
    t1 = histStart_date, t2 = run_date
  ) |>
    mutate(
      slot  = names(sdis)[match(sdi, sdis)],
      run   = run_date,
      Trace = "Historical",
      Date  = as.yearmon(time_step)
    ) |>
    select(-sdi, -mrid, -time_step) |>
    na.omit()

  # Bridge historical into 24MS start
  df_init_add <- df_hist |>
    filter(Date == max(Date)) |>
    select(slot, Date, value) |>
    left_join(
      distinct(select(df_hdb, slot, run, Trace, units)),
      by = "slot"
    )

  df_24MS <- bind_rows(df_hdb, df_hist, df_init_add)

  df_24ms_most <- df_24MS |>
    filter(slot == "Mead.Pool Elevation", Trace %in% c("24MS Most", "Historical")) |>
    mutate(
      trace_labels  = lab_names[Trace],
      nudge_amount  = if_else(Date == max(Date), -0.022, 0)
    )

  nn            <- lab_names[c("24MS Most", "Historical")]
  custom_colors <- setNames(c("#26AE44", "grey20"), nn)
  custom_size   <- setNames(c(1, 1), nn)
  custom_lt     <- setNames(c(1, 1), nn)
  custom_alpha  <- setNames(c(1, 1), nn)

  m_breaks  <- seq(1000, 1250, 25)
  m_breaks2 <- seq(1000, 1250, 5)
  yy        <- c(1000, 1125)
  alpha_1   <- 0.8

  # Elevation-to-storage lookup
  getData <- function(res) read.csv(file.path("data", paste0(res, "ElevationVolume.csv")))
  ev_tables <- setNames(lapply(c("mead", "powell"), getData), c("mead", "powell"))

  elevation_to_storage <- function(elevation, reservoir) {
    approxfun(ev_tables[[reservoir]][, 1], ev_tables[[reservoir]][, 2])(elevation)
  }

  # Ribbon band data (fixed horizontal bands; x range spans full date range)
  date_range <- range(df_24MS$Date)
  bands <- tibble(
    ymax  = c(1076.5, 1076, 1073, 1070, 1060, 1050, 1045, 1035, 1000),
    ymin  = c(1076,   1073, 1070, 1060, 1050, 1045, 1035, 1000,  950),
    fill  = c("#cccccc", "#f1e2cc", "#fff2ae", "#e6f5c9", "#cbd5e8",
               "#fdcdac", "#b3e2cd", "#cccccc", "#ffb6c1")
  )

  gg <- ggplot(df_24ms_most, aes(x = Date))

  # Add coloured background bands
  for (i in seq_len(nrow(bands))) {
    gg <- gg + geom_ribbon(
      data    = tibble(Date = date_range),
      mapping = aes(ymax = bands$ymax[i], ymin = bands$ymin[i]),
      fill    = bands$fill[i],
      alpha   = alpha_1,
      inherit.aes = FALSE
    )
  }

  gg <- gg +
    geom_vline(
      xintercept = as.yearmon(c("Dec 2025", "Dec 2026")),
      linewidth = 1, color = "#ffdc70", alpha = 0.8
    ) +
    annotate(
      "text",
      x = as.yearmon(ym(histStart_date) + months(1)),
      y = c(1078.2, 1074.6, 1071.6, 1066, 1057, 1054, 1049.1,
            995, 1046.6, 1040, 1020.6, 1017.7),
      label = c(
        "Hemenway Harbor Extension - 1,076.5'",
        "Temple Bar Extension - 1,076'",
        "Callville Bay Extension - 1,073'",
        "Echo Bay and South Cove Extension - 1,070'",
        "Hemenway Harbor, Echo Bay, Temple Bar, and South Cove",
        "Extension and Callville Bay Relocation Trigger - 1,060'",
        "Temple Bar and Echo Bay Relocation Trigger and",
        "Hemenway Harbor Relocation Trigger - 1,000'",
        "Hemenway Harbor Extension - 1,050'",
        "South Cove Closure - 1,045'",
        "Hemenway Harbor, Echo Bay, Calville Bay,",
        "and Temple Bar Extension - 1,035'"
      ),
      angle = 0, size = 2.5, hjust = 0
    ) +
    geom_line(
      aes(
        y        = value,
        color    = trace_labels,
        alpha    = trace_labels,
        group    = Trace,
        linetype = trace_labels,
        linewidth = trace_labels
      )
    ) +
    geom_point(
      aes(y = value, color = trace_labels, alpha = trace_labels, group = Trace),
      size = 2.5
    ) +
    scale_color_manual(values = custom_colors, breaks = unique(df_24ms_most$trace_labels)) +
    scale_linetype_manual(values = custom_lt,   breaks = unique(df_24ms_most$trace_labels)) +
    scale_linewidth_manual(values = custom_size, breaks = unique(df_24ms_most$trace_labels)) +
    scale_alpha_manual(values = custom_alpha,   breaks = unique(df_24ms_most$trace_labels)) +
    scale_x_yearmon(
      expand       = c(0, 0),
      breaks       = unique(df_24MS$Date),
      minor_breaks = unique(df_24MS$Date),
      limits       = c(min(df_24MS$Date), max(df_24MS$Date))
    ) +
    scale_y_continuous(
      labels       = scales::comma,
      breaks       = m_breaks,
      minor_breaks = m_breaks2,
      limits       = yy,
      expand       = c(0, 0),
      sec.axis     = sec_axis(
        \(x) elevation_to_storage(x, "mead"),
        breaks = elevation_to_storage(m_breaks, "mead"),
        labels = scales::comma_format(scale = 1 / 1e6, accuracy = 0.01),
        name   = "Storage (maf)"
      )
    ) +
    geom_vline(
      xintercept = as.yearmon(start_date_nps),
      linewidth = 1, color = "grey20", alpha = 0.8
    ) +
    annotate("text", x = as.yearmon(textStart_date), y = 1100,
             hjust = 0, label = "Historical", size = 4, fontface = "bold") +
    annotate("text", x = as.yearmon(run_date), y = 1100,
             hjust = 0, label = "Future",     size = 4, fontface = "bold") +
    geom_text(
      aes(
        y     = value,
        label = if_else(Date > start_date_nps, format(round(value, 2), nsmall = 2), "")
      ),
      angle   = 90,
      nudge_y = 10,
      nudge_x = df_24ms_most$nudge_amount,
      size    = 4
    ) +
    labs(
      y        = "Pool Elevation (ft)",
      x        = NULL,
      color    = NULL, linetype = NULL, linewidth = NULL, fill = NULL,
      title    = bquote("Lake Mead End-of-Month" ~ Elevations),
      subtitle = subtitleb
    ) +
    theme_bw(base_size = 14) +
    guides(
      alpha     = "none",
      color     = guide_legend(nrow = 2, order = 1),
      linetype  = guide_legend(nrow = 2, order = 1),
      linewidth = guide_legend(nrow = 2, order = 1),
      fill      = guide_legend(order = 2)
    ) +
    theme(
      axis.text.x      = element_text(angle = 90, vjust = 0.5),
      legend.position  = "bottom",
      legend.key.width = unit(1.2, "cm"),
      plot.margin      = unit(c(0.1, 0.1, 1, 0.1), "cm"),
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle    = element_text(hjust = 0.5, size = 13),
      plot.caption     = element_text(hjust = 0, size = 10, face = "italic")
    )

  ggsave("NPS-24MS-MOST.png", width = 11, height = 8)

  crmms_m  <- image_read("NPS-24MS-MOST.png")
  logo_raw <- image_read("data/BofR-vert.png")
  image_composite(crmms_m, image_resize(logo_raw, "325"), offset = "+2860+2060") |>
    image_write(paste0(run_date, "-NPS-24MS-MOST.png"))
}