# This R code uses a custom function (get_hdb_data) to pull data from the public API
# Plots for conference calls are generated including:
# Mohave/Havasu elevation, Davis Releases, and 2008-YYYY average release
# for the current month for Davis/Parker

### Everything below here is automatic ###
# Load the required packages
library(ggplot2)
library(tidyverse)
library(scales)
library(httr2)
library(zoo)
library(magick)

source("scripts/config.R")

# Fetch and process data from the USBR HDB database
#
# This function sends a request to the USBR HDB API, retrieves the data in
# JSON format, and processes it into a clean tibble.
# Example 1: Get the last 24 hours of data for two sites.
# df_hourly <- get_hdb_data(sdi = c(1930, 2100))
#
# Example 2: Get daily data for a different site over a longer period.
# df_daily <- get_hdb_data(sdi = 1934, tstp = "DY", t1 = -30, t2 = 0)
# get_hdb_data(c(1930,2101), t1="2025-06-01",t2="2025-06-25", tstp="DY")

get_hdb_data <- function(
  sdi,
  svr = "lchdb",
  tstp = "HR",
  t1 = -24,
  t2 = 0,
  table = "R",
  mrid = 0
) {
  # Base URL for the API
  hdb_base_url <- 'https://www.usbr.gov/pn-bin/hdb/hdb.pl?'

  # Convert the sdi vector to a comma-separated string if needed
  sdi_string <- paste(sdi, collapse = ",")

  # Build and perform the API request using the function arguments
  hdb_response <- request(hdb_base_url) |>
    req_url_query(
      svr = svr,
      sdi = sdi_string,
      tstp = tstp,
      t1 = t1,
      t2 = t2,
      table = table,
      mrid = mrid,
      format = 'json'
    ) |>
    req_perform()

  # Parse the JSON response and transform it into a tibble
  # The final expression in a function is automatically returned
  hdb_response |>
    resp_body_json(check_type = FALSE) |>
    pluck('Series') |>
    map_df(
      \(x) {
        tibble(
          sdi = x |> pluck('SDI'),
          time_step = x |>
            pluck('Data') |>
            map('t') |>
            unlist() |>
            parse_date_time2("%m/%d/%Y %I:%M:%S %p", tz = "MST"),
          mrid = x |> pluck('MRID', .default = NA_real_),
          value = x |> pluck('Data') |> map('v') |> unlist() |> as.numeric(),
          units = x |> pluck('DataTypeUnit')
        )
      }
    )
}


# Build all the elevation targets into a single data frame
targets <- data.frame(
  shortdate = c(
    as.Date(endmonth_1),
    as.Date(endmonth_2),
    as.Date(endmonth_1),
    as.Date(endmonth_2)
  ),
  value = c(mohave_elev_1, mohave_elev_2, havasu_elev_1, havasu_elev_2),
  Reservoir = c("Mohave", "Mohave", "Havasu", "Havasu")
)


# Set up dates to query data based on the current day
currentday = format(Sys.Date(), "%Y-%m-%d")
currentdaysplit <- strsplit(currentday, "-")[[1]]
yesterday = format(as.Date(currentday) - 1, "%Y-%m-%d")
start_date = "2008-01-01"
start_firstmonth = format(as.Date(endmonth_1), "%Y-%m-01")
enddate = format(as.Date(endmonth_2) %m+% months(1), "%Y-%m-%d")

# Only go back to the 15th of the first month
# if we're making presentation in the first 10 days of the second month,
# else go back to the beginning of the first month
startplot <- as.Date(paste0(
  format(as.Date(endmonth_1), "%Y-%m"),
  "-",
  ifelse(as.numeric(currentdaysplit[3]) > 10, 01, 15)
))

# Query daily releases for Hoover, Davis, and Parker 2008-yesterday
hist_releases <- get_hdb_data(
  sdi = c(1874, 2096, 2097),
  svr = "lchdb",
  tstp = "DY",
  t1 = start_date,
  t2 = yesterday
)


# Query modeled releases from today through the end of the second month
# for Hoover, Davis, and Parker
# The 4 is to pull modeled data from MRID 4
model_releases <- get_hdb_data(
  sdi = c(1863, 2166, 2146),
  svr = "lchdb",
  tstp = "DY",
  t1 = currentday,
  t2 = enddate,
  table = "M",
  mrid = 4
)

# Query observed daily elevations from this year for Mead, Mohave, and Havasu
hist_elevation <- get_hdb_data(
  sdi = c(1930, 2100, 2101),
  svr = "lchdb",
  tstp = "DY",
  t1 = start_firstmonth,
  t2 = yesterday
)

# Query modeled daily elevations for from today through the end of
# the second month for Hoover, Davis, and Parker
model_elevation <- get_hdb_data(
  sdi = c(1930, 2100, 2101),
  svr = "lchdb",
  tstp = "DY",
  t1 = currentday,
  t2 = enddate,
  table = "M",
  mrid = 4
)

# Use the function to format dates in historical and current data
hist_releases <- hist_releases %>%
  mutate(
    month = month(time_step),
    shortdate = as_date(time_step),
    Dam = recode(sdi, '1874' = 'Hoover', '2096' = 'Davis', '2097' = 'Parker'),
    Type = "Observed"
  )


model_releases <- model_releases %>%
  mutate(
    month = month(time_step),
    shortdate = as_date(time_step),
    Dam = recode(sdi, '1863' = 'Hoover', '2166' = 'Davis', '2146' = 'Parker'),
    Type = "Modeled"
  )


hist_elevation <- hist_elevation %>%
  mutate(
    month = month(time_step),
    shortdate = as_date(time_step),
    Reservoir = recode(
      sdi,
      '1930' = 'Mead',
      '2100' = 'Mohave',
      '2101' = 'Havasu'
    ),
    Type = "Observed"
  )


model_elevation <- model_elevation %>%
  mutate(
    month = month(time_step),
    shortdate = as_date(time_step),
    Reservoir = recode(
      sdi,
      '1930' = 'Mead',
      '2100' = 'Mohave',
      '2101' = 'Havasu'
    ),
    Type = "Modeled"
  )


# Combine the observed and modeled data into a single data frame
releases <- rbind(hist_releases, model_releases)
elevation <- rbind(hist_elevation, model_elevation)

# For Laughlin Plot #
### Make the average yearly release figure x 2 ###
if (plot_Laughlin_charts) {
  # # Create the text file to write the powerpoint data to
  file.create('AveragesforPptSlides.txt')
  avgs <- 'AveragesforPptSlides.txt'

  for (n in c(
    ifelse(monthflag == 0, endmonth_1, endmonth_2),
    ifelse(monthflag == 0, endmonth_2, enddate)
  )) {
    # Filter release data to keep just the month we want to make the plots for
    monthonly_releases <- filter(releases, month == month(as.Date(n)))

    # Average the release by month and rename the year column for plotting
    monthonly_avgrelease <- monthonly_releases %>%
      group_by(Dam, year(shortdate)) %>%
      summarise(value = mean(value)) # average the data for the month this year to plot

    monthonly_avgrelease <- monthonly_avgrelease %>%
      rename_at('year(shortdate)', ~'year')

    # Make dam into factor for plotting
    monthonly_avgrelease$Dam = factor(
      monthonly_avgrelease$Dam,
      levels = c('Hoover', 'Davis', 'Parker')
    )

    # Create a symbol column
    monthonly_avgrelease$symbol <- ifelse(
      monthonly_avgrelease$year == as.numeric(year(as.Date(n))),
      'open',
      'closed'
    )

    # Save the plots for each individual dam
    for (name in unique(monthonly_avgrelease$Dam)) {
      dam_data <- filter(monthonly_avgrelease, Dam == name)

      # Calculate the y-axis limits
      min_val <- floor(min(dam_data$value) / 1000) * 1000 - 1000
      max_val <- ceiling(max(dam_data$value) / 1000) * 1000 + 1000

      # Generate the plot with dynamic y-axis
      ggplot(
        filter(monthonly_avgrelease, Dam == name),
        aes(x = year, y = value)
      ) +
        geom_line(color = "#003E51", size = 1.5) +
        geom_point(
          color = "#003E51",
          size = 2.5,
          shape = 21,
          stroke = 2,
          aes(fill = as.factor(symbol))
        ) +
        scale_fill_manual(values = c("#003E51", 'white')) +

        # --- Set limits and breaks for the y-axis ---
        scale_y_continuous(
          limits = c(min_val, max_val),
          breaks = seq(min_val, max_val, 1000),
          labels = label_comma(),
          expand = c(0, 0)
        ) +

        scale_x_continuous(
          breaks = seq(
            min(monthonly_avgrelease$year),
            max(monthonly_avgrelease$year),
            1
          ),
          expand = c(.025, .025)
        ) +
        ylab("Power Release (cfs)") +
        xlab("Year") +
        theme_bw(base_size = 8) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.grid.minor.x = element_blank(),
          legend.position = "none",
          axis.title.y = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 8, face = "bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )

      ggsave(
        filename = paste0(
          name,
          "AverageReleasesin",
          month.name[month(as.Date(n))],
          ".png"
        ),
        width = 6.25,
        height = 3.1
      )
    }

    # Get averages ready to write to the text file
    historicmean = filter(monthonly_avgrelease, year != year(as.Date(n))) %>%
      group_by(Dam) %>%
      summarise(value = round(mean(value), -2))
    historicmean$year = paste0("2008-", as.character(year(as.Date(n)) - 1))

    thislastyear = filter(
      monthonly_avgrelease,
      year >= year(as.Date(n)) - 1
    ) %>%
      select(-"symbol")
    thislastyear$value = round(thislastyear$value, -2)
    thislastyear$year = as.character(thislastyear$year)
    allhist <- rbind(thislastyear, historicmean)

    # Write data to the text file
    cat(
      paste0("Average releases for ", month.name[month(as.Date(n))], ":"),
      file = avgs,
      append = TRUE,
      sep = "\n"
    )
    write.table(
      allhist[order(allhist$Dam, allhist$year), ],
      file = avgs,
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE
    )
  }
}

### Get elevation/release data ready for plotting ###

# Create points in the observed data frame to make no gap in the line
temp <- filter(
  elevation,
  shortdate == min(filter(elevation, Type == 'Modeled')$shortdate) |
    shortdate == min(filter(elevation, Type == 'Modeled')$shortdate) + 1
)
temp$Type <- "Observed"
elcomb <- rbind(elevation, temp)
elcomb$Type = factor(elcomb$Type, levels = c('Observed', 'Modeled'))
elcomb$datatype <- 'Elevation'

temp <- filter(
  releases,
  shortdate == min(filter(releases, Type == 'Modeled')$shortdate) |
    shortdate == min(filter(releases, Type == 'Modeled')$shortdate) + 1
)
temp$Type <- "Observed"
relcomb <- rbind(releases, temp)
relcomb$Type = factor(relcomb$Type, levels = c('Observed', 'Modeled'))
relcomb$datatype <- 'Releases'

comb <- bind_rows(elcomb, relcomb)
targets$Type = 'Target'

### Make release plots for each dam ###
if (plot_Laughlin_charts) {
  for (dam in unique(na.omit(comb$Dam))) {
    p <- ggplot(
      data = filter(comb, Dam == dam, datatype == "Releases"),
      mapping = aes(x = shortdate, y = value, color = Type)
    ) +
      geom_line(linewidth = 2, lineend = "round") +
      scale_x_date(
        limits = c(
          startplot, # could use start_firstmonth instead
          as.Date(endmonth_2)
        ),
        date_breaks = "week",
        date_minor_breaks = "days",
        # date_labels="%m/%d",
        labels = function(x) {
          # For each date break (x), get the month and day number and paste them
          paste(month(x), day(x), sep = "/")
        },
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(labels = function(x) {
        format(x, big.mark = ",")
      }) +
      labs(
        x = "",
        y = "Power Release (cfs)",
        title = paste0(dam, " Dam Releases")
      ) +
      ylim(
        min(filter(comb, shortdate >= startplot, Dam == dam)$value) - 2000,
        max(filter(comb, shortdate >= startplot, Dam == dam)$value) + 2000
      ) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.position = 'bottom'
      ) +
      scale_color_manual(
        values = c('#4472C4', '#ED7D31'),
        labels = c(
          paste0('Historical ', dam, ' Releases'),
          paste0('Projected ', dam, ' Releases')
        )
      )

    ggsave(
      p,
      filename = paste0(dam, '_releases.png'),
      width = 5.7,
      height = 4.1
    )
  }

  ### Make elevation plots for each reservoir ###

  for (reservoir in unique(na.omit(comb$Reservoir))) {
    p <- ggplot(
      data = filter(comb, Reservoir == reservoir, datatype == "Elevation"),
      mapping = aes(x = shortdate, y = value, color = Type)
    ) +
      geom_line(linewidth = 2, lineend = "round") +
      scale_x_date(
        limits = c(
          startplot, # could use start_firstmonth instead
          as.Date(endmonth_2)
        ),
        date_breaks = "week",
        date_minor_breaks = "days",
        # date_labels="%m/%d",
        labels = function(x) {
          # For each date break (x), get the month and day number and paste them
          paste(month(x), day(x), sep = "/")
        },
        expand = c(0.01, 0.01)
      ) +
      scale_y_continuous(
        labels = function(x) {
          format(x, big.mark = ",")
        }
      ) +
      ylim(
        min(c(
          filter(comb, shortdate >= startplot, Reservoir == reservoir)$value,
          filter(targets, Reservoir == reservoir)$value
        )) -
          0.5,
        max(c(
          filter(comb, shortdate >= startplot, Reservoir == reservoir)$value,
          filter(targets, Reservoir == reservoir)$value
        )) +
          0.5
      ) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.position = 'bottom'
      ) +
      labs(
        x = "",
        y = "Surface Water Elevation (feet)",
        title = paste0("Lake ", reservoir, " Elevation")
      )

    if (reservoir == 'Mead') {
      p <- p +
        scale_color_manual(
          values = c('#4472C4', '#ED7D31'),
          labels = c(
            paste0('Historical ', reservoir, ' Elevation'),
            paste0('Projected ', reservoir, ' Elevation')
          )
        )
      ggsave(
        p,
        filename = paste0(reservoir, '_elevation.png'),
        width = 5.7,
        height = 4.1
      )
    } else {
      p <- p +
        geom_point(
          data = filter(targets, Reservoir == reservoir),
          mapping = aes(x = shortdate, y = value, color = Type),
          size = 2.75
        ) +
        ggrepel::geom_label_repel(
          data = filter(targets, Reservoir == reservoir),
          mapping = aes(
            x = shortdate,
            y = value,
            label = sprintf("%0.2f", value)
          ),
          # box.padding = unit(0.2, "lines"),
          # point.padding = unit(0.8, "lines"),
          color = 'black',
          size = 3
        ) +
        # # Add the `fill` argument with a semi-transparent color
        # fill = rgb(1, 1, 1, alpha = 0.4)) + # White (R=1,G=1,B=1) with alpha=0.7) +
        scale_color_manual(
          values = c('#4472C4', '#ED7D31', '#FF0000'),
          labels = c(
            paste0('Projected\n', reservoir, ' Elevation'),
            paste0('Historical\n', reservoir, ' Elevation'),
            paste0(reservoir, ' Target\nElevation')
          ),
          guide = guide_legend(
            override.aes = list(
              linetype = c("solid", "solid", "blank"),
              shape = c(NA, NA, 16)
            )
          )
        )
      ggsave(
        p,
        filename = paste0(reservoir, '_elevation.png'),
        width = 5.7,
        height = 4.1
      )
    }
  }
}

### NPS Elevation Plots ###
if (plot_NPS_charts) {
  maxelevs <- list(
    "Mead" = "Maximum Elevation = 1,229 ft",
    "Mohave" = "Maximum Elevation = 647 ft",
    "Havasu" = "Maximum Elevation = 450 ft"
  )
  for (reservoir in unique(na.omit(comb$Reservoir))) {
    # Define y-axis limits before plotting
    y_min <- min(
      c(
        filter(
          comb,
          shortdate >= start_firstmonth,
          Reservoir == reservoir
        )$value,
        filter(targets, Reservoir == reservoir)$value
      )
    ) -
      2

    y_max <- max(
      c(
        filter(
          comb,
          shortdate >= start_firstmonth,
          Reservoir == reservoir
        )$value,
        filter(targets, Reservoir == reservoir)$value
      )
    ) +
      2

    y_max = ifelse(reservoir == "Havasu", 450, y_max)

    p <- ggplot(
      data = filter(comb, Reservoir == reservoir, datatype == "Elevation"),
      mapping = aes(x = shortdate, y = value, fill = Type)
    ) +
      geom_ribbon(aes(
        ymin = y_min,
        ymax = value,
        xmin = as.Date(start_firstmonth),
        xmax = as.Date(endmonth_2)
      )) +
      geom_line(size = 0.2) +
      scale_x_date(
        limits = c(
          startplot,
          as.Date(endmonth_2)
        ),
        date_breaks = "1 week",
        date_minor_breaks = "days",
        date_labels = "%m/%d",
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        limits = c(y_min, y_max),
        expand = c(0, 0),
        labels = function(x) {
          format(x, big.mark = ",")
        }
      ) +
      labs(
        x = ifelse(
          month(as.Date(endmonth_2)) == 1,
          paste0(year(as.Date(endmonth_1)), '/', year(as.Date(endmonth_2))),
          year(as.Date(endmonth_2))
        ),
        y = "Elevation (ft)",
        title = paste0("Lake ", reservoir, " End of Day Elevation"),
        subtitle = maxelevs[reservoir]
      ) +
      theme_bw(base_size = 6) +
      theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = 'none'
      )

    p <- p + scale_fill_manual(values = c('#1F497D', '#B7DEE8'))
    ggsave(
      p,
      filename = paste0(reservoir, '_filledelevation_NPS.png'),
      width = 4,
      height = 2.1
    )
  }

  ############## NPS - Mead EOM Elevation Projections Chart ##################
  month_heading = month.name[month(ym(run_date))]
  # add year to subheading
  month_heading = paste(month_heading, year(ym(run_date)))

  lab_names <- c(
    paste0(month_heading, " Probable 24-Month Study"),
    "Historical"
  )

  subtitleb = paste0(
    'Projections from ',
    month_heading,
    ' 24-Month Study Most Probable Inflow Scenario'
  )

  sdis <- c("Mead.Pool Elevation" = 1930)
  slots <- names(sdis)
  mrid_to_trace <- c("24MS Most")
  names(mrid_to_trace) <- c(most_mrid)

  hist_nMons = 12 # keep 7 months before start date
  end_date = format(ym(run_date) + months(23), "%Y-%m")
  start_date = format(ym(run_date) - months(1), "%Y-%m")
  histStart_date = format(ym(run_date) - months(hist_nMons), "%Y-%m")
  textStart_date = format(ym(run_date) - months(4), "%Y-%m")

  ## Read 24-MS data from hdb - no vpn needed
  df_hdb <- get_hdb_data(
    sdi = sdis["Mead.Pool Elevation"],
    tstp = "MN",
    svr = "lchdb",
    t1 = run_date,
    t2 = end_date,
    table = "M",
    mrid = most_mrid
  )

  df_hdb <- df_hdb %>%
    mutate(
      slot = names(sdis)[match(sdi, sdis)],
      run = run_date,
      Trace = mrid_to_trace[as.character(mrid)]
    ) %>%
    rename(Date = time_step) %>%
    mutate(Date = as.yearmon(Date)) %>%
    select(-sdi, -mrid)

  ## Read historical data from hdb
  df_hist <- get_hdb_data(
    sdi = sdis["Mead.Pool Elevation"],
    tstp = "MN",
    svr = "lchdb",
    t1 = histStart_date,
    t2 = run_date
  )

  ## Reorg histrical data
  df_hist <- df_hist %>%
    mutate(
      slot = names(sdis)[match(sdi, sdis)],
      run = run_date,
      Trace = 'Historical'
    ) %>%
    rename(Date = time_step) %>%
    mutate(Date = as.yearmon(Date)) %>%
    select(-sdi, -mrid) %>%
    na.omit()

  ## Add historical data to 24MS df
  df_hdb = rbind(df_hdb, df_hist)

  ## Connect historical data and initial conditions
  df_init = df_hdb %>%
    filter(Date == run_date) %>%
    select(-value, -Date) %>%
    distinct()
  df_hist2 = df_hist %>%
    filter(Date == max(Date)) %>%
    select(slot, Date, value)
  df_initAdd = left_join(df_init, df_hist2, by = 'slot')
  df_24MS <- rbind(df_hdb, df_initAdd)

  names(lab_names) <- c("24MS Most", "Historical")

  nn <- lab_names[1:2]

  df_24MS_m = df_24MS %>%
    filter(slot == 'Mead.Pool Elevation') %>%
    mutate(trace_labels = lab_names[Trace])

  ## Min, Max, Most, ESP is order of these colors, size, linetype
  custom_colors <- c('#26AE44', 'grey20') #, 'grey43')
  custom_size <- c(1, 1)
  custom_lt <- c(1, 1)
  custom_alpha <- c(rep(1, 2))
  names(custom_colors) <- names(custom_size) <- names(custom_lt) <-
    names(custom_alpha) <- nn

  #-------------------- NPS-24MS-MOST -------------------------------------------
  m_breaks <- seq(1000, 1250, 25)
  m_breaks2 <- seq(1000, 1250, 5)
  yy <- c(1000, 1125) # NULL for default ylimit
  alpha_1 = 0.8

  df_24ms_most = filter(
    df_24MS_m,
    (Trace == "24MS Most" | Trace == "Historical")
  )

  last_date <- max(df_24ms_most$Date)

  # Create a new column that is -0.022 for the last date and 0 for all others
  df_24ms_most$nudge_amount <- ifelse(df_24ms_most$Date == last_date, -0.022, 0)

  ## Function to get Powell and Mead Storage from Elevation
  res <- c("mead", "powell") # reservoirs

  getData <- function(res) {
    tmp <- read.csv(file.path("data", paste0(res, "ElevationVolume.csv")))
    tmp
  }

  evTables <- lapply(res, getData)
  names(evTables) <- tolower(res)

  elevation_to_storage <- function(elevation, reservoir) {
    # evTables are system data for this package
    e2vFunc <- stats::approxfun(
      evTables[[reservoir]][, 1],
      evTables[[reservoir]][, 2]
    )

    e2vFunc(elevation)
  }

  gg <-
    ggplot(df_24ms_most, aes(x = Date)) +

    geom_ribbon(
      aes(x = Date, ymax = 1076.5, ymin = 1076),
      fill = "#cccccc",
      alpha = alpha_1
    ) +
    geom_ribbon(
      aes(x = Date, ymax = 1076, ymin = 1073),
      fill = "#f1e2cc",
      alpha = alpha_1
    ) +
    geom_ribbon(
      aes(x = Date, ymax = 1073, ymin = 1070),
      fill = "#fff2ae",
      alpha = alpha_1
    ) +
    geom_ribbon(
      aes(x = Date, ymax = 1070, ymin = 1060),
      fill = "#e6f5c9",
      alpha = alpha_1
    ) +
    geom_ribbon(
      aes(x = Date, ymax = 1060, ymin = 1050),
      fill = "#cbd5e8",
      alpha = alpha_1
    ) +
    geom_ribbon(
      aes(x = Date, ymax = 1050, ymin = 1045),
      fill = "#fdcdac",
      alpha = alpha_1
    ) +
    geom_ribbon(
      aes(x = Date, ymax = 1045, ymin = 1035),
      fill = "#b3e2cd",
      alpha = alpha_1
    ) +
    geom_ribbon(
      aes(x = Date, ymax = 1035, ymin = 1000),
      fill = "#cccccc",
      alpha = alpha_1
    ) +
    geom_ribbon(
      aes(x = Date, ymax = 1000, ymin = 950),
      fill = "#ffb6c1",
      alpha = alpha_1
    ) +
    geom_vline(
      xintercept = as.yearmon(c("Dec 2025", "Dec 2026")),
      size = 1,
      color = "#ffdc70", #"#ffdc70" or "grey45"
      alpha = 0.8
    ) +
    annotate(
      "text",
      x = as.yearmon(ym(histStart_date) + months(1)),
      y = c(
        1078.2,
        1074.6,
        1071.6,
        1066,
        1057,
        1054,
        1049.1,
        995,
        1046.6,
        1040,
        1020.6,
        1017.7
      ),
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
      angle = 00,
      size = 2.5,
      hjust = 0
    ) +
    geom_line(
      data = df_24ms_most,
      aes(
        x = Date,
        y = value,
        color = trace_labels,
        alpha = trace_labels,
        group = Trace,
        linetype = trace_labels,
        size = trace_labels
      )
    ) +
    geom_point(
      data = df_24ms_most,
      aes(
        x = Date,
        y = value,
        color = trace_labels,
        alpha = trace_labels,
        group = Trace
      ),
      size = 2.5
    ) +

    scale_color_manual(
      values = custom_colors,
      breaks = unique(df_24ms_most$trace_labels)
    ) +
    scale_linetype_manual(
      values = custom_lt,
      breaks = unique(df_24ms_most$trace_labels)
    ) +
    scale_size_manual(
      values = custom_size,
      breaks = unique(df_24ms_most$trace_labels)
    ) +
    scale_alpha_manual(
      values = custom_alpha,
      breaks = unique(df_24ms_most$trace_labels)
    ) +
    scale_x_yearmon(
      expand = c(0, 0),
      breaks = unique(df_24MS$Date),
      minor_breaks = unique(df_24MS$Date),
      limits = c(min(df_24MS$Date), max(df_24MS$Date))
    ) +
    scale_y_continuous(
      labels = scales::comma,
      breaks = m_breaks,
      minor_breaks = m_breaks2,
      limits = yy,
      expand = c(0, 0),
      sec.axis = sec_axis(
        ~ elevation_to_storage(., "mead"),
        breaks = elevation_to_storage(m_breaks, "mead"),
        labels = scales::comma_format(scale = 1 / 1000000, accuracy = 0.01),
        name = "Storage (maf)"
      )
    ) +
    labs(
      y = "Pool Elevation (ft)",
      x = NULL,
      color = NULL,
      linetype = NULL,
      size = NULL,
      fill = NULL,
      title = bquote('Lake Mead End-of-Month' ~ Elevations),
      subtitle = subtitleb
    ) +
    #caption = bquote(' '^1~'Projected Lake Mead end-of-month elevations from the latest 24-Month Study inflow scenario.                  ') add this if we need footnote

    geom_vline(
      xintercept = as.yearmon(start_date), #as.yearmon(c("Dec 2023", "Dec 2024")),
      size = 1,
      color = "grey20", #"#ffdc70" or "grey45"
      alpha = 0.8
    ) +

    annotate(
      "text",
      x = as.yearmon(textStart_date),
      y = 1100,
      hjust = 0,
      label = "Historical",
      size = 4.0,
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = as.yearmon(run_date),
      y = 1100,
      hjust = 0,
      label = "Future",
      size = 4.0,
      fontface = "bold"
    ) +
    geom_text(
      aes(
        y = value,
        label = ifelse(
          Date > start_date,
          format(round(value, digits = 2), nsmall = 2),
          ""
        )
      ),
      angle = 90,
      nudge_y = 10,
      nudge_x = df_24ms_most$nudge_amount,
      size = 4.0
    ) +

    theme_bw(base_size = 14) +
    guides(
      alpha = 'none',
      color = guide_legend(nrow = 2, order = 1),
      linetype = guide_legend(nrow = 2, order = 1),
      size = guide_legend(nrow = 2, order = 1),
      fill = guide_legend(order = 2)
    ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      # panel.grid.major.y = element_blank() ,
      legend.position = "bottom",
      legend.key.width = unit(1.2, "cm"),
      plot.margin = unit(c(0.1, 0.1, 1, 0.1), "cm"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      plot.caption = element_text(hjust = 0, size = 10, face = "italic")
    )

  ggsave("NPS-24MS-MOST.png", width = 11, height = 8)

  crmms_m <- image_read("NPS-24MS-MOST.png")
  logo_raw <- image_read("data/BofR-vert.png")
  test_plot <- image_composite(
    crmms_m,
    image_resize(logo_raw, "325"),
    offset = "+2860+2060"
  )
  image_write(test_plot, paste0(run_date, "-NPS-24MS-MOST.png"))
}
