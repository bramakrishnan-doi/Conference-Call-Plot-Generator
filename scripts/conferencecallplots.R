# This R code uses the rhdb package to pull data from the public API
# Plots for conference calls are generated including: 
# Mohave/Havasu elevation, Davis Releases, and 2008-YYYY average release
# for the current month for Davis/Parker

### Everything below here is automatic ###
# Load the required packages
library(ggplot2)
library(tidyverse)
library(httr2)

source("config.R")

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

get_hdb_data <- function(sdi,
                         svr = "lchdb",
                         tstp = "HR",
                         t1 = -24,
                         t2 = 0,
                         table = "R",
                         mrid = 0) {
  
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
          time_step = x |> pluck('Data') |>
            map('t') |>
            unlist() |>
            parse_date_time2("%m/%d/%Y %I:%M:%S %p", tz = "MST"),
          mrid = x |> pluck('MRID', .default = NA_real_),
          value = x |> pluck('Data') |>
            map('v') |>
            unlist() |>
            as.numeric(),
          units = x |> pluck('DataTypeUnit')
        )
      }
    )
}


# Build all the elevation targets into a single data frame
targets <- data.frame (
  shortdate = c(as.Date(endmonth_1), 
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
startplot <- as.Date(paste0(format(as.Date(endmonth_1), "%Y-%m"), 
                            "-", 
                            ifelse(as.numeric(currentdaysplit[3]) > 10, 01, 15)
                            )
                     )

# Query daily releases for Hoover, Davis, and Parker 2008-yesterday
hist_releases <- get_hdb_data(sdi = c(1874,2096,2097),
                              svr="lchdb", 
                              tstp="DY", 
                              t1 = start_date, 
                              t2 = yesterday )


# Query modeled releases from today through the end of the second month 
# for Hoover, Davis, and Parker
# The 4 is to pull modeled data from MRID 4
model_releases <- get_hdb_data(sdi = c(1863,2166,2146),
                              svr="lchdb", 
                              tstp="DY", 
                              t1 = currentday, 
                              t2 = enddate,
                              table = "M",
                              mrid = 4)

# Query observed daily elevations from this year for Mead, Mohave, and Havasu
hist_elevation <- get_hdb_data(sdi = c(1930,2100,2101),
                              svr="lchdb", 
                              tstp="DY", 
                              t1 = start_firstmonth, 
                              t2 = yesterday )

# Query modeled daily elevations for from today through the end of 
# the second month for Hoover, Davis, and Parker
model_elevation <- get_hdb_data(sdi = c(1930,2100,2101),
                               svr="lchdb", 
                               tstp="DY", 
                               t1 = currentday, 
                               t2 = enddate,
                               table = "M",
                               mrid = 4)

# Use the function to format dates in historical and current data
hist_releases <- hist_releases %>% 
  mutate(month = month(time_step),
         shortdate = as_date(time_step),
         Dam = recode(sdi,
                      '1874' = 'Hoover', 
                      '2096' = 'Davis',
                      '2097' = 'Parker'),
         Type = "Observed")
                                           

model_releases <- model_releases %>% 
  mutate(month = month(time_step),
         shortdate = as_date(time_step),
         Dam = recode(sdi,
                      '1863' = 'Hoover', 
                      '2166' = 'Davis',
                      '2146' = 'Parker'),
         Type = "Modeled")
                                             

hist_elevation <- hist_elevation %>% 
  mutate(month = month(time_step),
         shortdate = as_date(time_step),
         Reservoir = recode(sdi,
                            '1930' = 'Mead', 
                            '2100' = 'Mohave',
                            '2101' = 'Havasu'),
         Type = "Observed")
                                            
                                                               

model_elevation <- model_elevation %>% 
  mutate(month = month(time_step),
         shortdate = as_date(time_step),
         Reservoir = recode(sdi,
                            '1930' = 'Mead', 
                            '2100' = 'Mohave',
                            '2101' = 'Havasu'),
         Type = "Modeled") 


# Combine the observed and modeled data into a single data frame
releases <- rbind(hist_releases, model_releases) 
elevation <- rbind(hist_elevation, model_elevation)

### Make the average yearly release figure x 2 ###

# # This is a function to format the yearly average plots
# format_yearlyavg <- function(ggname){
#   return(ggname + 
#            geom_line(color = "#003E51", size =2)+
#            geom_point(color="#003E51", size = 2, shape = 21, stroke = 2, aes(fill = as.factor(symbol)))+
#            scale_fill_manual(values = c("#003E51", 'white'))+
#            scale_y_continuous(breaks=seq(6000,20000,2000), limits=c(6000,20000))+
#            scale_x_continuous(breaks = seq(min(monthonly_avgrelease$year),max(monthonly_avgrelease$year),1), expand = c(.025, .025))+
#            ylab("Power Release (cfs)")+
#            xlab("Year")+
#            theme_bw(base_size = 8)+
#            theme(plot.title = element_text(hjust = 0.5))+
#            theme(panel.grid.minor.x = element_blank(), legend.position = "none")+
#            theme(plot.title = element_text(size=14, face = "bold"), 
#                  axis.title.y = element_text(size = 8, face = "bold"),
#                  axis.title.x = element_text(size = 8, face = "bold"),
#                  axis.text.x = element_text(angle = 90)))
# }
# 
# # Create the text file to write the powerpoint data to 
# file.create('AveragesforPptSlides.txt')
# avgs <- 'AveragesforPptSlides.txt'
# 
# for (n in c(endmonth_2, enddate)){
#   
#   # Filter release data to keep just the month we want to make the plots for
#   monthonly_releases <- filter(releases, month == month(as.Date(n)))
# 
#   # Average the release by month and rename the year column for plotting
#   monthonly_avgrelease <- monthonly_releases %>% 
#                           group_by(Dam, year(shortdate)) %>% 
#                           summarise(value = mean(value)) # average the data for the month this year to plot
#   
#   monthonly_avgrelease <- monthonly_avgrelease %>% rename_at('year(shortdate)', ~'year')
#   
#   # Make dam into factor for plotting
#   monthonly_avgrelease$Dam = factor(monthonly_avgrelease$Dam, levels = c('Hoover', 'Davis', 'Parker'))
#   
#   # Create a symbol column
#   monthonly_avgrelease$symbol <- ifelse(monthonly_avgrelease$year == as.numeric(year(as.Date(n))), 'open', 'closed')
#   
#   # Save the plot for the three dams combined
#   ggsave(format_yearlyavg(ggplot(monthonly_avgrelease, aes(x=year, y=value)))+
#            facet_grid(~Dam) + 
#            ggtitle(paste0("Average Releases in ", month.name[month(as.Date(n))])),
#          filename = paste0("Combined_AverageReleasesin", month.name[month(as.Date(n))],".png"),
#          width = 10,
#          height = 6)
#   
#   # Save the plots for each individual dam
#   for (name in unique(monthonly_avgrelease$Dam)) {
#     ggsave(format_yearlyavg(ggplot(filter(monthonly_avgrelease, Dam == name), aes(x=year, y=value)))+
#              ggtitle(paste0("Average ", name, " Releases in ", month.name[month(as.Date(n))])),
#            filename = paste0(name, "AverageReleasesin", month.name[month(as.Date(n))],".png"),
#            width = 5.25,
#            height = 3.1)
#   }
#   
#   # Get averages ready to write to the text file
#   historicmean = filter(monthonly_avgrelease, year != year(as.Date(n))) %>% 
#                  group_by(Dam) %>% 
#                  summarise(value = round(mean(value), -2)) 
#   historicmean$year = paste0("2008-", as.character(year(as.Date(n))-1))
#   
#   thislastyear = filter(monthonly_avgrelease, year >= year(as.Date(n)) -1) %>% 
#                  select(-"symbol")
#   thislastyear$value = round(thislastyear$value, -2)
#   thislastyear$year = as.character(thislastyear$year)
#   allhist <- rbind(thislastyear, historicmean)
#   
#   # Write data to the text file
#   cat(paste0("Average releases for ", month.name[month(as.Date(n))], ":"), file = avgs, append = TRUE, sep = "\n")
#   write.table(allhist[order(allhist$Dam, allhist$year), ], file = avgs, append = TRUE, row.names = FALSE, col.names = FALSE)
# }


### Get elevation/release data ready for plotting ###

# Create points in the observed data frame to make no gap in the line
temp <- filter(elevation, shortdate == min(filter(elevation, Type == 'Modeled')$shortdate) |
                 shortdate == min(filter(elevation, Type == 'Modeled')$shortdate)+ 1)
temp$Type <- "Observed"
elcomb <- rbind(elevation, temp)
elcomb$Type= factor(elcomb$Type, levels = c('Observed', 'Modeled'))
elcomb$datatype <- 'Elevation'

temp <- filter(releases, shortdate == min(filter(releases, Type == 'Modeled')$shortdate) | 
                 shortdate == min(filter(releases, Type == 'Modeled')$shortdate) +1 )
temp$Type <- "Observed"
relcomb <- rbind(releases, temp)
relcomb$Type= factor(relcomb$Type, levels = c('Observed', 'Modeled'))
relcomb$datatype <- 'Releases'

comb <- bind_rows(elcomb, relcomb)
targets$Type = 'Target'

### Make release plots for each dam ###

for (dam in unique(na.omit(comb$Dam))){
  p <- ggplot(data = filter(comb, Dam == dam, datatype == "Releases"), 
              mapping = aes(x=shortdate, y = value, color = Type))+
    geom_line(size = 2, lineend = "round")+
    scale_x_date(limits=c(startplot, # could use start_firstmonth instead
                          as.Date(endmonth_2)), 
                 date_breaks = "week", 
                 date_minor_breaks = "days", 
                 # date_labels="%m/%d", 
                 labels = function(x) {
                   # For each date break (x), get the month and day number and paste them
                   paste(month(x), day(x), sep = "/")
                 },
                 expand = c(0.01,0.01)) +
    scale_y_continuous(labels = scales::label_comma()) +
    labs(x="", y = "Power Release (cfs)", title = paste0(dam, " Dam Releases"))+
    ylim(min(filter(comb, shortdate >= startplot, Dam == dam)$value) - 2000, 
         max(filter(comb, shortdate >= startplot, Dam == dam)$value) + 2000)+
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(size=14, face = "bold", hjust = 0.5), 
          axis.title.y = element_text(face = "bold", size = 10),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          legend.title = element_blank(),
          legend.position = 'bottom')+
    scale_color_manual(values = c('#4472C4','#ED7D31'),
                       labels = c(paste0('Historical ', dam,' Releases'), 
                                  paste0('Projected ', dam,' Releases')))
    
  ggsave(p, filename = paste0(dam, '_releases.png'), width = 5.7, height = 4.1)
}

### Make elevation plots for each reservoir ###

for (reservoir in unique(na.omit(comb$Reservoir))){
  p <- ggplot(data = filter(comb, Reservoir == reservoir, datatype == "Elevation"), mapping = aes(x=shortdate, y = value, color = Type))+
    geom_line(size = 2, lineend = "round")+
    scale_x_date(limits=c(startplot, # could use start_firstmonth instead
                          as.Date(endmonth_2)), 
                 date_breaks = "week", 
                 date_minor_breaks = "days", 
                 # date_labels="%m/%d", 
                 labels = function(x) {
                   # For each date break (x), get the month and day number and paste them
                   paste(month(x), day(x), sep = "/")
                 }, 
                 expand = c(0.01,0.01)) +
    scale_y_continuous(labels = scales::label_comma()) +
    ylim(min(c(filter(comb, shortdate >= startplot, Reservoir == reservoir)$value, filter(targets, Reservoir == reservoir)$value)) - 0.5, 
         max(c(filter(comb, shortdate >= startplot, Reservoir == reservoir)$value, filter(targets, Reservoir == reservoir)$value)) + 0.5)+
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(size=14, face = "bold", hjust = 0.5), 
          axis.title.y = element_text(face = "bold", size = 10),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          legend.title = element_blank(),
          legend.position = 'bottom') +
    labs(x="", y = "Surface Water Elevation (feet)", title = paste0("Lake ", reservoir, " Elevation"))
  
  if (reservoir == 'Mead'){
    p <- p + scale_color_manual(values = c('#4472C4','#ED7D31'),
                                labels = c(paste0('Historical ', reservoir,' Elevation'), 
                                           paste0('Projected ', reservoir,' Elevation')))
    ggsave(p, filename = paste0(reservoir, '_elevation.png'), width = 5.7, height = 4.1)

  } else {
    p <- p + geom_point(data = filter(targets, Reservoir == reservoir), 
                        mapping = aes(x=shortdate, y = value, color = Type), 
                        size = 2.75) +
             ggrepel::geom_label_repel(
               data = filter(targets, Reservoir == reservoir),
               mapping = aes(x=shortdate, 
                             y = value, 
                             label =  sprintf("%0.2f", value)),
               # box.padding = unit(0.2, "lines"),
               # point.padding = unit(0.8, "lines"),
               color = 'black',
               size = 3) +
               # # Add the `fill` argument with a semi-transparent color
               # fill = rgb(1, 1, 1, alpha = 0.4)) + # White (R=1,G=1,B=1) with alpha=0.7) + 
            scale_color_manual(values = c('#4472C4','#ED7D31','#FF0000'),
                         labels = c(paste0('Projected\n', reservoir,' Elevation'),
                                    paste0('Historical\n', reservoir,' Elevation'), 
                                    paste0(reservoir, ' Target\nElevation')),
                         guide = guide_legend(override.aes = list(
                           linetype = c("solid", "solid", "blank"),
                           shape = c(NA, NA, 16)
                         )))
    ggsave(p, filename = paste0(reservoir, '_elevation.png'), width = 5.7, height = 4.1)
  }
}

### NPS Elevation Plots ###
maxelevs <- list("Mead" = "Maximum Elevation = 1,229 ft",
                 "Mohave" = "Maximum Elevation = 647 ft",
                 "Havasu" = "Maximum Elevation = 450 ft")
for (reservoir in unique(na.omit(comb$Reservoir))){
  p <- ggplot(data = filter(comb, Reservoir == reservoir, datatype == "Elevation"), mapping = aes(x=shortdate, y = value, fill = Type))+
    geom_ribbon(aes(ymin = min(c(filter(comb, shortdate >= start_firstmonth, Reservoir == reservoir)$value, filter(targets, Reservoir == reservoir)$value))-0.5, ymax = value, xmin = as.Date(start_firstmonth), xmax = as.Date(endmonth_2)))+
    geom_line(size = 0.2)+
    scale_x_date(limits=c(startplot, # could use start_firstmonth instead
                          as.Date(endmonth_2)), date_breaks = "1 week", date_minor_breaks = "days", 
                 date_labels="%m/%d", expand = c(0,0)) +
    scale_y_continuous(limits = c(min(c(filter(comb, shortdate >= start_firstmonth, Reservoir == reservoir)$value, 
                                        filter(targets, Reservoir == reservoir)$value)) - 0.5, 
                                  max(c(filter(comb, shortdate >= start_firstmonth, Reservoir == reservoir)$value, 
                                        filter(targets, Reservoir == reservoir)$value)) + 0.5), 
                       expand = c(0,0),
                       labels = scales::label_comma())+
    labs(x=ifelse(month(as.Date(endmonth_2))== 1, paste0(year(as.Date(endmonth_1)), '/', year(as.Date(endmonth_2))), year(as.Date(endmonth_2))), y = "Elevation (ft)", title = paste0("Lake ", reservoir, " End of Day Elevation"),
         subtitle = maxelevs[reservoir])+
    theme_bw(base_size = 6) +
    theme(plot.title = element_text(size=10, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 5), 
          axis.title.y = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          legend.title = element_blank(),
          legend.position = 'none') 
  
    p <- p + scale_fill_manual(values = c('#1F497D','#B7DEE8'))
    ggsave(p, filename = paste0(reservoir, '_filledelevation_NPS.png'), width = 4, height = 2.1)
  
}
