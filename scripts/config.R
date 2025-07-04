#### MANUAL SET UP SECTION ###

# Set these to dates that match the two elevation targets that we want to show 
# in the release/pool elevation plots. Use the format "YYYY-mm-dd"
endmonth_1 = "2025-06-30"
endmonth_2 = "2025-07-31"

# Set to the Lake Mohave targets matching endmonth_1 and endmonth_2 (two decimals)
mohave_elev_1 = 643.00
mohave_elev_2 = 642.50

# Set to the Lake Havasu targets matching endmonth_1 and endmonth_2 (two decimals)
havasu_elev_1 = 448.50
havasu_elev_2 = 448.00


# Set to 0 to show average release plots for the current month and next month,
# 1 to show average release plots for the next month
# and month after that (the month following endmonth_2 
# - cannot be past the end of the daily model)
monthflag = 0

## For NPS Mead EOM Elevation Projections Chart ##
run_date = c('2025-06')
most_mrid <- 3285 