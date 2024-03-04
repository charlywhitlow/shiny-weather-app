library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(ggrepel)
library(flextable)
library(lubridate)
library(rmarkdown)


# import sites from csv
sites <- read_csv("data/csv/Sites.csv", col_types="icdd") %>%
  arrange(Site_Name)

# load site csv, process, and create RData file
load.site.csv <- function(site.id){
  site.code <- paste("Site", site.id, sep="_")
  filename <- paste("data/RData/sites/", site.code, ".RData", sep="")
  
  # import csv for selected site
  selected.site <- read_csv(paste("data/csv/", site.code, ".csv", sep="")) %>%
    
    # convert date string to datetime
    mutate(ob_time = as.POSIXct(ob_time, format="%d/%m/%Y %H:%M")) %>%
    
    # add day aggregate column - round down date to nearest day to group by day
    mutate(day.start = floor_date(ob_time, "day") ) %>%
    
    # remove invalid dates (i.e. 29-Feb-22 which doesn't exist)
    filter(!is.na(day.start)) %>%
    
    # add aggregate month column - round down date to nearest month to group by month
    mutate(month.start = floor_date(ob_time, "month")) %>%
    
    # add Site_Name column
    inner_join(sites, by=c("Site"="Site_ID")) %>% 

    # reorder columns
    select(
      "ob_time", "hour", "day", "month", "day.start", "month.start",
      "Site_ID"="Site", "Site"="Site_Name",
      "wind_speed", "air_temperature", "rltv_hum", "visibility"
    ) %>% 
    
    # remove any fully duplicated rows, i.e. all values same
    distinct()
    
  # add column counting number of NAs in weather var columns
  selected.site <- selected.site %>%
    mutate(
      numNAs = rowSums(is.na(selected.site[c("wind_speed", "air_temperature", "rltv_hum", "visibility")]))
    ) %>%
    # group by site, day, and hour, and slice out duplicate values, selecting row with min number of NAs
    group_by(
      Site_ID, day.start, hour
    ) %>%
    slice_min(
      numNAs, with_ties=FALSE # TODO: is this enough? better to merge?
    ) %>% ungroup() # remove grouping
  
  # save to Rdata file
  save(selected.site, file=filename)
  return(selected.site)
}

# load csv or Rdata file for specified site.id
load.site <- function(site.id) {
  site.code <- paste("Site", site.id, sep="_")
  filename <- paste("data/RData/sites/", site.code, ".RData", sep="")

  # load RData file if already exists
  if (file.exists(filename)){
    load(file=filename)
  }else{
    # else load csv and create RData file
    selected.site <- load.site.csv(site.id)
  }
  return(selected.site)
}

# load and merge selected site data
load.sites <- function(selected.sites.names) {
  selected.sites <- sites %>% filter(Site_Name %in% selected.sites.names) # get selected sites
  # loop through selected sites and append site.data to dataframe
  sites.data <- c()
  for (i in 1:length(selected.sites$Site_ID)) {
    data <- load.site(selected.sites$Site_ID[i])
    sites.data <- sites.data %>% bind_rows(data)
  }
  sites.data$Site_ID <- as.factor(sites.data$Site_ID) # set Site_ID to factor
  sites.data$Site <- as.factor(sites.data$Site) # set Site name to factor
  return(sites.data)
}

# calendar time plots
calendar.time.plot <- function(selected.sites.names, weather.var, summary.fun, aggregation, font.size=20){
  selected.sites.data <- load.sites(selected.sites.names) # extract sites data

  # no aggregation - select data without grouping
  if (aggregation == "Hourly (no aggregation)"){
    chart.data <- selected.sites.data %>%
      select(
        "Site",
        "Date"="ob_time",
        "summary.stat" = contains(
          case_when(
            weather.var == "Wind speed" ~ "wind_speed",
            weather.var == "Air temperature" ~ "air_temperature",
            weather.var == "Relative humidity" ~ "rltv_hum",
            weather.var == "Visibility" ~ "visibility"
          )
        )
      )
  }
  
  # add data aggregation and summarise with selected summary statistic
  else{
    # group data based on selected aggregation
    if (aggregation == "Day of week"){
      selected.sites.data <- selected.sites.data %>%
        group_by(
          Site,
          "Weekday" = wday(day.start, week_start=1) # group by weekday, Mon=1
        )
    }
    else if (aggregation == "Hour of day"){
      selected.sites.data <- selected.sites.data %>%
        group_by(
          Site,
          "Hour" = hour
        )
    }
    else if (aggregation == "Daily") {
      selected.sites.data <- selected.sites.data %>%
        group_by(
          Site,
          "Day" = day.start
        )
    }
    else if (aggregation == "Monthly") {
      selected.sites.data <- selected.sites.data %>%
        group_by(
          Site,
          "Month" = month.start
        )
    }
    # add summary stat for selected grouping
    chart.data <- selected.sites.data %>% 
      summarise(
        summary.stat = (function(f, x) f(x, na.rm=TRUE))( # create new column 'summary.stat' = selected weather variable
          summary.fun[[2]], # e.g. mean
          case_when(
            weather.var == "Wind speed" ~ wind_speed,
            weather.var == "Air temperature" ~ air_temperature,
            weather.var == "Relative humidity" ~ rltv_hum,
            weather.var == "Visibility" ~ visibility # TODO: fix Blackpool bug caused by all NAs in visibility column - https://dplyr.tidyverse.org/reference/case_when.html ?
          )
        )
      ) 
  }

  # plot
  chart.data %>%
    ggplot(
      aes(
        x = UQ(as.name(
          case_when(
            aggregation == "Hourly (no aggregation)" ~ "Date",
            aggregation == "Daily" ~ "Day",
            aggregation == "Monthly" ~ "Month"
          )
        )),
        y = summary.stat,
        colour = Site
      )) +
    ylab(weather.var) +
    ggtitle(
      ifelse(
        aggregation == "Hourly (no aggregation)", 
        paste(weather.var, " (hourly - no aggregation)", sep=""), # no summary stat text if no aggegration
        paste(weather.var, " (", aggregation, " ", summary.fun[[1]], ")", sep="")
      )
    ) +
    theme(text=element_text(size=font.size)) + # increase text size
    geom_line()
}

# plot by day of the week (valid for hourly or daily aggregation, with summary stats for daily only)
day.of.week.plot <- function(selected.sites.names, weather.var, summary.fun, aggregation, font.size=20){
  selected.sites.data <- load.sites(selected.sites.names) # load data for selected sites
  
  if (aggregation == "Hourly (no aggregation)"){
    data <- selected.sites.data %>% 
      mutate(
        "Weekday" = weekdays(day.start, abbreviate=TRUE),
      ) %>%
      select(
        "Site",
        "Weekday",
        "Date"="ob_time",
        "summary.stat" = contains(
          case_when(
            weather.var == "Wind speed" ~ "wind_speed",
            weather.var == "Air temperature" ~ "air_temperature",
            weather.var == "Relative humidity" ~ "rltv_hum",
            weather.var == "Visibility" ~ "visibility"
          )
        )
      )
  }
  # daily aggregation
  else if (aggregation == "Daily"){
    data <- selected.sites.data %>% 
      group_by(
        Site,
        "Weekday" = weekdays(day.start, abbreviate=TRUE), # Weekday (Mon=1)
        "Date" = day.start
      ) %>%
      summarise(
        # create new column 'summary.stat' = summarised selected weather variable
        summary.stat = (function(f, x) f(x, na.rm=TRUE))(
          summary.fun[[2]], # e.g. mean
          case_when(
            weather.var == "Wind speed" ~ wind_speed,
            weather.var == "Air temperature" ~ air_temperature,
            weather.var == "Relative humidity" ~ rltv_hum,
            weather.var == "Visibility" ~ visibility # TODO: fix Blackpool bug caused by all NAs in visibility column - https://dplyr.tidyverse.org/reference/case_when.html ?
          )
        )
      )
  }
  # plot
  data %>% 
    na.omit() %>% # remove NA values
    ggplot(
      aes(
        x = factor(Weekday, levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")), # order days of week
        y = summary.stat,
        colour = Site
      )) +
    xlab("Day of week") +
    ylab(weather.var) +
    ggtitle(
      # plot title text
      ifelse(
        aggregation == "Hourly (no aggregation)", 
        paste(weather.var, " (hourly - no aggregation)", sep=""), # no summary stat text if no aggregation
        paste(weather.var, " (", aggregation, " ", summary.fun[[1]], ")", sep="")
      )
    ) +
    theme(text=element_text(size=font.size)) + # increase text size
    geom_jitter(width=0.1, alpha=0.4) # add jitter to get a feel for overlapping points/sites
}

# plot by hour of day (valid for hourly data only - no aggregation)
hour.of.day.plot <- function(selected.sites.names, weather.var, font.size=20){
  selected.sites.data <- load.sites(selected.sites.names) # load data for selected sites
  data <- selected.sites.data %>% 
    select(
      "Site",
      "Hour" = hour,
      "Date"="ob_time",
      "summary.stat" = contains( # summary.stat = raw hourly value, no calculation applicable
        case_when(
          weather.var == "Wind speed" ~ "wind_speed",
          weather.var == "Air temperature" ~ "air_temperature",
          weather.var == "Relative humidity" ~ "rltv_hum",
          weather.var == "Visibility" ~ "visibility"
        )
      )
    )
  # plot
  data %>% 
    na.omit() %>% # remove NA values
    ggplot(
      aes(
        x = Hour,
        y = summary.stat,
        colour = Site
      )) +
    xlab("Hour of day") +
    ylab(weather.var) +
    ggtitle(
      paste(weather.var, " (hourly - no aggregation)", sep=""), # plot title text
    ) +
    theme(text=element_text(size=font.size)) + # increase text size
    geom_jitter(width=0.1, alpha=0.4) # add jitter to get a feel for overlapping points/sites
}

# plot empty chart when no sites selected
null.chart.plot <- function(){
  NULL %>% ggplot(
    aes(x=1:20, y=1:20)) + # dummy values to build blank canvas
    theme(text=element_text(size=20)) + # increase text size
    ggtitle("Select some options to begin") +
    ylab("Weather Variable") +
    xlab("Date")
}

# plot chart
plot.chart <- function(selected.sites.names=NULL, weather.var, summary.fun, aggregation, time.display, font.size=20){
  # add summary function text labels
  summary.fun <- case_when(
    summary.fun == "Mean" ~ c("mean", mean),
    summary.fun == "Min" ~ c("min", min),
    summary.fun == "Max" ~ c("max", max)
  )
  # create empty chart if no sites selected
  if(is.null(selected.sites.names)){
    null.chart.plot()
  }
  # day of week chart
  else if(time.display == "Day of week"){
    day.of.week.plot(selected.sites.names, weather.var, summary.fun, aggregation, font.size)
  }
  # hour of day chart
  else if(time.display == "Hour of day"){
    hour.of.day.plot(selected.sites.names, weather.var, font.size)
  }
  # calendar time plots
  else{
    calendar.time.plot(selected.sites.names, weather.var, summary.fun, aggregation, font.size)
  }
}

# map API key
config <- config::get()
STADIA_MAPS_API_KEY <- config$STADIA_MAPS_API_KEY

# UK bounding box
uk <- c(left=-15, bottom=49, right=6, top=61.5) # uk bounding box

# create map plot, highlighting selected sites
plot.map <- function(selected.sites.names=NULL, show.selected.only=FALSE){
  # get selected sites
  if (!is.null(selected.sites.names)){
    selected.sites <- sites %>% 
      filter(Site_Name %in% selected.sites.names)
  }else{
    selected.sites <- NULL
  }

  # use label alpha to show/hide selected sites without moving labels
  label.alpha <- rep(1, length(sites$Site_ID)) # default all 1
  if(show.selected.only==TRUE){
    label.alpha <- ifelse(sites$Site_ID %in% selected.sites$Site_ID, 1, 0)
  }

  # plot map
  register_stadiamaps(config$STADIA_MAPS_API_KEY)
  get_stadiamap(uk, zoom=5, maptype = "stamen_toner") %>%
    ggmap() +
    geom_point(
      data=sites, aes(x=Longitude, y=Latitude), color="red", pch=16) +
    geom_label_repel(
      data=sites,
      aes(label=Site_Name, x=Longitude, y=Latitude),
      alpha = label.alpha, # use alpha to show/hide selected
      fill = ifelse(sites$Site_ID %in% selected.sites$Site_ID, "yellow", "white"), # colour selected sites
      xlim=c(uk["left"], uk["right"]),
      ylim=c(uk["top"], uk["bottom"]),
      segment.color="red", min.segment.length = unit(0, 'lines'), # always draw lead lines for clarity
      box.padding=0.3, max.overlaps = Inf
    )
}

# build data for last 7 days summary table
seven.days.table.data <- function(selected.sites.names=NULL){
  if (is.null(selected.sites.names)) return(NULL)
  selected.sites.data <- load.sites(selected.sites.names)
  
  # last 7 dates of data
  last.7.days <- selected.sites.data %>% 
    distinct(day.start) %>% 
    arrange(desc(day.start)) %>% 
    slice(1:7)
  
  # calculate daily averages for last 7 days
  seven.day.data <- selected.sites.data %>% 
    group_by(Site, day.start) %>% 
    summarise(
      "Air temperature (C)" = mean(air_temperature, na.rm=TRUE),
      "Wind speed (kn)" = mean(wind_speed, na.rm=TRUE),
      "Relative humidity (%)" = mean(rltv_hum, na.rm=TRUE),
      "Visibility (m)" = mean(visibility, na.rm=TRUE)
    ) %>% 
      filter(
        day.start %in% last.7.days$day.start
    ) %>% 
      rename(
        "Date"="day.start"
    )
    # format dates
    seven.day.data$Date <- seven.day.data$Date %>% 
      as.Date("%Y-%m-%d") %>% 
      format("%d-%b-%y")
    
  return(seven.day.data)
}

# plot last 7 days summary table
seven.days.table <- function(selected.sites.names=NULL){
  if (is.null(selected.sites.names)) return(NULL) # return NULL if no sites selected
  seven.day.data <- seven.days.table.data(selected.sites.names) # get data
  flextable(seven.day.data) %>% # create flextable
    theme_tron() %>%
    colformat_double(big.mark=",", decimal.mark = ".", digits = 1) %>% 
    add_header_lines("Daily mean values for last 7 days at selected sites") %>%
    fontsize(size = 20, part = "header", i = 1) %>%
    merge_v("Site") %>%
    valign(j = 1, valign = 'top') %>%
    autofit()
}

# Calculate when Hutton Criteria are met for selected sites
# The Hutton Criteria occurs for a particular day when both criteria are met:
# The two previous days have a minimum temperature of at least 10 C
# The two previous days have at least six hours of relative humidity in each day of 90% or higher
hutton.criteria <- function(selected.sites.names){
    selected.sites.data <- load.sites(selected.sites.names) # get selected sites data

    # calculate hutton crtieria for selected sites
    selected.sites.data <- selected.sites.data %>% 
      group_by(
        Site, Site_ID,
        day.start
      ) %>% 
      mutate(
        # for each hourly observation, calculate cumulative number of hours which have above 90% humidity
        cum.num.hours.rltv_hum.90 = cumsum(
          rltv_hum >= 90
        )
      ) %>%
      summarise(
        # largest cum.sum value reached per day = number of hours with 90%+ humidity
        num.hours.high.humidity = max(cum.num.hours.rltv_hum.90, na.rm=TRUE),
        max.possible.hours.high.humidity = sum(is.na(rltv_hum)) + num.hours.high.humidity, # num NAs + hrs with high humidity
        
        # high humidity when 6+ hours with humidity 90%+
        hutton.high.humidity.day = ifelse(
          # if 6hrs high humidity not hit, and enough missing data that it's possible to hit 6hrs, set to NA - insufficient data
          num.hours.high.humidity < 6 & max.possible.hours.high.humidity >= 6, NA,
          # otherwise check if 6hrs+ high humidity criteria met
          num.hours.high.humidity >= 6
        ),
        
        # daily min temp
        daily.min = min(air_temperature, na.rm=TRUE),
        air_temp.obs = sum(!is.na(air_temperature)), # number of hours with temperature data
        temp.incomplete = air_temp.obs < 24 # less than 24 hourly readings in day
      ) %>%
      
      # join with tibble of all dates in range, so we can use lag function to get data for previous days
      right_join(
        tibble(day = seq(as.Date(min(selected.sites.data$day.start)), as.Date(max(selected.sites.data$day.start)), by="days")), # all days in range, in case of missing values
        by = c("day.start"="day")
      ) %>%
      mutate(
        # get temperature for previous 2 days using lag
        temp.minus.1 = lag(daily.min, 1),
        temp.minus.2 = lag(daily.min, 2),
        # check if previous 2 days contain incomplete data
        temp.minus.1.incomplete = lag(temp.incomplete, 1), # return previous 2 days using lag
        temp.minus.2.incomplete = lag(temp.incomplete, 2),
        
        # huttom temp criteria met if previous 2 days were 10+ degrees
        hutton.temp.met = ifelse(
          # if either previous days have missing hours, and min temp is higher than 10, possible that criteria is met, so insufficient data - set to NA
          (temp.minus.1>=10 & temp.minus.1.incomplete) | (temp.minus.2>=10 & temp.minus.2.incomplete), NA,
          # otherwise check if previous 2 days were 10 or higher
          temp.minus.1>=10 & temp.minus.2>=10
        ),
        
        # calculate if humidity criteria met
        hum.minus.1 = lag(hutton.high.humidity.day, 1), # return previous 2 days using lag
        hum.minus.2 = lag(hutton.high.humidity.day, 2),

        # huttom humidity criteria met if previous 2 days had more than 6 hours humidity 90%+
        hutton.humidity.met = ifelse(
          # if either previous days are NA, insufficient data - set to NA
          is.na(hum.minus.1) | is.na(hum.minus.2), NA,
          # otherwise check if previous 2 days were both high humidity days
          hum.minus.1 & hum.minus.2
        ),
      ) %>%
      mutate(
        # hutton criteria met when both criteria met
        hutton.criteria.met = hutton.humidity.met & hutton.temp.met
      ) %>%
      select(
        "Site", "Site_ID", "day.start", 
        "num.hours.high.humidity", "daily.min", 
        "hutton.humidity.met", "hutton.temp.met", "hutton.criteria.met"
      )
}

# calculate hutton criteria for all sites and save to RData file
hutton.criteria.all.sites.data <- function(){
  hutton.criteria.all.sites <- hutton.criteria(sites$Site_Name)
  save(hutton.criteria.all.sites, file="data/RData/plots/hutton-criteria-all-sites.RData")
}

# return 1 month of Hutton data for selected sites
hutton.detail.data <- function(selected.site.names, selected.month){
  # load hutton critera data for all sites
  load(file="data/RData/plots/hutton-criteria-all-sites.RData")
  
  # return data for selected sites and month
  hutton.criteria.all.sites %>%
    ungroup() %>%
    filter(Site %in% selected.site.names) %>%
    filter(month(day.start) == selected.month) %>%
    select(
      Site,
      "Date" = day.start, 
      "Humidity Criteria Met" = hutton.humidity.met, 
      "Temperature Criteria Met" = hutton.temp.met,
      "Hutton Criteria Met" = hutton.criteria.met
    )
}

# build flextable showing 1 month of days at selected site and whether Hutton Criteria met
hutton.detail.table <- function(selected.site.names, selected.month){
  hutton.detail.data(selected.site.names, selected.month) %>%
    flextable() %>%
    theme_tron() %>%
    colformat_lgl(
      true = "Yes", # logical strings
      false = "",
      na_str = "Insufficient data"
    ) %>%
    colformat_datetime(fmt_date = "%d-%m-%Y") %>% # set date format
    add_footer_lines("Days where Hutton Criteria are met indicated with 'Yes' against that day. Blank cells indicate criteria not met. Where there was insufficient data to determine Hutton criteria, this is indicated.") %>%
    fontsize(size = 20, part = "header", i = 1) %>%
    color(part="footer", color="grey") %>%
    merge_v("Site") %>%
    valign(j = 1, valign = 'top') %>%
    autofit()
}

# build summary plot comparing proportion of days Hutton Criteria met at each site in data set
build.hutton.summary.plot <- function(){
  all.sites <- sites$Site_Name
  hutton.data <- hutton.criteria(all.sites)
  
  # summarise
  hutton.data %>%
    group_by(Site) %>%
    add_count(day.start, name="num.days.in.data") %>% # total number of days in data (including NAs)
    add_count(!is.na(hutton.criteria.met), name="num.non.na.hutton.obs") %>% # number of days with non-NA hutton criteria
    summarise(
      num.days.hutton.criteria.met = sum(hutton.criteria.met, na.rm=TRUE),
      num.days.humidity.criteria.met = sum(hutton.humidity.met, na.rm=TRUE), # TODO: add humidity/temp comparison charts
      num.days.temp.criteria.met = sum(hutton.temp.met, na.rm=TRUE),
      hutton.met.prop = num.days.hutton.criteria.met / max(num.non.na.hutton.obs) # proportion of days where hutton criteria met (excluding NAs)
    ) %>%
    
    # plot chart
    ggplot(
      aes(
        x = hutton.met.prop, # plot proportion of days hutton criteria met
        y = reorder(Site, hutton.met.prop) # re-order Sites by proportion
      )) +
      geom_bar(stat = "identity", width=0.7) + # plot bar data as is
      theme(text=element_text(size=15)) + # increase text size
      ggtitle("Proportion of days Hutton Criteria met") + 
      # theme(plot.title = element_text(hjust = -2)) +
      geom_text(aes(label = scales::percent(hutton.met.prop, accuracy=0.1)), hjust = -0.2, size = 4) + # add data labels to end of bars  #, size=3.2 
      theme(
        axis.ticks.x = element_blank(), # remove x-axis ticks and numbers
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank()
      ) +
      xlab("Proportion of days Hutton Criteria met") + ylab(element_blank()) + # remove axis label
      expand_limits(x = c(0, 0.16)) +
      scale_x_continuous(expand = expansion(mult = c(0, .1))) # remove space between bars and Site labels
}

# save hutton summary plot to RData file
save.hutton.summary.plot <- function(){
  hutton.summary.plot <- hutton.summary.chart()
  save(hutton.summary.plot, file="data/RData/plots/hutton-summary-plot.RData")
}

# load and return hutton summary plot from RData file
hutton.summary.plot <- function(){
  load(file="data/RData/plots/hutton-summary-plot.RData")
  return(hutton.summary.plot)
}

# generate RData objects of plots for markdown report, using current input values
build.report.plots <- function(selected.sites, weather.var, summary.stat, aggregation, time.display){
  map.plot <- plot.map(selected.sites)
  main.plot <- plot.chart(selected.sites, weather.var, summary.stat, aggregation, time.display, font.size = 12)
  seven.day.table.data <- seven.days.table.data(selected.sites)
  # TODO: reduce font size on plots / spacing on table, etc

  # save plots to RData files
  save(map.plot, file="data/RData/plots/map-plot.RData")
  save(main.plot, file="data/RData/plots/main-plot.RData")
  save(seven.day.table.data, file="data/RData/plots/seven-day-table-data.RData")
}

# create a comma separated string
comma.separated.string <- function(vector.of.strings){
  if (length(vector.of.strings) <= 1){
    return(vector.of.strings)
  }
  # collapse 2 words with and
  else if(length(vector.of.strings) == 2){
    return(paste(selected.sites.names, collapse=" and "))
  }else{
    # append 'and' to final element then collapse
    vector.of.strings[length(vector.of.strings)] <- paste("and", vector.of.strings[length(vector.of.strings)], sep=" ")
    paste(vector.of.strings, collapse=", ")
  }
}