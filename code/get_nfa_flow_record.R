# get NFA flow

# https://www.cnrfc.noaa.gov/graphicalRVF.php?id=NFDC1

library(dataRetrieval)
library(wateRshedTools)
library(lubridate)
library(tidyverse)
library(magrittr)
library(viridis)
library(glue)

# Get NFA HOURLY -----------------------------------------------------------

# this code gets hourly data and saves it to a single file. Run once. Then update.
# nfa <- 11427000 # set gage code
# parameters <- "00060" # flow 00060, stage 00065
# dataRetrieval::whatNWISdata(siteNumber=nfa, service="uv") # check what data avail for gage
# nfa_hrly <- dataRetrieval::readNWISuv(siteNumbers = nfa, parameterCd = "00060") # get data
# nfa_hrly <- addWaterYear(nfa_hrly) # add WY
# nfa_hrly <- add_WYD(nfa_hrly, datecolumn = "dateTime") # add DOWY
# save(nfa_hrly, file = "data/nfa_hrly_updated_2019-06-03.rda")

# Update NFA HOURLY --------------------------------------------------------

library(fs)

(hourly <- fs::dir_ls(path = "data/", regexp = "hrly")) # list most recent file
load(hourly) # hourly

nfa <- 11427000 # set gage

# find out last value (date) and add 15 min for use in start time
(maxDateTime <- format((max(nfa_hrly$dateTime) + minutes(15)), format = "%Y-%m-%dT%H:%MZ"))

# get data
nfa_add <- dataRetrieval::readNWISuv(siteNumbers = nfa, parameterCd = "00060", startDate = maxDateTime)

# add WY with two methods:
nfa_add <- add_WYD(nfa_add, "dateTime") # add WY as numeric
nfa_add <- addWaterYear(nfa_add) # add WY as integer
nfa_add <- dataRetrieval::renameNWISColumns(nfa_add)

# compare
identical(as.integer(nfa_add$waterYear), as.integer(nfa_add$WY))

# now add to original data by binding new to old
nfa_updated <- bind_rows(nfa_hrly, nfa_add)
nfa_hrly <- nfa_updated

## HOURLY: Make Some Plots ---------------------------------------------------------

# Plot last 3 years
plotyrs <- c(2018, 2019, 2020, 2021)
filter(nfa_hrly, WY %in% plotyrs, month(dateTime) %in% c(1:12)) %>% 
  ggplot() + geom_line(aes(x=dateTime, y=Flow_Inst), col="skyblue") +
  geom_vline(xintercept = max(nfa_hrly$dateTime), col="sienna", lty=2) + # current time
  geom_point(aes(x=max(nfa_hrly$dateTime), y=nfa_hrly$Flow_Inst[which.max(nfa_hrly$dateTime)]), fill="sienna", pch=21, size=4) +
  scale_x_datetime(date_breaks = "3 months", 
                   date_labels = "%b-%Y", 
                   date_minor_breaks = "1 month", 
                   sec.axis = sec_axis(trans = ~.,
                     name = "",
                     breaks=scales::date_breaks("1 month"),
                     labels = scales::date_format("%b"))) +
  labs(x="", y="Discharge (cfs)", title="Hourly Flow NF American",
  caption="Data from USGS 11427000 \n(https://waterdata.usgs.gov/ca/nwis/uv?site_no=11427000), \nDashed line is current day of month") + 
  ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
  theme(axis.text.x = element_text(angle = 270, vjust=.5))

ggsave(filename = glue("figs/nfa_hourly_recentyrs_{min(plotyrs)}_{max(plotyrs)}.png"), width = 8, height = 6, units = "in", dpi = 300)

# plot many years
plotyrs <- c(2011:2021)
filter(nfa_hrly, WY %in% plotyrs, month(dateTime) %in% c(4:9)) %>% 
  ggplot() + geom_line(aes(x=dateTime, y=Flow_Inst), col="skyblue") +
  geom_vline(xintercept = ymd_hm(maxDateTime), col="sienna", lty=2) +
  facet_wrap(WY~., scales = "free_x") + 
  labs(x="", y="Discharge (cfs)", title="Hourly Flow NF American",
       caption="Data from USGS 11427000 \n (https://waterdata.usgs.gov/ca/nwis/uv?site_no=11427000) \nDashed line is current day of month")+
  ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
  theme(axis.text.x = element_text(angle = 270, vjust=.5))

# last X years
ggsave(filename = glue("figs/nfa_hourly_flows_{min(plotyrs)}-{max(plotyrs)}.png"), width = 11, height = 8, units = "in", dpi = 300)


(maxDate <- max(as_date(nfa_hrly$dateTime)))

save(nfa_hrly, file = glue("data/nfa_hrly_updated_{maxDate}.rda"))

# remove old file:
if(file_exists(hourly)){
  fs::file_delete(hourly) # rm old file
  print(glue("{hourly} file removed"))
} else {
  print("No file to remove")
}

# Update NFA DAILY -----------------------------------------------------------

# get old file:
(dailyFile <- fs::dir_ls(path = "data/", regexp = "daily")) # list most recent file

nfa <- 11427000
nfa_daily <- readNWISdv(siteNumbers = nfa, parameterCd = "00060")
nfa_daily <- addWaterYear(nfa_daily)
nfa_daily <- add_WYD(nfa_daily, "Date")
nfa_daily <- dataRetrieval::renameNWISColumns(nfa_daily)
(maxDateDAY <- max(as_date(nfa_daily$Date)))

save(nfa_daily, file = glue("data/nfa_daily_updated_{maxDateDAY}.rda"))

# rm old file
if(file_exists(dailyFile)){
  fs::file_delete(dailyFile) # rm old file
  print(glue("{dailyFile} file removed!"))
} else {
  print("No file to remove")
}

summary(nfa_daily)


# Daily Plots -------------------------------------------------------------

# plot to check
filter(nfa_daily, WY %in% c(2017:2021)) %>% #, month(Date)>3 & month(Date)<9) %>% 
  ggplot() + 
  ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
  scale_x_date("", date_breaks = "3 months", 
               date_labels = "%b-%Y", 
               date_minor_breaks = "1 month") +
  labs(x="", y="Discharge (cfs)", title="Daily Flow NF American",
       caption="Data from USGS 11427000 \n (https://waterdata.usgs.gov/ca/nwis/uv?site_no=11427000) \nDashed line is current day of month")+
  geom_line(aes(x=Date, y=Flow), color="skyblue", alpha=0.9) +
  geom_point(aes(x=max(nfa_daily$Date), y=nfa_daily$Flow[which.max(nfa_daily$Date)]), fill="sienna", pch=21, size=4) +
  theme(axis.text.x = element_text(angle = 270, vjust=.5))

ggsave(filename = glue("figs/nfa_daily_flow_recent_5yrs.png"), width = 10, height = 7, units="in", dpi=300)

# plot to check
filter(nfa_daily, WY==2011 | WY==2017| WY==2019,
       month(Date)>3 & month(Date)<9) %>% 
  ggplot(aes(x=Date, y=Flow)) + 
  geom_line(color="skyblue", alpha=0.9) +
  ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
  theme(axis.text.x = element_text(angle = 270, vjust=.5)) +
  labs(x="", y="Discharge (cfs)", title="Daily Flow NF American",
       caption="Data from USGS 11427000 \n (https://waterdata.usgs.gov/ca/nwis/uv?site_no=11427000) \nDashed line is current day of month")+
  scale_x_date("", date_breaks = "1 months", 
               date_labels = "%b", 
               date_minor_breaks = "2 weeks")+
  facet_wrap(.~WY, scales = "free_x")

ggsave(filename = "figs/nfa_daily_flow_wet_yrs_dark.png", width = 10, height = 7, units="in", dpi=300)
#  ggforce::geom_mark_circle(data=nfa_daily, aes(x=Date, y=Flow, filter= Date==nfa_daily$Date[which.max(nfa_daily$Date)]), color="sienna")

