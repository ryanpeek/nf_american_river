# get NFA flow

# https://www.cnrfc.noaa.gov/graphicalRVF.php?id=NFDC1

library(dataRetrieval)
library(lubridate)
library(tidyverse)
library(magrittr)
library(viridis)


# Get NFA HOURLY -----------------------------------------------------------

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
all_equal(nfa_add$waterYear, as.numeric(nfa_add$WY))

# now add to original data by binding new to old
nfa_updated <- bind_rows(nfa_hrly, nfa_add)
nfa_hrly <- nfa_updated


# Make Some Plots ---------------------------------------------------------

# Plot last 3 years
plotyrs <- c(2018, 2019, 2020)
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

ggsave(filename = "figs/hourly_flow_nfa_last3yrs.png", width = 8, height = 6, units = "in", dpi = 300)

# plot many years
plotyrs <- c(2011:2019)
filter(nfa_hrly, WY %in% plotyrs, month(dateTime) %in% c(5:9)) %>% 
  ggplot() + geom_line(aes(x=dateTime, y=Flow_Inst), col="skyblue") +
  geom_vline(xintercept = ymd_hm(maxDateTime), col="sienna", lty=2) +
  facet_wrap(WY~., scales = "free_x") + 
  labs(x="", y="Discharge (cfs)", title="Hourly Flow NF American",
       caption="Data from USGS 11427000 \n (https://waterdata.usgs.gov/ca/nwis/uv?site_no=11427000) \nDashed line is current day of month")+
  ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
  theme(axis.text.x = element_text(angle = 270, vjust=.5))

# last 9 years
ggsave(filename = paste0("figs/nfa_hourly_flows_2011-2019.png"), width = 11, height = 8, units = "in", dpi = 300)


(maxDate <- max(as_date(nfa_hrly$dateTime)))

save(nfa_hrly, file = paste0("data/nfa_hrly_updated_",maxDate, ".rda"))

# remove old file:
if(file_exists(hourly)){
  fs::file_delete(hourly) # rm old file
  print(paste0(hourly, " file removed"))
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

save(nfa_daily, file = paste0("data/nfa_daily_updated_",maxDateDAY, ".rda"))

# rm old file
if(file_exists(dailyFile)){
  fs::file_delete(dailyFile) # rm old file
  print(paste0(dailyFile, " file removed!"))
} else {
  print("No file to remove")
}

summary(nfa_daily)


# Daily Plots -------------------------------------------------------------

# plot to check
filter(nfa_daily, WY==2017 | WY==2018| WY==2019) %>% #, month(Date)>3 & month(Date)<9) %>% 
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
ggsave(filename = "figs/nfa_daily_flow_last3yrs_dark.png", width = 10, height = 7, units="in", dpi=300)

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

# Calculate_flows ---------------------------------------------------------

library(fs)
(daily <- fs::dir_ls(path = "data/", regexp = "daily"))
load(daily) # daily
rm(daily)
(hourly <- fs::dir_ls(path = "data/", regexp = "hrly"))
load(hourly) # hourly
rm(hourly)

# monthS <- 4
# monthE <- 5
# 
# # filter to months of interest:
# nfa_daily_filt1 <- filter(nfa_daily, month(Date)>=monthS, month(Date)<=monthE)
# 
# # look at quantiles
# quantile(nfa_daily_filt1$Flow, probs = c(1- c(.02,.05,.1,.2, .25)))
# 
# # on Jun 3rd it hit 3327, expected to crest 4,200 on jun 5
# quantile(nfa_daily_filt1$Flow, probs = c(1- seq(0.01,.1,.01)))
# 
# # convert to probs for a specific flow:
# ecdf(nfa_daily_filt1$Flow)(3327)

# see here: 
# https://waterdata.usgs.gov/ca/nwis/dvstat/?site_no=11427000&por_11427000_10840=2209732,00060,10840

# Calc Daily --------------------------------------------------------------

# number of days that flows exceed X amount
flowX <- 3000
monthS <- 5
monthE <- 8

# filter data:
nfa_daily_filt <- nfa_daily %>% filter(Flow > flowX, month(Date)>=monthS, month(Date)<=monthE) %>% 
  group_by(WY) %>% 
  tally()  %>% print() 

# look at quantiles
quantile(nfa_daily$Flow, probs = c(1- seq(0.01,.1,.01)))

# calc proportions:
(props <- nrow(nfa_daily_filt)/(max(nfa_daily$WY) - min(nfa_daily$WY)))

# get WYT
wyt <- read_csv("data/WYT_1906-2019.csv") %>% 
  mutate(Sac_WY_type=factor(Sac_WY_type, levels=c("W","AN","BN","D","C")))
# join w data
nfa_daily_filt <- left_join(nfa_daily_filt, wyt %>% select(WY, Sac_WY_type), by="WY")
  
## PLOT:
(gg1 <- ggplot(data=nfa_daily_filt) + geom_col(aes(x=WY, y=n, fill=Sac_WY_type), color="black",
                                               alpha=0.95, width = .9, lwd=0.2) +
    scale_x_continuous(limits=c(1941, 2020), expand=c(.01,0), breaks = seq(1941,2020,2)) + 
    scale_fill_viridis_d("Water \nYear Type", option = "D")+
    scale_y_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    labs(title=paste0("NF American (1942-2019): Days daily mean flow exceeded ", 
                      flowX, " cfs (", month.name[monthS], "-", month.name[monthE],")"),
         x="Water Year", y="No. of Days",
         subtitle = paste0("(",nrow(nfa_daily_filt)," of 77 years)")) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 270, vjust=.5)))

# save
ggsave(filename = paste0("figs/nfa_daily_days_exceed_", flowX, "_",monthS,"-",monthE,".png"), width = 11, height = 7, units = "in", dpi = 300)

# Calc Hourly --------------------------------------------------------------

# number of days that flows exceed X amount (hit 3417 on 6/4)
flowX <- 3000
monthS <- 5
monthE <- 8


nfa_hrly_filt1 <- nfa_hrly %>% filter(month(dateTime)>=monthS, month(dateTime)<=monthE)

# look at quantiles
quantile(nfa_hrly_filt1$Flow_Inst, probs = c(1- c(.02,.05,.1,.2, .25)))

# on Jun 4rd it hit 3400
quantile(nfa_hrly_filt1$Flow_Inst, probs = c(1- seq(0.01,.1,.01)))

# filter data:
nfa_hrly_filt <- nfa_hrly %>% filter(Flow_Inst > flowX, month(dateTime)>=monthS, month(dateTime)<=monthE) %>% 
  group_by(WY, DOWY) %>% 
  tally()  %>% 
  add_count(DOWY) %>% ungroup(DOWY) %>% print()  %>% # this is now for each day that exceeds
  group_by(WY) %>% tally() %>% print()

# calc proportion:
(props <- nrow(nfa_hrly_filt)/(max(nfa_hrly$WY) - min(nfa_hrly$WY)))

## PLOT:
(gg1 <- ggplot(data=nfa_hrly_filt) + geom_col(aes(x=WY, y=as.integer(n)), fill="darkblue", alpha=0.8, width = .9) +
    scale_x_continuous(limits=c(1987, 2020), expand=c(.01,0), breaks = seq(1987, 2019, 2)) + 
    scale_y_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    labs(title=paste0("NF American (1987-2019): Days hourly flow exceeded ", flowX, " cfs"),
         x="Water Year", y="No. of Days",
         subtitle = paste0("(",nrow(nfa_hrly_filt)," of 32 years from ", month.name[monthS], "-", month.name[monthE],")")) +
    theme_bw())
    #theme(axis.text.x = element_text(angle = 270, vjust=.5)))

# save
ggsave(filename = paste0("figs/nfa_hourly_days_exceed_", flowX, "_",monthS,"-",monthE,".png"), width = 11, height = 7, units = "in", dpi = 300)


# Calc DAILY Flow Exeedance-------------------------------------------------------

# exceedance prob: calc the prob that 3 50-yr floods occur in 100 year period:
# dbinom(k,n,p): 
## k=successes (the 3 floods), 
## n=trials (100 year period), 
## p=prob (1 in 50 chance)
#dbinom(3, 100, 1/50) * 100 # so 18.23% chance

# for 3 or MORE 50 yr floods in 100 years:
#sum(dbinom(3:100,100,1/50))


library(hydroTSM)
# flow duration curves: 
# The cumulative distribution function with the axis swapped around, 
# x-axis shows % time equalled or exceeded, 
# representing 1 - the cdf probabilities.

# filter to months of interest:
nfa_daily_4 <- filter(nfa_daily, month(Date)==4)
nfa_daily_5 <- filter(nfa_daily, month(Date)==5)
nfa_daily_6 <- filter(nfa_daily, month(Date)==6)

nfa_fdc_daily_4 <- hydroTSM::fdc(nfa_daily_4$Flow/35.314666212661, col="blue2", verbose=FALSE,
              lQ.thr=0.1, hQ.thr=0.9, ylim=c(0,700), thr.shw=FALSE, main="NFA Flow Duration Curve (daily means)")
nfa_fdc_daily_5 <- hydroTSM::fdc(nfa_daily_5$Flow/35.314666212661, col="green2", verbose=FALSE,
              lQ.thr=0.1, hQ.thr=0.9, ylim=c(0,700), new=FALSE, thr.shw=FALSE)
nfa_fdc_daily_6 <- hydroTSM::fdc(nfa_daily_6$Flow/35.314666212661, col="orange2", thr.shw=FALSE,
              lQ.thr=0.1, hQ.thr=0.9, ylim=c(0,700), new=FALSE)

# make a dataframe:
fdc_daily_4 <- tibble(fdc=nfa_fdc_daily_4, cfs=nfa_daily_4$Flow, cms=nfa_daily_4$Flow/35.314666212661, date=nfa_daily_4$Date, WY=nfa_daily_4$WY)
fdc_daily_5 <- tibble(fdc=nfa_fdc_daily_5, cfs=nfa_daily_5$Flow, cms=nfa_daily_5$Flow/35.314666212661, date=nfa_daily_5$Date, WY=nfa_daily_5$WY)
fdc_daily_6 <- tibble(fdc=nfa_fdc_daily_6, cfs=nfa_daily_6$Flow, cms=nfa_daily_6$Flow/35.314666212661, date=nfa_daily_6$Date, WY=nfa_daily_6$WY)

save(fdc_daily_4, fdc_daily_5, fdc_daily_6, file = "data_output/nfa_flow_duration_curves_daily_apr_jun.rda")

library(ggforce)
library(ggrepel)

# ggplot
ggplot() + 
  geom_hline(yintercept = 84, color="gray", lty=2)+
  geom_hline(yintercept = 42.4, color="gray", lty=2)+
  #geom_vline(xintercept = .028, color="gray", lty=2)+
  geom_line(data=fdc_daily_4, aes(x=fdc, y=cms, color="Apr"), lwd=1.5) + 
  geom_line(data=fdc_daily_5, aes(x=fdc, y=cms, color="May"), lwd=1.5) +
  geom_line(data=fdc_daily_6, aes(x=fdc, y=cms, color="Jun"), lwd=1.5) +
  scale_color_manual("Month", breaks=c("Apr","May", "Jun"), values = c(viridis(3, option = "D"))) +
  scale_y_log10() + scale_x_continuous(labels = scales::percent, breaks = c(seq(0,1,.1)), expand=c(.01,.01)) + 
  theme_bw() + labs(x="% Time daily flow equalled or exceeded", y="Q (m3/s)",
                    title="NFA: Daily Flow Duration Curve", 
                    caption = paste0("Daily Flows from USGS Gage: 11427000 (",min(nfa_daily$WY), "-",max(nfa_daily$WY),")")) +
  annotate(geom="point", x=0.016365202, y=88.349, color="black", size=4, pch=21) +
  geom_label_repel(aes(x=.016365202, y=88.349), 
                   label="Flows greater than 3,100 cfs (~88 cms)\n in June are quite rare (<1%)",
                   nudge_y = 0.5, nudge_x = .2) +
  annotate(geom = "text", x=.5, y=74, label="3,000 cfs", color="gray30") +
  annotate(geom = "text", x=.34, y=32, label="1,500 cfs", color="gray30") 

# so based on these and the exceedance curves, daily mean flows above 2910 in early June happen about 2.6% of the time


# Hourly FLow exceedance --------------------------------------------------


library(hydroTSM)
# flow duration curves: 
# The cumulative distribution function with the axis swapped around, 
# x-axis shows % time equalled or exceeded, 
# representing 1 - the cdf probabilities.

# filter to months of interest:
nfa_hr_4 <- filter(nfa_hrly, month(dateTime)==4)
nfa_hr_5 <- filter(nfa_hrly, month(dateTime)==5)
nfa_hr_6 <- filter(nfa_hrly, month(dateTime)==6)


nfa_fdc_hrly_4 <- hydroTSM::fdc(nfa_hr_4$Flow_Inst/35.314666212661, col="blue2", verbose= FALSE,
              lQ.thr=0.1, hQ.thr=0.9, ylim=c(0,700), thr.shw=FALSE, main="NFA Flow Duration Curve (hourly means)")
nfa_fdc_hrly_5 <- hydroTSM::fdc(nfa_hr_5$Flow_Inst/35.314666212661, col="green2", verbose= FALSE,
              lQ.thr=0.1, hQ.thr=0.9, ylim=c(0,700), new=FALSE, thr.shw=FALSE)
nfa_fdc_hrly_6 <- hydroTSM::fdc(nfa_hr_6$Flow_Inst/35.314666212661, col="orange2", thr.shw=FALSE,
              lQ.thr=0.1, hQ.thr=0.9, ylim=c(0,700), new=FALSE, verbose= FALSE)

# make a dataframe:
fdc_hrly_4 <- tibble(fdc=nfa_fdc_hrly_4, cfs=nfa_hr_4$Flow_Inst, cms=nfa_hr_4$Flow_Inst/35.314666212661, datetime=nfa_hr_4$dateTime, WY=nfa_hr_4$WY)
fdc_hrly_5 <- tibble(fdc=nfa_fdc_hrly_5, cfs=nfa_hr_5$Flow_Inst, cms=nfa_hr_5$Flow_Inst/35.314666212661, datetime=nfa_hr_5$dateTime, WY=nfa_hr_5$WY)
fdc_hrly_6 <- tibble(fdc=nfa_fdc_hrly_6, cfs=nfa_hr_6$Flow_Inst, cms=nfa_hr_6$Flow_Inst/35.314666212661, datetime=nfa_hr_6$dateTime, WY=nfa_hr_6$WY)

save(fdc_hrly_4, fdc_hrly_5, fdc_hrly_6, file = "data_output/nfa_flow_duration_curves_hourly_apr_jun.rda")

library(ggforce)
library(ggrepel)

# HOURLY ggplot
ggplot() + 
  geom_hline(yintercept = 84, color="gray", lty=2)+
  geom_hline(yintercept = 42.4, color="gray", lty=2)+
  #geom_vline(xintercept = .028, color="gray", lty=2)+
  geom_line(data=fdc_hrly_4, aes(x=fdc, y=cms, color="Apr"), lwd=1.5) + 
  geom_line(data=fdc_hrly_5, aes(x=fdc, y=cms, color="May"), lwd=1.5) +
  geom_line(data=fdc_hrly_6, aes(x=fdc, y=cms, color="Jun"), lwd=1.5) +
  scale_color_manual("Month", breaks=c("Apr","May", "Jun"), values = c(viridis(3, option = "D"))) +
  scale_y_log10() + scale_x_continuous(labels = scales::percent, breaks = c(seq(0,1,.1)), expand=c(.01,.01)) + 
  theme_bw() + 
  labs(x="% Time hourly flow equalled or exceeded", y="Q (m3/s)",
       title="NFA Hourly Flow Duration Curve", 
       caption = paste0("Hourly Flows from USGS Gage: 11427000 (",min(nfa_hrly$WY), "-",max(nfa_hrly$WY),")")) +
  annotate(geom="point", x=0.009746719, y=96.56, color="black", size=4, pch=21) +
  geom_label_repel(aes(x=0.009746719, y=96.56),
                  label="Flows greater than 3,400 cfs (~96 cms)\n in June are quite rare (<1%)",
                  nudge_y = 0.5, nudge_x = .2) +
  annotate(geom = "text", x=.5, y=74, label="3,000 cfs", color="gray30") +
  annotate(geom = "text", x=.34, y=32, label="1,500 cfs", color="gray30")
  #geom_mark_ellipse(data=fdc_hrly_6, aes(x=fdc, y=cms, filter = cfs ==3410, label = 'Occurs <1% of the time', 
  #                       description = "Flows >3,400 cfs (~96 cms) in June are quite rare"), label.buffer = unit(40, 'mm'))
  
ggsave(filename = "figs/nfa_flow_dur_curves_hourly_apr_jun.png", width = 10, height = 7, units = "in", dpi = 300)



# GGlot of hourly facets --------------------------------------------------



# plot w ggplot
nfa_hrly %>% filter(WY>2013, month(dateTime)>=5, month(dateTime)<7) %>% 
  ggplot(.) + geom_line(aes(x=dateTime, y=Flow_Inst), col="dodgerblue")+
  #ylim(c(0,10000)) + 
  ylab("Flow (cfs)") + xlab("") +
  theme_bw(base_size = 9) +
  facet_wrap(.~WY, scales = "free_x")

ggsave(filename = "figs/nfa_hrly_may_jun.png", width = 10, height = 7, units = "in")

# all yrs continuous
# nfa_hrly %>% filter(WY>2013) %>% 
  # ggplot(.) + geom_line(aes(x=dateTime, y=Flow_Inst), col="dodgerblue")
