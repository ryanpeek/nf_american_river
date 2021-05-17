
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(viridis)
library(glue)
library(fs)
library(ggforce)
library(ggrepel)
library(hydroTSM)
library(hrbrthemes)

# Read in Data ------------------------------------------------------------

(daily <- fs::dir_ls(path = "data/", regexp = "daily"))
load(daily) # daily
rm(daily)
(hourly <- fs::dir_ls(path = "data/", regexp = "hrly"))
load(hourly) # hourly
rm(hourly)


# Look at Quantiles for Specific Flows ------------------------------------

monthS <- 4
monthE <- 5

# filter to months of interest:
nfa_daily_filt <- filter(nfa_daily, month(Date)>=monthS, month(Date)<=monthE)

# look at quantiles
quantile(nfa_daily_filt$Flow, probs = c(1- c(.02,.05,.1,.2, .25)))

# percentage of reaching a given flow
quantile(nfa_daily_filt$Flow, probs = c(1- seq(0.01,.1,.01)))

# convert to probs for a specific flow in April or May:
ecdf(nfa_daily_filt$Flow)(3327) # so 1 - this is prob of getting a flow at this level during this time

# see here: 
# https://waterdata.usgs.gov/ca/nwis/dvstat/?site_no=11427000&por_11427000_10840=2209732,00060,10840

# Calc Daily Exceedance Days -----------------------------------------

# number of days that flows exceed X amount
flowX <- 2000
monthS <- 5
monthE <- 8

# filter data:
nfa_daily_filt <- nfa_daily %>% 
  filter(Flow > flowX, 
         month(Date)>=monthS, month(Date)<=monthE) %>% 
  group_by(WY) %>% 
  tally()  %>% print() 

# look at quantiles
quantile(nfa_daily$Flow, probs = c(1- seq(0.01,.1,.01)))

# calc proportions:
(props <- nrow(nfa_daily_filt)/(max(nfa_daily$WY) - min(nfa_daily$WY)))

# get WYT
wyt <- read_csv("data/WYT_1906-2021.csv") %>% 
  mutate(Sac_WY_type=factor(Sac_WY_type, levels=c("W","AN","BN","D","C")))

# join w data
nfa_daily_filt <- left_join(nfa_daily_filt, wyt %>% select(WY, Sac_WY_type), by="WY")

# year range
range(nfa_daily_filt$WY)

## PLOT:
(gg1 <- ggplot(data=nfa_daily_filt) + geom_col(aes(x=WY, y=n, fill=Sac_WY_type),
                                               alpha=0.95, width = .9, lwd=0.2) +
    scale_x_continuous(limits=c(1941, 2021), expand=c(.01,0), breaks = seq(1941,2021,2)) + 
    scale_fill_viridis_d("Water \nYear Type", option = "A")+
    scale_y_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    labs(title=glue("NF American (1942-{max(nfa_daily$WY)})"),
         subtitle=glue("Days daily mean flow exceeded {flowX} cfs ({month.name[monthS]}-{month.name[monthE]}): ({nrow(nfa_daily_filt)} of 78 years)"),
         x="Water Year", y="No. of Days")+
    hrbrthemes::theme_ipsum_rc() + 
    theme(axis.text.x = element_text(angle = 270, vjust=.5)))

# save
ggsave(filename = glue("figs/nfa_daily_days_exceed_{flowX}_{monthS}-{monthE}.png"), width = 11, height = 7, units = "in", dpi = 300)

# Calc Hourly Exceedance Days ------------------------------------------

# number of days that flows exceed X amount (hit 3417 on 6/4)
flowX <- 3000
monthS <- 5
monthE <- 8


#nfa_hrly_filt1 <- nfa_hrly %>% filter(month(dateTime)>=monthS, month(dateTime)<=monthE)
# look at quantiles
#quantile(nfa_hrly_filt1$Flow_Inst, probs = c(1- c(.02,.05,.1,.2, .25)))
# on Jun 4rd it hit 3400
#quantile(nfa_hrly_filt1$Flow_Inst, probs = c(1- seq(0.01,.1,.01)))

# filter data:
nfa_hrly_filt <- nfa_hrly %>% filter(Flow_Inst > flowX, month(dateTime)>=monthS, month(dateTime)<=monthE) %>% 
  group_by(WY, DOWY) %>% 
  tally()  %>% 
  add_count(DOWY) %>% ungroup(DOWY) %>% print()  %>% # this is now for each day that exceeds
  group_by(WY) %>% tally() %>% print()

# calc proportion:
(props <- nrow(nfa_hrly_filt)/(max(nfa_hrly$WY) - min(nfa_hrly$WY)))

wyt <- read_csv("data/WYT_1906-2020.csv") %>% 
  mutate(Sac_WY_type=factor(Sac_WY_type, levels=c("W","AN","BN","D","C")))

# join w data
nfa_hrly_filt <- left_join(nfa_hrly_filt, wyt %>% select(WY, Sac_WY_type), by="WY")

## PLOT:
(gg1 <- ggplot(data=nfa_hrly_filt) + 
    geom_col(aes(x=WY, y=as.integer(n), fill=Sac_WY_type), alpha=0.8, width = .9) +
    scale_x_continuous(limits=c(1987, 2021), expand=c(.01,0), breaks = seq(1987, 2021, 2)) + 
    scale_y_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
    scale_fill_viridis_d("Water \nYear Type", option = "A")+
    labs(title=glue("NF American (1987-2020): Days hourly flow exceeded {flowX} cfs"),
         x="Water Year", y="No. of Days",
         subtitle = glue("({nrow(nfa_hrly_filt)} of 33 years from {month.name[monthS]}-{month.name[monthE]})")) +
    hrbrthemes::theme_ipsum_rc())


# save
ggsave(filename = glue("figs/nfa_hourly_days_exceed_{flowX}_{monthS}-{monthE}.png"), width = 11, height = 7, units = "in", dpi = 300)


# DAILY Exceedance Prob Curves ---------------------------------

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


# HOURLY Exceedance Prob Curves -------------------------------------

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

ggsave(filename = "figs/nfa_flow_dur_curves_hourly_apr_jun.png", width = 10, height = 7, units = "in", dpi = 300)

## GGlot of hourly facets --------------------------------------------------

# plot w ggplot
nfa_hrly %>% filter(WY>2013, month(dateTime)>=5, month(dateTime)<7) %>% 
  ggplot(.) + geom_line(aes(x=dateTime, y=Flow_Inst), col="dodgerblue")+
  #ylim(c(0,10000)) + 
  ylab("Flow (cfs)") + xlab("") +
  theme_bw(base_size = 9) +
  facet_wrap(.~WY, scales = "free_x")

ggsave(filename = "figs/nfa_hrly_may_jun_post2013.png", width = 10, height = 7, units = "in")

