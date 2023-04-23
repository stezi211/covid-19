
library(readr)
library(ggplot2)
library(dplyr)

confirmed_cases_worldwide<-read.csv("confirmed_cases_worldwide.csv")
confirmed_cases_worldwide



str(confirmed_cases_worldwide)
confirmed_cases_worldwide$date<-as.Date(confirmed_cases_worldwide$date)


ggplot(confirmed_cases_worldwide,aes(x=date,y=cum_cases))+
  geom_jitter(alpha = 0.6)+
  stat_smooth(method = "lm",se=FALSE,col="red")+
  scale_y_continuous("Cumulative confirmed cases")

confirmed_cases_china_vs_world<-read.csv("confirmed_cases_china_vs_world.csv")
str(confirmed_cases_china_vs_world)
confirmed_cases_china_vs_world

str(confirmed_cases_china_vs_world)
confirmed_cases_china_vs_world$date<-as.Date(confirmed_cases_china_vs_world$date)

plt_cum_confirmed_cases_china_vs_world <- ggplot(confirmed_cases_china_vs_world) +
  geom_line(aes(x=date,y=cum_cases,col=is_china, group=)) +
  ylab("Cumulative confirmed cases")

plt_cum_confirmed_cases_china_vs_world


who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))

plt_cum_confirmed_cases_china_vs_world+
  geom_vline(data = who_events,linetype="dashed",aes(xintercept=date))+
  geom_text(aes(x=date, label=event), y=100000, data=who_events)
  
  
p<-subset(confirmed_cases_china_vs_world,confirmed_cases_china_vs_world$date>"2020-02-15")
p
str(p)
china_after_feb15<-subset(p,p$is_china=="China")
china_after_feb15

ggplot(china_after_feb15,aes(x=date,y=cum_cases))+
  geom_line(alpha=0.6)+
  stat_smooth(method = "lm",se=FALSE,col="red")+
  ylab("Cumulative confirmed cases")

not_china<-subset(confirmed_cases_china_vs_world,confirmed_cases_china_vs_world$is_china=="Not China")

plt_not_china_trend_lin<-ggplot(not_china,aes(x=date,y=cum_cases))+
  geom_line(alpha=0.6)+
  stat_smooth(method = "lm",se=FALSE,col="red")+
  ylab("Cumulative confirmed cases")

plt_not_china_trend_lin 


plt_not_china_trend_lin +
  scale_y_log10()

confirmed_cases_by_country <- read.csv("confirmed_cases_by_country.csv")
  
glimpse(confirmed_cases_by_country)
  
confirmed_cases_by_country$date<-as.Date(confirmed_cases_by_country$date)  

grouped<-group_by(confirmed_cases_by_country,country)

summarized<-summarize(grouped, total_cases=max(cum_cases))

top_countries_by_total_cases <- confirmed_cases_by_country %>%
  group_by(country) %>%
  summarize(total_cases=max(cum_cases)) %>%
  top_n(7)

top_countries_by_total_cases

confirmed_cases_top7_outside_china <-read.csv("confirmed_cases_top7_outside_china.csv")

str(confirmed_cases_top7_outside_china)
confirmed_cases_top7_outside_china$date<-as.Date(confirmed_cases_top7_outside_china$date)
confirmed_cases_top7_outside_china


ggplot(confirmed_cases_top7_outside_china)+
  geom_line(aes(x=date,y=cum_cases,group=country,col=country))+
  ylab("Cumulative confirmed cases")
