library(tidyverse)###contains dyplr
library(readxl)
library(lubridate)
library(data.table)
library("gridExtra")

#######FUNCTION FROM: https://rdrr.io/github/USGS-R/EflowStats/src/R/get_waterYearDay.R
#' Day of water year
#' @description Given a vector of dates, calculates day of water year accounting for leap years.
#' @param x A vector of class date.
#' @return A numeric vector of day of water year
#' @importFrom lubridate leap_year
#' @importFrom lubridate yday
#' @export
#' @examples
#' x <- seq(from=as.Date("2010-01-01"),to=as.Date("2016-01-01"),by="1 days")
#' get_waterYearDay(x)

get_waterYearDay <- function(x) {
  
  year_day <- lubridate::yday(x)
  yrs_leap <- lubridate::leap_year(x)
  
  # October 1st (day 1 of water year) is day 274 (or 275 in leap year) of calendar year.
  # (274 + years_leap) == 275 for dates in leap year and 274 for dates not in leap year.
  # This provides a boolean selector for days before (false) / after (true) Oct 1.
  after_waterday <- year_day >= (274 + yrs_leap)
  
  # 273 (or 274 in leap year) days from January 1 to October 1
  # This gives us 1 for October 1st and corrects everything till December 31st.
  year_day[after_waterday] <- year_day[after_waterday] - (273 + yrs_leap[after_waterday])
  
  # 92 days from October 1 to January 1
  # This gives us 93 for January 1 and corrects everything till September 29th.
  year_day[!after_waterday] <- year_day[!after_waterday] + 92
  
  return(year_day)
  
}

####INTRODUCE HYDRLOGICAL DATA##########################
setwd("c:/UTEP/ThesisRelated/Data/climate")
getwd()
hydro_climate1 <- read.csv("eddy.hydro_climate.csv")
hydro_climate2 <- mutate(hydro_climate1, date= as.Date(date),
                         doy.hydro=get_waterYearDay(date))

hydro_climate3 <- hydro_climate2%>%
  arrange(date)

rain_cum <- hydro_climate3%>%
  select(date, hydro_year, precip.tot)%>%
  filter(!is.na(precip.tot) & !is.na(hydro_year))%>%
  group_by(hydro_year)%>%
  mutate(rain_accumlt = cumsum(precip.tot))%>%
  select(date, rain_accumlt)

###this one seems right:
weekly <- hydro_climate3%>%
  select(date, precip.tot, year)%>%
  filter(!is.na(precip.tot))%>%
  group_by(date)%>%
  mutate(wkly = week(ymd(date)))%>%
  select(date, wkly)

hydro_climate4 <- merge(hydro_climate3, rain_cum, by=c("date", "hydro_year"))

hydro_climate4 <- merge(hydro_climate3, weekly, by=c("date"))

ggplot(hydro_climate4, aes(wkly, precip.tot)) + geom_col() + facet_grid(.~year)

hydro_climate5<-hydro_climate4%>%
  mutate(year_label=case_when(doy.hydro == 365~hydro_year,
                              TRUE~NA_integer_))

P1<- hydro_climate5%>%
  filter(hydro_year%in%c(2011, 2012, 2013))%>%
  ggplot(., aes(doy.hydro, rain_accumlt, color=factor(hydro_year))) + geom_line(size=1) + geom_label(aes(label=year_label))

P2<- hydro_climate5%>%
  filter(hydro_year%in%c(2014, 2015, 2016))%>%
  ggplot(., aes(doy.hydro, rain_accumlt, color=factor(hydro_year))) + geom_line(size=1) + geom_label(aes(label=year_label))

P3<- hydro_climate5%>%
  filter(hydro_year%in%c(2017, 2018, 2019))%>%
  ggplot(., aes(doy.hydro, rain_accumlt, color=factor(hydro_year))) + geom_line(size=1) + geom_label(aes(label=year_label))


ggplot(hydro_climate5, aes(doy.hydro, rain_accumlt, color=factor(hydro_year))) + geom_line() + geom_text(aes(label=year_label))

ggplot(hydro_climate5, aes(date, rain_accumlt, color=factor(hydro_year))) + geom_line() + geom_label(aes(label=year_label))

ggplot(hydro_climate5, aes(month, precip.tot)) + geom_col() + facet_grid(.~year) 

grid.arrange(P1, P2, P3)

ydm()

#############INTRODUCE THE PRGL DATA##############################################################################
##################################################################################################################
setwd("c:/UTEP/ThesisRelated/Data/Phenology/Data/PRGL")

prgl_start <- read.csv("prgl_first_date.csv")

prgl_start <- prgl_start %>%
  mutate(pheno_start = as.Date(pheno_start), plant_id = as.factor(plant_id),
         phenophase =as.factor(phenophase))

prgl_start_length <- prgl_start %>%
  group_by(year, phenophase, doy)%>%
  mutate(pheno_start_length = length(pheno_start))

#write.csv(prgl_start_length, file = 'C:/UTEP/ThesisRelated/Data/Phenology/Data/PRGL/prgl_start_length.csv', row.names = FALSE)

ggplot(subset(prgl_start, phenophase=="Breaking Leaf Bud"), aes(doy)) + geom_histogram(binwidth=7)+ 
  ggtitle("First breaking Leaf Bud for each plant and year: 2010-2019") + facet_grid(.~year)

ggplot(subset(prgl_start, phenophase=="Young Unfolded Leaves"), aes(doy)) + geom_histogram(binwidth = 7)+ 
  ggtitle("First young Unfolded Leaves for each plant and year: 2010-2019") + facet_grid(.~year)


ggplot(prgl_start, aes(pheno_start, doy,colour=phenophase))+geom_point()+facet_grid(year~.)

ggplot(subset(prgl_start, aes( doy, pheno_start, colour=phenophase)))+geom_point()#+facet_grid(year~.)

prgl.expand <- expand.grid(date=seq(as.Date("2010-01-01"),  as.Date("2019-12-31"), 
                                    by="1 day"),
                           plant_id=levels(prgl_start$plant_id),
                           phenophase=levels(prgl_start$phenophase))

sum(is.na(harMet15.09.11$datetime))


#make a review 