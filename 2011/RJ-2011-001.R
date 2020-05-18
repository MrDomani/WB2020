showClass("timeDate")
# Class "timeDate" [package "timeDate"]
# Slots:
# Name: Data format FinCenter
# Class: POSIXct character character
Dates <- c("2009-09-28","2010-01-15")
Times <- c( "23:12:55", "10:34:02")
charvec <- paste(Dates, Times)
getRmetricsOption("myFinCenter")
# myFinCenter
# "GMT"
timeDate(charvec)
# GMT
# [1] [2009-09-28 23:12:55] [2010-01-15 10:34:02]
setRmetricsOptions(myFinCenter = "Zurich")
timeDate(charvec)
# Zurich
# [1] [2009-09-28 23:12:55] [2010-01-15 10:34:02]
timeDate(charvec, zone = "Tokyo")
# Zurich
# [1] [2009-09-28 16:12:55] [2010-01-15 02:34:02]
timeDate(charvec, zone = "Zurich",
FinCenter = "NewYork")
# NewYork
# [1] [2009-09-28 17:12:55] [2010-01-15 04:34:02]
td <- timeDate(charvec, zone = "Zurich",
FinCenter = "NewYork")
finCenter(td)
# [1] "NewYork"
finCenter(td) <- "Zurich"
td
# Zurich
# [1] [2009-09-28 23:12:55] [2010-01-15 10:34:02]
whichFormat(charvec)
# [1] "%Y-%m-%d %H:%M:%S"
setRmetricsOptions(myFinCenter = "GMT")
# 'timeDate' is now in the financial center "GMT"
timeDate(charvec)
# GMT
# [1] [2009-09-28 23:12:55] [2010-01-15 10:34:02]
# first three days in January 2010,
timeSequence(from = "2010-01-01",
to = "2010-01-03", by = "day")
# GMT
# [1] [2010-01-01] [2010-01-02] [2010-01-03]
# first 3 months in 2010:
timeSequence(from = "2010-01-01",
to = "2010-03-31", by = "month")
# GMT
# [1] [2010-01-01] [2010-02-01] [2010-03-01]
timeCalendar()
# GMT
# [1] [2011-01-01] [2011-02-01] [2011-03-01]
# [4] [2011-04-01] [2011-05-01] [2011-06-01]
# [7] [2011-07-01] [2011-08-01] [2011-09-01]
# [10] [2011-10-01] [2011-11-01] [2011-12-01]
head(listFinCenter())
timeCalendar(2010, m=1, d=1:4, h=16,
zone = "Tokyo", FinCenter = "Zurich")
# Zurich
# [1] [2010-01-01 08:00:00] [2010-01-02 08:00:00]
# [3] [2010-01-03 08:00:00] [2010-01-04 08:00:00]
timeDate(ch <- "2010-01-31 24:00:00")
# GMT
# [1] [2010-02-01]
getRmetricsOption("myFinCenter")
# myFinCenter
# "GMT"
# change to Zurich:
setRmetricsOptions(myFinCenter = "Zurich")
# [1] "Africa/Abidjan" "Africa/Accra"
# [3] "Africa/Addis_Ababa" "Africa/Algiers"
# [5] "Africa/Asmara" "Africa/Bamako"
# European centers starting with A or B:
listFinCenter("Europe/[AB].*") # -> nine
# [1] "Europe/Amsterdam" "Europe/Andorra"
# [3] "Europe/Athens" "Europe/Belgrade"
# [5] "Europe/Berlin" "Europe/Bratislava"
# [7] "Europe/Brussels" "Europe/Bucharest"
# [9] "Europe/Budapest"
Zurich()[64:67, ]
# Zurich offSet isdst TimeZone
# 64 2010-03-28 01:00:00 7200 1 CEST
# 65 2010-10-31 01:00:00 3600 0 CET
# 66 2011-03-27 01:00:00 7200 1 CEST
# 67 2011-10-30 01:00:00 3600 0 CET
# numeric
# 64 1269738000
# 65 1288486800
# 66 1301187600
# 67 1319936400
# "Tue" "Wed"
thisYear <- getRmetricsOption("currentYear")
Easter(thisYear:(thisYear+5))
# Zurich
# [1] [2011-04-24] [2012-04-08] [2013-03-31]
# [4] [2014-04-20] [2015-04-05] [2016-03-27]
Easter(2010)
# Zurich
# [1] [2010-04-04]
(tS <- timeSequence(Easter(2010, -2),
Easter(2010, +3)))
# Zurich
# [1] [2010-04-02] [2010-04-03] [2010-04-04]
# [4] [2010-04-05] [2010-04-06] [2010-04-07]
(tS1 <- tS[isWeekday(tS)])
# Zurich
# [1] [2010-04-02] [2010-04-05] [2010-04-06]
# [4] [2010-04-07]
(tS2 <- tS[isBizday(tS, holidayZURICH(2010))])
# Zurich
# [1] [2010-04-06] [2010-04-07]
dayOfWeek(tS2)
charvec <- c("2011-03-01", "2011-04-01")
# Last day in quarter
timeLastDayInQuarter(charvec)
# Zurich
# [1] [2011-03-31] [2011-06-30]
# Second Sunday of each month:
timeNthNdayInMonth(charvec, nday = 0, nth = 2)
# Zurich
# [1] [2011-03-13] [2011-04-10]
# Closest Friday that occurred before:
timeNdayOnOrBefore(charvec, nday = 5)
# Zurich
# [1] [2011-02-25] [2011-04-01]
showMethods("coerce", class = "timeDate")
# Function: coerce (package methods)
# from="ANY", to="timeDate"
# from="Date", to="timeDate"
# from="POSIXt", to="timeDate"
# from="timeDate", to="Date"
# from="timeDate", to="POSIXct"
# from="timeDate", to="POSIXlt"
# from="timeDate", to="character"
# from="timeDate", to="data.frame"
# from="timeDate", to="list"
# from="timeDate", to="numeric"
ZH <- timeDate("2010-01-01 16:00", zone = "GMT",
FinCenter = "Zurich")
NY <- timeDate("2010-01-01 18:00", zone = "GMT",
FinCenter = "NewYork")
c(ZH, NY)
# Zurich
# [1] [2010-01-01 17:00:00] [2010-01-01 19:00:00]
# NewYork
# [1] [2010-01-01 13:00:00] [2010-01-01 11:00:00]
