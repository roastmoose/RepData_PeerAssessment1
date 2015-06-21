---
title: "Effects of Severe Weather"
author: "Alexander Spray"
date: "Tuesday, June 16, 2015"
output: html_document
---

## Synopsis

The purpose of this report is to study the NOAA Storm Database to discover the effects of various 
types of severe weather on health and economic wellbeing in the United States. Specifically, the 
following two questions will be addressed:

1. Accross the United States, which types of events are most harmful with respect to population
health?

2. Across the United States, which types of events have the greatest economic consequences?


## Data Processing

Data is loaded from the coursera website in the form of a bzip2 csv file



```r
#first accessed on 6/16/2015 at 8:41am central time (US)
temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",temp)
rawData <- read.csv(temp)
unlink(temp)
```

And we should at least somewhat care about whether there are NAs?

And there are none:

```r
length(complete.cases(rawData))/length(rawData[[1]])
```

```
## [1] 1
```

Of course, we might expect that the best predictor of future events is past events (that's all we
have!), but our data quality decreases with age. So maybe just the recent history is necessary. As such we will truncate the history at the year 2000. Further data transformations are unnecessary as the data is already in a clean (no NA) format:


```r
#we loaded things as factor so fix that...
rawData$BGN_DATE<-as.Date(rawData$BGN_DATE,"%m/%d/%Y")

#this is maybe bad practice, since these data frames are so big... but we have enough ram so it's ok
data<-subset(rawData,rawData$BGN_DATE>as.Date("20000101","%Y%m%d"))
```

## Results (surely we can do more than this?)

A simple summation across the various event types within the applicable date range presents the ordering of event types by either health effects or property damage below. When handling health effects in total, some sort of "indexing" is necessary to handle both injuries and fatalities together. While it is simplistic, below we are simply weighting fatalities at twice the impact of injuries. Artificial, yes, but convenient, and not necessarily too damaging to the ordering of events' impact on health when dealing with such limited data.


```r
#the boring tables... wait, do tables count as charts?
library(data.table)
dt<-data.table(data) #more useless replication/taking up memory
MEDICAL_RESULT<-dt[,list(sum_fatalities=sum(FATALITIES),sum_injuries=sum(INJURIES)),by=EVTYPE]

#I'd hate to have to weight these... maybe we should just sum them... i dunno though, medical
#science is getting pretty cool... maybe injuries don't matter... Let's assume they matter half
#as much.

MEDICAL_RESULT$TOTALBADTHINGSHEALTHWISE<-MEDICAL_RESULT$sum_fatalities+0.5*MEDICAL_RESULT$sum_injuries

MEDICAL_RESULT[order(-MEDICAL_RESULT$TOTALBADTHINGSHEALTHWISE),]
```

```
##                     EVTYPE sum_fatalities sum_injuries
##   1:               TORNADO           1193        15213
##   2:        EXCESSIVE HEAT           1013         3708
##   3:             LIGHTNING            466         2993
##   4:           FLASH FLOOD            600          812
##   5:             TSTM WIND            116         1753
##  ---                                                  
## 192:      LAKE-EFFECT SNOW              0            0
## 193:           DENSE SMOKE              0            0
## 194:       LAKESHORE FLOOD              0            0
## 195: ASTRONOMICAL LOW TIDE              0            0
## 196:      VOLCANIC ASHFALL              0            0
##      TOTALBADTHINGSHEALTHWISE
##   1:                   8799.5
##   2:                   2867.0
##   3:                   1962.5
##   4:                   1006.0
##   5:                    992.5
##  ---                         
## 192:                      0.0
## 193:                      0.0
## 194:                      0.0
## 195:                      0.0
## 196:                      0.0
```

Property and crop damage, both being presented in dollars can simply be summed. Presumably markets have normalized the values in dollars well enough that summing the various damage can be thought of (simply, at the level of this analysis/data certainly) as comparing apples to apples.


```r
#should look up the EXP fields for damage... probably not "experience". Dood that tornado is lvl 12!
PROPERTY_RESULT<-dt[,list(sum_grassroofedcottages=sum(PROPDMG),sum_cornandstuff=sum(CROPDMG)),by=EVTYPE]

#as an economist I'm going to note that crops and property damage are already rated in terms of
#dollars therefore no weighting is needed... assumptions are useful.
PROPERTY_RESULT$TOTALBADTHINGSOWNINGSTUFFWISE<-PROPERTY_RESULT$sum_grassroofedcottages+PROPERTY_RESULT$sum_cornandstuff

PROPERTY_RESULT[with(PROPERTY_RESULT,order(-PROPERTY_RESULT$TOTALBADTHINGSOWNINGSTUFFWISE)),]
```

```
##                            EVTYPE sum_grassroofedcottages sum_cornandstuff
##   1:                  FLASH FLOOD                999333.4        132381.63
##   2:                      TORNADO                907111.7         73634.91
##   3:            THUNDERSTORM WIND                862257.4         66663.00
##   4:                    TSTM WIND                811528.2         53758.70
##   5:                         HAIL                452533.5        363279.18
##  ---                                                                      
## 192:      GUSTY THUNDERSTORM WIND                     0.0             0.00
## 193:         HIGH SURF ADVISORIES                     0.0             0.00
## 194:                  SLEET STORM                     0.0             0.00
## 195: COLD WIND CHILL TEMPERATURES                     0.0             0.00
## 196:             VOLCANIC ASHFALL                     0.0             0.00
##      TOTALBADTHINGSOWNINGSTUFFWISE
##   1:                     1131715.1
##   2:                      980746.6
##   3:                      928920.4
##   4:                      865286.9
##   5:                      815812.6
##  ---                              
## 192:                           0.0
## 193:                           0.0
## 194:                           0.0
## 195:                           0.0
## 196:                           0.0
```

As can be clearly seen, the results of this study imply quite strongly that people shouldn't
live in oklahoma. It may also be useful to examine the geographic distribution of the data. As such we will filter on the most damaging event types and plot.

Plot of tornado fatalities+0.5*injuries:

```r
library(googleVis)
op <- options(gvis.plot.tag='chart')
tornado<-subset(dt,dt$EVTYPE=="TORNADO")
TORNADOINJURIES<-dt[,list(sum_fatalities=sum(FATALITIES),sum_injuries=sum(INJURIES)),by=STATE]

TORNADOINJURIES$TOTALBADTHINGSHEALTHWISE<-TORNADOINJURIES$sum_fatalities+0.5*TORNADOINJURIES$sum_injuries


GeoStates <- gvisGeoChart(TORNADOINJURIES, "STATE", "TOTALBADTHINGSHEALTHWISE",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)
```

```
## <!-- GeoChart generated in R 3.0.2 by googleVis 0.5.8 package -->
## <!-- Sun Jun 21 14:08:34 2015 -->
## 
## 
## <!-- jsHeader -->
## <script type="text/javascript">
##  
## // jsData 
## function gvisDataGeoChartID1ac02f0e3b1 () {
## var data = new google.visualization.DataTable();
## var datajson =
## [
##  [
##  "AL",
## 1972 
## ],
## [
##  "AK",
## 72.5 
## ],
## [
##  "AZ",
## 313 
## ],
## [
##  "AR",
## 561 
## ],
## [
##  "CA",
## 1380 
## ],
## [
##  "CO",
## 309 
## ],
## [
##  "FL",
## 1318.5 
## ],
## [
##  "CT",
## 98.5 
## ],
## [
##  "DE",
## 114.5 
## ],
## [
##  "DC",
## 27.5 
## ],
## [
##  "GA",
## 741 
## ],
## [
##  "ID",
## 104 
## ],
## [
##  "IL",
## 731 
## ],
## [
##  "HI",
## 48.5 
## ],
## [
##  "IN",
## 481 
## ],
## [
##  "IA",
## 368 
## ],
## [
##  "KS",
## 389 
## ],
## [
##  "KY",
## 405.5 
## ],
## [
##  "LA",
## 294.5 
## ],
## [
##  "MD",
## 413.5 
## ],
## [
##  "MI",
## 457.5 
## ],
## [
##  "MN",
## 233.5 
## ],
## [
##  "ME",
## 61.5 
## ],
## [
##  "MA",
## 299.5 
## ],
## [
##  "MO",
## 2939.5 
## ],
## [
##  "MT",
## 95 
## ],
## [
##  "MS",
## 661 
## ],
## [
##  "NE",
## 129.5 
## ],
## [
##  "NH",
## 58 
## ],
## [
##  "NJ",
## 259.5 
## ],
## [
##  "NM",
## 88 
## ],
## [
##  "NY",
## 406.5 
## ],
## [
##  "NV",
## 139.5 
## ],
## [
##  "NC",
## 607.5 
## ],
## [
##  "OH",
## 309.5 
## ],
## [
##  "ND",
## 80 
## ],
## [
##  "OK",
## 884.5 
## ],
## [
##  "OR",
## 70 
## ],
## [
##  "PA",
## 631 
## ],
## [
##  "SC",
## 271.5 
## ],
## [
##  "SD",
## 68 
## ],
## [
##  "RI",
## 18.5 
## ],
## [
##  "TN",
## 1213.5 
## ],
## [
##  "TX",
## 1597 
## ],
## [
##  "VT",
## 24 
## ],
## [
##  "VA",
## 421 
## ],
## [
##  "WV",
## 81.5 
## ],
## [
##  "UT",
## 229 
## ],
## [
##  "WA",
## 156.5 
## ],
## [
##  "WI",
## 255 
## ],
## [
##  "WY",
## 134.5 
## ],
## [
##  "VI",
## 8 
## ],
## [
##  "AS",
## 123 
## ],
## [
##  "GU",
## 252 
## ],
## [
##  "PR",
## 90 
## ],
## [
##  "LC",
## 0 
## ],
## [
##  "PH",
## 1 
## ],
## [
##  "GM",
## 1 
## ],
## [
##  "PZ",
## 6.5 
## ],
## [
##  "AM",
## 25 
## ],
## [
##  "AN",
## 23.5 
## ],
## [
##  "LH",
## 0 
## ],
## [
##  "LM",
## 5 
## ],
## [
##  "LE",
## 0 
## ],
## [
##  "LS",
## 1 
## ],
## [
##  "SL",
## 0 
## ],
## [
##  "LO",
## 0 
## ],
## [
##  "PM",
## 0 
## ],
## [
##  "PK",
## 0 
## ],
## [
##  "XX",
## 0 
## ] 
## ];
## data.addColumn('string','STATE');
## data.addColumn('number','TOTALBADTHINGSHEALTHWISE');
## data.addRows(datajson);
## return(data);
## }
##  
## // jsDrawChart
## function drawChartGeoChartID1ac02f0e3b1() {
## var data = gvisDataGeoChartID1ac02f0e3b1();
## var options = {};
## options["width"] =    600;
## options["height"] =    400;
## options["region"] = "US";
## options["displayMode"] = "regions";
## options["resolution"] = "provinces";
## 
## 
##     var chart = new google.visualization.GeoChart(
##     document.getElementById('GeoChartID1ac02f0e3b1')
##     );
##     chart.draw(data,options);
##     
## 
## }
##   
##  
## // jsDisplayChart
## (function() {
## var pkgs = window.__gvisPackages = window.__gvisPackages || [];
## var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
## var chartid = "geochart";
##   
## // Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
## var i, newPackage = true;
## for (i = 0; newPackage && i < pkgs.length; i++) {
## if (pkgs[i] === chartid)
## newPackage = false;
## }
## if (newPackage)
##   pkgs.push(chartid);
##   
## // Add the drawChart function to the global list of callbacks
## callbacks.push(drawChartGeoChartID1ac02f0e3b1);
## })();
## function displayChartGeoChartID1ac02f0e3b1() {
##   var pkgs = window.__gvisPackages = window.__gvisPackages || [];
##   var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
##   window.clearTimeout(window.__gvisLoad);
##   // The timeout is set to 100 because otherwise the container div we are
##   // targeting might not be part of the document yet
##   window.__gvisLoad = setTimeout(function() {
##   var pkgCount = pkgs.length;
##   google.load("visualization", "1", { packages:pkgs, callback: function() {
##   if (pkgCount != pkgs.length) {
##   // Race condition where another setTimeout call snuck in after us; if
##   // that call added a package, we must not shift its callback
##   return;
## }
## while (callbacks.length > 0)
## callbacks.shift()();
## } });
## }, 100);
## }
##  
## // jsFooter
## </script>
##  
## <!-- jsChart -->  
## <script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartGeoChartID1ac02f0e3b1"></script>
##  
## <!-- divChart -->
##   
## <div id="GeoChartID1ac02f0e3b1" 
##   style="width: 600; height: 400;">
## </div>
```

Plot of flood Property Damage+Crop Damage

```r
library(googleVis)
flood<-subset(dt,dt$EVTYPE=="FLASH FLOOD")
floodDamage<-dt[,list(sum_grassroofedcottages=sum(PROPDMG),sum_cornandstuff=sum(CROPDMG)),by=STATE]

floodDamage$DollarDamages<-floodDamage$sum_grassroofedcottages+0.5*floodDamage$sum_cornandstuff


GeoStates <- gvisGeoChart(floodDamage, "STATE", "DollarDamages",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)
```

```
## <!-- GeoChart generated in R 3.0.2 by googleVis 0.5.8 package -->
## <!-- Sun Jun 21 14:08:34 2015 -->
## 
## 
## <!-- jsHeader -->
## <script type="text/javascript">
##  
## // jsData 
## function gvisDataGeoChartID1ac0415a2a47 () {
## var data = new google.visualization.DataTable();
## var datajson =
## [
##  [
##  "AL",
## 203348.99 
## ],
## [
##  "AK",
## 27301.46 
## ],
## [
##  "AZ",
## 56879.74 
## ],
## [
##  "AR",
## 230846.365 
## ],
## [
##  "CA",
## 150197.695 
## ],
## [
##  "CO",
## 38601.52 
## ],
## [
##  "FL",
## 143909.585 
## ],
## [
##  "CT",
## 21245.85 
## ],
## [
##  "DE",
## 13460.27 
## ],
## [
##  "DC",
## 2272.5 
## ],
## [
##  "GA",
## 315617.12 
## ],
## [
##  "ID",
## 19678.09 
## ],
## [
##  "IL",
## 222362.52 
## ],
## [
##  "HI",
## 3727.315 
## ],
## [
##  "IN",
## 136735.73 
## ],
## [
##  "IA",
## 417490.865 
## ],
## [
##  "KS",
## 220007.77 
## ],
## [
##  "KY",
## 143081.27 
## ],
## [
##  "LA",
## 162932.305 
## ],
## [
##  "MD",
## 48215.56 
## ],
## [
##  "MI",
## 125163.74 
## ],
## [
##  "MN",
## 129098.87 
## ],
## [
##  "ME",
## 45053.5 
## ],
## [
##  "MA",
## 50708.6 
## ],
## [
##  "MO",
## 165944.355 
## ],
## [
##  "MT",
## 26097.06 
## ],
## [
##  "MS",
## 346126.845 
## ],
## [
##  "NE",
## 257112.95 
## ],
## [
##  "NH",
## 35589.15 
## ],
## [
##  "NJ",
## 50774.3 
## ],
## [
##  "NM",
## 34140.5 
## ],
## [
##  "NY",
## 194614.985 
## ],
## [
##  "NV",
## 18348.34 
## ],
## [
##  "NC",
## 131228.09 
## ],
## [
##  "OH",
## 320992.75 
## ],
## [
##  "ND",
## 123070.51 
## ],
## [
##  "OK",
## 128347.9 
## ],
## [
##  "OR",
## 14548.55 
## ],
## [
##  "PA",
## 190191.95 
## ],
## [
##  "SC",
## 66462.45 
## ],
## [
##  "SD",
## 76425.86 
## ],
## [
##  "RI",
## 6268.4 
## ],
## [
##  "TN",
## 171295.5 
## ],
## [
##  "TX",
## 543128.885 
## ],
## [
##  "VT",
## 57038.2 
## ],
## [
##  "VA",
## 101799.945 
## ],
## [
##  "WV",
## 92526.6 
## ],
## [
##  "UT",
## 25307.86 
## ],
## [
##  "WA",
## 62304.895 
## ],
## [
##  "WI",
## 178318.23 
## ],
## [
##  "WY",
## 26107.34 
## ],
## [
##  "VI",
## 1293.9 
## ],
## [
##  "AS",
## 2151.5 
## ],
## [
##  "GU",
## 8453.575 
## ],
## [
##  "PR",
## 22211.84 
## ],
## [
##  "LC",
## 0 
## ],
## [
##  "PH",
## 0 
## ],
## [
##  "GM",
## 606.04 
## ],
## [
##  "PZ",
## 76 
## ],
## [
##  "AM",
## 5678.8 
## ],
## [
##  "AN",
## 294 
## ],
## [
##  "LH",
## 0 
## ],
## [
##  "LM",
## 1633.1 
## ],
## [
##  "LE",
## 30 
## ],
## [
##  "LS",
## 400 
## ],
## [
##  "SL",
## 15 
## ],
## [
##  "LO",
## 70 
## ],
## [
##  "PM",
## 0 
## ],
## [
##  "PK",
## 31 
## ],
## [
##  "XX",
## 0 
## ] 
## ];
## data.addColumn('string','STATE');
## data.addColumn('number','DollarDamages');
## data.addRows(datajson);
## return(data);
## }
##  
## // jsDrawChart
## function drawChartGeoChartID1ac0415a2a47() {
## var data = gvisDataGeoChartID1ac0415a2a47();
## var options = {};
## options["width"] =    600;
## options["height"] =    400;
## options["region"] = "US";
## options["displayMode"] = "regions";
## options["resolution"] = "provinces";
## 
## 
##     var chart = new google.visualization.GeoChart(
##     document.getElementById('GeoChartID1ac0415a2a47')
##     );
##     chart.draw(data,options);
##     
## 
## }
##   
##  
## // jsDisplayChart
## (function() {
## var pkgs = window.__gvisPackages = window.__gvisPackages || [];
## var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
## var chartid = "geochart";
##   
## // Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
## var i, newPackage = true;
## for (i = 0; newPackage && i < pkgs.length; i++) {
## if (pkgs[i] === chartid)
## newPackage = false;
## }
## if (newPackage)
##   pkgs.push(chartid);
##   
## // Add the drawChart function to the global list of callbacks
## callbacks.push(drawChartGeoChartID1ac0415a2a47);
## })();
## function displayChartGeoChartID1ac0415a2a47() {
##   var pkgs = window.__gvisPackages = window.__gvisPackages || [];
##   var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
##   window.clearTimeout(window.__gvisLoad);
##   // The timeout is set to 100 because otherwise the container div we are
##   // targeting might not be part of the document yet
##   window.__gvisLoad = setTimeout(function() {
##   var pkgCount = pkgs.length;
##   google.load("visualization", "1", { packages:pkgs, callback: function() {
##   if (pkgCount != pkgs.length) {
##   // Race condition where another setTimeout call snuck in after us; if
##   // that call added a package, we must not shift its callback
##   return;
## }
## while (callbacks.length > 0)
## callbacks.shift()();
## } });
## }, 100);
## }
##  
## // jsFooter
## </script>
##  
## <!-- jsChart -->  
## <script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartGeoChartID1ac0415a2a47"></script>
##  
## <!-- divChart -->
##   
## <div id="GeoChartID1ac0415a2a47" 
##   style="width: 600; height: 400;">
## </div>
```
