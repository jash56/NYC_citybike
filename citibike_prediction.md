---
title: "CitiBike Data Analysis"
author:
  - Rishikesh Gawade (rkg63)
  - Jash Gaglani (jg1700)
  - Rohit Upadhyay (rru9)
  - Ekta Dhobley (ed560)
  - Advait Bhat (ab2253)
  - Hardik Singh (hs1001)
output:
  html_document:
    highlight: pygments
    theme: cerulean
    code_folding: show
    toc: no
    toc_float: yes
    toc_depth: 3
    keep_md: yes
    md_extensions: +grid_tables
classoption: portrait
urlcolor: blue
linkcolor: blue
editor_options:
  chunk_output_type: inline
header-includes: 
- \usepackage{graphicx}
- \usepackage{float}
---

<style>
  .main-container {
    max-width: 1200px !important;
  }
</style>

---



# Loading Required Libraries

```r
library(dplyr)
library(ggplot2)
library(GGally)
library(lubridate)
library(caret)
library(gbm)
library(tidyverse)
library(caret)
```

\newpage

The NYC "CitiBike" bicycle sharing scheme went live (in midtown and downtown Manhattan) in 2013, and has been expanding ever since, both as measured by daily ridership as well as the expanding geographic footprint incorporating a growing number of "docking stations" as the system welcomes riders in Brooklyn, Queens, and northern parts of Manhattan which were not previously served.

One problem that many bikeshare systems face is money. An increase in the number of riders who want to use the system necessitates that more bikes be purchased and put into service in order to accommodate them. Heavy ridership induces wear on the bikes, requiring for more frequent repairs. However, an increase in the number of trips does not necessarily translate to an increase in revenue because riders who are clever can avoid paying surcharges by keeping the length of each trip below a specified limit (either 30 or 45 minutes, depending on user category.)

We seek to examine CitiBike ridership data, joined with daily NYC weather data, to study the impact of weather on shared bike usage and generate a predictive model which can estimate the trip duration.  
The goal is to estimate future demand which would enable the system operator to make expansion plans.

Our finding is that ridership exhibits strong seasonality, with correlation to weather-related variables such as daily temperature and precipitation. Additionally, ridership is segmented by by user_type (annual subscribers use the system much more heavily than casual users), gender (there are many more male users than female) and age (a large number of users are clustered in their late 30s).

# Introduction 
Since 2013 a shared bicycle system known as [CitiBike](http://www.citibikenyc.com) has been available in New York City. The benefits to having such a system include reducing New Yorkers' dependence on automobiles and encouraging public health through the exercise attained by cycling. Additionally, users who would otherwise spend money on public transit may find bicycling more economical -- so long as they are aware of CitiBike's pricing constraints.   

There are currently about 12,000 shared bikes which users can rent from about 750 [docking stations](https://member.citibikenyc.com/map/) located in Manhattan and in western portions of Brooklyn and Queens.  A rider can pick up a bike at one station and return it at a different station. The system has been expanding each year, with increases in the number of bicycles available and expansion of the geographic footprint of docking stations. For planning purposes, the system operator needs to project future ridership in order to make good investments.

The available usage data provides a wealth of information which can be mined to seek trends in usage. With such intelligence, the company would be better positioned to determine what actions might optimize its revenue stream.

# 3. Data Gathering 

## Data sources and uploading

We obtained data from the following sources:  

### 1. CitiBike trip dataset

CitiBike makes a vast amount of [data](https://www.citibikenyc.com/system-data) available regarding system usage as well as sales of memberships and short-term passes.  

For [each month](https://s3.amazonaws.com/tripdata/index.html) since the system's inception, there is a file containing details of (almost) every trip.  (Certain "trips" are omitted from the dataset. For example, if a user checks out a bike from a dock but then returns it within one minute, the system drops such a "trip" from the listing, as such "trips" are not interesting.)

There are currently 108 monthly data files for the New York City bikeshare system, spanning July 2013 through December 2019.  Each file contains a line for every trip. The number of trips per month varies from as few as 200,000 during winter months in the system's early days to more than 2 million trips this past summer.
Because of the computational limitations which this presented, we created samples of 1/1000.  The samples were created non-deterministically, by randomly selecteing 'r nrow(file)/1000' from the file.  


```r
citibike_2019 <- read.csv("citibike_2019.csv")
citibike_2019 <- citibike_2019[sample(nrow(citibike_2019), 20000),]
```

### 2. Central Park daily weather data

We also obtained historical weather information for the year 2019 from the NCDC (National Climatic Data Center) by submitting an online request to https://www.ncdc.noaa.gov/cdo-web/search. Although the weather may vary slightly within New York City, we opted to use just the data associated with the Central Park observations as proxy for the entire city's weather.

We believe that the above data provides a reasonable representation of the target population (all CitiBike rides) and the citywide weather.


```r
weather <- read.csv("2812766.csv",header=FALSE)
names(weather)<-weather[1,]
weather<-weather[-1,]
weather<-subset(weather[weather$NAME == "NY CITY CENTRAL PARK, NY US" & year(weather$DATE) == 2019,],)
attr_indexes <- names(weather) %>% grep("ATTR",x = .)
weather <- weather[,-attr_indexes]
weather <- weather[,-c(1:5)]
weather<-weather %>%mutate(across(c(where(is.character), -DATE), as.numeric))%>%mutate_all(~replace_na(., 0))
weather$DATE<-as.Date(weather$DATE)
weather <- weather %>% select("DATE","PRCP","SNOW","SNWD","TMAX","TMIN","WT01","WDF2","WDF5","WSF2","WSF5","WT08")
head(weather)
```

```
##             DATE PRCP SNOW SNWD TMAX TMIN WT01 WDF2 WDF5 WSF2 WSF5 WT08
## 58385 2019-01-01 0.06    0    0   58   39    1    0    0    0    0    0
## 58386 2019-01-02 0.00    0    0   40   35    0    0    0    0    0    0
## 58387 2019-01-03 0.00    0    0   44   37    0    0    0    0    0    0
## 58388 2019-01-04 0.00    0    0   47   35    0    0    0    0    0    0
## 58389 2019-01-05 0.50    0    0   47   41    1    0    0    0    0    0
## 58390 2019-01-06 0.00    0    0   49   31    0    0    0    0    0    0
```

PRCP = Precipitation (mm or inches as per user preference, inches to hundredths on Daily Form pdf file)
SNOW = Snowfall (mm or inches as per user preference, inches to tenths on Daily Form pdf file)
SNWD = Snow depth (mm or inches as per user preference, inches on Daily Form pdf file
WT01 = Fog, ice fog, or freezing fog (may include heavy fog)
WT08 = Smoke or haze 
WDF2 = Direction of fastest 2-minute wind (degrees)
WDF5 = Direction of fastest 5-second wind (degrees)
WSF2 = Fastest 2-minute wind speed (miles per hour or meters per second as per user preference
WSF5 = Fastest 5-second wind speed (miles per hour or meters per second as per user preference)

## Description of Data    

In this section, we examine selected individual variables from the CitiBike and Weather datasets.  These items require transformation and/or cleaning as there are missing values or outliers which impede analysis otherwise.


```r
print(paste0("The citibike data consists of ", nrow(citibike_2019), " data points spread across ", ncol(citibike_2019), " features"))
```

```
## [1] "The citibike data consists of 20000 data points spread across 15 features"
```

#### Examine variable **trip_duration**:    
The trip_duration is specified in seconds, but there are some outliers which may be incorrect, as the value for Max is quite high:  1.530568\times 10^{6} seconds, or 17.7149074 days.  We can assume that this data is bad, as nobody would willingly rent a bicycle for this period of time, given the fees that would be charged.  Here is a histogram of the original data distribution:

```r
plot(citibike_2019$tripduration/60) 
```

![](citibike_prediction_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### Delete cases with unreasonable trip_duration values
Let's assume that nobody would rent a bicycle for more than a specified timelimit (say, 3 hours), and drop any records which exceed this:

```r
num_long_trips_removed<- nrow(citibike_2019[citibike_2019$tripduration > 7200,])
citibike_2019<-citibike_2019[citibike_2019$tripduration<=7200,]
print(paste0("Removed ", num_long_trips_removed, " trips of longer than 2 hours."))
```

```
## [1] "Removed 51 trips of longer than 2 hours."
```


#### Examine birth_year
Other inconsistencies concern the collection of birth_year, from which we can infer the age of the participant.  There are some months in which this value is omitted, while there are other months in which all values are populated.  However, there are a few records which suggest that the rider is a centenarian -- it seems highly implausible that someone born in the 1880s is cycling around Central Park -- but the data does have such anomalies.  Thus, a substantial amount of time was needed for detecting and cleaning such inconsistencies.

The birth year for some users is as old as 1888, which is not possible:

```r
summary(citibike_2019$birth.year)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1888    1970    1983    1980    1990    2003
```

```r
citibike_2019$age <- 2019 - citibike_2019$birth.year
```

#### Remove trips associated with very old users (age>90)

```r
num_old_age_removed<- nrow(citibike_2019[citibike_2019$age>90,])
citibike_2019<-citibike_2019[citibike_2019$age<90,]
print(paste0("Removed ", num_old_age_removed, " trips of people older thatn 90 years"))
```

```
## [1] "Removed 7 trips of people older thatn 90 years"
```



#### Compute distance between start and end stations 

```r
library(geosphere)
citibike_2019$distance <- distHaversine(citibike_2019[,6:7], citibike_2019[,10:11])
citibike_2019$dist.lat <- abs((citibike_2019$start.station.latitude - citibike_2019$end.station.latitude))
citibike_2019$dist.long <- abs((citibike_2019$start.station.longitude - citibike_2019$end.station.longitude))
summary(citibike_2019$distance)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0     571    1039    1330    1764   10725
```

# Data Wrangling

```r
is_weekday = function(timestamp){
  lubridate::wday(timestamp, week_start = 1) < 6
}
```

## Feature Extraction
Add columns for the hour when the trip started, day of the week and if it was a week day

```r
citibike_2019$start_date<-as.Date(citibike_2019$starttime)
citibike_2019$Hour<-hour(citibike_2019$starttime)
citibike_2019$dayofweek <- as.factor(wday(citibike_2019$starttime))
citibike_2019$weekday<-as.factor(as.numeric(sapply(citibike_2019$starttime, is_weekday)))
head(citibike_2019 %>% select("Hour","dayofweek","weekday"))
```

```
##        Hour dayofweek weekday
## 18119     8         7       0
## 179484   19         1       0
## 200023   13         7       0
## 88344    19         5       1
## 178658   13         7       0
## 181364   14         4       1
```

## Convert into factor variables

```r
citibike_2019$usertype<-as.factor(citibike_2019$usertype)
citibike_2019$gender<-as.factor(citibike_2019$gender)
```

## Convert trip duration from seconds to minutes

```r
citibike_2019$tripduration<-floor(citibike_2019$tripduration/60)
head(citibike_2019$tripduration)
```

```
## [1] 12  1 11  8 13 17
```

## Join Citibike and Weather data

```r
citibike_2019 <- citibike_2019 %>% inner_join(weather, by = c("start_date" = "DATE" ))
head(citibike_2019)
```

```
##   tripduration                starttime                 stoptime
## 1           12 2019-02-16 08:07:24.9170 2019-02-16 08:19:34.9190
## 2            1 2019-10-13 19:41:37.1900 2019-10-13 19:42:52.1920
## 3           11 2019-12-21 13:26:47.9320 2019-12-21 13:38:44.9870
## 4            8 2019-06-06 19:45:11.9330 2019-06-06 19:53:40.7360
## 5           13 2019-10-19 13:08:21.8810 2019-10-19 13:21:24.0490
## 6           17 2019-11-27 14:12:42.4400 2019-11-27 14:30:12.9820
##   start.station.id        start.station.name start.station.latitude
## 1              327  Vesey Pl & River Terrace               40.71534
## 2              439            E 4 St & 2 Ave               40.72628
## 3             2003           1 Ave & E 18 St               40.73381
## 4               72          W 52 St & 11 Ave               40.76727
## 5             3686 Gansevoort St & Hudson St               40.73945
## 6              532           S 5 Pl & S 5 St               40.71045
##   start.station.longitude end.station.id              end.station.name
## 1               -74.01658           3260       Mercer St & Bleecker St
## 2               -73.98978            439                E 4 St & 2 Ave
## 3               -73.98054            487           E 20 St & FDR Drive
## 4               -73.99393            520               W 52 St & 5 Ave
## 5               -74.00507            229                Great Jones St
## 6               -73.96088           3046 Marcus Garvey Blvd & Macon St
##   end.station.latitude end.station.longitude bikeid   usertype birth.year
## 1             40.72706             -73.99662  32241 Subscriber       1971
## 2             40.72628             -73.98978  40540 Subscriber       1991
## 3             40.73314             -73.97574  40755 Subscriber       1963
## 4             40.75992             -73.97649  16127 Subscriber       1986
## 5             40.72743             -73.99379  18097 Subscriber       1982
## 6             40.68260             -73.93804  41111 Subscriber       1980
##   gender age  distance    dist.lat   dist.long start_date Hour dayofweek
## 1      2  48 2251.0936 0.011725733 0.019962170 2019-02-16    8         7
## 2      1  28    0.0000 0.000000000 0.000000000 2019-10-13   19         1
## 3      1  56  535.3301 0.000669602 0.004805399 2019-12-21   13         7
## 4      1  33 1954.9004 0.007349540 0.017443720 2019-06-06   19         5
## 5      1  37 1308.6882 0.012013971 0.011280079 2019-10-19   13         7
## 6      1  39 2683.0345 0.027850000 0.022839000 2019-11-27   14         4
##   weekday PRCP SNOW SNWD TMAX TMIN WT01 WDF2 WDF5 WSF2 WSF5 WT08
## 1       0 0.00    0    0   47   32    0    0    0  0.0  0.0    0
## 2       0 0.00    0    0   67   53    0   50  280  8.1 12.1    0
## 3       0 0.00    0    0   35   23    0   20   10 13.0 19.9    0
## 4       1 0.04    0    0   83   68    1   50  270 12.1 21.0    0
## 5       0 0.00    0    0   60   43    0  240  240  8.9 13.0    0
## 6       1 0.04    0    0   59   49    0  280  260 21.9 35.1    0
```






## Correlations of individual trip data features

We can examine the correlations between variables to understand the relationship between variables, and also to help be alert to potential problems of multicollinearity.  Here we compute rank correlations (Pearson and Spearman) as well as actual correlations between key variables.   Here we compute the correlations between key variables on the individual CitiBike Trip data




![](citibike_prediction_files/figure-html/pearson-rank-correl-by-ride-1.png)<!-- -->


![](citibike_prediction_files/figure-html/spearman-rank-correl-by-ride-1.png)<!-- -->

![](citibike_prediction_files/figure-html/act-correlations-by-ride-1.png)<!-- -->

## Train - Test Split

```r
smp_size<- floor(0.8*nrow(citibike_2019))
set.seed(123)
train_index<-sample(seq_len(nrow(citibike_2019)), size = smp_size)
train_data <- citibike_2019[train_index,]
test_data <-  citibike_2019[- train_index,]
rm(citibike_2019)
```



# Prediction
## Linear Model

### Determine the columns to use for prediction

```r
colstouse <-setdiff(names(train_data),c("starttime","start.station.id","end.station.id",
                                           "stoptime","start.station.name","end.station.name",
                                           "start.station.longitude","start_date",
                                           "end.station.latitude","end.station.longitude",
                                           "weekday","bikeid","birth.year","PRCP","DATE","SNOW","SNWD",
                                        "TMIN","WT01","WDF2","WDF5","WSF2","WSF5","WT08"))
```


```r
X_train<-train_data[ , which(names(train_data) %in% colstouse)]
names(X_train)
```

```
##  [1] "tripduration"           "start.station.latitude" "usertype"              
##  [4] "gender"                 "age"                    "distance"              
##  [7] "dist.lat"               "dist.long"              "Hour"                  
## [10] "dayofweek"              "TMAX"
```

```r
linear_model<-lm(tripduration~., data = X_train)
```


```r
summary(linear_model)
```

```
## 
## Call:
## lm(formula = tripduration ~ ., data = X_train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.977  -3.450  -1.788   0.898 102.388 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -2.924e+02  8.744e+01  -3.344 0.000828 ***
## start.station.latitude  7.413e+00  2.147e+00   3.453 0.000556 ***
## usertypeSubscriber     -7.510e+00  2.253e-01 -33.337  < 2e-16 ***
## gender1                -1.339e+00  3.048e-01  -4.392 1.13e-05 ***
## gender2                -1.713e-01  3.189e-01  -0.537 0.591105    
## age                     3.127e-02  5.618e-03   5.565 2.66e-08 ***
## distance                1.181e-02  7.348e-04  16.078  < 2e-16 ***
## dist.lat                2.583e+02  1.152e+01  22.425  < 2e-16 ***
## dist.long              -9.370e+02  7.660e+01 -12.231  < 2e-16 ***
## Hour                    6.009e-02  1.325e-02   4.536 5.76e-06 ***
## dayofweek2             -1.755e+00  2.563e-01  -6.847 7.80e-12 ***
## dayofweek3             -1.760e+00  2.532e-01  -6.951 3.77e-12 ***
## dayofweek4             -1.861e+00  2.536e-01  -7.336 2.30e-13 ***
## dayofweek5             -1.555e+00  2.544e-01  -6.111 1.01e-09 ***
## dayofweek6             -1.381e+00  2.528e-01  -5.461 4.81e-08 ***
## dayofweek7             -2.590e-01  2.562e-01  -1.011 0.312023    
## TMAX                    2.423e-02  3.954e-03   6.128 9.11e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.126 on 15936 degrees of freedom
## Multiple R-squared:  0.4714,	Adjusted R-squared:  0.4709 
## F-statistic: 888.4 on 16 and 15936 DF,  p-value: < 2.2e-16
```

## Stochastic Gradient Boosting Model

```r
colstouse <-setdiff(names(train_data),c("starttime","start.station.id","end.station.id",
                                           "stoptime","start.station.name","end.station.name",
                                           "start.station.longitude","start_date",
                                           "end.station.latitude","end.station.longitude",
                                           "weekday","bikeid","birth.year","DATE","SNOW","SNWD"))
X_train<-train_data[ , which(names(train_data) %in% colstouse)]
names(X_train)
```

```
##  [1] "tripduration"           "start.station.latitude" "usertype"              
##  [4] "gender"                 "age"                    "distance"              
##  [7] "dist.lat"               "dist.long"              "Hour"                  
## [10] "dayofweek"              "PRCP"                   "TMAX"                  
## [13] "TMIN"                   "WT01"                   "WDF2"                  
## [16] "WDF5"                   "WSF2"                   "WSF5"                  
## [19] "WT08"
```



```r
library('gbm')

caretGrid <- expand.grid(interaction.depth=c(5,7,9,11), 
                         n.trees = (0:30)*50,
                         shrinkage=c(0.01),
                         n.minobsinnode=20)

trainControl <- trainControl(method="cv", number=10)

metric <- "RMSE"

gbmmodel <- train(tripduration ~ ., 
                 data = X_train,
                 distribution = "gaussian",
                 method = "gbm", 
                 trControl = trainControl,
                 tuneGrid=caretGrid,
                 metric=metric, 
                 bag.fraction=0.75,
                 verbose = F)
```


```r
gbmmodel
```

```
## Stochastic Gradient Boosting 
## 
## 15947 samples
##    20 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 14352, 14352, 14353, 14352, 14351, 14353, ... 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.trees  RMSE      Rsquared   MAE     
##   5                  150      8.396824  0.4842752  5.071274
##   5                  300      7.969259  0.5036105  4.570354
##   5                  500      7.841625  0.5128264  4.408715
##   9                  150      8.233405  0.4984280  4.921338
##   9                  300      7.876072  0.5122470  4.472726
##   9                  500      7.791592  0.5180836  4.351670
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.01
## 
## Tuning parameter 'n.minobsinnode' was held constant at a value of 20
## RMSE was used to select the optimal model using the smallest value.
## The final values used for the model were n.trees = 500, interaction.depth =
##  9, shrinkage = 0.01 and n.minobsinnode = 20.
```


```r
summary(gbmmodel)
```

![](citibike_prediction_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```
##                                           var      rel.inf
## dist.lat                             dist.lat 54.316202421
## distance                             distance 19.083842261
## usertypeSubscriber         usertypeSubscriber 14.977561106
## start.station.latitude start.station.latitude  3.432306201
## dist.long                           dist.long  2.387969483
## Hour                                     Hour  1.229529693
## gender1                               gender1  1.011399331
## age                                       age  0.866055486
## WDF5                                     WDF5  0.488515892
## WSF5                                     WSF5  0.436981549
## TMAX                                     TMAX  0.426249924
## WDF2                                     WDF2  0.280782470
## dayofweek7                         dayofweek7  0.274624728
## dayofweek4                         dayofweek4  0.214640256
## PRCP                                     PRCP  0.160037537
## TMIN                                     TMIN  0.153969439
## WSF2                                     WSF2  0.135890022
## gender2                               gender2  0.069431631
## dayofweek2                         dayofweek2  0.021243737
## WT08                                     WT08  0.015523094
## WT01                                     WT01  0.009623772
## dayofweek6                         dayofweek6  0.004684990
## dayofweek5                         dayofweek5  0.002934976
## dayofweek3                         dayofweek3  0.000000000
## SNOW                                     SNOW  0.000000000
## SNWD                                     SNWD  0.000000000
```

```r
trellis.par.set(caretTheme())
plot(gbmmodel)  
```

![](citibike_prediction_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
trellis.par.set(caretTheme())
plot(gbmmodel, metric = "RMSE", plotType = "level",
     scales = list(x = list(rot = 90)))
```

![](citibike_prediction_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
trellis.par.set(caretTheme())
densityplot(gbmmodel, pch = "|")
```

![](citibike_prediction_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
### Evaluate Performance on the train data

```r
y_test<-train_data$tripduration
predicted = predict(gbmmodel,train_data)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the train data is ', round(RMSE,3),'\n')
```

```
## The root mean square error of the train data is  7.702
```


```r
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the train data is ', round(rsq,3), '\n')
```

```
## The R-square of the train data is  0.525
```


```r
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                            observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
    geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Gradient Boosting Machine: Prediction vs Test Data") +
      xlab("Predecited Trip Duration in Seconds ") + ylab("Observed Trip Duration in Seconds") + 
        theme(plot.title = element_text(size=16,hjust = 0.5),
         axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
         axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](citibike_prediction_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

### Evaluate Performance on the test data


```r
y_test<-test_data$tripduration
predicted = predict(gbmmodel,test_data)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')
```

```
## The root mean square error of the test data is  7.863
```


```r
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')
```

```
## The R-square of the test data is  0.496
```


```r
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                            observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
    geom_smooth(method=lm)+ ggtitle('Gradient Boosted Machines ') + ggtitle("Gradient Boosting Machines: Prediction vs Test Data") +
      xlab("Predecited Trip Duration in Minutes ") + ylab("Observed Trip Duration in Minutes") + 
        theme(plot.title = element_text(size=16,hjust = 0.5),
         axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
         axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](citibike_prediction_files/figure-html/unnamed-chunk-25-1.png)<!-- -->


## Extreme Gradient Boosting (xgboost)


```r
colstouse <-setdiff(names(train_data),c("starttime","start.station.id","end.station.id",
                                           "stoptime","start.station.name","end.station.name",
                                           "start.station.longitude","start_date",
                                           "end.station.latitude","end.station.longitude",
                                           "weekday","bikeid","birth.year","DATE"))
X_train<-train_data[ , which(names(train_data) %in% colstouse)]
X_train<-X_train %>%mutate(across(c(where(is.factor)), as.numeric))
summary(X_train)
```

```
##   tripduration    start.station.latitude    usertype         gender     
##  Min.   :  1.00   Min.   :40.66          Min.   :1.000   Min.   :1.000  
##  1st Qu.:  6.00   1st Qu.:40.72          1st Qu.:2.000   1st Qu.:2.000  
##  Median : 10.00   Median :40.74          Median :2.000   Median :2.000  
##  Mean   : 13.15   Mean   :40.74          Mean   :1.861   Mean   :2.167  
##  3rd Qu.: 17.00   3rd Qu.:40.76          3rd Qu.:2.000   3rd Qu.:2.000  
##  Max.   :115.00   Max.   :40.81          Max.   :2.000   Max.   :3.000  
##       age           distance          dist.lat          dist.long       
##  Min.   :16.00   Min.   :    0.0   Min.   :0.000000   Min.   :0.000000  
##  1st Qu.:29.00   1st Qu.:  571.2   1st Qu.:0.003851   1st Qu.:0.004050  
##  Median :36.00   Median : 1039.6   Median :0.008292   Median :0.008417  
##  Mean   :38.63   Mean   : 1333.5   Mean   :0.012059   Mean   :0.010988  
##  3rd Qu.:49.00   3rd Qu.: 1769.4   3rd Qu.:0.016019   3rd Qu.:0.014980  
##  Max.   :84.00   Max.   :10724.6   Max.   :0.148076   Max.   :0.096340  
##       Hour         dayofweek          PRCP             SNOW        
##  Min.   : 0.00   Min.   :1.000   Min.   :0.0000   Min.   :0.00000  
##  1st Qu.:10.00   1st Qu.:2.000   1st Qu.:0.0000   1st Qu.:0.00000  
##  Median :15.00   Median :4.000   Median :0.0000   Median :0.00000  
##  Mean   :13.92   Mean   :4.088   Mean   :0.1011   Mean   :0.01892  
##  3rd Qu.:18.00   3rd Qu.:6.000   3rd Qu.:0.0400   3rd Qu.:0.00000  
##  Max.   :23.00   Max.   :7.000   Max.   :1.8300   Max.   :4.00000  
##       SNWD              TMAX            TMIN           WT01       
##  Min.   :0.00000   Min.   :14.00   Min.   : 2.0   Min.   :0.0000  
##  1st Qu.:0.00000   1st Qu.:57.00   1st Qu.:42.0   1st Qu.:0.0000  
##  Median :0.00000   Median :72.00   Median :56.0   Median :0.0000  
##  Mean   :0.02846   Mean   :68.23   Mean   :53.7   Mean   :0.3993  
##  3rd Qu.:0.00000   3rd Qu.:81.00   3rd Qu.:67.0   3rd Qu.:1.0000  
##  Max.   :3.90000   Max.   :95.00   Max.   :82.0   Max.   :1.0000  
##       WDF2            WDF5            WSF2            WSF5      
##  Min.   :  0.0   Min.   :  0.0   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 50.0   1st Qu.: 50.0   1st Qu.: 8.90   1st Qu.:15.00  
##  Median :160.0   Median :180.0   Median :12.10   Median :19.00  
##  Mean   :161.9   Mean   :162.4   Mean   :11.41   Mean   :18.45  
##  3rd Qu.:270.0   3rd Qu.:260.0   3rd Qu.:14.10   3rd Qu.:23.00  
##  Max.   :360.0   Max.   :360.0   Max.   :25.10   Max.   :40.90  
##       WT08       
##  Min.   :0.0000  
##  1st Qu.:0.0000  
##  Median :0.0000  
##  Mean   :0.1548  
##  3rd Qu.:0.0000  
##  Max.   :1.0000
```


Convert the training and testing sets into DMatrixes: DMatrix is the recommended class in xgboost.

```r
library(xgboost)
```

```
## 
## Attaching package: 'xgboost'
```

```
## The following object is masked from 'package:dplyr':
## 
##     slice
```

```r
X_train<-as.matrix(X_train %>% select(-tripduration))
Y_train<-train_data$tripduration
```


```r
xgb_trcontrol = trainControl(
  method = "cv",
  number = 10,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200,300),
                       max_depth = c(5,10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
                      )

xgb_model = train(
  X_train, Y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)
```


```r
print(xgb_model)
```

```
## eXtreme Gradient Boosting 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 14353, 14352, 14352, 14352, 14353, 14352, ... 
## Resampling results across tuning parameters:
## 
##   max_depth  colsample_bytree  nrounds  RMSE      Rsquared   MAE     
##   10         0.5               100      7.990662  0.4939391  4.459054
##   10         0.5               200      8.028360  0.4898218  4.507921
##   10         0.6               100      7.985940  0.4950087  4.448261
##   10         0.6               200      8.031759  0.4901626  4.501365
##   10         0.7               100      8.031170  0.4907930  4.459901
##   10         0.7               200      8.073485  0.4865763  4.512910
##   10         0.8               100      8.089547  0.4845796  4.470737
##   10         0.8               200      8.128734  0.4808463  4.529494
##   10         0.9               100      8.091202  0.4850372  4.487502
##   10         0.9               200      8.134691  0.4808676  4.546471
##   15         0.5               100      8.115434  0.4767399  4.671796
##   15         0.5               200      8.120068  0.4761192  4.679978
##   15         0.6               100      8.100678  0.4797061  4.611761
##   15         0.6               200      8.106604  0.4790475  4.625847
##   15         0.7               100      8.113880  0.4784341  4.584467
##   15         0.7               200      8.120708  0.4777263  4.598688
##   15         0.8               100      8.163480  0.4754049  4.565503
##   15         0.8               200      8.172585  0.4745141  4.583392
##   15         0.9               100      8.304776  0.4620645  4.618012
##   15         0.9               200      8.313216  0.4613428  4.634577
##   20         0.5               100      8.158981  0.4727551  4.739602
##   20         0.5               200      8.159012  0.4727186  4.741379
##   20         0.6               100      8.116777  0.4771562  4.648182
##   20         0.6               200      8.117489  0.4770717  4.650551
##   20         0.7               100      8.143830  0.4745711  4.608688
##   20         0.7               200      8.145033  0.4744563  4.611458
##   20         0.8               100      8.212076  0.4681641  4.600514
##   20         0.8               200      8.213553  0.4680448  4.604386
##   20         0.9               100      8.309599  0.4605969  4.640181
##   20         0.9               200      8.311563  0.4604600  4.644056
##   25         0.5               100      8.155933  0.4728047  4.748209
##   25         0.5               200      8.156126  0.4727438  4.750144
##   25         0.6               100      8.111947  0.4775316  4.639966
##   25         0.6               200      8.112506  0.4774562  4.642173
##   25         0.7               100      8.173949  0.4707071  4.607061
##   25         0.7               200      8.174838  0.4706242  4.609271
##   25         0.8               100      8.169826  0.4733180  4.595642
##   25         0.8               200      8.171277  0.4731924  4.598050
##   25         0.9               100      8.318991  0.4586380  4.632721
##   25         0.9               200      8.320909  0.4584981  4.635162
## 
## Tuning parameter 'eta' was held constant at a value of 0.1
## Tuning
##  parameter 'min_child_weight' was held constant at a value of 1
## 
## Tuning parameter 'subsample' was held constant at a value of 1
## RMSE was used to select the optimal model using the smallest value.
## The final values used for the model were nrounds = 100, max_depth = 10, eta
##  = 0.1, gamma = 0, colsample_bytree = 0.6, min_child_weight = 1 and subsample
##  = 1.
```


```r
summary(xgb_model)
```

```
##               Length  Class              Mode       
## handle              1 xgb.Booster.handle externalptr
## raw           4084628 -none-             raw        
## niter               1 -none-             numeric    
## call                5 -none-             call       
## params              8 -none-             list       
## callbacks           1 -none-             list       
## feature_names      20 -none-             character  
## nfeatures           1 -none-             numeric    
## xNames             20 -none-             character  
## problemType         1 -none-             character  
## tuneValue           7 data.frame         list       
## obsLevels           1 -none-             logical    
## param               0 -none-             list
```


```r
trellis.par.set(caretTheme())
plot(xgb_model)  
```

![](citibike_prediction_files/figure-html/unnamed-chunk-29-1.png)<!-- -->


```r
trellis.par.set(caretTheme())
plot(xgb_model, metric = "RMSE", plotType = "level",
     scales = list(x = list(rot = 90)))
```

![](citibike_prediction_files/figure-html/unnamed-chunk-30-1.png)<!-- -->


```r
trellis.par.set(caretTheme())
densityplot(xgb_model, pch = "|")
```

![](citibike_prediction_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

### Evaluate Performance on the train data

```r
y_test<-train_data$tripduration
predicted = predict(xgb_model,X_train)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the train data is ', round(RMSE,3),'\n')
```

```
## The root mean square error of the train data is  7.641
```


```r
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the train data is ', round(rsq,3), '\n')
```

```
## The R-square of the train data is  0.532
```


```r
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                            observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
    geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("XGBoost: Prediction vs Test Data") +
      xlab("Predecited Trip Duration in Seconds ") + ylab("Observed Trip Duration in Secons") + 
        theme(plot.title = element_text(size=16,hjust = 0.5),
         axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
         axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](citibike_prediction_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

### Evaluate Performance on the test data


```r
y_test<-test_data$tripduration
X_test<-test_data[ , which(names(test_data) %in% colstouse)]
X_test<-X_test %>%mutate(across(c(where(is.factor)), as.numeric))

predicted = predict(xgb_model,X_test)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')
```

```
## The root mean square error of the test data is  8
```


```r
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')
```

```
## The R-square of the test data is  0.478
```


```r
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                            observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
    geom_smooth(method=lm)+ ggtitle('Gradient Boosted Machines ') + ggtitle("XGBoost: Prediction vs Test Data") +
      xlab("Predecited Trip Duration in Minutes ") + ylab("Observed Trip Duration in Minutes") + 
        theme(plot.title = element_text(size=16,hjust = 0.5),
         axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
         axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](citibike_prediction_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

## Random Forest Model

```r
colstouse <-setdiff(names(train_data),c("starttime","start.station.id","end.station.id",
                                           "stoptime","start.station.name","end.station.name",
                                           "start.station.longitude","start_date",
                                           "end.station.latitude","end.station.longitude",
                                           "weekday","bikeid","birth.year","DATE","SNOW","SNWD"))
X_train<-train_data[ , which(names(train_data) %in% colstouse)]
names(X_train)
```

```
##  [1] "tripduration"           "start.station.latitude" "usertype"              
##  [4] "gender"                 "age"                    "distance"              
##  [7] "dist.lat"               "dist.long"              "Hour"                  
## [10] "dayofweek"              "PRCP"                   "TMAX"                  
## [13] "TMIN"                   "WT01"                   "WDF2"                  
## [16] "WDF5"                   "WSF2"                   "WSF5"                  
## [19] "WT08"
```


```r
library('randomForest')

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

#Metric compare model is Root Mean Squared Error
metric <- "RMSE"
set.seed(123)

#Number randomly variable selected is mtry
mtry <- sqrt(ncol(X_train)-1)
tunegrid <- expand.grid(.mtry=mtry)
rfmodel <- train(tripduration ~ ., 
                      data = X_train,
                      method='rf', 
                      metric=metric, 
                      tuneGrid=tunegrid, 
                      trControl=control)
```


```r
rfmodel
```

```
## Random Forest 
## 
## 15947 samples
##    18 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 3 times) 
## Summary of sample sizes: 14352, 14351, 14352, 14350, 14352, 14354, ... 
## Resampling results:
## 
##   RMSE      Rsquared   MAE     
##   7.874974  0.5089116  4.489987
## 
## Tuning parameter 'mtry' was held constant at a value of 4.242641
```


```r
summary(rfmodel)
```

```
##                 Length Class      Mode     
## call                4  -none-     call     
## type                1  -none-     character
## predicted       15947  -none-     numeric  
## mse               500  -none-     numeric  
## rsq               500  -none-     numeric  
## oob.times       15947  -none-     numeric  
## importance         24  -none-     numeric  
## importanceSD        0  -none-     NULL     
## localImportance     0  -none-     NULL     
## proximity           0  -none-     NULL     
## ntree               1  -none-     numeric  
## mtry                1  -none-     numeric  
## forest             11  -none-     list     
## coefs               0  -none-     NULL     
## y               15947  -none-     numeric  
## test                0  -none-     NULL     
## inbag               0  -none-     NULL     
## xNames             24  -none-     character
## problemType         1  -none-     character
## tuneValue           1  data.frame list     
## obsLevels           1  -none-     logical  
## param               0  -none-     list
```



```r
trellis.par.set(caretTheme())
densityplot(rfmodel, pch = "|")
```

![](citibike_prediction_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

### Evaluate Performance on the train data

```r
y_test<-train_data$tripduration
predicted = predict(rfmodel,train_data)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the train data is ', round(RMSE,3),'\n')
```

```
## The root mean square error of the train data is  7.559
```


```r
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the train data is ', round(rsq,3), '\n')
```

```
## The R-square of the train data is  0.542
```


```r
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                            observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
    geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Random Forest: Prediction vs Test Data") +
      xlab("Predecited Trip Duration in Seconds ") + ylab("Observed Trip Duration in Secons") + 
        theme(plot.title = element_text(size=16,hjust = 0.5),
         axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
         axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](citibike_prediction_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

### Evaluate Performance on the test data


```r
y_test<-test_data$tripduration
predicted = predict(gbmmodel,test_data)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')
```

```
## The root mean square error of the test data is  7.863
```


```r
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')
```

```
## The R-square of the test data is  0.496
```


```r
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                            observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
    geom_smooth(method=lm)+ ggtitle('Gradient Boosted Machines ') + ggtitle("Random Forest: Prediction vs Test Data") +
      xlab("Predecited Trip Duration in Minutes ") + ylab("Observed Trip Duration in Minutes") + 
        theme(plot.title = element_text(size=16,hjust = 0.5),
         axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
         axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](citibike_prediction_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

