NUSmodAn
================
Aaron0696

  - [Phase 1: Setting Up Environment, Packages And Loading
    Data.](#phase-1-setting-up-environment-packages-and-loading-data.)
      - [\>Packages And Options](#packages-and-options)
      - [\>\>Bidding Data From `nusmods`](#bidding-data-from-nusmods)
      - [\>\>Load `myBid.RDS`](#load-mybid.rds)
      - [\>Module Information](#module-information)
      - [\>\>Load `myModInfo.RDS`](#load-mymodinfo.rds)
  - [Phase 2: Filter, Transform And Merge
    Data](#phase-2-filter-transform-and-merge-data)
      - [\>`myModInfo`](#mymodinfo)
          - [\>\>Filter](#filter)
      - [\>`myBid`](#mybid)
          - [\>\>Filter](#filter-1)
      - [\>\>Transform And Merge](#transform-and-merge)
      - [\>Coercing Columns To
        Factors/Numeric](#coercing-columns-to-factorsnumeric)
      - [\>Vectors Of Column Names](#vectors-of-column-names)
      - [\>Rearranging `DayText` Levels](#rearranging-daytext-levels)
      - [\>Rearranging `LessonTime`
        Levels](#rearranging-lessontime-levels)
  - [Phase 3: Data Diagnostics](#phase-3-data-diagnostics)
      - [Univariate Descriptive
        Statistics](#univariate-descriptive-statistics)
      - [Univariate Histograms](#univariate-histograms)
      - [Bivariate Plots](#bivariate-plots)
          - [Categorical-Categorical](#categorical-categorical)
          - [Continuous-Continuous](#continuous-continuous)
          - [Correlation Matrix](#correlation-matrix)
          - [Continuous-Categorical](#continuous-categorical)
          - [By Module](#by-module)
  - [Phase 5: Exploration](#phase-5-exploration)

# Phase 1: Setting Up Environment, Packages And Loading Data.

## \>Packages And Options

  - Load packages.

<!-- end list -->

``` r
# rmarkdown::render(input = "NUSmodAn.Rmd",
#                   output_format = "github_document",
#                   output_file = "README.md")
library(semTools)
library(ggplot2)
library(rjson)
library(stringr)
library(DT)
library(psych)
library(corrplot)
library(dplyr)
options(width = 999)
knitr::opts_chunk$set(dpi = 300)
knitr::opts_chunk$set(out.width = "50%")
```

## \>\>Bidding Data From `nusmods`

  - Extract data from `nusmods` API at <https://nusmods.com/api/>.
  - CORS bidding data.

<!-- end list -->

``` r
# load bidding data
# calculate loading times
before <- Sys.time()
# read data directly from URL
myjson <- fromJSON(file = url("https://api.nusmods.com/corsBiddingStatsRaw.json"))
# create empty dataframe which will act as a container to be populated with data
myBid <- data.frame()
# for each element in the myjson list, append it to myBid
for(r in 1:length(myjson))
{
  if(myjson[[r]]$Semester == 1 | myjson[[r]]$Semester == 2)
  {
    myBid <- rbind(myBid, myjson[[r]])
  }
  myjson[[r]] <- NA
}
# calculate loading time
after <- Sys.time()
after - before
# remove myjson to free up some RAM
rm(myjson)
# peek at the data
head(myBid)
tail(myBid)
# data struct
str(myBid)
# save
saveRDS(myBid, file = "myBid.RDS")
```

## \>\>Load `myBid.RDS`

``` r
myBid <- readRDS("mydata.RDS")
```

## \>Module Information

``` r
myjson <- fromJSON(file = url("https://nusmods.com/api/moduleTimetableDeltaRaw.json"))
# create empty dataframe which will act as a container to be populated with data
myModInfo <- data.frame()
# for each element in the myjson list, append it to myBid
for(r in 1:length(myjson))
{
  if(myjson[[r]]$Semester == 1 | myjson[[r]]$Semester == 2)
  {
    myModInfo <- rbind(myModInfo, myjson[[r]])
  }
  myjson[[r]] <- NA
}

# save
saveRDS(myModInfo, file = "myModInfo.RDS")
```

## \>\>Load `myModInfo.RDS`

``` r
myModInfo <- readRDS("myModInfo.RDS")
```

# Phase 2: Filter, Transform And Merge Data

## \>`myModInfo`

  - Filter Module Information, `myModInfo`.
      - Removing non-Psychology modules.
      - Removing tutorial information.
      - Removing duplicated rows.

### \>\>Filter

``` r
# only keep the Psychology modules information
myModInfo <- subset(myModInfo,
                     str_detect(myModInfo$ModuleCode, "^PL"))
```

``` r
# remove information about tutorials
myModInfo <- subset(myModInfo,
                    myModInfo$LessonType != "TUTORIAL")
```

``` r
# only keep these columns
myModInfo <- myModInfo[,grep("ModuleCode|DayText|StartTime|Semester|AcadYear", names(myModInfo))]
# remove duplicated rows based on columns of ModuleCode, Acadyear and Semester
myModInfo <- distinct(myModInfo, 
                      ModuleCode, AcadYear, Semester, StartTime, DayText)
```

## \>`myBid`

  - Filter CORS Bidding Information, `myBid`.
      - Removing non-Psychology modules, including Roots and Wings (PLS)
        and Psychology for non-Psychology students (PLB).
      - Removing information from reserved modules.

### \>\>Filter

``` r
# remove non-psychology modules
myBid <- subset(myBid,
                 # only keep rows where module code begins with PL
                 str_detect(myBid$ModuleCode, "^PL"))

# also remove Roots and Wings (PLS8001) and psychology for non-psych students (PLB1201)
myBid <- subset(myBid,
                 !str_detect(myBid$ModuleCode, "PLS|PLB"))

# remove the rounds where it was reserved
myBid <- subset(myBid,
                     !str_detect(myBid$StudentAcctType, "Reserved"))

# remove unneeded columns
myBid <- myBid[, -grep("Group|Faculty", names(myBid))]
```

## \>\>Transform And Merge

  - Transform
      - Created a new variable `Level` that denotes whether the module
        is Level 1, 2, 3 or 4.
      - Created a new variable `BpQ` that represents Bids per Quota,
        which is the number of bidders for each available quota of the
        module, derived from `Bidders` and `Quota`. Used as a measure of
        the popularity of a module, Higher `BpQ` signifies greater
        popularity.
      - Created a new variable `LessonTime` that denotes whether the
        lecture begins in the morning (before 12pm), in the afternoon
        (12pm to 4pm), in the evening (after 4pm).
  - Merge
      - Add the information from `myModInfo` to `myBid`.

<!-- end list -->

``` r
# create new column that indicates the level of the module, based on their module code
myBid$Level <- ifelse(str_detect(myBid$ModuleCode, "1[0-9][0-9][0-9]"), "Level 1",
                       ifelse(str_detect(myBid$ModuleCode, "2[0-9][0-9][0-9]"), "Level 2",
                              ifelse(str_detect(myBid$ModuleCode, "3[0-9][0-9][0-9]"), "Level 3",
                                     ifelse(str_detect(myBid$ModuleCode, "4[0-9][0-9][0-9]"), "Level 4", 
                                            "Graduate Module"))))
# crosstabs to doublecheck
# xtabs( ~ ModuleCode + Level, 
#        data = myBid, subset = NULL)
```

``` r
# create new column Bids Per Quota (BpQ)
myBid$BpQ <- as.numeric(myBid$Bidders)/as.numeric(myBid$Quota)
```

``` r
# create new column Bids Per Quota (BpQ)
myModInfo$LessonTime <- ifelse(as.numeric(myModInfo$StartTime) < 1200, "Morning",
                            ifelse(as.numeric(myModInfo$StartTime) > 1600, "Evening",
                                   "Afternoon"))
```

``` r
# note: there is only module information from AY2016/17 onwards
# all data before that period will be dropped
mydata <- merge(x = myBid, 
                 y = myModInfo,
                 by = c("ModuleCode", "AcadYear", "Semester"))
```

## \>Coercing Columns To Factors/Numeric

``` r
# transform these columns to numeric
for(r in c("Quota", "Bidders", "LowestBid", "LowestSuccessfulBid", "HighestBid", "StartTime"))
{
  mydata[,grep(r, names(mydata))] <- as.numeric(mydata[,grep(r, names(mydata))])
}
# transform these columns to factors
for(r in c("AcadYear", "Semester", "ModuleCode", "Round", "Level", "StudentAcctType", "DayText", "LessonTime"))
{
  mydata[,grep(r, names(mydata))] <- factor(mydata[,grep(r, names(mydata))])
}
```

## \>Vectors Of Column Names

``` r
# create vector of the column names which are factors
facnames <- names(select_if(mydata, is.factor))
# factor names without ModuleCode and StudentAcctType
facnames.mod <- facnames[-grep("ModuleCode|StudentAcctType", facnames)]
# create vector of the column names which are numeric
numnames <- names(select_if(mydata, is.numeric))
# numeric names without StartTime
numnames.time <- names(select_if(mydata, is.numeric))[-grep("StartTime", numnames)]
```

## \>Rearranging `DayText` Levels

``` r
mydata$DayText <- factor(mydata$DayText,
                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
```

## \>Rearranging `LessonTime` Levels

``` r
mydata$LessonTime <- factor(mydata$LessonTime,
                         levels = c("Morning", "Afternoon", "Evening"))
```

# Phase 3: Data Diagnostics

  - Plot univariate histograms and bivariate plots using loops for
    **almost every** combination of variables.
  - The graphs from this section are predominantly for diagnostics
    rather than exploration, what I mean is that the graphs from this
    section would make little sense if one tried to draw insights from
    them. This is because they are aggregated across all other
    variables.
      - For example: The mean of `Bidders` is calculated across all
        academic years, all bidding rounds, all modules…
  - What I am looking out for in this section are odd patterns, like
    zeroes in places where they shouldn’t be, missing data, highly
    non-normal data, variables with outliers, etc…
  - Some observations and thoughts
      - The bidding statistics are highly non-normal, likely due to
        being bounded by zero (they cannot bid negative points or have
        negative bidders). May consider using zero-inflated or poisson
        regression if considering these statistics as dependent
        variables.

## Univariate Descriptive Statistics

``` r
describe(mydata)
```

    ##                     vars   n    mean     sd  median trimmed     mad min  max range  skew kurtosis    se
    ## ModuleCode*            1 654   23.37  21.27   14.00   21.66   19.27   1   61    60  0.54    -1.30  0.83
    ## AcadYear*              2 654    2.06   0.82    2.00    2.08    1.48   1    3     2 -0.11    -1.50  0.03
    ## Semester*              3 654    1.31   0.46    1.00    1.26    0.00   1    2     1  0.84    -1.29  0.02
    ## Round*                 4 654    3.55   2.02    3.00    3.45    2.97   1    7     6  0.19    -1.23  0.08
    ## Quota                  5 654   25.30  34.57   12.00   17.46   16.31   1  203   202  2.29     5.60  1.35
    ## Bidders                6 654   15.42  30.37    6.00    8.61    7.41   0  215   215  4.13    19.49  1.19
    ## LowestBid              7 654   71.65 186.78    1.00   22.55    0.00   0 1519  1519  4.14    21.56  7.30
    ## LowestSuccessfulBid    8 654  385.88 617.59    1.00  255.52    1.48   0 2700  2700  1.56     1.53 24.15
    ## HighestBid             9 654  905.76 896.33  700.00  797.31 1036.34   0 4140  4140  0.80    -0.10 35.05
    ## StudentAcctType*      10 654    4.24   1.68    5.00    4.33    1.48   1    7     6 -0.55    -0.48  0.07
    ## Level*                11 654    3.01   1.00    3.00    3.14    1.48   1    4     3 -0.90    -0.22  0.04
    ## BpQ                   12 654    1.52   2.29    0.68    0.99    0.97   0   18    18  2.66     8.76  0.09
    ## StartTime             13 654 1336.39 264.01 1400.00 1341.22  296.52 800 1800  1000 -0.19    -0.71 10.32
    ## DayText*              14 654    3.00   1.34    3.00    2.99    1.48   1    5     4 -0.05    -1.18  0.05
    ## LessonTime*           15 654    1.88   0.58    2.00    1.85    0.00   1    3     2  0.01    -0.13  0.02

``` r
summary(mydata)
```

    ##    ModuleCode       AcadYear   Semester Round        Quota          Bidders         LowestBid       LowestSuccessfulBid   HighestBid                                        StudentAcctType     Level          BpQ            StartTime         DayText        LessonTime 
    ##  PL1101E: 96   2016/2017:200   1:454    1A:158   Min.   :  1.0   Min.   :  0.00   Min.   :   0.00   Min.   :   0.0      Min.   :   0.0   New Students [P]                           : 75    Level 1: 96   Min.   : 0.0000   Min.   : 800   Monday   :116   Morning  :154  
    ##  PL4880L: 32   2017/2018:214   2:200    1B: 79   1st Qu.:  3.0   1st Qu.:  2.00   1st Qu.:   1.00   1st Qu.:   1.0      1st Qu.:  10.0   NUS Students [G]                           : 43    Level 2: 36   1st Qu.: 0.0878   1st Qu.:1200   Tuesday  :134   Afternoon:427  
    ##  PL3232 : 29   2018/2019:240            1C: 91   Median : 12.0   Median :  6.00   Median :   1.00   Median :   1.0      Median : 700.0   NUS Students [P, G]                        : 78    Level 3:285   Median : 0.6750   Median :1400   Wednesday:143   Evening  : 73  
    ##  PL3233 : 28                            2A: 97   Mean   : 25.3   Mean   : 15.42   Mean   :  71.65   Mean   : 385.9      Mean   : 905.8   NUS Students [P]                           : 61    Level 4:237   Mean   : 1.5223   Mean   :1336   Thursday :159                  
    ##  PL4235 : 28                            2B: 90   3rd Qu.: 35.0   3rd Qu.: 12.75   3rd Qu.:  20.00   3rd Qu.: 700.0      3rd Qu.:1529.8   Returning Students [P]                     :298                  3rd Qu.: 1.7500   3rd Qu.:1575   Friday   :102                  
    ##  PL4237 : 28                            3A: 77   Max.   :203.0   Max.   :215.00   Max.   :1519.00   Max.   :2700.0      Max.   :4140.0   Returning Students [P] and NUS Students [G]: 46                  Max.   :18.0000   Max.   :1800                                  
    ##  (Other):413                            3B: 62                                                                                           Returning Students and New Students [P]    : 53

## Univariate Histograms

``` r
# plot the categorical variables
# note: I did not include ModuleCode in this exploratory graph because it has too many levels (83)
for(r in facnames.mod)
{
  cat(paste0("Histogram Of ", r))
  
  plot(
    ggplot(data = mydata, aes_string(x = r, fill = r)) + 
  geom_histogram(stat = "count") + 
  ylab("Count") +
  ggtitle(paste0("Count of ", r)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 6, vjust = -0.3),
        axis.title.x = element_blank())
  )
}
```

    ## Histogram Of AcadYear

<img src="README_files/figure-gfm/explore1-1.png" width="50%" />

    ## Histogram Of Semester

<img src="README_files/figure-gfm/explore1-2.png" width="50%" />

    ## Histogram Of Round

<img src="README_files/figure-gfm/explore1-3.png" width="50%" />

    ## Histogram Of Level

<img src="README_files/figure-gfm/explore1-4.png" width="50%" />

    ## Histogram Of DayText

<img src="README_files/figure-gfm/explore1-5.png" width="50%" />

    ## Histogram Of LessonTime

<img src="README_files/figure-gfm/explore1-6.png" width="50%" />

``` r
# plot the continuous variables
for(r in numnames)
{
  cat(paste0("Histogram Of ", r))
  
  plot(
    ggplot(data = mydata, aes_string(x = r, fill = r)) + 
  geom_histogram(bins = 90, fill = "violetred") + 
  ylab("Histogram") +
  ggtitle(paste0("Frequency of ", r)) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, size = 6, vjust = -0.3),
        axis.title.x = element_text())
  )
}
```

    ## Histogram Of Quota

<img src="README_files/figure-gfm/explore2-1.png" width="50%" />

    ## Histogram Of Bidders

<img src="README_files/figure-gfm/explore2-2.png" width="50%" />

    ## Histogram Of LowestBid

<img src="README_files/figure-gfm/explore2-3.png" width="50%" />

    ## Histogram Of LowestSuccessfulBid

<img src="README_files/figure-gfm/explore2-4.png" width="50%" />

    ## Histogram Of HighestBid

<img src="README_files/figure-gfm/explore2-5.png" width="50%" />

    ## Histogram Of BpQ

<img src="README_files/figure-gfm/explore2-6.png" width="50%" />

    ## Histogram Of StartTime

<img src="README_files/figure-gfm/explore2-7.png" width="50%" />

## Bivariate Plots

### Categorical-Categorical

``` r
# create vector to loop across
for(r in 1:length(facnames.mod))
{
  for(i in 1:length(facnames.mod))
  {
   # dont do anything if they are the same or the graph has been made before
   if(i == r | i < r)
   {
   } else {
     cat(paste0(facnames.mod[r]," ~ ",facnames.mod[i]))
     
     # create formula for xtabs
     tempform <- paste0("~ ", facnames.mod[r], " + ", facnames.mod[i])
     # temp is a dataframe that is only going to exist in this section
     # and overwritten with each loop
     temp <- as.data.frame(xtabs(eval(parse(text = tempform)),
                            data = mydata,
                            subset = NULL))
     plot(
       ggplot(data = temp, aes_string(x = facnames.mod[r], y = facnames.mod[i], fill = "Freq", label = "Freq")) +
         geom_tile() + 
         geom_text() + 
         scale_fill_gradient(low = "white", high = "violetred") + 
         theme_minimal() + 
         theme(axis.text.x = element_text(angle = 90, vjust = -0.3))
     )
   }
  }
}
```

    ## AcadYear ~ Semester

<img src="README_files/figure-gfm/explorecatcat-1.png" width="50%" />

    ## AcadYear ~ Round

<img src="README_files/figure-gfm/explorecatcat-2.png" width="50%" />

    ## AcadYear ~ Level

<img src="README_files/figure-gfm/explorecatcat-3.png" width="50%" />

    ## AcadYear ~ DayText

<img src="README_files/figure-gfm/explorecatcat-4.png" width="50%" />

    ## AcadYear ~ LessonTime

<img src="README_files/figure-gfm/explorecatcat-5.png" width="50%" />

    ## Semester ~ Round

<img src="README_files/figure-gfm/explorecatcat-6.png" width="50%" />

    ## Semester ~ Level

<img src="README_files/figure-gfm/explorecatcat-7.png" width="50%" />

    ## Semester ~ DayText

<img src="README_files/figure-gfm/explorecatcat-8.png" width="50%" />

    ## Semester ~ LessonTime

<img src="README_files/figure-gfm/explorecatcat-9.png" width="50%" />

    ## Round ~ Level

<img src="README_files/figure-gfm/explorecatcat-10.png" width="50%" />

    ## Round ~ DayText

<img src="README_files/figure-gfm/explorecatcat-11.png" width="50%" />

    ## Round ~ LessonTime

<img src="README_files/figure-gfm/explorecatcat-12.png" width="50%" />

    ## Level ~ DayText

<img src="README_files/figure-gfm/explorecatcat-13.png" width="50%" />

    ## Level ~ LessonTime

<img src="README_files/figure-gfm/explorecatcat-14.png" width="50%" />

    ## DayText ~ LessonTime

<img src="README_files/figure-gfm/explorecatcat-15.png" width="50%" />

### Continuous-Continuous

``` r
for(r in 1:length(numnames))
{
  for(i in 1:length(numnames))
  {
   # dont do anything if they are the same or the graph has been made before
   if(i == r | i < r)
   {
   } else {
     cat(paste0(numnames[r]," ~ ",numnames[i]))
     # create formula for lm()
     tempform.std <- paste0("scale(", numnames[i],")", " ~ ", "scale(", numnames[r], ")")
     tempform <- paste0(numnames[i], " ~ ", numnames[r])
     # regress to get best fit line
     # standardized
     stdreg <- lm(eval(parse(text = tempform.std)),
               data = mydata)
     # unstandardized
     reg <- lm(eval(parse(text = tempform)),
               data = mydata)
     
     plot(
       ggplot(data = mydata, aes_string(x = numnames[r], y = numnames[i])) +
         geom_point(color = "violetred", size = 2, alpha = 0.3) +
         theme_classic() + 
         geom_abline(slope = reg$coefficients[2], intercept = reg$coefficients[1], lty = "dashed") + 
         geom_label(aes(x = Inf, y = Inf, label = paste0("Standardized Regression Coefficient = ",
                                                         round(stdreg$coefficients[2],3)),
                        hjust = 1, vjust = 1)) + 
         theme(axis.text.x = element_text(angle = 90, vjust = -0.3))
     )
   }
  }
}
```

    ## Quota ~ Bidders

<img src="README_files/figure-gfm/exploreconcon-1.png" width="50%" />

    ## Quota ~ LowestBid

<img src="README_files/figure-gfm/exploreconcon-2.png" width="50%" />

    ## Quota ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploreconcon-3.png" width="50%" />

    ## Quota ~ HighestBid

<img src="README_files/figure-gfm/exploreconcon-4.png" width="50%" />

    ## Quota ~ BpQ

<img src="README_files/figure-gfm/exploreconcon-5.png" width="50%" />

    ## Quota ~ StartTime

<img src="README_files/figure-gfm/exploreconcon-6.png" width="50%" />

    ## Bidders ~ LowestBid

<img src="README_files/figure-gfm/exploreconcon-7.png" width="50%" />

    ## Bidders ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploreconcon-8.png" width="50%" />

    ## Bidders ~ HighestBid

<img src="README_files/figure-gfm/exploreconcon-9.png" width="50%" />

    ## Bidders ~ BpQ

<img src="README_files/figure-gfm/exploreconcon-10.png" width="50%" />

    ## Bidders ~ StartTime

<img src="README_files/figure-gfm/exploreconcon-11.png" width="50%" />

    ## LowestBid ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploreconcon-12.png" width="50%" />

    ## LowestBid ~ HighestBid

<img src="README_files/figure-gfm/exploreconcon-13.png" width="50%" />

    ## LowestBid ~ BpQ

<img src="README_files/figure-gfm/exploreconcon-14.png" width="50%" />

    ## LowestBid ~ StartTime

<img src="README_files/figure-gfm/exploreconcon-15.png" width="50%" />

    ## LowestSuccessfulBid ~ HighestBid

<img src="README_files/figure-gfm/exploreconcon-16.png" width="50%" />

    ## LowestSuccessfulBid ~ BpQ

<img src="README_files/figure-gfm/exploreconcon-17.png" width="50%" />

    ## LowestSuccessfulBid ~ StartTime

<img src="README_files/figure-gfm/exploreconcon-18.png" width="50%" />

    ## HighestBid ~ BpQ

<img src="README_files/figure-gfm/exploreconcon-19.png" width="50%" />

    ## HighestBid ~ StartTime

<img src="README_files/figure-gfm/exploreconcon-20.png" width="50%" />

    ## BpQ ~ StartTime

<img src="README_files/figure-gfm/exploreconcon-21.png" width="50%" />

### Correlation Matrix

``` r
corrplot.mixed(cor(mydata[,grep(paste0(numnames.time, collapse = "|"), names(mydata))]),
               upper = "color",
               tl.pos = "lt",
               tl.cex = 0.5,
               cl.cex = 0.5)
```

<img src="README_files/figure-gfm/corrmatrix-1.png" width="50%" />

### Continuous-Categorical

``` r
for(r in facnames.mod)
{
  for(i in numnames)
  {
    cat(paste0(r," ~ ",i))
    # graph
    plot(
      ggplot(data = mydata, aes_string(x = r, y = i, fill = r)) + 
      geom_bar(stat = "identity") + 
      theme_classic() + 
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = -0.3))
    )
  }
}
```

    ## AcadYear ~ Quota

<img src="README_files/figure-gfm/exploreconcat-1.png" width="50%" />

    ## AcadYear ~ Bidders

<img src="README_files/figure-gfm/exploreconcat-2.png" width="50%" />

    ## AcadYear ~ LowestBid

<img src="README_files/figure-gfm/exploreconcat-3.png" width="50%" />

    ## AcadYear ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploreconcat-4.png" width="50%" />

    ## AcadYear ~ HighestBid

<img src="README_files/figure-gfm/exploreconcat-5.png" width="50%" />

    ## AcadYear ~ BpQ

<img src="README_files/figure-gfm/exploreconcat-6.png" width="50%" />

    ## AcadYear ~ StartTime

<img src="README_files/figure-gfm/exploreconcat-7.png" width="50%" />

    ## Semester ~ Quota

<img src="README_files/figure-gfm/exploreconcat-8.png" width="50%" />

    ## Semester ~ Bidders

<img src="README_files/figure-gfm/exploreconcat-9.png" width="50%" />

    ## Semester ~ LowestBid

<img src="README_files/figure-gfm/exploreconcat-10.png" width="50%" />

    ## Semester ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploreconcat-11.png" width="50%" />

    ## Semester ~ HighestBid

<img src="README_files/figure-gfm/exploreconcat-12.png" width="50%" />

    ## Semester ~ BpQ

<img src="README_files/figure-gfm/exploreconcat-13.png" width="50%" />

    ## Semester ~ StartTime

<img src="README_files/figure-gfm/exploreconcat-14.png" width="50%" />

    ## Round ~ Quota

<img src="README_files/figure-gfm/exploreconcat-15.png" width="50%" />

    ## Round ~ Bidders

<img src="README_files/figure-gfm/exploreconcat-16.png" width="50%" />

    ## Round ~ LowestBid

<img src="README_files/figure-gfm/exploreconcat-17.png" width="50%" />

    ## Round ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploreconcat-18.png" width="50%" />

    ## Round ~ HighestBid

<img src="README_files/figure-gfm/exploreconcat-19.png" width="50%" />

    ## Round ~ BpQ

<img src="README_files/figure-gfm/exploreconcat-20.png" width="50%" />

    ## Round ~ StartTime

<img src="README_files/figure-gfm/exploreconcat-21.png" width="50%" />

    ## Level ~ Quota

<img src="README_files/figure-gfm/exploreconcat-22.png" width="50%" />

    ## Level ~ Bidders

<img src="README_files/figure-gfm/exploreconcat-23.png" width="50%" />

    ## Level ~ LowestBid

<img src="README_files/figure-gfm/exploreconcat-24.png" width="50%" />

    ## Level ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploreconcat-25.png" width="50%" />

    ## Level ~ HighestBid

<img src="README_files/figure-gfm/exploreconcat-26.png" width="50%" />

    ## Level ~ BpQ

<img src="README_files/figure-gfm/exploreconcat-27.png" width="50%" />

    ## Level ~ StartTime

<img src="README_files/figure-gfm/exploreconcat-28.png" width="50%" />

    ## DayText ~ Quota

<img src="README_files/figure-gfm/exploreconcat-29.png" width="50%" />

    ## DayText ~ Bidders

<img src="README_files/figure-gfm/exploreconcat-30.png" width="50%" />

    ## DayText ~ LowestBid

<img src="README_files/figure-gfm/exploreconcat-31.png" width="50%" />

    ## DayText ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploreconcat-32.png" width="50%" />

    ## DayText ~ HighestBid

<img src="README_files/figure-gfm/exploreconcat-33.png" width="50%" />

    ## DayText ~ BpQ

<img src="README_files/figure-gfm/exploreconcat-34.png" width="50%" />

    ## DayText ~ StartTime

<img src="README_files/figure-gfm/exploreconcat-35.png" width="50%" />

    ## LessonTime ~ Quota

<img src="README_files/figure-gfm/exploreconcat-36.png" width="50%" />

    ## LessonTime ~ Bidders

<img src="README_files/figure-gfm/exploreconcat-37.png" width="50%" />

    ## LessonTime ~ LowestBid

<img src="README_files/figure-gfm/exploreconcat-38.png" width="50%" />

    ## LessonTime ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploreconcat-39.png" width="50%" />

    ## LessonTime ~ HighestBid

<img src="README_files/figure-gfm/exploreconcat-40.png" width="50%" />

    ## LessonTime ~ BpQ

<img src="README_files/figure-gfm/exploreconcat-41.png" width="50%" />

    ## LessonTime ~ StartTime

<img src="README_files/figure-gfm/exploreconcat-42.png" width="50%" />

### By Module

``` r
for(i in numnames)
{
  cat(paste0("ModuleCode"," ~ ",i))
  # graph
  plot(
    ggplot(data = mydata, aes_string(x = "ModuleCode", y = i, fill = "ModuleCode")) + 
      geom_bar(stat = "identity") + 
      theme_classic() + 
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = -1)) + 
      coord_flip()
  )
}
```

    ## ModuleCode ~ Quota

<img src="README_files/figure-gfm/exploremodule-1.png" width="50%" />

    ## ModuleCode ~ Bidders

<img src="README_files/figure-gfm/exploremodule-2.png" width="50%" />

    ## ModuleCode ~ LowestBid

<img src="README_files/figure-gfm/exploremodule-3.png" width="50%" />

    ## ModuleCode ~ LowestSuccessfulBid

<img src="README_files/figure-gfm/exploremodule-4.png" width="50%" />

    ## ModuleCode ~ HighestBid

<img src="README_files/figure-gfm/exploremodule-5.png" width="50%" />

    ## ModuleCode ~ BpQ

<img src="README_files/figure-gfm/exploremodule-6.png" width="50%" />

    ## ModuleCode ~ StartTime

<img src="README_files/figure-gfm/exploremodule-7.png" width="50%" />

# Phase 5: Exploration

``` r
# number of modules
unique(mydata$ModuleCode)
```

    ##  [1] PL1101E PL2131  PL2132  PL3232  PL3233  PL3234  PL3235  PL3236  PL3239  PL3240  PL3241  PL3242  PL3244  PL3248  PL3254  PL3256  PL3257  PL3258  PL3259  PL3260  PL3281  PL3281A PL3281D PL3282  PL3282A PL3282C PL3283  PL3283A PL3283B PL3287  PL3289  PL4201  PL4202  PL4203  PL4205  PL4207  PL4214  PL4218  PL4219  PL4221  PL4222  PL4223  PL4224  PL4225  PL4226  PL4228  PL4229  PL4231  PL4235  PL4237  PL4238  PL4239  PL4240  PL4241  PL4880F PL4880G PL4880K PL4880L PL4880P PL4880Q PL4880R
    ## Levels: PL1101E PL2131 PL2132 PL3232 PL3233 PL3234 PL3235 PL3236 PL3239 PL3240 PL3241 PL3242 PL3244 PL3248 PL3254 PL3256 PL3257 PL3258 PL3259 PL3260 PL3281 PL3281A PL3281D PL3282 PL3282A PL3282C PL3283 PL3283A PL3283B PL3287 PL3289 PL4201 PL4202 PL4203 PL4205 PL4207 PL4214 PL4218 PL4219 PL4221 PL4222 PL4223 PL4224 PL4225 PL4226 PL4228 PL4229 PL4231 PL4235 PL4237 PL4238 PL4239 PL4240 PL4241 PL4880F PL4880G PL4880K PL4880L PL4880P PL4880Q PL4880R

``` r
# number of rows belonging to each module
xtabs(~ ModuleCode, data  = mydata, subset = NULL)
```

    ## ModuleCode
    ## PL1101E  PL2131  PL2132  PL3232  PL3233  PL3234  PL3235  PL3236  PL3239  PL3240  PL3241  PL3242  PL3244  PL3248  PL3254  PL3256  PL3257  PL3258  PL3259  PL3260  PL3281 PL3281A PL3281D  PL3282 PL3282A PL3282C  PL3283 PL3283A PL3283B  PL3287  PL3289  PL4201  PL4202  PL4203  PL4205  PL4207  PL4214  PL4218  PL4219  PL4221  PL4222  PL4223  PL4224  PL4225  PL4226  PL4228  PL4229  PL4231  PL4235  PL4237  PL4238  PL4239  PL4240  PL4241 PL4880F PL4880G PL4880K PL4880L PL4880P PL4880Q PL4880R 
    ##      96      18      18      29      28      20      27      24       7      18       7      20      11       6      11       7       8       7       5       4      13       3       7       7       1       1       1       1       7       4       1       9       5       4      15       3       2       3       4       8       5       3       1       1       6       4       1       5      28      28       6       2       4       5       2       2      14      32       3       4      28

``` r
# datatable(mydata)
aggregate(BpQ ~ AcadYear + Semester + ModuleCode,
          data = mydata,
          FUN = mean)
```

    ##      AcadYear Semester ModuleCode        BpQ
    ## 1   2017/2018        1    PL1101E 1.56868739
    ## 2   2018/2019        1    PL1101E 2.40971145
    ## 3   2016/2017        2    PL1101E 0.35215039
    ## 4   2017/2018        1     PL2131 1.29896694
    ## 5   2018/2019        1     PL2131 2.01756432
    ## 6   2016/2017        2     PL2131 1.49786980
    ## 7   2017/2018        1     PL2132 1.73152848
    ## 8   2018/2019        1     PL2132 2.23710317
    ## 9   2016/2017        2     PL2132 2.70238095
    ## 10  2017/2018        1     PL3232 0.33741406
    ## 11  2018/2019        1     PL3232 1.43125000
    ## 12  2016/2017        2     PL3232 0.28010602
    ## 13  2017/2018        1     PL3233 0.11450686
    ## 14  2018/2019        1     PL3233 4.02482270
    ## 15  2016/2017        2     PL3233 0.18957370
    ## 16  2017/2018        1     PL3234 0.58333333
    ## 17  2018/2019        1     PL3234 1.17023810
    ## 18  2016/2017        2     PL3234 0.77738772
    ## 19  2017/2018        1     PL3235 3.70454545
    ## 20  2018/2019        1     PL3235 0.78156146
    ## 21  2016/2017        2     PL3235 0.36139025
    ## 22  2017/2018        1     PL3236 1.00378788
    ## 23  2018/2019        1     PL3236 0.51097829
    ## 24  2016/2017        2     PL3236 0.13751359
    ## 25  2018/2019        1     PL3239 0.05111517
    ## 26  2017/2018        1     PL3240 0.10205696
    ## 27  2018/2019        1     PL3240 0.05622694
    ## 28  2016/2017        2     PL3240 1.98088972
    ## 29  2017/2018        1     PL3241 0.82491944
    ## 30  2017/2018        1     PL3242 0.09184581
    ## 31  2016/2017        2     PL3242 2.13561254
    ## 32  2017/2018        1     PL3244 1.45694444
    ## 33  2018/2019        1     PL3244 0.07664683
    ## 34  2016/2017        2     PL3248 1.92543860
    ## 35  2018/2019        1     PL3254 0.02986168
    ## 36  2016/2017        2     PL3254 4.29411765
    ## 37  2016/2017        2     PL3256 0.79467193
    ## 38  2017/2018        1     PL3257 2.03125000
    ## 39  2018/2019        1     PL3257 1.60227273
    ## 40  2016/2017        2     PL3258 0.05754481
    ## 41  2017/2018        1     PL3259 2.35000000
    ## 42  2016/2017        2     PL3260 2.40789474
    ## 43  2017/2018        1     PL3281 5.82000000
    ## 44  2018/2019        1     PL3281 1.01696970
    ## 45  2016/2017        2     PL3281 2.00000000
    ## 46  2016/2017        2    PL3281A 3.29629630
    ## 47  2017/2018        1    PL3281D 0.25309393
    ## 48  2017/2018        1     PL3282 1.36000000
    ## 49  2018/2019        1     PL3282 1.46000000
    ## 50  2016/2017        2    PL3282A 1.52000000
    ## 51  2017/2018        1    PL3282C 1.44000000
    ## 52  2017/2018        1     PL3283 1.44000000
    ## 53  2016/2017        2    PL3283A 1.44000000
    ## 54  2018/2019        1    PL3283B 0.57142857
    ## 55  2018/2019        1     PL3287 1.34833333
    ## 56  2016/2017        2     PL3289 1.36000000
    ## 57  2017/2018        1     PL4201 4.06306306
    ## 58  2018/2019        1     PL4201 1.48834499
    ## 59  2016/2017        2     PL4202 1.41242001
    ## 60  2018/2019        1     PL4203 2.00833333
    ## 61  2017/2018        1     PL4205 2.42982456
    ## 62  2018/2019        1     PL4205 1.40512821
    ## 63  2017/2018        1     PL4207 1.40000000
    ## 64  2018/2019        1     PL4207 6.18750000
    ## 65  2016/2017        2     PL4214 1.80000000
    ## 66  2016/2017        2     PL4218 3.73333333
    ## 67  2018/2019        1     PL4219 3.45000000
    ## 68  2016/2017        2     PL4219 2.12820513
    ## 69  2017/2018        1     PL4221 1.83333333
    ## 70  2018/2019        1     PL4221 0.50416667
    ## 71  2016/2017        2     PL4222 0.82336134
    ## 72  2017/2018        1     PL4223 1.15555556
    ## 73  2018/2019        1     PL4223 5.73750000
    ## 74  2018/2019        1     PL4224 1.27500000
    ## 75  2016/2017        2     PL4225 1.02500000
    ## 76  2017/2018        1     PL4226 7.16666667
    ## 77  2018/2019        1     PL4226 1.37500000
    ## 78  2016/2017        2     PL4226 5.12500000
    ## 79  2017/2018        1     PL4228 3.27500000
    ## 80  2018/2019        1     PL4228 1.47500000
    ## 81  2016/2017        2     PL4228 1.32500000
    ## 82  2017/2018        1     PL4229 1.42500000
    ## 83  2016/2017        2     PL4231 1.10269231
    ## 84  2017/2018        1     PL4235 3.29332298
    ## 85  2018/2019        1     PL4235 3.04285714
    ## 86  2017/2018        1     PL4237 0.27413985
    ## 87  2016/2017        2     PL4238 1.91765873
    ## 88  2017/2018        1     PL4239 6.30000000
    ## 89  2017/2018        1     PL4240 1.07500000
    ## 90  2016/2017        2     PL4240 4.00000000
    ## 91  2016/2017        2     PL4241 1.44920635
    ## 92  2017/2018        1    PL4880F 6.71250000
    ## 93  2016/2017        2    PL4880G 2.56250000
    ## 94  2018/2019        1    PL4880K 3.72380952
    ## 95  2018/2019        1    PL4880L 0.42664072
    ## 96  2016/2017        2    PL4880L 2.83333333
    ## 97  2018/2019        1    PL4880P 1.35000000
    ## 98  2016/2017        2    PL4880P 4.01282051
    ## 99  2016/2017        2    PL4880Q 2.05833333
    ## 100 2017/2018        1    PL4880R 3.90773810
    ## 101 2018/2019        1    PL4880R 1.64209838
    ## 102 2016/2017        2    PL4880R 2.57500000
