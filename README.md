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

  - Notes:
    1.  AY2017/2018 Semester 2, AY2018/2019 Semester 2, bidding data not
        available.
    
    2.  The bidding statistics are highly non-normal, likely due to
        being bounded by zero (they cannot bid negative points or have
        negative bidders). May consider using zero-inflated or poisson
        regression if considering these statistics as dependent
        variables.
    
    3.  
# Phase 1: Setting Up Environment, Packages And Loading Data.

<details>

<summary><b>View Code/Details</b></summary>

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
knitr::opts_chunk$set(dpi = 300, out.width = "50%", eval = TRUE)
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

# save
saveRDS(myBid, file = "myBid.RDS")
```

## \>\>Load `myBid.RDS`

``` r
myBid <- readRDS("mydata.RDS")
```

## \>Module Information

``` r
# create empty dataframe which will act as a container to be populated with data
myModInfo <- data.frame()
# looping through each year
for(year in c(2011:2018))
{
  for(semester in c(1,2))
  {
    # create the url where data is to be extracted from
    myurl <- paste0("https://api.nusmods.com/", year, "-", year + 1, "/", semester, "/moduleTimetableDeltaRaw.json")
    myjson <- fromJSON(file = url(myurl))
    
    # for each element in the myjson list, append it to myModInfo
    for(r in 1:length(myjson))
    {
      if(isTRUE(str_detect(myjson[[r]]$ModuleCode, "^PL")))
      {
        if(myjson[[r]]$Semester == 1 | myjson[[r]]$Semester == 2)
        {
          myModInfo <- rbind(myModInfo, myjson[[r]])
        }
      }
      myjson[[r]] <- NA
    }
    cat(year, "Semester", semester, "Done!")
  }
}

# save
saveRDS(myModInfo, file = "myModInfo.RDS")
```

## \>\>Load `myModInfo.RDS`

``` r
myModInfo <- readRDS("myModInfo.RDS")
```

</details>

# Phase 2: Filter, Transform And Merge Data

<details>

<summary><b>View Code/Details</b></summary>

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
# remove duplicated rows based on columns of ModuleCode, Acadyear, Semester, StartTime and DayText
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

</details>

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

## Univariate Descriptive Statistics

``` r
describe(mydata)
```

    ##                     vars    n    mean     sd  median trimmed    mad min  max range  skew kurtosis    se
    ## ModuleCode*            1 2829   29.00  26.99   19.00   25.98  25.20   1   91    90  0.69    -0.83  0.51
    ## AcadYear*              2 2829    4.32   2.02    4.00    4.27   1.48   1    8     7  0.16    -0.88  0.04
    ## Semester*              3 2829    1.49   0.50    1.00    1.48   0.00   1    2     1  0.05    -2.00  0.01
    ## Round*                 4 2829    3.69   2.05    4.00    3.61   2.97   1    7     6  0.08    -1.32  0.04
    ## Quota                  5 2829   25.95  38.61   15.00   17.88  17.79   1  430   429  4.37    30.11  0.73
    ## Bidders                6 2829   13.20  31.11    3.00    5.98   4.45   0  440   440  5.15    35.11  0.58
    ## LowestBid              7 2829   70.16 212.23    1.00   15.76   1.48   0 2430  2430  5.11    34.36  3.99
    ## LowestSuccessfulBid    8 2829  258.66 513.65    1.00  131.63   1.48   0 3459  3459  2.34     5.68  9.66
    ## HighestBid             9 2829  732.71 865.31  400.00  594.56 593.04   0 4801  4801  1.16     0.73 16.27
    ## StudentAcctType*      10 2829    4.37   1.73    5.00    4.47   1.48   1    7     6 -0.49    -0.42  0.03
    ## Level*                11 2829    3.00   0.90    3.00    3.12   0.00   1    4     3 -0.95     0.35  0.02
    ## BpQ                   12 2829    1.05   1.80    0.33    0.64   0.49   0   18    18  3.41    15.35  0.03
    ## StartTime             13 2829 1305.30 268.79 1300.00 1301.81 296.52 800 1900  1100  0.03    -0.72  5.05
    ## DayText*              14 2829    2.97   1.30    3.00    2.96   1.48   1    5     4 -0.03    -1.10  0.02
    ## LessonTime*           15 2829    1.84   0.57    2.00    1.81   0.00   1    3     2  0.00    -0.17  0.01

``` r
summary(mydata)
```

    ##    ModuleCode        AcadYear   Semester Round        Quota           Bidders        LowestBid       LowestSuccessfulBid   HighestBid                                        StudentAcctType     Level           BpQ             StartTime         DayText        LessonTime  
    ##  PL1101E: 324   2013/2014:493   1:1449   1A:634   Min.   :  1.00   Min.   :  0.0   Min.   :   0.00   Min.   :   0.0      Min.   :   0.0   New Students [P]                           : 314   Level 1: 324   Min.   : 0.00000   Min.   : 800   Monday   :479   Morning  : 723  
    ##  PL3232 : 124   2015/2016:485   2:1380   1B:389   1st Qu.:  4.00   1st Qu.:  1.0   1st Qu.:   1.00   1st Qu.:   1.0      1st Qu.:   1.0   NUS Students [G]                           : 141   Level 2: 169   1st Qu.: 0.02326   1st Qu.:1100   Tuesday  :588   Afternoon:1834  
    ##  PL3236 : 112   2014/2015:439            1C:273   Median : 15.00   Median :  3.0   Median :   1.00   Median :   1.0      Median : 400.0   NUS Students [P, G]                        : 327   Level 3:1520   Median : 0.33333   Median :1300   Wednesday:688   Evening  : 272  
    ##  PL3234 : 110   2016/2017:366            2A:410   Mean   : 25.95   Mean   : 13.2   Mean   :  70.16   Mean   : 258.7      Mean   : 732.7   NUS Students [P]                           : 331   Level 4: 816   Mean   : 1.05331   Mean   :1305   Thursday :684                   
    ##  PL3235 : 109   2012/2013:350            2B:463   3rd Qu.: 32.00   3rd Qu.: 10.0   3rd Qu.:   8.00   3rd Qu.: 271.0      3rd Qu.:1237.0   Returning Students [P]                     :1191                  3rd Qu.: 1.30000   3rd Qu.:1500   Friday   :390                   
    ##  PL3233 : 107   2011/2012:242            3A:366   Max.   :430.00   Max.   :440.0   Max.   :2430.00   Max.   :3459.0      Max.   :4801.0   Returning Students [P] and NUS Students [G]: 155                  Max.   :18.00000   Max.   :1900                                   
    ##  (Other):1943   (Other)  :454            3B:294                                                                                           Returning Students and New Students [P]    : 370

## Univariate Histograms

<details>

<summary><b>View Histograms</b></summary>

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

</details>

## Bivariate Plots

<details>

<summary><b>View Categorical-Categorical</b></summary>

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
          theme(axis.text.x = element_text(angle = 90, vjust = -0.3),
                legend.position = "none")
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

</details>

<details>

<summary><b>View Continuous-Continuous</b></summary>

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
                         hjust = 1, vjust = 2)) + 
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

<details>

<summary><b>View Continuous-Categorical</b></summary>

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

</details>

# Phase 5: Exploration

``` r
# number of modules
unique(mydata$ModuleCode)
```

    ##  [1] PL1101E PL2131  PL2132  PL3232  PL3233  PL3234  PL3235  PL3236  PL3237  PL3238  PL3239  PL3240  PL3241  PL3242  PL3243  PL3244  PL3248  PL3249  PL3250  PL3251  PL3252  PL3253  PL3254  PL3255  PL3256  PL3257  PL3258  PL3259  PL3260  PL3261  PL3281  PL3281A PL3281B PL3281C PL3281D PL3282  PL3282A PL3282C PL3283  PL3283A PL3283B PL3284  PL3285  PL3286  PL3287  PL3288  PL3289  PL4201  PL4202  PL4203  PL4205  PL4206  PL4207  PL4208  PL4213  PL4214  PL4217  PL4218  PL4219  PL4220  PL4221  PL4222  PL4223  PL4224  PL4225  PL4226  PL4227  PL4228  PL4229  PL4230  PL4231  PL4232  PL4233  PL4234  PL4235  PL4237  PL4238  PL4239  PL4240  PL4241  PL4880F PL4880G PL4880H PL4880I PL4880J PL4880K PL4880L PL4880N PL4880P PL4880Q PL4880R
    ## Levels: PL1101E PL2131 PL2132 PL3232 PL3233 PL3234 PL3235 PL3236 PL3237 PL3238 PL3239 PL3240 PL3241 PL3242 PL3243 PL3244 PL3248 PL3249 PL3250 PL3251 PL3252 PL3253 PL3254 PL3255 PL3256 PL3257 PL3258 PL3259 PL3260 PL3261 PL3281 PL3281A PL3281B PL3281C PL3281D PL3282 PL3282A PL3282C PL3283 PL3283A PL3283B PL3284 PL3285 PL3286 PL3287 PL3288 PL3289 PL4201 PL4202 PL4203 PL4205 PL4206 PL4207 PL4208 PL4213 PL4214 PL4217 PL4218 PL4219 PL4220 PL4221 PL4222 PL4223 PL4224 PL4225 PL4226 PL4227 PL4228 PL4229 PL4230 PL4231 PL4232 PL4233 PL4234 PL4235 PL4237 PL4238 PL4239 PL4240 PL4241 PL4880F PL4880G PL4880H PL4880I PL4880J PL4880K PL4880L PL4880N PL4880P PL4880Q PL4880R

``` r
# number of rows belonging to each module
xtabs(~ ModuleCode, data  = mydata, subset = NULL)
```

    ## ModuleCode
    ## PL1101E  PL2131  PL2132  PL3232  PL3233  PL3234  PL3235  PL3236  PL3237  PL3238  PL3239  PL3240  PL3241  PL3242  PL3243  PL3244  PL3248  PL3249  PL3250  PL3251  PL3252  PL3253  PL3254  PL3255  PL3256  PL3257  PL3258  PL3259  PL3260  PL3261  PL3281 PL3281A PL3281B PL3281C PL3281D  PL3282 PL3282A PL3282C  PL3283 PL3283A PL3283B  PL3284  PL3285  PL3286  PL3287  PL3288  PL3289  PL4201  PL4202  PL4203  PL4205  PL4206  PL4207  PL4208  PL4213  PL4214  PL4217  PL4218  PL4219  PL4220  PL4221  PL4222  PL4223  PL4224  PL4225  PL4226  PL4227  PL4228  PL4229  PL4230  PL4231  PL4232  PL4233  PL4234  PL4235  PL4237  PL4238  PL4239  PL4240  PL4241 PL4880F PL4880G PL4880H PL4880I PL4880J PL4880K PL4880L PL4880N PL4880P PL4880Q PL4880R 
    ##     324      84      85     124     107     110     109     112      49      35      41      37      50      46      14      29      24      21      27      16      29       3      36      21      17      17      21      15       4      50      45      44      10      11      40      39      13       6       2       6      22      37       6      30      22      22       1      23      37      43      36      43      19      13      22      20      13      26      24       5      39      30      13      20       2      18      24      16      20       7      17       5       3      22      40      43      11       2       4       5      15      15       6       6      18      15      33       4       7       4      28

``` r
# datatable(myBid, filter = "top")
aggregate(BpQ ~ AcadYear + Semester + ModuleCode,
          data = mydata,
          FUN = mean)
```

    ##      AcadYear Semester ModuleCode         BpQ
    ## 1   2012/2013        1    PL1101E 2.208133722
    ## 2   2013/2014        1    PL1101E 2.524620712
    ## 3   2014/2015        1    PL1101E 1.722038378
    ## 4   2015/2016        1    PL1101E 1.693148131
    ## 5   2016/2017        1    PL1101E 0.659687257
    ## 6   2017/2018        1    PL1101E 1.568687389
    ## 7   2018/2019        1    PL1101E 2.409711449
    ## 8   2011/2012        2    PL1101E 0.294144638
    ## 9   2012/2013        2    PL1101E 0.286434469
    ## 10  2013/2014        2    PL1101E 0.776424152
    ## 11  2014/2015        2    PL1101E 0.926792764
    ## 12  2015/2016        2    PL1101E 0.359536712
    ## 13  2016/2017        2    PL1101E 0.352150391
    ## 14  2012/2013        1     PL2131 0.726495823
    ## 15  2013/2014        1     PL2131 0.641789304
    ## 16  2014/2015        1     PL2131 2.179794725
    ## 17  2015/2016        1     PL2131 1.140703446
    ## 18  2016/2017        1     PL2131 0.780113182
    ## 19  2017/2018        1     PL2131 1.298966942
    ## 20  2018/2019        1     PL2131 2.017564322
    ## 21  2011/2012        2     PL2131 3.316666667
    ## 22  2012/2013        2     PL2131 0.943904556
    ## 23  2013/2014        2     PL2131 0.953399123
    ## 24  2014/2015        2     PL2131 2.329221689
    ## 25  2015/2016        2     PL2131 0.726503072
    ## 26  2016/2017        2     PL2131 1.497869797
    ## 27  2012/2013        1     PL2132 3.792721519
    ## 28  2013/2014        1     PL2132 3.764240506
    ## 29  2014/2015        1     PL2132 0.182142857
    ## 30  2015/2016        1     PL2132 2.515822785
    ## 31  2016/2017        1     PL2132 0.808805031
    ## 32  2017/2018        1     PL2132 1.731528483
    ## 33  2018/2019        1     PL2132 2.237103175
    ## 34  2011/2012        2     PL2132 0.088833179
    ## 35  2012/2013        2     PL2132 0.790277778
    ## 36  2013/2014        2     PL2132 0.253423017
    ## 37  2014/2015        2     PL2132 0.730583812
    ## 38  2015/2016        2     PL2132 1.131439394
    ## 39  2016/2017        2     PL2132 2.702380952
    ## 40  2012/2013        1     PL3232 0.673518149
    ## 41  2013/2014        1     PL3232 0.377452381
    ## 42  2014/2015        1     PL3232 0.067959280
    ## 43  2015/2016        1     PL3232 0.295174539
    ## 44  2016/2017        1     PL3232 0.071852044
    ## 45  2017/2018        1     PL3232 0.337414057
    ## 46  2018/2019        1     PL3232 1.431250000
    ## 47  2011/2012        2     PL3232 0.848480392
    ## 48  2012/2013        2     PL3232 0.682440476
    ## 49  2013/2014        2     PL3232 0.357608929
    ## 50  2014/2015        2     PL3232 0.916030663
    ## 51  2015/2016        2     PL3232 1.321901946
    ## 52  2016/2017        2     PL3232 0.280106025
    ## 53  2012/2013        1     PL3233 0.880612245
    ## 54  2013/2014        1     PL3233 0.031176473
    ## 55  2014/2015        1     PL3233 0.343282027
    ## 56  2015/2016        1     PL3233 0.775238095
    ## 57  2016/2017        1     PL3233 1.026647745
    ## 58  2017/2018        1     PL3233 0.114506864
    ## 59  2018/2019        1     PL3233 4.024822695
    ## 60  2011/2012        2     PL3233 0.915155971
    ## 61  2012/2013        2     PL3233 0.778259167
    ## 62  2013/2014        2     PL3233 2.388888889
    ## 63  2014/2015        2     PL3233 0.577500000
    ## 64  2015/2016        2     PL3233 1.381154810
    ## 65  2016/2017        2     PL3233 0.189573701
    ## 66  2012/2013        1     PL3234 0.873536706
    ## 67  2013/2014        1     PL3234 0.293870220
    ## 68  2014/2015        1     PL3234 0.298354978
    ## 69  2015/2016        1     PL3234 0.257067915
    ## 70  2016/2017        1     PL3234 0.086509761
    ## 71  2017/2018        1     PL3234 0.583333333
    ## 72  2018/2019        1     PL3234 1.170238095
    ## 73  2011/2012        2     PL3234 0.311370744
    ## 74  2012/2013        2     PL3234 0.784012329
    ## 75  2013/2014        2     PL3234 0.644649804
    ## 76  2014/2015        2     PL3234 0.534751197
    ## 77  2015/2016        2     PL3234 0.489929174
    ## 78  2016/2017        2     PL3234 0.777387716
    ## 79  2012/2013        1     PL3235 1.205245945
    ## 80  2013/2014        1     PL3235 1.424603175
    ## 81  2014/2015        1     PL3235 0.311516864
    ## 82  2015/2016        1     PL3235 0.800000000
    ## 83  2016/2017        1     PL3235 0.959126984
    ## 84  2017/2018        1     PL3235 3.704545455
    ## 85  2018/2019        1     PL3235 0.781561462
    ## 86  2011/2012        2     PL3235 0.880261704
    ## 87  2012/2013        2     PL3235 0.633896796
    ## 88  2013/2014        2     PL3235 1.157365128
    ## 89  2014/2015        2     PL3235 1.325764267
    ## 90  2015/2016        2     PL3235 0.501413451
    ## 91  2016/2017        2     PL3235 0.361390248
    ## 92  2012/2013        1     PL3236 1.088934898
    ## 93  2013/2014        1     PL3236 0.406033951
    ## 94  2014/2015        1     PL3236 0.242634680
    ## 95  2015/2016        1     PL3236 0.532539683
    ## 96  2016/2017        1     PL3236 0.290483785
    ## 97  2017/2018        1     PL3236 1.003787879
    ## 98  2018/2019        1     PL3236 0.510978286
    ## 99  2011/2012        2     PL3236 0.516648301
    ## 100 2012/2013        2     PL3236 0.547731119
    ## 101 2013/2014        2     PL3236 0.857116314
    ## 102 2014/2015        2     PL3236 1.064689974
    ## 103 2015/2016        2     PL3236 0.578307600
    ## 104 2016/2017        2     PL3236 0.137513591
    ## 105 2016/2017        1     PL3237 0.196769374
    ## 106 2011/2012        2     PL3237 0.148519519
    ## 107 2012/2013        2     PL3237 0.417975714
    ## 108 2013/2014        2     PL3237 0.698611111
    ## 109 2014/2015        2     PL3237 0.997350519
    ## 110 2015/2016        2     PL3237 0.533928571
    ## 111 2012/2013        1     PL3238 0.860000000
    ## 112 2013/2014        1     PL3238 0.635238095
    ## 113 2014/2015        1     PL3238 0.026982977
    ## 114 2015/2016        1     PL3238 0.122826909
    ## 115 2011/2012        2     PL3238 0.275251031
    ## 116 2013/2014        2     PL3238 2.153333333
    ## 117 2013/2014        1     PL3239 0.056542191
    ## 118 2016/2017        1     PL3239 0.132096997
    ## 119 2018/2019        1     PL3239 0.051115174
    ## 120 2011/2012        2     PL3239 0.143011536
    ## 121 2012/2013        2     PL3239 1.015259740
    ## 122 2014/2015        2     PL3239 2.435323383
    ## 123 2015/2016        2     PL3239 2.399425287
    ## 124 2013/2014        1     PL3240 0.253465419
    ## 125 2014/2015        1     PL3240 1.125000000
    ## 126 2015/2016        1     PL3240 0.166612752
    ## 127 2017/2018        1     PL3240 0.102056957
    ## 128 2018/2019        1     PL3240 0.056226942
    ## 129 2012/2013        2     PL3240 1.997222222
    ## 130 2016/2017        2     PL3240 1.980889724
    ## 131 2014/2015        1     PL3241 0.418197279
    ## 132 2016/2017        1     PL3241 0.655952381
    ## 133 2017/2018        1     PL3241 0.824919441
    ## 134 2011/2012        2     PL3241 0.111904762
    ## 135 2012/2013        2     PL3241 1.283000000
    ## 136 2013/2014        2     PL3241 2.221714286
    ## 137 2015/2016        2     PL3241 2.398290598
    ## 138 2013/2014        1     PL3242 0.060005429
    ## 139 2015/2016        1     PL3242 1.286868687
    ## 140 2017/2018        1     PL3242 0.091845809
    ## 141 2012/2013        2     PL3242 1.408095238
    ## 142 2016/2017        2     PL3242 2.135612536
    ## 143 2013/2014        2     PL3243 0.057774456
    ## 144 2015/2016        2     PL3243 0.166634194
    ## 145 2012/2013        1     PL3244 0.052287100
    ## 146 2016/2017        1     PL3244 1.700000000
    ## 147 2017/2018        1     PL3244 1.456944444
    ## 148 2018/2019        1     PL3244 0.076646825
    ## 149 2011/2012        2     PL3244 0.201140978
    ## 150 2012/2013        1     PL3248 1.088441558
    ## 151 2013/2014        1     PL3248 0.427777778
    ## 152 2014/2015        2     PL3248 1.458823529
    ## 153 2015/2016        2     PL3248 2.510714286
    ## 154 2016/2017        2     PL3248 1.925438596
    ## 155 2015/2016        1     PL3249 0.046634962
    ## 156 2012/2013        2     PL3249 0.220085709
    ## 157 2013/2014        2     PL3249 0.137344491
    ## 158 2011/2012        2     PL3250 0.051734274
    ## 159 2012/2013        2     PL3250 0.640109890
    ## 160 2013/2014        2     PL3250 1.060515873
    ## 161 2015/2016        2     PL3250 1.654074074
    ## 162 2015/2016        1     PL3251 2.343750000
    ## 163 2011/2012        2     PL3251 0.058193722
    ## 164 2013/2014        2     PL3251 1.215873016
    ## 165 2015/2016        1     PL3252 0.029834494
    ## 166 2016/2017        1     PL3252 1.350000000
    ## 167 2011/2012        2     PL3252 0.545382395
    ## 168 2012/2013        2     PL3252 1.605128205
    ## 169 2014/2015        2     PL3252 1.060905125
    ## 170 2015/2016        2     PL3253 0.000000000
    ## 171 2012/2013        1     PL3254 0.117806887
    ## 172 2013/2014        1     PL3254 0.624615385
    ## 173 2014/2015        1     PL3254 0.257808858
    ## 174 2018/2019        1     PL3254 0.029861682
    ## 175 2011/2012        2     PL3254 0.167426108
    ## 176 2016/2017        2     PL3254 4.294117647
    ## 177 2012/2013        1     PL3255 0.608585859
    ## 178 2013/2014        1     PL3255 0.355555556
    ## 179 2011/2012        2     PL3255 0.024602213
    ## 180 2014/2015        2     PL3255 1.059027778
    ## 181 2012/2013        1     PL3256 0.017600354
    ## 182 2013/2014        2     PL3256 2.755555556
    ## 183 2016/2017        2     PL3256 0.794671929
    ## 184 2014/2015        1     PL3257 0.898036759
    ## 185 2017/2018        1     PL3257 2.031250000
    ## 186 2018/2019        1     PL3257 1.602272727
    ## 187 2015/2016        2     PL3257 1.769423559
    ## 188 2015/2016        1     PL3258 0.012586917
    ## 189 2014/2015        2     PL3258 0.155285873
    ## 190 2016/2017        2     PL3258 0.057544805
    ## 191 2015/2016        1     PL3259 0.088790157
    ## 192 2016/2017        1     PL3259 0.671428571
    ## 193 2017/2018        1     PL3259 2.350000000
    ## 194 2016/2017        2     PL3260 2.407894737
    ## 195 2016/2017        1     PL3261 0.070859923
    ## 196 2015/2016        2     PL3261 0.027913713
    ## 197 2012/2013        1     PL3281 0.104166667
    ## 198 2013/2014        1     PL3281 0.363809524
    ## 199 2015/2016        1     PL3281 0.180272109
    ## 200 2017/2018        1     PL3281 5.820000000
    ## 201 2018/2019        1     PL3281 1.016969697
    ## 202 2011/2012        2     PL3281 0.500000000
    ## 203 2014/2015        2     PL3281 0.800000000
    ## 204 2015/2016        2     PL3281 1.160000000
    ## 205 2016/2017        2     PL3281 2.000000000
    ## 206 2012/2013        1    PL3281A 0.128571429
    ## 207 2013/2014        1    PL3281A 0.000000000
    ## 208 2014/2015        1    PL3281A 0.000000000
    ## 209 2015/2016        1    PL3281A 0.020408163
    ## 210 2011/2012        2    PL3281A 0.000000000
    ## 211 2012/2013        2    PL3281A 0.453571429
    ## 212 2015/2016        2    PL3281A 0.347023810
    ## 213 2016/2017        2    PL3281A 3.296296296
    ## 214 2011/2012        2    PL3281B 0.305555556
    ## 215 2012/2013        2    PL3281B 1.053333333
    ## 216 2014/2015        2    PL3281B 1.000000000
    ## 217 2013/2014        1    PL3281C 0.000000000
    ## 218 2016/2017        1    PL3281C 3.500000000
    ## 219 2012/2013        2    PL3281C 1.000000000
    ## 220 2012/2013        1    PL3281D 0.062074830
    ## 221 2014/2015        1    PL3281D 0.009523810
    ## 222 2016/2017        1    PL3281D 0.121428571
    ## 223 2017/2018        1    PL3281D 0.253093930
    ## 224 2011/2012        2    PL3281D 0.034722222
    ## 225 2013/2014        2    PL3281D 0.563095238
    ## 226 2012/2013        1     PL3282 1.480000000
    ## 227 2013/2014        1     PL3282 0.424000000
    ## 228 2014/2015        1     PL3282 0.733333333
    ## 229 2016/2017        1     PL3282 1.520000000
    ## 230 2017/2018        1     PL3282 1.360000000
    ## 231 2018/2019        1     PL3282 1.460000000
    ## 232 2013/2014        2     PL3282 1.440000000
    ## 233 2015/2016        2     PL3282 1.660000000
    ## 234 2013/2014        1    PL3282A 0.916000000
    ## 235 2015/2016        1    PL3282A 1.520000000
    ## 236 2011/2012        2    PL3282A 0.903333333
    ## 237 2014/2015        2    PL3282A 1.320000000
    ## 238 2016/2017        2    PL3282A 1.520000000
    ## 239 2015/2016        1    PL3282C 3.020000000
    ## 240 2017/2018        1    PL3282C 1.440000000
    ## 241 2013/2014        2    PL3282C 1.200000000
    ## 242 2015/2016        2    PL3282C 3.200000000
    ## 243 2014/2015        1     PL3283 1.120000000
    ## 244 2017/2018        1     PL3283 1.440000000
    ## 245 2013/2014        2    PL3283A 0.008333333
    ## 246 2016/2017        2    PL3283A 1.440000000
    ## 247 2013/2014        1    PL3283B 1.777777778
    ## 248 2018/2019        1    PL3283B 0.571428571
    ## 249 2011/2012        2    PL3283B 0.450000000
    ## 250 2014/2015        2    PL3283B 1.816666667
    ##  [ reached 'max' / getOption("max.print") -- omitted 230 rows ]
