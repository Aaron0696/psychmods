NUSmodAn
================
Aaron0696

  - [Phase 1: Setting Up Environment, Packages And Loading
    Data.](#phase-1-setting-up-environment-packages-and-loading-data.)
      - [Packages And Options](#packages-and-options)
      - [Bidding Data From `nusmods`](#bidding-data-from-nusmods)
      - [Load `myBid.RDS`](#load-mybid.rds)
      - [Module Information](#module-information)
      - [Load `myModInfo.RDS`](#load-mymodinfo.rds)
  - [Phase 2: Filter, Transform And Merge
    Data](#phase-2-filter-transform-and-merge-data)
      - [`myModInfo`](#mymodinfo)
          - [Filter](#filter)
      - [`myBid`](#mybid)
          - [Filter](#filter-1)
      - [Transform And Merge](#transform-and-merge)
      - [Coercing Columns To
        Factors/Numeric](#coercing-columns-to-factorsnumeric)
      - [Vectors Of Column Names](#vectors-of-column-names)
      - [Rearranging `DayText` Levels](#rearranging-daytext-levels)
      - [Rearranging `LessonTime`
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

<details>

<summary>Workflow</summary>

# Phase 1: Setting Up Environment, Packages And Loading Data.

## Packages And Options

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
knitr::opts_chunk$set(dpi = 300, out.width = "50%", eval = FALSE)
```

## Bidding Data From `nusmods`

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

## Load `myBid.RDS`

``` r
myBid <- readRDS("mydata.RDS")
```

## Module Information

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

## Load `myModInfo.RDS`

``` r
myModInfo <- readRDS("myModInfo.RDS")
```

# Phase 2: Filter, Transform And Merge Data

## `myModInfo`

  - Filter Module Information, `myModInfo`.
      - Removing non-Psychology modules.
      - Removing tutorial information.
      - Removing duplicated rows.

### Filter

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

## `myBid`

  - Filter CORS Bidding Information, `myBid`.
      - Removing non-Psychology modules, including Roots and Wings (PLS)
        and Psychology for non-Psychology students (PLB).
      - Removing information from reserved modules.

### Filter

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

## Transform And Merge

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

## Coercing Columns To Factors/Numeric

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

## Vectors Of Column Names

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

## Rearranging `DayText` Levels

``` r
mydata$DayText <- factor(mydata$DayText,
                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
```

## Rearranging `LessonTime` Levels

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

## Univariate Descriptive Statistics

``` r
describe(mydata)
```

``` r
summary(mydata)
```

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
          theme(axis.text.x = element_text(angle = 90, vjust = -0.3),
                legend.position = "none")
      )
    }
  }
}
```

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

### Correlation Matrix

``` r
corrplot.mixed(cor(mydata[,grep(paste0(numnames.time, collapse = "|"), names(mydata))]),
               upper = "color",
               tl.pos = "lt",
               tl.cex = 0.5,
               cl.cex = 0.5)
```

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

# Phase 5: Exploration

``` r
# number of modules
unique(mydata$ModuleCode)
```

``` r
# number of rows belonging to each module
xtabs(~ ModuleCode, data  = mydata, subset = NULL)
```

``` r
# datatable(myBid, filter = "top")
aggregate(BpQ ~ AcadYear + Semester + ModuleCode,
          data = mydata,
          FUN = mean)
```

</details>
