Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$wind.sp <- met$wind.sp/10
met$temp <- met$temp/10
met$dew.point <- met$dew.point/10
met$atm.press <- met$atm.press/10
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met_dt <- merge(
# Data
 x = met,
 y = stations,
# List of variables to match
 by.x = "USAFID",
 by.y = "USAF",
# Which obs to keep?
 all.x = TRUE,
 all.y = FALSE
 )
```

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
state_median_temp = quantile(met_dt$temp, 0.5, na.rm=TRUE)
state_median_wind = quantile(met_dt$wind.sp, 0.5, na.rm=TRUE)
state_median_atm = quantile(met_dt$atm.press, 0.5, na.rm=TRUE)
```

Next identify the stations have these median values.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.0
    ## ✔ readr     2.1.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()         masks data.table::between()
    ## ✖ nlme::collapse()         masks dplyr::collapse()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ dplyr::first()           masks data.table::first()
    ## ✖ kableExtra::group_rows() masks dplyr::group_rows()
    ## ✖ lubridate::hour()        masks data.table::hour()
    ## ✖ lubridate::isoweek()     masks data.table::isoweek()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ dplyr::last()            masks data.table::last()
    ## ✖ lubridate::mday()        masks data.table::mday()
    ## ✖ lubridate::minute()      masks data.table::minute()
    ## ✖ lubridate::month()       masks data.table::month()
    ## ✖ lubridate::quarter()     masks data.table::quarter()
    ## ✖ lubridate::second()      masks data.table::second()
    ## ✖ purrr::transpose()       masks data.table::transpose()
    ## ✖ lubridate::wday()        masks data.table::wday()
    ## ✖ lubridate::week()        masks data.table::week()
    ## ✖ lubridate::yday()        masks data.table::yday()
    ## ✖ lubridate::year()        masks data.table::year()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
station_median_temp <- met_dt |>
 group_by(USAFID) |>
  summarize(temp = median(temp, na.rm=TRUE), STATE = first(STATE), LAT = first(LAT), LON = first(LON)
  ) |> filter(!is.na(temp)) 

station_median_wind <- met_dt |>
 group_by(USAFID) |>
  summarize(wind.sp = median(wind.sp, na.rm=TRUE), STATE = first(STATE), LAT = first(LAT), LON = first(LON)
  ) |> filter(!is.na(wind.sp))

station_median_atm <- met_dt |>
 group_by(USAFID) |>
  summarize(atm.press = median(atm.press, na.rm=TRUE), STATE = first(STATE), LAT = first(LAT), LON = first(LON)) |> filter(!is.na(atm.press))

stations_temp <-station_median_temp[station_median_temp$temp==state_median_temp, "USAFID"]
stations_wind <- station_median_wind[station_median_wind$wind.sp==state_median_wind,"USAFID"]
stations_atm <- station_median_atm[station_median_atm$atm.press==state_median_atm,"USAFID"]

temp = intersect(stations_temp, stations_wind)
intersect(temp, stations_atm)
```

    ## # A tibble: 1 × 1
    ##   USAFID
    ##    <int>
    ## 1 723119

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
station_median_temp$distance = sqrt((station_median_temp$temp - state_median_temp)^2 + (station_median_wind$wind.sp - state_median_wind)^2)
```

    ## Warning in (station_median_temp$temp - state_median_temp)^2 +
    ## (station_median_wind$wind.sp - : longer object length is not a multiple of
    ## shorter object length

``` r
closest <- station_median_temp %>% 
   group_by(STATE) %>% 
   filter(distance == min(distance)) %>%
  select(USAFID, STATE, distance, LAT, LON)
```

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
state_median_lat = quantile(met_dt$LAT, 0.5, na.rm=TRUE)
state_median_lon = quantile(met_dt$LAT, 0.5, na.rm=TRUE)
stations_lat_lon = met_dt %>% select(USAFID, LAT, LON, STATE)
stations_lat_lon = distinct(stations_lat_lon)
stations_lat_lon$distance = sqrt((stations_lat_lon$LAT - state_median_lat)^2 + (stations_lat_lon$LON - state_median_lon)^2)
closest2 <- stations_lat_lon %>% 
   group_by(STATE) %>% 
   filter(distance == min(distance)) %>%
  select(USAFID, STATE, distance, LAT, LON)
```

``` r
install.packages("webshot")
webshot::install_phantomjs()
```

``` r
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>% 
  addCircles(
 data = closest,
 lat = ~LAT, lng = ~LON, opacity = 1, fillOpacity = 1, radius = 400, color = "blue"
 ) %>%
 addCircles(
 data = closest2,
 lat = ~LAT, lng = ~LON, opacity=1, fillOpacity=1, radius = 1400, color = "red")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

``` r
met_elev <- met_dt %>% mutate(elevation_category = ifelse(elev < 93, "Low",
                                ifelse(elev >= 93 & elev < 401, "Mid", "High")))
met_elev <- met_elev %>%
  group_by(STATE, elevation_category) %>%
  summarise(avg_temp = mean(temp, na.rm = TRUE), .groups = 'drop')

met_elev <- met_elev %>%
  pivot_wider(names_from = elevation_category, values_from = avg_temp)

kable(met_elev)
```

| STATE |      Low |      Mid |      High |
|:------|---------:|---------:|----------:|
| AL    | 25.07106 | 23.79775 |        NA |
| AR    | 25.58698 | 24.40578 | 23.723926 |
| AZ    | 29.28585 | 30.38057 | 23.892609 |
| CA    | 18.25508 | 18.77071 | 18.148808 |
| CO    |       NA |       NA | 15.184075 |
| CT    | 19.37249 | 18.78433 |        NA |
| DE    | 21.40611 |       NA |        NA |
| FL    | 26.61484 |       NA |        NA |
| GA    | 24.80529 | 23.23841 |        NA |
| IA    |       NA | 22.26228 | 21.992787 |
| ID    |       NA |       NA | 16.415667 |
| IL    |       NA | 22.11707 | 20.843173 |
| IN    |       NA | 20.12731 |        NA |
| KS    |       NA | 24.16196 | 22.098776 |
| KY    |       NA | 21.36103 | 20.178196 |
| LA    | 27.61819 | 26.09414 |        NA |
| MA    | 17.44477 | 17.59058 |        NA |
| MD    | 21.25462 | 20.62255 | 20.648332 |
| ME    | 15.23159 | 15.43930 | 15.329681 |
| MI    |       NA | 18.54432 | 17.977982 |
| MN    | 22.66275 | 21.15523 | 19.931963 |
| MO    | 25.79654 | 23.77652 | 23.300286 |
| MS    | 26.34285 | 24.66682 |        NA |
| MT    |       NA |       NA | 16.293015 |
| NC    | 22.82945 | 21.21073 | 18.046833 |
| ND    |       NA | 21.79236 | 20.415848 |
| NE    |       NA | 23.48598 | 21.048920 |
| NH    | 17.78844 | 16.77731 |  7.243417 |
| NJ    | 19.96563 | 19.31963 |        NA |
| NM    |       NA |       NA | 22.448418 |
| NV    |       NA |       NA | 20.849170 |
| NY    | 18.75621 | 18.31489 | 15.887585 |
| OH    |       NA | 19.43774 |        NA |
| OK    |       NA | 25.07676 | 24.000040 |
| OR    | 15.20318 | 16.39100 | 16.711553 |
| PA    | 20.34185 | 19.40527 | 17.286934 |
| RI    | 17.88116 | 17.46589 |        NA |
| SC    | 23.68407 | 22.38995 |        NA |
| SD    |       NA | 22.79495 | 20.639922 |
| TN    | 25.81362 | 22.89642 | 19.457179 |
| TX    | 28.74462 | 28.08021 | 26.500393 |
| UT    |       NA |       NA | 19.754720 |
| VA    | 21.34826 | 20.49998 | 17.954522 |
| VT    |      NaN | 16.89971 |        NA |
| WA    | 15.25193 | 17.80542 | 16.810354 |
| WI    |       NA | 19.56563 | 17.994615 |
| WV    |       NA | 19.31079 | 17.492150 |
| WY    |       NA |       NA | 13.748173 |

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
