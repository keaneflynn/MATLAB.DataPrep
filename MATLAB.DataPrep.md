MATLAB.DataPrep
================
Keane Flynn
12/21/2018

Loading Libraries
=================

``` r
library(readr)
library(dbplyr)
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ ggplot2 3.0.0     ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::ident()  masks dbplyr::ident()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ dplyr::sql()    masks dbplyr::sql()

Import Dataset
==============

``` r
BACI_RoachRun_Control <- 
  readr::read_csv(file = "Porter_BACI_RoachRun_29June2018_Part3.csv",
                  skip = 1,
                  col_names = c("objects", "event", "timecode", "time", "X", "Y", "Z", "pld_error", "projection_error", "nearest_camera_distance", "screen_coordinates")) %>%
  arrange(objects) %>%
  filter(!objects == "Surface_Shots 3") %>%
  filter(!grepl("^Length.*", event)) %>%
  mutate(subsample = str_extract(objects, "\\d")) %>%
  mutate(index = str_extract(objects, "\\h\\d{1,2}")) %>%
  mutate(species = str_extract(objects, "Omykiss|Okisutch")) %>%
  select(objects, subsample, index, species, time, X, Y, Z)
```

    ## Parsed with column specification:
    ## cols(
    ##   objects = col_character(),
    ##   event = col_character(),
    ##   timecode = col_character(),
    ##   time = col_double(),
    ##   X = col_double(),
    ##   Y = col_double(),
    ##   Z = col_double(),
    ##   pld_error = col_double(),
    ##   projection_error = col_double(),
    ##   nearest_camera_distance = col_double(),
    ##   screen_coordinates = col_character()
    ## )

``` r
BACI_RoachRun_Control
```

    ## # A tibble: 143 x 8
    ##    objects                 subsample index species  time     X     Y     Z
    ##    <chr>                   <chr>     <chr> <chr>   <dbl> <dbl> <dbl> <dbl>
    ##  1 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss  90.0 -23.1  3.41 11.9 
    ##  2 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss  99.0 -27.4 10.0  14.5 
    ##  3 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss  93.0 -23.0  5.42 12.8 
    ##  4 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss 105.  -31.5  7.74 11.6 
    ##  5 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss  96.0 -22.1  6.17 13.3 
    ##  6 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss 102.  -29.5  8.89 12.3 
    ##  7 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss 108.  -33.2  6.77 12.2 
    ##  8 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss 114.  -33.5  6.81 10.6 
    ##  9 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss 117.  -32.6  6.79  9.35
    ## 10 Subsample_1 1 (Omykiss) 1         " 1"  Omykiss 120.  -32.1  7.14 11.2 
    ## # ... with 133 more rows
