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
  filter(!grepl("Surface_Shots.*", objects)) %>%
  filter(!grepl("^Length.*", event)) %>%
  mutate(subsample = str_extract(objects, "\\d")) %>%
  mutate(index = str_extract(objects, "\\h\\d{1,2}")) %>%
  mutate(species = str_extract(objects, "Omykiss|Okisutch")) %>%
  transform(index = as.numeric(index),
            subsample = as.numeric(subsample)) %>%
  arrange(subsample, index, time) %>%
  select(index, X, Y, Z)
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

    ##     index          X          Y         Z
    ## 1       1 -23.078009   3.411723 11.938233
    ## 2       1 -22.973185   5.415001 12.786056
    ## 3       1 -22.106491   6.173606 13.333946
    ## 4       1 -27.442520  10.006479 14.518783
    ## 5       1 -29.527451   8.890507 12.267792
    ## 6       1 -31.508133   7.744561 11.626307
    ## 7       1 -33.159050   6.767259 12.171721
    ## 8       1 -33.763794   5.878051 12.061245
    ## 9       1 -33.479637   6.812507 10.638883
    ## 10      1 -32.608955   6.793159  9.348812
    ## 11      1 -32.604950   6.731676  9.386993
    ## 12      1 -32.140171   7.141226 11.228156
    ## 13      2  -7.026154  34.834404 26.768690
    ## 14      2 -19.101324  40.421997 26.564219
    ## 15      2 -26.246796  44.470341 30.144695
    ## 16      2 -31.349220  41.586670 28.209734
    ## 17      2 -33.984299  42.365936 29.235270
    ## 18      2 -34.051712  42.753422 29.325544
    ## 19      2 -34.114788  42.872814 29.447214
    ## 20      2 -33.821335  42.798191 29.379492
    ## 21      2 -33.336456  42.583626 29.357468
    ## 22      2 -33.651917  42.813026 29.410383
    ## 23      2 -34.055374  43.040688 29.425312
    ## 24      4  40.772434  16.246338 23.405752
    ## 25      4  28.572575  20.982519 21.084307
    ## 26      4  23.362726  24.858883 23.597343
    ## 27      4  18.514069  25.507168 23.272663
    ## 28      4  16.835274  17.660643 18.810835
    ## 29      4   3.826874  13.366543 19.605042
    ## 30      4  -9.188037  10.832491 15.312627
    ## 31      4   4.885240  13.714439 12.339429
    ## 32      4  23.678432  22.044739 17.393818
    ## 33      5 -32.126385   7.132728 11.203806
    ## 34      5 -31.402906   6.582304 13.228374
    ## 35      5 -31.156258   7.701831 12.292812
    ## 36      5 -30.863628   7.890131 11.275361
    ## 37      5 -28.615021   8.571738 11.289070
    ## 38      5 -26.301426   9.017186 11.500470
    ## 39      5 -23.687634   7.152688 10.748343
    ## 40      5 -22.161510   6.159070 12.159154
    ## 41      5 -21.526276   5.270853 13.399899
    ## 42      5 -21.072355   5.538324 14.664489
    ## 43      5 -21.357321   4.887283 12.851006
    ## 44      6 -34.104053  42.914009 29.444729
    ## 45      6 -34.367905  43.147938 29.204493
    ## 46      6 -31.707382  38.683304 24.608328
    ## 47      6 -11.089192  36.657261 17.124126
    ## 48      6  22.345396  28.578291 17.454411
    ## 49      6  71.237396  62.993378 24.199121
    ## 50      6  82.372070  80.302994 33.584347
    ## 51      7  23.657663  22.015530 17.350163
    ## 52      7  10.057391  19.583582 13.718589
    ## 53      7  19.665714  16.520994  9.403399
    ## 54      8 -29.963449   7.097096 11.618945
    ## 55      8 -30.434814   6.915797 12.922447
    ## 56      8 -31.453930   6.434896 13.962059
    ## 57      8 -31.698435   6.894128 12.883967
    ## 58      8 -30.989958   8.011254 12.309785
    ## 59      8 -30.609930   8.766670 11.919057
    ## 60      8 -30.966528   8.702211 12.639673
    ## 61      8 -31.058008   8.805740 12.581199
    ## 62      8 -31.343084   8.262635 13.553599
    ## 63      8 -31.268032   7.971519 13.074303
    ## 64      8 -30.586563   8.554869 12.418899
    ## 65      9  28.500837  20.519726 24.184092
    ## 66      9  22.060797  26.873400 23.569561
    ## 67      9  12.657820  23.679113 21.586330
    ## 68      9   1.119950  15.664955 17.770964
    ## 69      9   0.518677  12.591192 12.550055
    ## 70      9  14.009833  21.358189 16.558275
    ## 71      9  32.998287  11.675905 16.902779
    ## 72      9  40.545143  10.021884 18.700928
    ## 73     10  50.478569  33.408245  3.902659
    ## 74     10  37.036655  36.654034  1.314986
    ## 75     10  23.502205  20.466698  1.845530
    ## 76     10  35.924854  22.852163  4.152395
    ## 77     10  41.214928  23.678543  5.688920
    ## 78     10  44.906376  23.336260  7.744632
    ## 79     11 -30.635191   8.594416 12.411424
    ## 80     11 -30.559694   8.856770 13.003700
    ## 81     11 -30.090473   9.123665 12.605051
    ## 82     11 -30.026884   8.952987 12.817900
    ## 83     11 -30.557674   8.438985 12.328672
    ## 84     11 -30.960842   7.794116 12.162962
    ## 85     11 -30.996244   7.200426 12.510031
    ## 86     11 -31.031397   7.578411 11.933863
    ## 87     11 -31.038284   7.382028 12.382677
    ## 88     11 -30.734951   8.080538 12.547264
    ## 89     11 -30.500679   8.508602 12.619700
    ## 90     12  45.039738  23.583334  7.680876
    ## 91     12  48.837521  23.687290  9.845806
    ## 92     12  52.243008  24.372482 12.080979
    ## 93     12  54.695778  23.858971 14.388877
    ## 94     12  55.738770  23.339039 15.073475
    ## 95     13 -13.689900   0.044163 12.509172
    ## 96     13 -13.837792  -1.006487  9.660345
    ## 97     13 -14.377450  -1.202235 11.769453
    ## 98     13 -15.962735  -3.887674 13.263625
    ## 99     13 -16.125690  -6.092714 12.528745
    ## 100    13 -15.656535   2.435479 12.857511
    ## 101    13 -17.681627  -0.726423 11.693510
    ## 102    13 -19.163017  -1.424518 11.742021
    ## 103    13 -20.169086  -2.457029 13.524416
    ## 104    13 -20.182798  -3.250389 15.464918
    ## 105    13 -19.554798  -3.947975 16.862404
    ## 106    14   1.951003  20.231867 21.684807
    ## 107    14  22.077633   5.740751 18.265329
    ## 108    14  40.360245   4.308479 21.480764
    ## 109    15  15.452000  21.480284 25.551214
    ## 110    15  14.941460  19.187593 26.014481
    ## 111    15  11.378820  25.592571 25.562218
    ## 112    15   8.648345  23.831715 23.778713
    ## 113    15   5.833257  20.283167 20.095591
    ## 114    15  11.611193  26.928522 24.814606
    ## 115    15  20.959175  25.592369 24.368664
    ## 116    15  19.978947  22.571234 28.965345
    ## 117    15  15.566965  22.625849 24.391991
    ## 118    15  11.987823  23.393911 25.821377
    ## 119    15  14.536018  27.092579 27.544935
    ## 120    16  48.923500  14.527243  7.029369
    ## 121    16  41.273911  11.753334  5.014964
    ## 122    16  47.956833   7.092026  7.193985
    ## 123    17  10.553918  16.066872 20.865450
    ## 124    17   6.507578  16.052048 22.254284
    ## 125    17  -0.461419  17.950447 23.136850
    ## 126    17  -5.964930  20.336172 23.581335
    ## 127    17 -17.395208  19.151602 24.195642
    ## 128    17 -13.982160  19.801207 23.373255
    ## 129    17   9.459368  22.880774 27.023039
    ## 130    17   0.428975  22.276934 24.810120
    ## 131    17 -12.589611  24.351517 24.504801
    ## 132    17 -22.068800  20.614349 23.788166
    ## 133    17 -21.439043  19.664572 23.292780
    ## 134    18  22.852430  -3.962516 18.414503
    ## 135    18   3.522006  -9.642806 18.036297
    ## 136    18 -19.010666  -8.871580 15.947102
    ## 137    18   6.556968  -2.735390 17.371771
    ## 138    18  27.485966   1.816277 20.145166
    ## 139    18  35.870716   6.374202 21.644958
    ## 140    19 -22.708870 -29.283552 12.422365
    ## 141    19 -23.688078 -26.968407 11.549685
    ## 142    19 -24.779497 -24.629625 11.354573
    ## 143    19 -25.691687 -21.529154 11.752361

Write.CSV prepping for matlab
=============================

``` r
index1 <- BACI_RoachRun_Control %>%
  filter(index == 1) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_1.csv", row.names = FALSE)
index1
```

    ## NULL

``` r
index2 <- BACI_RoachRun_Control %>%
  filter(index == 2) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_2.csv", row.names = FALSE)
index2
```

    ## NULL

``` r
index4 <- BACI_RoachRun_Control %>%
  filter(index == 4) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_4.csv", row.names = FALSE)
index4
```

    ## NULL

``` r
index5 <- BACI_RoachRun_Control %>%
  filter(index == 5) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_5.csv", row.names = FALSE)
index5
```

    ## NULL

``` r
index6 <- BACI_RoachRun_Control %>%
  filter(index == 6) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_6.csv", row.names = FALSE)
index6
```

    ## NULL

``` r
index7 <- BACI_RoachRun_Control %>%
  filter(index == 7) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_7.csv", row.names = FALSE)
index7
```

    ## NULL

``` r
index8 <- BACI_RoachRun_Control %>%
  filter(index == 8) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_8.csv", row.names = FALSE)
index8
```

    ## NULL

``` r
index9 <- BACI_RoachRun_Control %>%
  filter(index == 9) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_9.csv", row.names = FALSE)
index9
```

    ## NULL

``` r
index10 <- BACI_RoachRun_Control %>%
  filter(index == 10) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_10.csv", row.names = FALSE)
index10
```

    ## NULL

``` r
index11 <- BACI_RoachRun_Control %>%
  filter(index == 11) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_11.csv", row.names = FALSE)
index11
```

    ## NULL

``` r
index12 <- BACI_RoachRun_Control %>%
  filter(index == 12) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_12.csv", row.names = FALSE)
index12
```

    ## NULL

``` r
index13 <- BACI_RoachRun_Control %>%
  filter(index == 13) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_13.csv", row.names = FALSE)
index13
```

    ## NULL

``` r
index14 <- BACI_RoachRun_Control %>%
  filter(index == 14) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_14.csv", row.names = FALSE)
index14
```

    ## NULL

``` r
index15 <- BACI_RoachRun_Control %>%
  filter(index == 15) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_15.csv", row.names = FALSE)
index15
```

    ## NULL

``` r
index16 <- BACI_RoachRun_Control %>%
  filter(index == 16) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_16.csv", row.names = FALSE)
index16
```

    ## NULL

``` r
index17 <- BACI_RoachRun_Control %>%
  filter(index == 17) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_17.csv", row.names = FALSE)
index17
```

    ## NULL

``` r
index18 <- BACI_RoachRun_Control %>%
  filter(index == 18) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_18.csv", row.names = FALSE)
index18
```

    ## NULL

``` r
index19 <- BACI_RoachRun_Control %>%
  filter(index == 19) %>%
  select(X, Y, Z) %>%
  `colnames<-`(NULL) %>%
  write.csv(file = "BACI_RoachRun_Control_19.csv", row.names = FALSE)
index19
```

    ## NULL
