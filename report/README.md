Programming assignment 4
================

**Author**: Ivan Andreu Rascon  
**Date**: Last update: 2022-04-17 22:43:45

# Overview

# Description of the assignment

In this programming assignment I worked on a relatively large project
production of 6 participants. I used Praat to normalize the scale peak
intensity, create textgrids and annotate them. After fixing the extra
“#” on that made the code not work, adding the number of files and
changing the names I calculated values for (f1, f2, vot) and put then on
a CVS file.

# Hypothesis

Based on the article I presented in class Bradlow (1995) F1 and F2
values should be higher for native English of English in comparison with
Spanish-English bilinguals.

In relation with the VOTs analysis, the l2 learners, might have longer
VOTs than the bilinguals, due to the stop consonant differences between
Spanish and English. This is because Spanish has a short and lead lag,
and English has short, long, and lead lag.

# Prep

## Libraries

``` r
library(tidyverse)
library(untidydata)
library(ggplot2)
library(readr)
library(dplyr)
library(knitr)
library(here)
```

## Load data

``` r
bi01 <- read_csv("../data/bi01.csv") 
```

    ## Rows: 45 Columns: 5
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): fileID, notes
    ## dbl (3): f1, f2, vot
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bi02 <- read_csv("../data/bi02.csv")
```

    ## Rows: 45 Columns: 5
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): fileID
    ## dbl (3): f1, f2, vot
    ## lgl (1): notes
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
bi03 <- read_csv("../data/bi03.csv")
```

    ## Rows: 45 Columns: 5
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): fileID
    ## dbl (3): f1, f2, vot
    ## lgl (1): notes
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ne01 <- read_csv("../data/ne01.csv")
```

    ## Rows: 45 Columns: 5
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): fileID
    ## dbl (3): f1, f2, vot
    ## lgl (1): notes
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ne02 <- read_csv("../data/ne02.csv")
```

    ## Rows: 45 Columns: 5
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): fileID
    ## dbl (3): f1, f2, vot
    ## lgl (1): notes
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ne03 <- read_csv("../data/ne03.csv")
```

    ## Rows: 45 Columns: 5
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): fileID
    ## dbl (3): f1, f2, vot
    ## lgl (1): notes
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
datos = bind_rows(bi01, bi02, bi03, ne01, ne02, ne03)   
```

## Tidy data

``` r
# Convert from long to wide or wide to long format as necessary (check 
# examples from class)
# Create any other relevant variables here 

datos = separate(data = datos, col = fileID, into = c("id", "word"), sep = "_")
datos = separate(data = datos, col = word, into = c("word", "trial"), sep = "(?<=[A-Za-z])(?=[0-9])", extra = "drop", fill = "right")
datos = separate(data = datos, col = id, into = c("group", "id"), sep = "(?<=[A-Za-z])(?=[0-9])")


glimpse(datos)
```

    ## Rows: 270
    ## Columns: 8
    ## $ group <chr> "bi", "bi", "bi", "bi", "bi", "bi", "bi", "bi", "bi", "bi", "bi"~
    ## $ id    <chr> "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "01"~
    ## $ word  <chr> "kaka", "kaka", "kaka", "keke", "keke", "keke", "kiki", "kiki", ~
    ## $ trial <chr> NA, "1", "2", NA, "1", "2", NA, "1", "2", NA, "1", "2", NA, "1",~
    ## $ f1    <dbl> 650.90, 714.32, 709.19, 495.24, 893.01, 579.31, 823.90, 348.26, ~
    ## $ f2    <dbl> 1637.02, 1567.58, 1560.03, 2168.42, 2152.98, 2339.40, 2711.97, 2~
    ## $ vot   <dbl> 24.81, 25.49, 28.29, 31.55, 32.39, 32.87, 18.54, 52.80, 50.48, 3~
    ## $ notes <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "error",~

# Analysis

## Descriptives

``` r
means = datos %>%
  group_by(group) %>%
  summarise(mean_f1 = mean(f1), sd_f1 = sd(f1), mean_f2 = mean(f2), sd_f2 = sd(f2), mean_vot = mean(vot), sd_vot = sd(vot), n = n())
kable(means, caption = "**Means and SD per group**")
```

| group |  mean_f1 |    sd_f1 |  mean_f2 |    sd_f2 | mean_vot |   sd_vot |   n |
|:------|---------:|---------:|---------:|---------:|---------:|---------:|----:|
| bi    | 552.4887 | 188.0309 | 1682.925 | 676.2459 | 21.89963 | 18.28747 | 135 |
| ne    | 570.7985 | 202.1939 | 1774.304 | 650.3782 | 24.00452 | 15.49217 | 135 |

**Means and SD per group**

``` r
table <- datos %>%
  group_by(group) %>%
  summarise(mean_vot = mean(vot), sd_vot = sd(vot), n = n())
kable(table, caption = "**Mean VOT per group**")
```

| group | mean_vot |   sd_vot |   n |
|:------|---------:|---------:|----:|
| bi    | 21.89963 | 18.28747 | 135 |
| ne    | 24.00452 | 15.49217 | 135 |

**Mean VOT per group**

------------------------------------------------------------------------

``` r
# Give some descriptive summaries of your data 
# Display your descriptives in a table

table <-datos %>%
  group_by(group, word) %>%
  summarize(meanf1 = mean(f1), sdf1 = sd(f1), meanf2 = mean(f2), sdf2 = sd(f2), meanvot = mean(vot), sdvot = sd(vot), n = n() )
```

    ## `summarise()` has grouped output by 'group'. You can override using the
    ## `.groups` argument.

``` r
kable(table, caption = "table")
```

| group | word |   meanf1 |      sdf1 |    meanf2 |      sdf2 |  meanvot |     sdvot |   n |
|:------|:-----|---------:|----------:|----------:|----------:|---------:|----------:|----:|
| bi    | kaka | 750.4800 | 153.81093 | 1604.2122 |  92.42319 | 22.60000 | 41.958136 |   9 |
| bi    | keke | 562.1900 | 130.57849 | 2291.4878 | 116.02054 | 23.08889 | 10.083459 |   9 |
| bi    | kiki | 420.7322 | 159.33616 | 2639.8656 | 108.13647 | 32.05222 | 12.094547 |   9 |
| bi    | koko | 576.2489 |  86.15305 | 1009.6744 | 156.57539 | 28.40667 |  5.371273 |   9 |
| bi    | kuku | 386.3722 |  99.95772 | 1064.3867 | 343.58741 | 45.67889 | 46.108261 |   9 |
| bi    | paka | 740.6400 | 198.08087 | 1611.5900 | 196.82318 | 14.43222 |  5.724010 |   9 |
| bi    | peke | 541.7556 | 143.83993 | 2274.6422 | 231.43129 | 16.50556 |  4.056625 |   9 |
| bi    | piki | 500.7978 | 388.23985 | 2557.6622 | 379.08701 | 21.16778 |  6.201796 |   9 |
| bi    | poko | 541.3256 |  86.55752 | 1092.5300 | 307.87907 | 18.64111 |  4.711386 |   9 |
| bi    | puku | 477.4356 |  87.14523 |  945.4067 | 311.53910 | 25.80222 | 10.639493 |   9 |
| bi    | taka | 693.9478 | 185.99154 | 1566.3756 |  98.81015 | 15.13000 |  2.978498 |   9 |
| bi    | teke | 492.9300 |  81.49080 | 2252.9344 | 283.03867 | 15.50000 |  2.002892 |   9 |
| bi    | tiki | 496.6178 | 186.63976 | 2424.7400 | 280.88930 | 19.35333 |  5.144842 |   9 |
| bi    | toko | 623.3511 | 118.36424 | 1039.1378 |  80.58253 | 15.14889 |  2.805635 |   9 |
| bi    | tuku | 482.5067 |  82.75363 |  869.2244 | 104.23037 | 14.98667 |  5.584463 |   9 |
| ne    | kaka | 770.9789 | 291.23106 | 1686.2978 | 290.86143 | 29.76889 | 16.242350 |   9 |
| ne    | keke | 540.0633 |  73.55358 | 2401.4744 | 269.48730 | 29.20556 |  7.014453 |   9 |
| ne    | kiki | 449.4411 | 199.68443 | 2653.7878 | 267.84131 | 46.46222 | 20.419941 |   9 |
| ne    | koko | 553.0233 |  76.25111 | 1081.9422 |  56.36185 | 26.70778 |  9.242023 |   9 |
| ne    | kuku | 568.1044 | 249.08427 | 1213.8922 | 351.66396 | 33.51222 | 15.959377 |   9 |
| ne    | paka | 823.8022 | 104.05865 | 1694.3678 | 209.13116 | 11.28667 |  4.789045 |   9 |
| ne    | peke | 529.3667 | 120.40930 | 2371.0544 | 408.19042 | 16.82556 | 13.452713 |   9 |
| ne    | piki | 375.1767 |  42.14757 | 2624.1944 | 326.52218 | 10.74444 |  8.276201 |   9 |
| ne    | poko | 619.8189 | 156.77297 | 1263.4078 | 351.94215 | 17.86444 |  8.776474 |   9 |
| ne    | puku | 492.0767 | 127.91044 | 1263.1656 | 319.08776 | 27.33556 | 15.954532 |   9 |
| ne    | taka | 822.9156 | 200.74233 | 1516.1156 | 246.09334 | 13.23667 |  6.372941 |   9 |
| ne    | teke | 575.0556 |  81.82047 | 2058.4967 | 363.33581 | 19.22222 | 10.632554 |   9 |
| ne    | tiki | 345.5700 |  44.87714 | 2493.5467 | 520.50691 | 27.54000 | 19.898339 |   9 |
| ne    | toko | 557.0667 |  58.21457 | 1075.9722 |  70.25063 | 21.72222 | 13.606207 |   9 |
| ne    | tuku | 539.5178 | 216.17059 | 1216.8378 | 223.96784 | 28.63333 | 13.165715 |   9 |

table

``` r
table2 <-datos %>%
  group_by(group, word) %>%
  summarize(mean(vot), sd(vot))
```

    ## `summarise()` has grouped output by 'group'. You can override using the
    ## `.groups` argument.

``` r
kable(table2, caption = "table2")
```

| group | word | mean(vot) |   sd(vot) |
|:------|:-----|----------:|----------:|
| bi    | kaka |  22.60000 | 41.958136 |
| bi    | keke |  23.08889 | 10.083459 |
| bi    | kiki |  32.05222 | 12.094547 |
| bi    | koko |  28.40667 |  5.371273 |
| bi    | kuku |  45.67889 | 46.108261 |
| bi    | paka |  14.43222 |  5.724010 |
| bi    | peke |  16.50556 |  4.056625 |
| bi    | piki |  21.16778 |  6.201796 |
| bi    | poko |  18.64111 |  4.711386 |
| bi    | puku |  25.80222 | 10.639493 |
| bi    | taka |  15.13000 |  2.978498 |
| bi    | teke |  15.50000 |  2.002892 |
| bi    | tiki |  19.35333 |  5.144842 |
| bi    | toko |  15.14889 |  2.805635 |
| bi    | tuku |  14.98667 |  5.584463 |
| ne    | kaka |  29.76889 | 16.242350 |
| ne    | keke |  29.20556 |  7.014453 |
| ne    | kiki |  46.46222 | 20.419941 |
| ne    | koko |  26.70778 |  9.242023 |
| ne    | kuku |  33.51222 | 15.959377 |
| ne    | paka |  11.28667 |  4.789045 |
| ne    | peke |  16.82556 | 13.452713 |
| ne    | piki |  10.74444 |  8.276201 |
| ne    | poko |  17.86444 |  8.776474 |
| ne    | puku |  27.33556 | 15.954532 |
| ne    | taka |  13.23667 |  6.372941 |
| ne    | teke |  19.22222 | 10.632554 |
| ne    | tiki |  27.54000 | 19.898339 |
| ne    | toko |  21.72222 | 13.606207 |
| ne    | tuku |  28.63333 | 13.165715 |

table2

## Visualization

``` r
# Include some plots here
```

``` r
ggplot(datos, aes(x = group, y = vot)) +
  geom_point(aes(color = group))
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
datos %>% 
  ggplot(.,aes(x=vot, y=word, color=id))+
  geom_point( size=3, color="green", fill=alpha("blue", 0.2), alpha=0.4, shape=21, stroke=2)
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
plot = filter(datos, word == "taka" | word == "teke" | word == "tiki" | word == "toko" | word == "tuku")
ggplot(data = plot, aes(x=word, y=vot, color=group)) + 
  geom_point( size=3, color="red", fill=alpha("black", 0.2), alpha=0.4, shape=21, stroke=2) +
  labs(title = "VOT  function of vowel type and group", y = "VOT", x = "Vowel type")
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Below I am including an image that represents F1 and F2 of /u/ in the
first syllable of /kuku/ of a Spanish-English bilingual

``` r
#Here I attach an image that represents F1 and F2 of /a/ in the first syllable of /kaka/ of a Spanish-English bilingual. VOT differences can algo be distinguished. 

knitr::include_graphics('../images/bi.png')
```

![](../images/bi.png)<!-- -->

``` r
#Here I attach an image that represents F1 and F2 of /a/ in the first syllable of /kaka/ of a L2 native of english. VOT differences can also be distinguished. 

#Native English speakers seem to have longer lag VOT than bilniguals.



knitr::include_graphics('../images/en.png')
```

![](../images/en.png)<!-- -->

As seen in Bradlow(1995) F1 and F2 values should be higher for native
English of English in comparison with Spanish-English bilinguals.

``` r
# Include some plots here

plot = filter(datos, word == "kuku" | word == "puku" | word == "tuku")
ggplot(plot, aes(x=word, y=f2, fill=group)) + 
    geom_boxplot()+ geom_point( size=3, color="red", fill=alpha("black", 0.2), alpha=0.4, shape=21, stroke=2)+
  facet_wrap(~group)+
  labs(title = "F2 of /u/ stop  and group", y = "F2", x = "Stop type /p/ /t/ /k/")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# Include some plots here

plot = filter(datos, word == "keke" | word == "peke" | word == "teke")
ggplot(plot, aes(x=word, y=f2, fill=group)) + 
    geom_boxplot()+ geom_point( size=3, color="red", fill=alpha("black", 0.2), alpha=0.4, shape=21, stroke=2)+
  facet_wrap(~group)+
  labs(title = "F2 of /e/ stop  and group", y = "F2", x = "Stop type /p/ /t/ /k/")
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Include some plots here

plot = filter(datos, word == "kiki" | word == "piki" | word == "tiki")
ggplot(plot, aes(x=word, y=f2, fill=group)) + 
    geom_boxplot()+ geom_point( size=3, color="red", fill=alpha("black", 0.2), alpha=0.4, shape=21, stroke=2)+
  facet_wrap(~group)+
  labs(title = "F2 of /i/ stop  and group", y = "F2", x = "Stop type /p/ /t/ /k/")
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Include some plots here

plot = filter(datos, word == "koko" | word == "poko" | word == "toko")
ggplot(plot, aes(x=word, y=f2, fill=group)) + 
    geom_boxplot()+ geom_point( size=3, color="red", fill=alpha("black", 0.2), alpha=0.4, shape=21, stroke=2)+
  facet_wrap(~group)+
  labs(title = "F2 of /o/ stop  and group", y = "F2", x = "Stop type /p/ /t/ /k/")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# Include some plots here

plot = filter(datos, word == "kaka" | word == "paka" | word == "taka")
ggplot(plot, aes(x=word, y=f2, fill=group)) + 
    geom_boxplot()+
  facet_wrap(~group)+ geom_point( size=3, color="red", fill=alpha("black", 0.2), alpha=0.4, shape=21, stroke=2)+
  labs(title = "F2 of /a/ stop  and group", y = "F2", x = "Stop type /p/ /t/ /k/")
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
plot = filter(datos, word == "keke" | word == "peke" | word == "teke")
ggplot(plot, aes(x=f2, y=f1, color = group, shape = group)) + 
    geom_point(size=3, alpha=0.7)+
  labs(title = "F1 and F2 of /e/ per group", y = "F1", x = "F2")
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
plot = filter(datos, word == "kiki" | word == "piki" | word == "tiki")
ggplot(plot, aes(x=f2, y=f1, color = group, shape = group)) + 
    geom_point(size=3, alpha=0.7)+
  labs(title = "F1 and F2 of /i/ per group", y = "F1", x = "F2")
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
plot = filter(datos, word == "kuku" | word == "puku" | word == "tuku")
ggplot(plot, aes(x=f2, y=f1, color = group, shape = group)) + 
    geom_point(size=3, alpha=0.7)+
  labs(title = "F1 and F2 of /u/ per group", y = "F1", x = "F2")
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
plot = filter(datos, word == "koko" | word == "poko" | word == "toko")
ggplot(plot, aes(x=f2, y=f1, color = group, shape = group)) + 
    geom_point(size=3, alpha=0.7)+
  labs(title = "F1 and F2 of /o/ per group", y = "F1", x = "F2")
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

–>

## Hypothesis test

``` r
# Conduct a simple statistical analysis here (optional)
```

# Conclusion

As seen on the plots, native English speakers seem to have longer lag
VOT’s than bilniguals. This is because Spanish has a short and lead lag,
and English has short, long, and lead lag.

In relation with my initial hypothesis from Bradlow (1995) the values
(F1 and F2) are slightly higher, therefore the present study seems to
corroborate the findings.
