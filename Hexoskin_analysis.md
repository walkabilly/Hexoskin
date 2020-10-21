---
title: "Hexoskin"
author: "Jonathan Slaney and Daniel Fuller"
date: "03/10/2019"
output:
   html_document:
      keep_md: true
---

## Necessary packages


```r
library(knitr)
library(tidyverse)
```

```
## ── Attaching packages ───────────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ──────────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(tidymodels)
```

```
## ── Attaching packages ──────────────────────────────────────────────────── tidymodels 0.1.1 ──
```

```
## ✓ broom     0.7.0      ✓ recipes   0.1.13
## ✓ dials     0.0.9      ✓ rsample   0.0.7 
## ✓ infer     0.5.3      ✓ tune      0.1.1 
## ✓ modeldata 0.0.2      ✓ workflows 0.2.0 
## ✓ parsnip   0.1.3      ✓ yardstick 0.0.7
```

```
## ── Conflicts ─────────────────────────────────────────────────────── tidymodels_conflicts() ──
## x scales::discard() masks purrr::discard()
## x dplyr::filter()   masks stats::filter()
## x recipes::fixed()  masks stringr::fixed()
## x dplyr::lag()      masks stats::lag()
## x yardstick::spec() masks readr::spec()
## x recipes::step()   masks stats::step()
```

```r
library(ggmap)
```

```
## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
```

```
## Please cite ggmap if you use it! See citation("ggmap") for details.
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(readr)
library(psych)
```

```
## 
## Attaching package: 'psych'
```

```
## The following objects are masked from 'package:scales':
## 
##     alpha, rescale
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```

```r
library(ggpubr)
library(ggpmisc)
```

```
## 
## Attaching package: 'ggpmisc'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```r
library(effsize)
```

```
## 
## Attaching package: 'effsize'
```

```
## The following object is masked from 'package:psych':
## 
##     cohen.d
```

```r
library(SimDesign)
library(mgcv)
```

```
## Loading required package: nlme
```

```
## 
## Attaching package: 'nlme'
```

```
## The following object is masked from 'package:dplyr':
## 
##     collapse
```

```
## This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.
```

### Reading analysis data from "Hexoskin_wrangling.Rmd"

```r
hxd <- read_csv("hexo_data_analysis.csv")
```

```
## Parsed with column specification:
## cols(
##   participants = col_character(),
##   minute = col_datetime(format = ""),
##   rr = col_double(),
##   hr = col_double(),
##   ve_L = col_double(),
##   g = col_double(),
##   age = col_double(),
##   city = col_character(),
##   height = col_double(),
##   mass = col_double(),
##   sex = col_character(),
##   hrmax = col_double(),
##   percentmaxhr = col_double(),
##   VO = col_double(),
##   hr_max_220 = col_double(),
##   MET = col_double()
## )
```

```r
### Descriptives (for heart rate, breathing) psych package describe function
```


### Recoding city names


```r
hxd <- hxd %>%
  mutate(city = case_when(
  city == "Auckl" ~ "Auckland", 
  city == "Chris" ~ "Christchurch", 
  city == "Copen" ~ "Copenhagen", 
  city == "Delhi" ~ "Delhi", 
  city == "Lyon1" ~ "Lyon",
  city == "Montr" ~ "Montreal", 
  city == "Paris" ~ "Paris")
  )

table(hxd$city)
```

```
## 
##     Auckland Christchurch   Copenhagen        Delhi         Lyon     Montreal 
##         5555         4647         8428         7241         6075         6041 
##        Paris 
##         5089
```


```r
describe(hxd$hr)
```

```
##    vars     n   mean    sd median trimmed   mad min    max  range  skew
## X1    1 43076 119.16 21.45 120.39  119.45 22.58  50 199.52 149.52 -0.11
##    kurtosis  se
## X1     -0.3 0.1
```

```r
ggplot(hxd, aes(hr)) + 
    geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
describe(hxd$ve_L)
```

```
##    vars     n  mean    sd median trimmed  mad min    max  range skew kurtosis
## X1    1 43076 36.14 18.21  34.35   34.81 18.4   0 135.66 135.66 0.75     0.75
##      se
## X1 0.09
```

```r
ggplot(hxd, aes(ve_L)) + 
    geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
describe(hxd$MET)
```

```
##    vars     n  mean   sd median trimmed  mad min max range  skew kurtosis   se
## X1    1 43076 12.66 5.19     13   12.75 5.93   1  26    25 -0.14    -0.56 0.03
```

```r
ggplot(hxd, aes(MET)) + 
    geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Descriptives for Participants and Cities


```r
table(hxd$participants)
```

```
## 
##   DD   EL   JG   MG   PA   TA   VJ 
## 6175 5364 9965 2151 9425 3099 6897
```

```r
table(hxd$city)
```

```
## 
##     Auckland Christchurch   Copenhagen        Delhi         Lyon     Montreal 
##         5555         4647         8428         7241         6075         6041 
##        Paris 
##         5089
```

```r
table(hxd$city, hxd$participants)
```

```
##               
##                  DD   EL   JG   MG   PA   TA   VJ
##   Auckland        0 1892 1724    0 1939    0    0
##   Christchurch    0 1591 1622    0 1434    0    0
##   Copenhagen   2637    0 2692    0    0 3099    0
##   Delhi           0    0 2306    0 2364    0 2571
##   Lyon            0 1881    0    0 1999    0 2195
##   Montreal     1759    0    0 2151    0    0 2131
##   Paris        1779    0 1621    0 1689    0    0
```

### Histograms by city 


```r
ggplot(hxd, aes(hr)) +
  geom_histogram(aes(y = ..density..), fill = "cornflowerblue", colour = "black", size = 0.4) +
  geom_density(aes(y = ..density..), colour = "darkred", size = 1) + 
  ylab("HR density") + 
  xlab("Heart rate (bpm)") + 
   ggtitle("HR by city") + 
   facet_wrap(~ city)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### Histograms by city for Ve


```r
ggplot(hxd, aes(ve_L)) +
  geom_histogram(aes(y = ..density..), fill = "cornflowerblue", colour = "black", size = 0.4) +
  geom_density(aes(y = ..density..), colour = "darkred", size = 1) + 
  ylab("Density") + 
  xlab("Ve (L/min)") + 
   ggtitle("Ve by city") + 
   facet_wrap(~ city)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Histogram of all data


```r
ggplot(hxd, aes(hr)) +
  geom_histogram(aes(y = ..density..), fill = "cornflowerblue", colour = "black", size = 0.4) +
  geom_density(aes(y = ..density..), colour = "darkred", size = 1) + 
  ylab("HR density") + 
  xlab("Heart rate (bpm)")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Linear regressions by participant

### scatterplot for predictor and outcome


```r
cor_bar <- ggplot(hxd, aes(x = hr, y = ve_L)) + 
                  geom_point(size = 0.05) + 
  geom_smooth(method='lm', formula = y ~ x) + 
   facet_wrap(~ city)
plot(cor_bar)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### Calculate R^2

### Fit linear regression to participants


```r
cor_hexo_all <- ggplot(hxd, aes(x = hr, y = ve_L)) + 
                  geom_point(size = 0.1) + 
   geom_smooth(method='lm', formula = y ~ x) + 
   facet_wrap(~ participants)
plot(cor_hexo_all)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Here VJ is strange. Appears to have two distinct lines. Might indicates that this is two participants mixed? 

## Figure 1

Histograms of METS by City and VE by City. 


```r
met_histo_city <- ggplot(hxd, aes(x = MET)) + 
  geom_histogram(aes(y = ..density..), fill = "cornflowerblue", colour = "black", size = 0.4) +
  geom_density(aes(y = ..density..), colour = "darkred", size = 1) + 
  ylab("MET density") + 
  xlab("MET (bpm)") + 
   facet_wrap(~ city) + 
   theme_classic()
plot(met_histo_city)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
ve_histo_city <- ggplot(hxd, aes(x = ve_L)) + 
  geom_histogram(aes(y = ..density..), fill = "cornflowerblue", colour = "black", size = 0.4) +
  geom_density(aes(y = ..density..), colour = "darkred", size = 1) + 
  ylab("VE density") + 
  xlab("VE (bpm)") + 
   facet_wrap(~ city) + 
   theme_classic()
plot(ve_histo_city)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


```r
figure1 <- ggarrange(met_histo_city, ve_histo_city,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```r
figure1
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
ggsave("figure1.pdf", figure1, width = 10, height = 7, dpi = 200)
```

### Fit linear regression to all data

### Simple linear regression for hr and Ve


```r
lm_hr_ve <- lm(ve_L ~ hr, data = hxd)
summary(lm_hr_ve)
```

```
## 
## Call:
## lm(formula = ve_L ~ hr, data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -63.792  -7.931  -1.340   6.554  89.849 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -35.376222   0.350255  -101.0   <2e-16 ***
## hr            0.600181   0.002893   207.5   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.88 on 43074 degrees of freedom
## Multiple R-squared:  0.4998,	Adjusted R-squared:  0.4998 
## F-statistic: 4.304e+04 on 1 and 43074 DF,  p-value: < 2.2e-16
```

```r
fit_m1 <- glance(lm_hr_ve)
hxd$lm_fitted <- predict(lm_hr_ve)

ggplot(hxd, aes(x = hr, y = ve_L)) + 
   geom_point(size = 1, alpha = 0.05) + 
   geom_line(aes(y = lm_fitted), size = 1, colour = "red") +
   xlab("Cycling Intensity (bpm)") + 
   ylab("Breathing Volume (L/minute)")
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

### Simple linear regression for hr and Ve with VE squared


```r
lm_hr_ve2 <- lm(ve_L ~ hr + I(hr^2), data = hxd)
summary(lm_hr_ve2)
```

```
## 
## Call:
## lm(formula = ve_L ~ hr + I(hr^2), data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -75.359  -7.698  -1.336   6.661  78.500 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 11.6255889  1.4207438   8.183 2.85e-16 ***
## hr          -0.2243117  0.0243416  -9.215  < 2e-16 ***
## I(hr^2)      0.0034957  0.0001025  34.107  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.71 on 43073 degrees of freedom
## Multiple R-squared:  0.513,	Adjusted R-squared:  0.513 
## F-statistic: 2.268e+04 on 2 and 43073 DF,  p-value: < 2.2e-16
```

```r
fit_m2 <- glance(lm_hr_ve2)
hxd$lm2_fitted <- predict(lm_hr_ve2)

ggplot(hxd, aes(x = hr, y = ve_L)) + 
   geom_point(size = 1, alpha = 0.05) + 
   geom_line(aes(y = lm2_fitted), size = 1, colour = "red") +
   xlab("Cycling Intensity (bpm)") + 
   ylab("Breathing Volume (L/minute)")
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

### Simple linear regression for hr and Ve with VE cubed


```r
lm_hr_ve3 <- lm(ve_L ~ hr + I(hr^2) + I(hr^3), data = hxd)
summary(lm_hr_ve3)
```

```
## 
## Call:
## lm(formula = ve_L ~ hr + I(hr^2) + I(hr^3), data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -63.336  -7.690  -1.363   6.591  77.520 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.347e+02  4.772e+00   28.23   <2e-16 ***
## hr          -3.538e+00  1.251e-01  -28.29   <2e-16 ***
## I(hr^2)      3.226e-02  1.070e-03   30.15   <2e-16 ***
## I(hr^3)     -8.077e-05  2.991e-06  -27.00   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.6 on 43072 degrees of freedom
## Multiple R-squared:  0.5211,	Adjusted R-squared:  0.521 
## F-statistic: 1.562e+04 on 3 and 43072 DF,  p-value: < 2.2e-16
```

```r
fit_m3 <- glance(lm_hr_ve3)
hxd$lm3_fitted <- predict(lm_hr_ve3)

ggplot(hxd, aes(x = hr, y = ve_L)) + 
   geom_point(size = 1, alpha = 0.05) + 
   geom_line(aes(y = lm3_fitted), size = 1, colour = "red") +
   xlab("Cycling Intensity (bpm)") + 
   ylab("Breathing Volume (L/minute)")
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

### Linear regression for hr and Ve with VE squared plus gender


```r
lm_hr_ve4 <- lm(ve_L ~ hr + I(hr^2) + sex, data = hxd)
summary(lm_hr_ve4)
```

```
## 
## Call:
## lm(formula = ve_L ~ hr + I(hr^2) + sex, data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -74.753  -7.699  -1.487   6.666  78.875 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 11.2327814  1.4186711   7.918 2.47e-15 ***
## hr          -0.2233079  0.0242999  -9.190  < 2e-16 ***
## I(hr^2)      0.0034835  0.0001023  34.045  < 2e-16 ***
## sexWoman     1.6884166  0.1382434  12.213  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.69 on 43072 degrees of freedom
## Multiple R-squared:  0.5147,	Adjusted R-squared:  0.5146 
## F-statistic: 1.522e+04 on 3 and 43072 DF,  p-value: < 2.2e-16
```

```r
fit_m4 <- glance(lm_hr_ve4)

hxd$lm4_fitted <- predict(lm_hr_ve4, newdata = hxd, level = 0)

ggplot(hxd, aes(x = hr, y = ve_L, color = sex) ) +
     geom_point(size = 1, alpha = 0.05) + 
     geom_line(data = hxd, aes(y = lm4_fitted), size = 1) +
      xlab("Cycling Intensity (bpm)") + 
      ylab("Breathing Volume (L/minute)")
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

### Linear regression for hr and Ve with VE squared plus gender plus city 


```r
lm_hr_ve5 <- lm(ve_L ~ hr + I(hr^2) + sex + factor(city), data = hxd)
summary(lm_hr_ve5)
```

```
## 
## Call:
## lm(formula = ve_L ~ hr + I(hr^2) + sex + factor(city), data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -73.795  -7.264  -1.224   6.238  76.275 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               2.030e+01  1.346e+00  15.083   <2e-16 ***
## hr                       -3.713e-01  2.290e-02 -16.217   <2e-16 ***
## I(hr^2)                   4.090e-03  9.639e-05  42.433   <2e-16 ***
## sexWoman                 -2.036e-01  1.337e-01  -1.523    0.128    
## factor(city)Christchurch  2.837e+00  2.375e-01  11.946   <2e-16 ***
## factor(city)Copenhagen    3.828e+00  2.043e-01  18.734   <2e-16 ***
## factor(city)Delhi        -7.346e+00  2.164e-01 -33.939   <2e-16 ***
## factor(city)Lyon         -2.997e+00  2.245e-01 -13.347   <2e-16 ***
## factor(city)Montreal      7.535e+00  2.223e-01  33.891   <2e-16 ***
## factor(city)Paris        -2.307e+00  2.302e-01 -10.023   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.8 on 43066 degrees of freedom
## Multiple R-squared:  0.5803,	Adjusted R-squared:  0.5803 
## F-statistic:  6617 on 9 and 43066 DF,  p-value: < 2.2e-16
```

```r
fit_m5 <- glance(lm_hr_ve5)

hxd$lm5_fitted <- predict(lm_hr_ve5, newdata = hxd, level = 0)

ggplot(hxd, aes(x = hr, y = ve_L, color = city) ) +
     geom_point(size = 1, alpha = 0.05) + 
     geom_line(data = hxd, aes(y = lm5_fitted), size = 1) +
      xlab("Cycling Intensity (bpm)") + 
      ylab("Breathing Volume (L/minute)")
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

### Getting fit statistics for each model 


```r
fitstats <- rbind(fit_m1, fit_m2, fit_m3, fit_m4, fit_m5)
fitstats
```

```
## # A tibble: 5 x 12
##   r.squared adj.r.squared sigma statistic p.value    df  logLik    AIC    BIC
##       <dbl>         <dbl> <dbl>     <dbl>   <dbl> <dbl>   <dbl>  <dbl>  <dbl>
## 1     0.500         0.500  12.9    43043.       0     1 -1.71e5 3.42e5 3.42e5
## 2     0.513         0.513  12.7    22684.       0     2 -1.71e5 3.41e5 3.41e5
## 3     0.521         0.521  12.6    15621.       0     3 -1.70e5 3.41e5 3.41e5
## 4     0.515         0.515  12.7    15224.       0     3 -1.71e5 3.41e5 3.41e5
## 5     0.580         0.580  11.8     6617.       0     9 -1.67e5 3.35e5 3.35e5
## # … with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>
```



```r
city_regressions <- hxd %>% 
                     group_by(city) %>% 
                     do(tidy(lm(ve_L ~ hr + I(hr^2), .)))
city_regressions
```

```
## # A tibble: 21 x 6
## # Groups:   city [7]
##    city         term         estimate std.error statistic  p.value
##    <chr>        <chr>           <dbl>     <dbl>     <dbl>    <dbl>
##  1 Auckland     (Intercept)  42.3      6.64         6.37  2.00e-10
##  2 Auckland     hr           -0.960    0.107       -8.97  4.15e-19
##  3 Auckland     I(hr^2)       0.00726  0.000425    17.1   6.47e-64
##  4 Christchurch (Intercept)  13.4      5.46         2.45  1.42e- 2
##  5 Christchurch hr           -0.360    0.0954      -3.78  1.61e- 4
##  6 Christchurch I(hr^2)       0.00469  0.000413    11.4   1.65e-29
##  7 Copenhagen   (Intercept)   1.65     2.81         0.588 5.56e- 1
##  8 Copenhagen   hr           -0.116    0.0485      -2.39  1.66e- 2
##  9 Copenhagen   I(hr^2)       0.00351  0.000205    17.1   1.10e-64
## 10 Delhi        (Intercept) -32.8      2.87       -11.4   5.06e-30
## # … with 11 more rows
```

## Regressions using MET outcome variable

### Simple linear regression for MET and Ve


```r
lm_met_ve <- lm(ve_L ~ MET, data = hxd)
summary(lm_met_ve)
```

```
## 
## Call:
## lm(formula = ve_L ~ MET, data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -54.698  -8.842  -2.380   6.873  82.473 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  5.99364    0.16977    35.3   <2e-16 ***
## MET          2.38202    0.01241   191.9   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.37 on 43074 degrees of freedom
## Multiple R-squared:  0.461,	Adjusted R-squared:  0.461 
## F-statistic: 3.684e+04 on 1 and 43074 DF,  p-value: < 2.2e-16
```

```r
fit_mv1_met <- glance(lm_met_ve)
hxd$lm_fitted_met <- predict(lm_met_ve)

reg_met_lm <- ggplot(hxd, aes(x = MET, y = ve_L)) + 
   geom_point(size = 1, alpha = 0.05) + 
   geom_line(aes(y = lm_fitted_met), size = 1, colour = "red") +
   xlab("MET") + 
   ylab("Breathing Volume (L/minute)") + 
   theme_classic()
plot(reg_met_lm)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

### Simple linear regression for MET and Ve with MET squared


```r
lm_met_ve2 <- lm(ve_L ~ MET + I(MET^2), data = hxd)
summary(lm_met_ve2)
```

```
## 
## Call:
## lm(formula = ve_L ~ MET + I(MET^2), data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -61.936  -8.686  -2.464   6.970  76.137 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 14.130666   0.297564   47.49   <2e-16 ***
## MET          0.767676   0.050289   15.27   <2e-16 ***
## I(MET^2)     0.065703   0.001985   33.10   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.2 on 43073 degrees of freedom
## Multiple R-squared:  0.4743,	Adjusted R-squared:  0.4743 
## F-statistic: 1.943e+04 on 2 and 43073 DF,  p-value: < 2.2e-16
```

```r
fit_mv2_met <- glance(lm_met_ve2)
hxd$lm2_fitted_met <- predict(lm_met_ve2)

reg_met_sqr <- ggplot(hxd, aes(x = MET, y = ve_L)) + 
   geom_point(size = 1, alpha = 0.05) + 
   geom_line(aes(y = lm2_fitted_met), size = 1, colour = "red") +
   xlab("MET") + 
   ylab("Breathing Volume (L/minute)") + 
   theme_classic()
plot(reg_met_sqr)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

### Simple linear regression for MET and Ve with MET cubed


```r
lm_met_ve3 <- lm(ve_L ~ MET + I(MET^2) + I(MET^3), data = hxd)
summary(lm_met_ve3)
```

```
## 
## Call:
## lm(formula = ve_L ~ MET + I(MET^2) + I(MET^3), data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -59.539  -8.701  -2.505   6.948  76.833 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 17.7133882  0.4485958  39.486  < 2e-16 ***
## MET         -0.5115626  0.1300738  -3.933 8.41e-05 ***
## I(MET^2)     0.1838117  0.0112541  16.333  < 2e-16 ***
## I(MET^3)    -0.0031328  0.0002938 -10.661  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.19 on 43072 degrees of freedom
## Multiple R-squared:  0.4757,	Adjusted R-squared:  0.4757 
## F-statistic: 1.303e+04 on 3 and 43072 DF,  p-value: < 2.2e-16
```

```r
fit_mv3_met <- glance(lm_met_ve3)
hxd$lm3_fitted_met <- predict(lm_met_ve3)

reg_met_cub <- ggplot(hxd, aes(x = MET, y = ve_L)) + 
   geom_point(size = 1, alpha = 0.1) + 
   geom_line(aes(y = lm3_fitted_met), size = 1, colour = "red") +
   xlab("MET") + 
   ylab("Breathing Volume (L/minute)") + 
   theme_classic()
plot(reg_met_cub)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

## Figure 2

Regression plots


```r
figure2 <- ggarrange(reg_met_lm, reg_met_sqr, reg_met_cub,
                    labels = c("A. Linear Regression", "B. Squared Regression", "C. Cubed Regression"),
                    ncol = 2, nrow = 2)

ggsave("figure2.pdf", figure2, width = 10, height = 7, dpi = 200)
```

### Linear regression for MET and Ve with MET squared plus gender and age


```r
lm_met_ve4 <- lm(ve_L ~ MET + I(MET^2) + sex + age, data = hxd)
summary(lm_met_ve4)
```

```
## 
## Call:
## lm(formula = ve_L ~ MET + I(MET^2) + sex + age, data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -64.927  -7.813  -1.285   6.648  74.226 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 30.486817   0.342284   89.07   <2e-16 ***
## MET          1.081642   0.046923   23.05   <2e-16 ***
## I(MET^2)     0.053385   0.001853   28.81   <2e-16 ***
## sexWoman    -1.811487   0.136414  -13.28   <2e-16 ***
## age         -0.552199   0.006728  -82.08   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.28 on 43071 degrees of freedom
## Multiple R-squared:  0.5454,	Adjusted R-squared:  0.5454 
## F-statistic: 1.292e+04 on 4 and 43071 DF,  p-value: < 2.2e-16
```

```r
fit_mv4_met <- glance(lm_met_ve4)

hxd$lm4_fitted_met <- predict(lm_met_ve4, newdata = hxd, level = 0)

ggplot(hxd, aes(x = MET, y = ve_L, color = sex) ) +
     geom_point(size = 1, alpha = 0.05) + 
     geom_line(data = hxd, aes(y = lm4_fitted_met), size = 1) +
      xlab("MET") + 
      ylab("Breathing Volume (L/minute)") +
      theme_classic()
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

### Linear regression for MET and Ve with MET squared plus gender, age and city 


```r
lm_met_ve5 <- lm(ve_L ~ MET + I(MET^2) + city, data = hxd)
summary(lm_met_ve5)
```

```
## 
## Call:
## lm(formula = ve_L ~ MET + I(MET^2) + city, data = hxd)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -58.519  -8.116  -2.033   6.599  73.890 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      14.070109   0.334893  42.014  < 2e-16 ***
## MET               0.674851   0.048377  13.950  < 2e-16 ***
## I(MET^2)          0.067592   0.001909  35.410  < 2e-16 ***
## cityChristchurch  2.695432   0.253280  10.642  < 2e-16 ***
## cityCopenhagen    3.916609   0.217493  18.008  < 2e-16 ***
## cityDelhi        -4.699154   0.227712 -20.636  < 2e-16 ***
## cityLyon         -2.216627   0.240004  -9.236  < 2e-16 ***
## cityMontreal      8.059760   0.237106  33.992  < 2e-16 ***
## cityParis        -1.718466   0.245735  -6.993 2.73e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.55 on 43067 degrees of freedom
## Multiple R-squared:  0.525,	Adjusted R-squared:  0.5249 
## F-statistic:  5951 on 8 and 43067 DF,  p-value: < 2.2e-16
```

```r
fit_mv5_met <- glance(lm_met_ve5)

hxd$lm5_fitted_met <- predict(lm_met_ve5, newdata = hxd, level = 0)

figure3 <- ggplot(hxd, aes(x = MET, y = ve_L, color = city)) +
     geom_point(size = 1, alpha = 0.05) + 
     geom_line(data = hxd, aes(y = lm5_fitted_met), size = 1) +
      xlab("MET") + 
      ylab("Breathing Volume (L/minute)") + 
   theme_classic()
plot(figure3)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

```r
ggsave("figure3.pdf", figure3, width = 10, height = 7, dpi = 200)
```

### Getting fit statistics for each model


```r
fitstats_met <- rbind(fit_mv1_met, fit_mv2_met, fit_mv3_met, fit_mv4_met, fit_mv5_met)
kable(fitstats_met)
```



| r.squared| adj.r.squared|    sigma| statistic| p.value| df|    logLik|      AIC|      BIC| deviance| df.residual|  nobs|
|---------:|-------------:|--------:|---------:|-------:|--:|---------:|--------:|--------:|--------:|-----------:|-----:|
| 0.4609705|     0.4609580| 13.37029| 36836.286|       0|  1| -172818.8| 345643.5| 345669.5|  7700104|       43074| 43076|
| 0.4743409|     0.4743165| 13.20358| 19433.966|       0|  2| -172277.8| 344563.6| 344598.3|  7509107|       43073| 43076|
| 0.4757244|     0.4756879| 13.18634| 13027.756|       0|  3| -172221.0| 344452.0| 344495.4|  7489343|       43072| 43076|
| 0.5454429|     0.5454007| 12.27846| 12920.692|       0|  4| -169147.7| 338307.4| 338359.4|  6493406|       43071| 43076|
| 0.5250291|     0.5249408| 12.55172|  5950.739|       0|  8| -170093.8| 340207.7| 340294.4|  6785020|       43067| 43076|


```r
city_regressions <- hxd %>% 
                     group_by(city) %>% 
                     do(tidy(lm(ve_L ~ MET + I(MET^2), .)))
kable(city_regressions)
```



|city         |term        |   estimate| std.error|  statistic|   p.value|
|:------------|:-----------|----------:|---------:|----------:|---------:|
|Auckland     |(Intercept) |  1.3629843| 1.8014652|  0.7565976| 0.4493231|
|Auckland     |MET         |  2.0110464| 0.2582678|  7.7866700| 0.0000000|
|Auckland     |I(MET^2)    |  0.0380809| 0.0087325|  4.3608130| 0.0000132|
|Christchurch |(Intercept) |  7.1837610| 1.1517109|  6.2374692| 0.0000000|
|Christchurch |MET         |  2.0883060| 0.1937940| 10.7759073| 0.0000000|
|Christchurch |I(MET^2)    |  0.0207760| 0.0078171|  2.6577795| 0.0078927|
|Copenhagen   |(Intercept) |  5.9802126| 0.6594479|  9.0685141| 0.0000000|
|Copenhagen   |MET         |  2.3388642| 0.1080329| 21.6495586| 0.0000000|
|Copenhagen   |I(MET^2)    |  0.0169909| 0.0041544|  4.0898179| 0.0000436|
|Delhi        |(Intercept) |  7.9317955| 0.5176902| 15.3215085| 0.0000000|
|Delhi        |MET         |  1.0279911| 0.0930973| 11.0421093| 0.0000000|
|Delhi        |I(MET^2)    |  0.0510498| 0.0038935| 13.1114560| 0.0000000|
|Lyon         |(Intercept) | 18.1207328| 0.4032252| 44.9394886| 0.0000000|
|Lyon         |MET         | -0.2202238| 0.0798754| -2.7570906| 0.0058492|
|Lyon         |I(MET^2)    |  0.0896484| 0.0035617| 25.1702253| 0.0000000|
|Montreal     |(Intercept) | 26.3872408| 0.7488547| 35.2367974| 0.0000000|
|Montreal     |MET         |  0.4942296| 0.1307042|  3.7812843| 0.0001575|
|Montreal     |I(MET^2)    |  0.0546695| 0.0052730| 10.3677816| 0.0000000|
|Paris        |(Intercept) | 16.5484041| 0.8866902| 18.6631191| 0.0000000|
|Paris        |MET         |  0.1302889| 0.1426824|  0.9131388| 0.3612128|
|Paris        |I(MET^2)    |  0.0825952| 0.0054815| 15.0679763| 0.0000000|

## GAM using Ve_L and MET


```r
met_gam <- gam(ve_L ~ s(MET), data = hxd)
summary(met_gam)
```

```
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## ve_L ~ s(MET)
## 
## Parametric coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 36.14131    0.06352     569   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##          edf Ref.df    F p-value    
## s(MET) 8.084  8.747 4472  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.476   Deviance explained = 47.6%
## GCV = 173.84  Scale est. = 173.8     n = 43076
```

```r
plot(met_gam)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

### GAM using ve_L, MET and city


```r
met_city_gam <- gam(ve_L ~ s(MET) + city, data = hxd)
summary(met_city_gam)
```

```
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## ve_L ~ s(MET) + city
## 
## Parametric coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       35.3489     0.1714  206.29  < 2e-16 ***
## cityChristchurch   2.6060     0.2532   10.29  < 2e-16 ***
## cityCopenhagen     3.7843     0.2178   17.38  < 2e-16 ***
## cityDelhi         -4.8044     0.2277  -21.10  < 2e-16 ***
## cityLyon          -2.4076     0.2400  -10.03  < 2e-16 ***
## cityMontreal       8.0542     0.2368   34.01  < 2e-16 ***
## cityParis         -1.7904     0.2456   -7.29 3.14e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##          edf Ref.df    F p-value    
## s(MET) 8.535  8.928 4331  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.527   Deviance explained = 52.7%
## GCV = 156.89  Scale est. = 156.83    n = 43076
```

```r
plot(met_city_gam)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

### GAM using ve_L, MET, City and Sex


```r
met_city_sex_gam <- gam(ve_L ~ s(MET) + city + sex, data = hxd)
summary(met_city_sex_gam)
```

```
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## ve_L ~ s(MET) + city + sex
## 
## Parametric coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       35.7699     0.1770 202.078  < 2e-16 ***
## cityChristchurch   2.6499     0.2530  10.474  < 2e-16 ***
## cityCopenhagen     3.7636     0.2176  17.298  < 2e-16 ***
## cityDelhi         -5.2153     0.2317 -22.508  < 2e-16 ***
## cityLyon          -2.3947     0.2398  -9.987  < 2e-16 ***
## cityMontreal       8.0301     0.2366  33.943  < 2e-16 ***
## cityParis         -1.7449     0.2454  -7.111 1.17e-12 ***
## sexWoman          -1.3303     0.1424  -9.344  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##          edf Ref.df    F p-value    
## s(MET) 8.527  8.926 4341  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.528   Deviance explained = 52.8%
## GCV = 156.58  Scale est. = 156.52    n = 43076
```

```r
plot(met_city_sex_gam)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

## GAM using ve, MET, city, age, sex


```r
met_all_gam <- gam(ve_L ~ s(MET) + city + sex + age, data = hxd)
summary(met_all_gam)
```

```
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## ve_L ~ s(MET) + city + sex + age
## 
## Parametric coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      54.806362   0.318872 171.876  < 2e-16 ***
## cityChristchurch  2.051665   0.239833   8.555  < 2e-16 ***
## cityCopenhagen   -0.680190   0.215610  -3.155  0.00161 ** 
## cityDelhi        -7.540257   0.221968 -33.970  < 2e-16 ***
## cityLyon         -2.876051   0.227272 -12.655  < 2e-16 ***
## cityMontreal      2.394426   0.238021  10.060  < 2e-16 ***
## cityParis        -3.223803   0.233428 -13.811  < 2e-16 ***
## sexWoman         -3.189685   0.137467 -23.203  < 2e-16 ***
## age              -0.509420   0.007259 -70.178  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##          edf Ref.df    F p-value    
## s(MET) 7.246  8.215 5238  <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.576   Deviance explained = 57.6%
## GCV = 140.56  Scale est. = 140.5     n = 43076
```

```r
plot(met_all_gam)
```

![](Hexoskin_analysis_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

### Calculate rmse for models 1 to 5

### Calculate RMSE 1

```r
RMSE(hxd$ve_L, hxd$lm_fitted_met)
```

```
## [1] 13.36998
```

### Calculate RMSE 2

```r
RMSE(hxd$ve_L, hxd$lm2_fitted_met)
```

```
## [1] 13.20312
```

### Calculate RMSE 3

```r
RMSE(hxd$ve_L, hxd$lm3_fitted_met)
```

```
## [1] 13.18573
```

### Calculate RMSE 4

```r
RMSE(hxd$ve_L, hxd$lm4_fitted_met)
```

```
## [1] 12.27774
```

### Calculate RMSE lm5

```r
RMSE(hxd$ve_L, hxd$lm5_fitted_met)
```

```
## [1] 12.55041
```
