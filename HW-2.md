Assignment 2
================
Edward Kim
9/20/2020

``` r
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv", destfile= "individual.csv")
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv", destfile = "regional.csv")

individual <- data.table::fread("individual.csv")
regional <- data.table::fread("regional.csv")
```

# 1\. Merge the two data sets

``` r
merge <-merge(
  x = individual,
  y = regional,
  by.x = "townname",
  by.y = "townname",
  all.x = TRUE,
  all.y = TRUE)

head(merge)
```

    ##    townname sid male race hispanic    agepft height weight      bmi asthma
    ## 1:   Alpine 835    0    W        0 10.099932    143     69 15.33749      0
    ## 2:   Alpine 838    0    O        1  9.486653    133     62 15.93183      0
    ## 3:   Alpine 839    0    M        1 10.053388    142     86 19.38649      0
    ## 4:   Alpine 840    0    W        0  9.965777    146     78 16.63283      0
    ## 5:   Alpine 841    1    W        1 10.548939    150     78 15.75758      0
    ## 6:   Alpine 842    1    M        1  9.489391    139     65 15.29189      0
    ##    active_asthma father_asthma mother_asthma wheeze hayfever allergy
    ## 1:             0             0             0      0        0       1
    ## 2:             0             0             0      0        0       0
    ## 3:             0             0             1      1        1       1
    ## 4:             0             0             0      0        0       0
    ## 5:             0             0             0      0        0       0
    ## 6:             0             0             0      1        0       0
    ##    educ_parent smoke pets gasstove      fev      fvc     mmef pm25_mass
    ## 1:           3     0    1        0 2529.276 2826.316 3406.579      8.74
    ## 2:           4    NA    1        0 1737.793 1963.545 2133.110      8.74
    ## 3:           3     1    1        0 2121.711 2326.974 2835.197      8.74
    ## 4:          NA    NA    0       NA 2466.791 2638.221 3466.464      8.74
    ## 5:           5     0    1        0 2251.505 2594.649 2445.151      8.74
    ## 6:           1     1    1        0 2188.716 2423.934 2524.599      8.74
    ##    pm25_so4 pm25_no3 pm25_nh4 pm25_oc pm25_ec pm25_om pm10_oc pm10_ec pm10_tc
    ## 1:     1.73     1.59     0.88    2.54    0.48    3.04    3.25    0.49    3.75
    ## 2:     1.73     1.59     0.88    2.54    0.48    3.04    3.25    0.49    3.75
    ## 3:     1.73     1.59     0.88    2.54    0.48    3.04    3.25    0.49    3.75
    ## 4:     1.73     1.59     0.88    2.54    0.48    3.04    3.25    0.49    3.75
    ## 5:     1.73     1.59     0.88    2.54    0.48    3.04    3.25    0.49    3.75
    ## 6:     1.73     1.59     0.88    2.54    0.48    3.04    3.25    0.49    3.75
    ##    formic acetic  hcl hno3 o3_max o3106 o3_24   no2  pm10 no_24hr pm2_5_fr
    ## 1:   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28
    ## 2:   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28
    ## 3:   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28
    ## 4:   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28
    ## 5:   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28
    ## 6:   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28
    ##    iacid oacid total_acids       lon      lat
    ## 1:  2.39  3.52         5.5 -116.7664 32.83505
    ## 2:  2.39  3.52         5.5 -116.7664 32.83505
    ## 3:  2.39  3.52         5.5 -116.7664 32.83505
    ## 4:  2.39  3.52         5.5 -116.7664 32.83505
    ## 5:  2.39  3.52         5.5 -116.7664 32.83505
    ## 6:  2.39  3.52         5.5 -116.7664 32.83505

``` r
merge %>% nrow()
```

    ## [1] 1200

The two dataset for the individual and regional levels were merged using
the location (townname). There is a total of 1200 rows, which confirms
that there was no duplicates in the data. There is 49 total variables,
which is consistent with the variables present in both datasets.

# 2\. Create new categorical variable

``` r
merge <- merge %>% 
  mutate("obesity_level" = case_when( bmi<14 ~ "Underweight",
                                      bmi>=14 & bmi <22 ~"Normal",
                                      bmi>=22 & bmi<24 ~"Overweight",
                                      bmi>=24 ~ "Obese")
         )

summary(merge$bmi)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   11.30   15.78   17.48   18.50   20.35   41.27      89

``` r
merge %>% count(obesity_level)
```

    ##    obesity_level   n
    ## 1:        Normal 886
    ## 2:         Obese 103
    ## 3:    Overweight  87
    ## 4:   Underweight  35
    ## 5:          <NA>  89

The summary table shows that there are 89 NA’s in the BMI variable,
which accounts for the 89 NA variables that were counted in the total
observations per BMI category.

Because the mean BMI of 18.5 and the median BMI of 17.48 are both in the
Healthy category, a majority of the observations would be Healthy.

# 3\. Create Categorical Variable “Smoke\_gas\_exposure”

``` r
merge %>% group_by(smoke,gasstove) %>% count()
```

    ## # A tibble: 9 x 3
    ## # Groups:   smoke, gasstove [9]
    ##   smoke gasstove     n
    ##   <int>    <int> <int>
    ## 1     0        0   214
    ## 2     0        1   739
    ## 3     0       NA    17
    ## 4     1        0    36
    ## 5     1        1   151
    ## 6     1       NA     3
    ## 7    NA        0     5
    ## 8    NA        1    22
    ## 9    NA       NA    13

``` r
merge <- merge %>% mutate(smoke_gas_exposure = case_when(smoke==0 & gasstove ==0 ~ "No Exposure",
                                                         smoke==0 & gasstove==1 ~ "Gas Exposure",
                                                         smoke==1 & gasstove ==0~ "Smoke Exposure",
                                                         smoke==1 & gasstove ==1 ~ "Smoke and Gas Exposure")
                          )
merge %>% count(smoke_gas_exposure)
```

    ##        smoke_gas_exposure   n
    ## 1:           Gas Exposure 739
    ## 2:            No Exposure 214
    ## 3: Smoke and Gas Exposure 151
    ## 4:         Smoke Exposure  36
    ## 5:                   <NA>  60

Four different categories were created indicating whether the
participant was exposed to second-hand smoke, a gas stove, or both.