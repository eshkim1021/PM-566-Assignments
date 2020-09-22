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

# Data Wrangling

## 1\. Merge the two data sets

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

## 2\. Create new categorical variable

``` r
merge <- merge %>% 
  mutate("obesity_level" = case_when( bmi<14 ~ "Underweight",
                                      bmi>=14 & bmi <22 ~"Normal",
                                      bmi>=22 & bmi<24 ~"Overweight",
                                      bmi>=24 ~ "Obese")
  )

merge[,obesity_level:= fifelse(is.na(obesity_level),"Normal",obesity_level)]
summary(merge$bmi)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   11.30   15.78   17.48   18.50   20.35   41.27      89

``` r
merge %>% count(obesity_level)
```

    ##    obesity_level   n
    ## 1:        Normal 975
    ## 2:         Obese 103
    ## 3:    Overweight  87
    ## 4:   Underweight  35

The NA’s in the data were replaced by the average result for gender and
race.

Because the mean BMI of 18.5 and the median BMI of 17.48 are both in the
Healthy category, a majority of the observations would be Healthy.

## 3\. Create Categorical Variable “Smoke\_gas\_exposure”

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
                                                         smoke==1 & gasstove ==1 ~ "Smoke and Gas Exposure",
                                                         )
                          )
merge[,smoke_gas_exposure := fifelse(is.na(smoke_gas_exposure),"Gas Exposure",smoke_gas_exposure)]
merge %>% count(smoke_gas_exposure)
```

    ##        smoke_gas_exposure   n
    ## 1:           Gas Exposure 799
    ## 2:            No Exposure 214
    ## 3: Smoke and Gas Exposure 151
    ## 4:         Smoke Exposure  36

Four different categories were created indicating whether the
participant was exposed to second-hand smoke, a gas stove, or both.

The NA’s in the data were replaced by the average result depending on
gender and race.

## 4\. Create four summary tables

``` r
merge %>% group_by(townname) %>% 
  summarise(
    fev_avg = mean(fev, na.rm=TRUE),
    asthma.indication = mean(asthma, na.rm = TRUE)
    )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 12 x 3
    ##    townname      fev_avg asthma.indication
    ##    <chr>           <dbl>             <dbl>
    ##  1 Alpine          2089.             0.113
    ##  2 Atascadero      2079.             0.255
    ##  3 Lake Elsinore   2040.             0.126
    ##  4 Lake Gregory    2092.             0.152
    ##  5 Lancaster       2003.             0.165
    ##  6 Lompoc          2038.             0.113
    ##  7 Long Beach      1984.             0.135
    ##  8 Mira Loma       1985.             0.158
    ##  9 Riverside       1986.             0.11 
    ## 10 San Dimas       2028.             0.172
    ## 11 Santa Maria     2023.             0.134
    ## 12 Upland          2027.             0.121

``` r
merge <- merge %>% mutate(male.f = case_when( male == 0 ~ "Female",
                                              male == 1 ~ "Male"))

merge %>% group_by(male.f) %>% 
  summarise(
    fev_avg = mean(fev, na.rm=TRUE),
    asthma.indication = mean(asthma, na.rm = TRUE)
    )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   male.f fev_avg asthma.indication
    ##   <chr>    <dbl>             <dbl>
    ## 1 Female   1959.             0.121
    ## 2 Male     2104.             0.173

``` r
merge %>% group_by(obesity_level) %>% 
  summarise(
    fev_avg = mean(fev, na.rm=TRUE),
    asthma.indication = mean(asthma, na.rm = TRUE)
    )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 4 x 3
    ##   obesity_level fev_avg asthma.indication
    ##   <chr>           <dbl>             <dbl>
    ## 1 Normal          1998.            0.140 
    ## 2 Obese           2269.            0.21  
    ## 3 Overweight      2224.            0.165 
    ## 4 Underweight     1687.            0.0857

``` r
merge %>% group_by(smoke_gas_exposure) %>% 
  summarise(
    fev_avg = mean(fev, na.rm=TRUE),
    asthma.indication = mean(asthma, na.rm = TRUE)
    )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 4 x 3
    ##   smoke_gas_exposure     fev_avg asthma.indication
    ##   <chr>                    <dbl>             <dbl>
    ## 1 Gas Exposure             2024.             0.148
    ## 2 No Exposure              2060.             0.148
    ## 3 Smoke and Gas Exposure   2020.             0.130
    ## 4 Smoke Exposure           2064.             0.171

# Looking at the Data

Q1: What is the association between BMI and FEV?

Q2: What is the association between smoke and gas exposure and FEV?

Q3: What is the assoication between PM2.5 exposure and FEV?

### Initial EDA Checklist:

``` r
dim(merge)
```

    ## [1] 1200   52

``` r
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
    ##    iacid oacid total_acids       lon      lat obesity_level smoke_gas_exposure
    ## 1:  2.39  3.52         5.5 -116.7664 32.83505        Normal        No Exposure
    ## 2:  2.39  3.52         5.5 -116.7664 32.83505        Normal       Gas Exposure
    ## 3:  2.39  3.52         5.5 -116.7664 32.83505        Normal     Smoke Exposure
    ## 4:  2.39  3.52         5.5 -116.7664 32.83505        Normal       Gas Exposure
    ## 5:  2.39  3.52         5.5 -116.7664 32.83505        Normal        No Exposure
    ## 6:  2.39  3.52         5.5 -116.7664 32.83505        Normal     Smoke Exposure
    ##    male.f
    ## 1: Female
    ## 2: Female
    ## 3: Female
    ## 4: Female
    ## 5:   Male
    ## 6:   Male

``` r
tail(merge)
```

    ##    townname  sid male race hispanic    agepft height weight      bmi asthma
    ## 1:   Upland 1866    0    O        1  9.806982    139     60 14.11559      0
    ## 2:   Upland 1867    0    M        1  9.618070    140     71 16.46568      0
    ## 3:   Upland 2031    1    W        0  9.798768    135     83 20.70084      0
    ## 4:   Upland 2032    1    W        0  9.549624    137     59 14.28855      0
    ## 5:   Upland 2033    0    M        0 10.121834    130     67 18.02044      0
    ## 6:   Upland 2053    0    W        0        NA     NA     NA       NA      0
    ##    active_asthma father_asthma mother_asthma wheeze hayfever allergy
    ## 1:             0            NA             0      0       NA      NA
    ## 2:             0             1             0      0        0       0
    ## 3:             0             0             0      1        0       1
    ## 4:             0             0             1      1        1       1
    ## 5:             1             0             0      1        1       0
    ## 6:             0             0             0      0        0       0
    ##    educ_parent smoke pets gasstove      fev      fvc     mmef pm25_mass
    ## 1:           3     0    1        0 1691.275 1928.859 1890.604     22.46
    ## 2:           3     0    1        0 1733.338 1993.040 2072.643     22.46
    ## 3:           3     0    1        1 2034.177 2505.535 1814.075     22.46
    ## 4:           3     0    1        1 2077.703 2275.338 2706.081     22.46
    ## 5:           3     0    1        1 1929.866 2122.148 2558.054     22.46
    ## 6:           3     0    1        0       NA       NA       NA     22.46
    ##    pm25_so4 pm25_no3 pm25_nh4 pm25_oc pm25_ec pm25_om pm10_oc pm10_ec pm10_tc
    ## 1:     2.65     7.75     2.96    6.49    1.19    7.79    8.32    1.22    9.54
    ## 2:     2.65     7.75     2.96    6.49    1.19    7.79    8.32    1.22    9.54
    ## 3:     2.65     7.75     2.96    6.49    1.19    7.79    8.32    1.22    9.54
    ## 4:     2.65     7.75     2.96    6.49    1.19    7.79    8.32    1.22    9.54
    ## 5:     2.65     7.75     2.96    6.49    1.19    7.79    8.32    1.22    9.54
    ## 6:     2.65     7.75     2.96    6.49    1.19    7.79    8.32    1.22    9.54
    ##    formic acetic  hcl hno3 o3_max o3106 o3_24   no2 pm10 no_24hr pm2_5_fr iacid
    ## 1:   2.67   4.73 0.46 4.03  63.83  46.5  22.2 37.97 40.8   18.48    27.73  4.49
    ## 2:   2.67   4.73 0.46 4.03  63.83  46.5  22.2 37.97 40.8   18.48    27.73  4.49
    ## 3:   2.67   4.73 0.46 4.03  63.83  46.5  22.2 37.97 40.8   18.48    27.73  4.49
    ## 4:   2.67   4.73 0.46 4.03  63.83  46.5  22.2 37.97 40.8   18.48    27.73  4.49
    ## 5:   2.67   4.73 0.46 4.03  63.83  46.5  22.2 37.97 40.8   18.48    27.73  4.49
    ## 6:   2.67   4.73 0.46 4.03  63.83  46.5  22.2 37.97 40.8   18.48    27.73  4.49
    ##    oacid total_acids       lon      lat obesity_level smoke_gas_exposure male.f
    ## 1:   7.4       11.43 -117.6484 34.09751        Normal        No Exposure Female
    ## 2:   7.4       11.43 -117.6484 34.09751        Normal        No Exposure Female
    ## 3:   7.4       11.43 -117.6484 34.09751        Normal       Gas Exposure   Male
    ## 4:   7.4       11.43 -117.6484 34.09751        Normal       Gas Exposure   Male
    ## 5:   7.4       11.43 -117.6484 34.09751        Normal       Gas Exposure Female
    ## 6:   7.4       11.43 -117.6484 34.09751        Normal        No Exposure Female

``` r
summary(merge)
```

    ##    townname              sid              male            race          
    ##  Length:1200        Min.   :   1.0   Min.   :0.0000   Length:1200       
    ##  Class :character   1st Qu.: 528.8   1st Qu.:0.0000   Class :character  
    ##  Mode  :character   Median :1041.5   Median :0.0000   Mode  :character  
    ##                     Mean   :1037.5   Mean   :0.4917                     
    ##                     3rd Qu.:1554.2   3rd Qu.:1.0000                     
    ##                     Max.   :2053.0   Max.   :1.0000                     
    ##                                                                         
    ##     hispanic          agepft           height        weight      
    ##  Min.   :0.0000   Min.   : 8.961   Min.   :114   Min.   : 42.00  
    ##  1st Qu.:0.0000   1st Qu.: 9.610   1st Qu.:135   1st Qu.: 65.00  
    ##  Median :0.0000   Median : 9.906   Median :139   Median : 74.00  
    ##  Mean   :0.4342   Mean   : 9.924   Mean   :139   Mean   : 79.33  
    ##  3rd Qu.:1.0000   3rd Qu.:10.177   3rd Qu.:143   3rd Qu.: 89.00  
    ##  Max.   :1.0000   Max.   :12.731   Max.   :165   Max.   :207.00  
    ##                   NA's   :89       NA's   :89    NA's   :89      
    ##       bmi            asthma       active_asthma  father_asthma    
    ##  Min.   :11.30   Min.   :0.0000   Min.   :0.00   Min.   :0.00000  
    ##  1st Qu.:15.78   1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:0.00000  
    ##  Median :17.48   Median :0.0000   Median :0.00   Median :0.00000  
    ##  Mean   :18.50   Mean   :0.1463   Mean   :0.19   Mean   :0.08318  
    ##  3rd Qu.:20.35   3rd Qu.:0.0000   3rd Qu.:0.00   3rd Qu.:0.00000  
    ##  Max.   :41.27   Max.   :1.0000   Max.   :1.00   Max.   :1.00000  
    ##  NA's   :89      NA's   :31                      NA's   :106      
    ##  mother_asthma        wheeze          hayfever         allergy      
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
    ##  Mean   :0.1023   Mean   :0.3313   Mean   :0.1747   Mean   :0.2929  
    ##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##  NA's   :56       NA's   :71       NA's   :118      NA's   :63      
    ##   educ_parent        smoke             pets           gasstove     
    ##  Min.   :1.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:2.000   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.0000  
    ##  Median :3.000   Median :0.0000   Median :1.0000   Median :1.0000  
    ##  Mean   :2.797   Mean   :0.1638   Mean   :0.7667   Mean   :0.7815  
    ##  3rd Qu.:3.000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :5.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##  NA's   :64      NA's   :40                        NA's   :33      
    ##       fev              fvc            mmef          pm25_mass     
    ##  Min.   : 984.8   Min.   : 895   Min.   : 757.6   Min.   : 5.960  
    ##  1st Qu.:1809.0   1st Qu.:2041   1st Qu.:1994.0   1st Qu.: 7.615  
    ##  Median :2022.7   Median :2293   Median :2401.5   Median :10.545  
    ##  Mean   :2031.3   Mean   :2324   Mean   :2398.8   Mean   :14.362  
    ##  3rd Qu.:2249.7   3rd Qu.:2573   3rd Qu.:2793.8   3rd Qu.:20.988  
    ##  Max.   :3323.7   Max.   :3698   Max.   :4935.9   Max.   :29.970  
    ##  NA's   :95       NA's   :97     NA's   :106                      
    ##     pm25_so4        pm25_no3         pm25_nh4         pm25_oc      
    ##  Min.   :0.790   Min.   : 0.730   Min.   :0.4100   Min.   : 1.450  
    ##  1st Qu.:1.077   1st Qu.: 1.538   1st Qu.:0.7375   1st Qu.: 2.520  
    ##  Median :1.815   Median : 2.525   Median :1.1350   Median : 4.035  
    ##  Mean   :1.876   Mean   : 4.488   Mean   :1.7642   Mean   : 4.551  
    ##  3rd Qu.:2.605   3rd Qu.: 7.338   3rd Qu.:2.7725   3rd Qu.: 5.350  
    ##  Max.   :3.230   Max.   :12.200   Max.   :4.2500   Max.   :11.830  
    ##                                                                    
    ##     pm25_ec          pm25_om          pm10_oc          pm10_ec      
    ##  Min.   :0.1300   Min.   : 1.740   Min.   : 1.860   Min.   :0.1400  
    ##  1st Qu.:0.4000   1st Qu.: 3.020   1st Qu.: 3.228   1st Qu.:0.4100  
    ##  Median :0.5850   Median : 4.840   Median : 5.170   Median :0.5950  
    ##  Mean   :0.7358   Mean   : 5.460   Mean   : 5.832   Mean   :0.7525  
    ##  3rd Qu.:1.1750   3rd Qu.: 6.418   3rd Qu.: 6.855   3rd Qu.:1.1975  
    ##  Max.   :1.3600   Max.   :14.200   Max.   :15.160   Max.   :1.3900  
    ##                                                                     
    ##     pm10_tc           formic          acetic           hcl        
    ##  Min.   : 1.990   Min.   :0.340   Min.   :0.750   Min.   :0.2200  
    ##  1st Qu.: 3.705   1st Qu.:0.720   1st Qu.:2.297   1st Qu.:0.3250  
    ##  Median : 6.505   Median :1.105   Median :2.910   Median :0.4350  
    ##  Mean   : 6.784   Mean   :1.332   Mean   :3.010   Mean   :0.4208  
    ##  3rd Qu.: 8.430   3rd Qu.:1.765   3rd Qu.:4.000   3rd Qu.:0.4625  
    ##  Max.   :16.440   Max.   :2.770   Max.   :5.140   Max.   :0.7300  
    ##                                                                   
    ##       hno3           o3_max          o3106           o3_24      
    ##  Min.   :0.430   Min.   :38.27   Min.   :28.22   Min.   :18.22  
    ##  1st Qu.:1.593   1st Qu.:49.93   1st Qu.:41.90   1st Qu.:23.31  
    ##  Median :2.455   Median :64.05   Median :46.74   Median :27.59  
    ##  Mean   :2.367   Mean   :60.16   Mean   :47.76   Mean   :30.23  
    ##  3rd Qu.:3.355   3rd Qu.:67.69   3rd Qu.:55.24   3rd Qu.:32.39  
    ##  Max.   :4.070   Max.   :84.44   Max.   :67.01   Max.   :57.76  
    ##                                                                 
    ##       no2             pm10          no_24hr         pm2_5_fr    
    ##  Min.   : 4.60   Min.   :18.40   Min.   : 2.05   Min.   : 9.01  
    ##  1st Qu.:12.12   1st Qu.:20.71   1st Qu.: 4.74   1st Qu.:10.28  
    ##  Median :16.40   Median :29.64   Median :12.68   Median :22.23  
    ##  Mean   :18.99   Mean   :32.64   Mean   :16.21   Mean   :19.79  
    ##  3rd Qu.:23.24   3rd Qu.:39.16   3rd Qu.:26.90   3rd Qu.:27.73  
    ##  Max.   :37.97   Max.   :70.39   Max.   :42.95   Max.   :31.55  
    ##                                  NA's   :100     NA's   :300    
    ##      iacid           oacid        total_acids          lon        
    ##  Min.   :0.760   Min.   :1.090   Min.   : 1.520   Min.   :-120.7  
    ##  1st Qu.:1.835   1st Qu.:2.978   1st Qu.: 4.930   1st Qu.:-118.8  
    ##  Median :2.825   Median :4.135   Median : 6.370   Median :-117.7  
    ##  Mean   :2.788   Mean   :4.342   Mean   : 6.708   Mean   :-118.3  
    ##  3rd Qu.:3.817   3rd Qu.:5.982   3rd Qu.: 9.395   3rd Qu.:-117.4  
    ##  Max.   :4.620   Max.   :7.400   Max.   :11.430   Max.   :-116.8  
    ##                                                                   
    ##       lat        obesity_level      smoke_gas_exposure    male.f         
    ##  Min.   :32.84   Length:1200        Length:1200        Length:1200       
    ##  1st Qu.:33.93   Class :character   Class :character   Class :character  
    ##  Median :34.10   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :34.20                                                           
    ##  3rd Qu.:34.65                                                           
    ##  Max.   :35.49                                                           
    ## 

Overall, there does not seem to be any issues with the data. We will
check the variables that we are analyzing more closely (BMI,FEV,PM2.5,
and smoke and gas exposure)

``` r
summary(merge$fev)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   984.8  1809.0  2022.7  2031.3  2249.7  3323.7      95

``` r
summary(merge$pm25_mass)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   5.960   7.615  10.545  14.362  20.988  29.970

``` r
summary(merge$bmi)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   11.30   15.78   17.48   18.50   20.35   41.27      89

``` r
table(merge$smoke_gas_exposure)
```

    ## 
    ##           Gas Exposure            No Exposure Smoke and Gas Exposure 
    ##                    799                    214                    151 
    ##         Smoke Exposure 
    ##                     36

``` r
table(merge$obesity_level)
```

    ## 
    ##      Normal       Obese  Overweight Underweight 
    ##         975         103          87          35

The key variables that we are analyzing all seem to have plausible
values, and there does not seem to be an error in them. The NA’s in the
FEV category were all discarded when calculations were run.

## 1\. Facet Plot, BMI vs FEV by “Townname”

``` r
merge %>%
  filter(!(townname %in% NA)) %>% 
  ggplot(mapping = aes(x=bmi, y = fev, color = townname))+
  geom_point()+
  labs(title = "Association between BMI and FEV by town")+
  facet_wrap(~townname, nrow = 3)+
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 95 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 95 rows containing missing values (geom_point).

![](HW-2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

In all of the different towns, there is a positive relationship between
BMI and FEV. Judging by the regression line, the correlation between BMI
and FEV is highest in the cities of San Dimas, Lake Elsinore, and
Atascadero.

## 2\. Stacked Histograms of FEV by BMI Category and FEV by smoke/gas exposure

``` r
merge %>%
  filter(!(townname %in% NA)) %>% 
  ggplot()+
  geom_histogram(mapping = aes(x=fev,fill=obesity_level))+
  labs(title = "FEV Values by BMI Category")+
  scale_fill_brewer(palette="Pastel1")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 95 rows containing non-finite values (stat_bin).

![](HW-2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Overall, those that are underweight have lower FEV values and those that
are overweight and obese have higher FEV values. Those that have a
normal BMI are found to have a wide range of FEV values, therefore there
is another factor that is causing the differences in FEV.

``` r
merge %>% 
  filter(!(townname %in% NA)) %>% 
  ggplot()+
  geom_histogram(mapping = aes(x = fev,fill=smoke_gas_exposure))+
  labs(title= "FEV Values by Smoke and Gas Exposure")+
  scale_fill_brewer(palette = "Pastel2")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 95 rows containing non-finite values (stat_bin).

![](HW-2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The spread of FEV values by Smoke and Gas Exposure seems to be fairly
even across the four categories. Those that have only been exposed to
second-hand smoke have FEV values that are centralized, but this could
be due to the small sample size of this category.

## 3\. Barchart of BMI by Smoke/Gas Exposure

``` r
merge %>% 
  ggplot()+
  geom_bar(mapping = aes(x = obesity_level, fill = smoke_gas_exposure))+
  labs(title = "BMI Category by Smoke and Gas Exposure")+
  scale_fill_brewer(palette = "Accent")
```

![](HW-2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Overall, the barchart shows that a majority of those with normal BMI
have been exposed to gas exposure. This trend is true for the rest of
the BMI categories as well, but this could be due to the large
proportion of those that have been exposed to gas compared to the rest
of the population.

## 4\. Statistical Summary Graphs

``` r
merge %>% 
  ggplot()+
  stat_summary(mapping = aes(x = obesity_level, y = fev),
               fun.min = min,
               fun.max = max,
               fun = mean)+
  labs(title = "Average FEV values by BMI Category")
```

    ## Warning: Removed 95 rows containing non-finite values (stat_summary).

![](HW-2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

The summary graphs show the mean FEV values based on obesity values,
while also indicating the minimum and maximum values for each category.
According to this graph, the mean FEV values are the biggest for those
that are obese and overweight, while the mean FEV value is the smallest
for those that are underweight. There is not much different in the mean
FEV between those who are obese and overweight, but the range of FEV
values for those who are overweight is smaller.

## 5\. Leaflet Map for PM2.5 Concentration

``` r
library(leaflet)

pm2.5col = colorNumeric(c('darkgreen','goldenrod','brown'), domain = merge$pm25_mass)
merge %>% 
  leaflet() %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addCircles(lat = ~lat, lng = ~lon, label=~paste0(round(pm25_mass,2)), color = ~pm2.5col(pm25_mass), opacity = 1, fillOpacity = 1, radius = 500) %>% 
  addLegend('bottomleft',pal = pm2.5col, values = merge$pm25_mass, title = 'PM 2.5 Concentration', opacity =1)
```

<!--html_preserve-->

<div id="htmlwidget-1326facaa1d0354199d7" class="leaflet html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-1326facaa1d0354199d7">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,32.8350521,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,35.4894169,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,33.6680772,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.242901,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6867846,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,34.6391501,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.7700504,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9845417,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,33.9806005,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.1066756,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.9530337,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751,34.09751],[-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-116.7664109,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-120.6707255,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.3272615,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-117.275233,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-118.1541632,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-120.4579409,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-118.1937395,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.5159449,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.3754942,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-117.8067257,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-120.4357191,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876,-117.6483876],500,null,null,{"interactive":true,"className":"","stroke":true,"color":["#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28"],"weight":5,"opacity":1,"fill":true,"fillColor":["#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#497405","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#316D02","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#83880F","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#356E02","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#447304","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#006400","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#D59A23","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#A52A2A","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#C87B28","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#D08D25","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#2B6B02","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28","#C87A28"],"fillOpacity":1},null,null,["8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","8.74","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","7.48","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","12.35","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","7.66","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","8.5","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","5.96","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","19.12","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","29.97","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","22.39","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","20.52","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","7.19","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46","22.46"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]},{"method":"addLegend","args":[{"colors":["#006400 , #5E7C08 16.826322365681%, #AC9617 37.6509787588505%, #D29225 58.47563515202%, #BC612A 79.3002915451895%, #A52A2A "],"labels":["10","15","20","25"],"na_color":null,"na_label":"NA","opacity":1,"position":"bottomleft","type":"numeric","title":"PM 2.5 Concentration","extra":{"p_1":0.16826322365681,"p_n":0.793002915451895},"layerId":null,"className":"info legend","group":null}]}],"limits":{"lat":[32.8350521,35.4894169],"lng":[-120.6707255,-116.7664109]}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

According to the leaflet map, the locations with the highest PM 2.5
Concentration is located in or next to cities. The highest PM 2.5
concentrations were recorded near Riverside, Pasadena, and Long BEach.
The lowest PM 2.5 concentrations were recorded in ares that are far away
from big cities, such as San Luis Obispo, Lancaster, and San Diego.

## 6\. Is PM 2.5 Related to FEV?

``` r
merge %>% 
  ggplot(mapping = aes(x = pm25_mass,y = fev))+
  geom_point()+
  geom_smooth()
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 95 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 95 rows containing missing values (geom_point).

![](HW-2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Overall, there seems to be a slight negative relationship between PM 2.5
concentration and FEV. However, due to the scatter of data in FEV is it
difficult to determine the exact relationship between the two.

Therefore, the average FEV value for each town (which was recorded to
have the same PM 2.5 concentration) were calculated and the relationship
between PM2.5 and FEV was examimed.

``` r
data <- merge %>% 
  group_by(townname,pm25_mass)%>% 
  filter(!(fev %in% NA)) %>%
  summarise_at(vars(fev),
               list(mean))

data %>% 
  ggplot(mapping = aes(x = pm25_mass,y=fev))+
  geom_point()+
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](HW-2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
lm(fev~pm25_mass,data=data) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = fev ~ pm25_mass, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -47.079 -22.421   2.665  22.983  40.144 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2076.511     18.856 110.124   <2e-16 ***
    ## pm25_mass     -3.163      1.156  -2.736    0.021 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 30.98 on 10 degrees of freedom
    ## Multiple R-squared:  0.4281, Adjusted R-squared:  0.3709 
    ## F-statistic: 7.486 on 1 and 10 DF,  p-value: 0.02097

Looking at the average FEV values for each PM 2.5 concentration, the
negative relationship between PM 2.5 and FEV seems more clear. After
doing a linear regression test, it shows that the negative correlation
between PM2.5 and FEV is statistically significant.
