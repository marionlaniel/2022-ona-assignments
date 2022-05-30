Exercice 4
================

## Load Data

``` r
data_path <- "/Users/marionlaniel/Documents/McGill/S2022/ORGB672/RStudio/Patent Data/672_project_data/"
applications <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
edges <- read.csv(paste0(data_path,"edges_sample.csv"))
```

## Get Genders for Examiners

``` r
examiner_names <- applications %>% 
  distinct(examiner_name_first)

examiner_names
```

    ## # A tibble: 2,595 × 1
    ##    examiner_name_first
    ##    <chr>              
    ##  1 JACQUELINE         
    ##  2 BEKIR              
    ##  3 CYNTHIA            
    ##  4 MARY               
    ##  5 MICHAEL            
    ##  6 LINDA              
    ##  7 KARA               
    ##  8 VANESSA            
    ##  9 TERESA             
    ## 10 SUN                
    ## # … with 2,585 more rows

``` r
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

examiner_names_gender
```

    ## # A tibble: 1,822 × 3
    ##    examiner_name_first gender proportion_female
    ##    <chr>               <chr>              <dbl>
    ##  1 AARON               male              0.0082
    ##  2 ABDEL               male              0     
    ##  3 ABDOU               male              0     
    ##  4 ABDUL               male              0     
    ##  5 ABDULHAKIM          male              0     
    ##  6 ABDULLAH            male              0     
    ##  7 ABDULLAHI           male              0     
    ##  8 ABIGAIL             female            0.998 
    ##  9 ABIMBOLA            female            0.944 
    ## 10 ABRAHAM             male              0.0031
    ## # … with 1,812 more rows

``` r
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells  4156510 222.0    7281510 388.9         NA  4176038 223.1
    ## Vcells 49053289 374.3   94874955 723.9      16384 79368808 605.6

## Get Race for Examiners

``` r
examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_surnames
```

    ## # A tibble: 3,806 × 1
    ##    surname   
    ##    <chr>     
    ##  1 HOWARD    
    ##  2 YILDIRIM  
    ##  3 HAMILTON  
    ##  4 MOSHER    
    ##  5 BARR      
    ##  6 GRAY      
    ##  7 MCMILLIAN 
    ##  8 FORD      
    ##  9 STRZELECKA
    ## 10 KIM       
    ## # … with 3,796 more rows

``` r
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 698
    ## surnames that could not be matched to Census list.

``` r
examiner_race
```

    ## # A tibble: 3,806 × 6
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198
    ## # … with 3,796 more rows

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

examiner_race
```

    ## # A tibble: 3,806 × 8
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth max_race_p race 
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <dbl> <chr>
    ##  1 HOWARD       0.643   0.295    0.0237   0.005     0.0333      0.643 white
    ##  2 YILDIRIM     0.861   0.0271   0.0609   0.0135    0.0372      0.861 white
    ##  3 HAMILTON     0.702   0.237    0.0245   0.0054    0.0309      0.702 white
    ##  4 MOSHER       0.947   0.00410  0.0241   0.00640   0.0185      0.947 white
    ##  5 BARR         0.827   0.117    0.0226   0.00590   0.0271      0.827 white
    ##  6 GRAY         0.687   0.251    0.0241   0.0054    0.0324      0.687 white
    ##  7 MCMILLIAN    0.359   0.574    0.0189   0.00260   0.0463      0.574 black
    ##  8 FORD         0.620   0.32     0.0237   0.0045    0.0313      0.620 white
    ##  9 STRZELECKA   0.666   0.0853   0.137    0.0797    0.0318      0.666 white
    ## 10 KIM          0.0252  0.00390  0.00650  0.945     0.0198      0.945 Asian
    ## # … with 3,796 more rows

``` r
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)

applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) limit (Mb) max used  (Mb)
    ## Ncells  4578244 244.6    7281510 388.9         NA  7281510 388.9
    ## Vcells 52860531 403.3   94874955 723.9      16384 94004940 717.3

## Get Tenure for Examiners

``` r
examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 

examiner_dates
```

    ## # A tibble: 2,018,477 × 3
    ##    examiner_id filing_date appl_status_date  
    ##          <dbl> <date>      <chr>             
    ##  1       96082 2000-01-26  30jan2003 00:00:00
    ##  2       87678 2000-10-11  27sep2010 00:00:00
    ##  3       63213 2000-05-17  30mar2009 00:00:00
    ##  4       73788 2001-07-20  07sep2009 00:00:00
    ##  5       77294 2000-04-10  19apr2001 00:00:00
    ##  6       68606 2000-04-28  16jul2001 00:00:00
    ##  7       89557 2004-01-26  15may2017 00:00:00
    ##  8       97543 2000-06-23  03apr2002 00:00:00
    ##  9       98714 2000-02-04  27nov2002 00:00:00
    ## 10       65530 2002-02-20  23mar2009 00:00:00
    ## # … with 2,018,467 more rows

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))

examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    ) %>% 
  filter(year(latest_date)<2018)

examiner_dates
```

    ## # A tibble: 5,625 × 4
    ##    examiner_id earliest_date latest_date tenure_days
    ##          <dbl> <date>        <date>            <dbl>
    ##  1       59012 2004-07-28    2015-07-24         4013
    ##  2       59025 2009-10-26    2017-05-18         2761
    ##  3       59030 2005-12-12    2017-05-22         4179
    ##  4       59040 2007-09-11    2017-05-23         3542
    ##  5       59052 2001-08-21    2007-02-28         2017
    ##  6       59054 2000-11-10    2016-12-23         5887
    ##  7       59055 2004-11-02    2007-12-26         1149
    ##  8       59056 2000-03-24    2017-05-22         6268
    ##  9       59074 2000-01-31    2017-03-17         6255
    ## 10       59081 2011-04-21    2017-05-19         2220
    ## # … with 5,615 more rows

``` r
applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc() 
```

    ##            used  (Mb) gc trigger   (Mb) limit (Mb)  max used   (Mb)
    ## Ncells  4666102 249.2   13124591  701.0         NA  13124591  701.0
    ## Vcells 65344344 498.6  136795935 1043.7      16384 136397991 1040.7

## Get Application Process Time for Examiners

``` r
# Create column with final decision date
applications <- applications %>% mutate(final_decision_date = coalesce(patent_issue_date, abandon_date)) 

# Calculate number of days
examiner_dates <- applications %>% 
  select(examiner_id, filing_date, final_decision_date) 

examiner_dates
```

    ## # A tibble: 2,018,477 × 3
    ##    examiner_id filing_date final_decision_date
    ##          <dbl> <date>      <date>             
    ##  1       96082 2000-01-26  2003-02-18         
    ##  2       87678 2000-10-11  2002-08-27         
    ##  3       63213 2000-05-17  1997-03-04         
    ##  4       73788 2001-07-20  2005-08-09         
    ##  5       77294 2000-04-10  2000-12-27         
    ##  6       68606 2000-04-28  2001-07-31         
    ##  7       89557 2004-01-26  NA                 
    ##  8       97543 2000-06-23  2001-08-22         
    ##  9       98714 2000-02-04  2002-07-15         
    ## 10       65530 2002-02-20  2005-02-22         
    ## # … with 2,018,467 more rows

``` r
examiner_dates <- examiner_dates %>% 
  mutate(app_date = ymd(filing_date), decision_date = as_date(dmy_hms(final_decision_date)))
```

    ## Warning: All formats failed to parse. No formats found.

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    app_earliest_date = min(filing_date, na.rm = TRUE), 
    app_latest_date = max(final_decision_date, na.rm = TRUE),
    app_proc_time = interval(app_earliest_date, app_latest_date) %/% days(1)
    ) %>% 
  filter(year(app_latest_date)<2018)
```

    ## Warning in max.default(structure(c(NA_real_, NA_real_), class = "Date"), : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(NA_real_, class = "Date"), na.rm = TRUE): no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_), class = "Date"), : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(NA_real_, class = "Date"), na.rm = TRUE): no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(NA_real_, class = "Date"), na.rm = TRUE): no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_: no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

    ## Warning in max.default(structure(c(NA_real_, NA_real_, NA_real_, NA_real_, : no
    ## non-missing arguments to max; returning -Inf

``` r
examiner_dates
```

    ## # A tibble: 5,548 × 4
    ##    examiner_id app_earliest_date app_latest_date app_proc_time
    ##          <dbl> <date>            <date>                  <dbl>
    ##  1       59012 2004-07-28        2011-11-22               2673
    ##  2       59025 2009-10-26        2017-06-20               2794
    ##  3       59030 2005-12-12        2017-06-06               4194
    ##  4       59040 2007-09-11        2017-06-20               3570
    ##  5       59052 2001-08-21        2006-01-11               1604
    ##  6       59054 2000-11-10        2005-10-11               1796
    ##  7       59055 2004-11-02        2007-08-21               1022
    ##  8       59056 2000-03-24        2017-06-20               6297
    ##  9       59074 2000-01-31        2016-03-08               5881
    ## 10       59081 2011-04-21        2017-04-18               2189
    ## # … with 5,538 more rows

``` r
applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc() 
```

    ##            used  (Mb) gc trigger   (Mb) limit (Mb)  max used   (Mb)
    ## Ncells  4667543 249.3   12960631  692.2         NA  20250985 1081.6
    ## Vcells 73422641 560.2  197162146 1504.3      16384 163403144 1246.7

## Calculate Centralities

``` r
applications <- subset(applications, select = c(examiner_art_unit, examiner_id, tc, gender, race, tenure_days, app_proc_time))

applications <- applications %>% drop_na(examiner_id)

examiner_aus = distinct(subset(applications, select=c(examiner_art_unit, examiner_id)))

examiner_aus$workgroup = substr(examiner_aus$examiner_art_unit, 1,3)

examiner_aus = examiner_aus[examiner_aus$workgroup==162 | examiner_aus$workgroup==179,]

advNet = merge(x=edges, y=examiner_aus, by.x="ego_examiner_id", by.y="examiner_id", all.x=TRUE)
advNet = advNet %>% rename(ego_art_unit=examiner_art_unit, ego_workgroup=workgroup)

advNet = drop_na(advNet)

advNet = merge(x=advNet, y=examiner_aus, by.x="alter_examiner_id", by.y="examiner_id", all.x=TRUE)
advNet = advNet %>% rename(alter_art_unit=examiner_art_unit, alter_workgroup=workgroup)
advNet = drop_na(advNet)

egoNodes = subset(advNet, select=c(ego_examiner_id,ego_art_unit, ego_workgroup)) %>% rename(examiner_id=ego_examiner_id,art_unit=ego_art_unit,workgroup=ego_workgroup)
alterNodes = subset(advNet, select=c(alter_examiner_id,alter_art_unit, alter_workgroup))%>% rename(examiner_id=alter_examiner_id,art_unit=alter_art_unit,workgroup=alter_workgroup)
nodes = rbind(egoNodes, alterNodes)
nodes = distinct(nodes)

nodes = nodes %>% group_by(examiner_id) %>% summarise(examiner_id=first(examiner_id), art_unit=first(art_unit), workgroup=first(workgroup))

uspto_net <- graph_from_data_frame(d=advNet, vertices=nodes, directed=TRUE)

Degree <- degree(uspto_net)
Closeness <- closeness(uspto_net)
Betweenness <- betweenness(uspto_net)
Eigenvector <- evcent(uspto_net)$vector

centralities <- data.frame(nodes, Degree,Closeness, Betweenness, Eigenvector)   
centralities
```

    ##       examiner_id art_unit workgroup Degree   Closeness Betweenness
    ## 59165       59165     1792       179      3 1.000000000   2.5000000
    ## 59227       59227     1793       179      4 0.333333333   0.0000000
    ## 59359       59359     1793       179     10         NaN   0.0000000
    ## 59447       59447     1791       179      8         NaN   0.0000000
    ## 59456       59456     1794       179      4 0.250000000   0.0000000
    ## 59475       59475     1791       179     16 0.033333333   0.0000000
    ## 59539       59539     1798       179     18         NaN   0.0000000
    ## 59589       59589     1795       179      2 1.000000000   0.0000000
    ## 59616       59616     1794       179      6 0.333333333   0.0000000
    ## 59632       59632     1626       162      9 1.000000000   0.0000000
    ## 59698       59698     1792       179     10 0.058823529   0.0000000
    ## 59706       59706     1793       179      7 0.166666667   0.0000000
    ## 59771       59771     1791       179     43 0.015384615 118.0000000
    ## 59801       59801     1791       179      2         NaN   0.0000000
    ## 59816       59816     1796       179      2 1.000000000   0.0000000
    ## 59866       59866     1792       179     23         NaN   0.0000000
    ## 59870       59870     1792       179      2 0.333333333   2.0000000
    ## 59975       59975     1792       179     17 0.100000000   0.0000000
    ## 59987       59987     1793       179     39 1.000000000   6.0000000
    ## 60077       60077     1791       179      7 0.125000000   0.0000000
    ## 60078       60078     1794       179     11 0.166666667   0.0000000
    ## 60203       60203     1795       179     26 0.100000000  33.4000000
    ## 60302       60302     1626       162      6         NaN   0.0000000
    ## 60309       60309     1797       179      1 1.000000000   0.0000000
    ## 60400       60400     1793       179      6 0.500000000   0.0000000
    ## 60431       60431     1791       179      4 1.000000000   2.0000000
    ## 60437       60437     1795       179     10 0.111111111   0.0000000
    ## 60569       60569     1794       179      2         NaN   0.0000000
    ## 60575       60575     1793       179      7         NaN   0.0000000
    ## 60584       60584     1796       179      1         NaN   0.0000000
    ## 60651       60651     1791       179     25         NaN   0.0000000
    ## 60706       60706     1797       179      8 0.333333333   0.0000000
    ## 60820       60820     1791       179      2         NaN   0.0000000
    ## 60837       60837     1791       179     40         NaN   0.0000000
    ## 60879       60879     1791       179      1 0.083333333   0.0000000
    ## 60922       60922     1798       179     12         NaN   0.0000000
    ## 60991       60991     1797       179      4         NaN   0.0000000
    ## 61047       61047     1795       179      4 0.200000000   0.0000000
    ## 61121       61121     1793       179      8 1.000000000   0.0000000
    ## 61126       61126     1791       179     13 0.111111111  45.0000000
    ## 61182       61182     1796       179      2 0.006802721   0.0000000
    ## 61283       61283     1791       179      1 1.000000000   0.0000000
    ## 61328       61328     1794       179     16         NaN   0.0000000
    ## 61417       61417     1626       162      2         NaN   0.0000000
    ## 61435       61435     1793       179      4 0.250000000   0.0000000
    ## 61517       61517     1791       179     10         NaN   0.0000000
    ## 61529       61529     1628       162      3         NaN   0.0000000
    ## 61534       61534     1792       179      4         NaN   0.0000000
    ## 61615       61615     1793       179      8         NaN   0.0000000
    ## 61698       61698     1794       179      5 0.007194245  73.0000000
    ## 61759       61759     1792       179     12         NaN   0.0000000
    ## 61863       61863     1795       179     30 0.026315789   0.0000000
    ## 61979       61979     1794       179      2 1.000000000   0.0000000
    ## 62024       62024     1794       179      2 0.111111111   0.0000000
    ## 62064       62064     1792       179      8 0.142857143   0.0000000
    ## 62098       62098     1793       179     38 1.000000000   1.0000000
    ## 62152       62152     1795       179    109         NaN   0.0000000
    ## 62164       62164     1794       179      6 0.250000000   0.0000000
    ## 62170       62170     1794       179     12         NaN   0.0000000
    ## 62206       62206     1795       179      1 1.000000000   0.0000000
    ## 62253       62253     1623       162      5 1.000000000   0.0000000
    ## 62284       62284     1794       179     46         NaN   0.0000000
    ## 62346       62346     1793       179      2         NaN   0.0000000
    ## 62413       62413     1795       179      7         NaN   0.0000000
    ## 62464       62464     1797       179      1 1.000000000   0.0000000
    ## 62495       62495     1794       179      7 0.200000000   5.0000000
    ## 62498       62498     1793       179      7 0.250000000   0.0000000
    ## 62499       62499     1795       179     31 0.020000000   0.0000000
    ## 62610       62610     1797       179      4         NaN   0.0000000
    ## 62659       62659     1795       179     47 0.333333333  63.2928571
    ## 62661       62661     1627       162     13         NaN   0.0000000
    ## 62767       62767     1797       179     16 1.000000000   9.0000000
    ## 62778       62778     1794       179     19 0.043478261  13.0000000
    ## 62815       62815     1799       179     24 0.111111111  17.0000000
    ## 62862       62862     1797       179      9         NaN   0.0000000
    ## 62892       62892     1798       179     14         NaN   0.0000000
    ## 62976       62976     1792       179      8 0.500000000   0.0000000
    ## 62990       62990     1797       179      1         NaN   0.0000000
    ## 63011       63011     1793       179      1         NaN   0.0000000
    ## 63027       63027     1797       179      6         NaN   0.0000000
    ## 63065       63065     1791       179      5 0.200000000   0.0000000
    ## 63101       63101     1798       179      9 0.142857143   0.0000000
    ## 63176       63176     1791       179     82 0.025000000 109.0000000
    ## 63188       63188     1794       179     20 0.090909091   0.0000000
    ## 63213       63213     1795       179      1 0.007246377   0.0000000
    ## 63219       63219     1794       179      9 1.000000000  11.0000000
    ## 63277       63277     1792       179      2 1.000000000   0.0000000
    ## 63324       63324     1793       179      3 1.000000000   1.0000000
    ## 63326       63326     1791       179      3 0.500000000   0.0000000
    ## 63358       63358     1796       179      9         NaN   0.0000000
    ## 63363       63363     1796       179     11 0.142857143   0.0000000
    ## 63366       63366     1792       179      3 1.000000000   0.0000000
    ## 63384       63384     1791       179      3 1.000000000   0.0000000
    ## 63422       63422     1792       179      7 0.111111111   0.0000000
    ## 63428       63428     1796       179      1 1.000000000   0.0000000
    ## 63475       63475     1794       179      2         NaN   0.0000000
    ## 63577       63577     1791       179     18         NaN   0.0000000
    ## 63585       63585     1795       179      3 0.500000000   0.0000000
    ## 63609       63609     1796       179      6 0.014705882   0.0000000
    ## 63613       63613     1792       179      5 1.000000000   3.0000000
    ## 63649       63649     1795       179      1         NaN   0.0000000
    ## 63713       63713     1794       179      2 0.250000000   0.0000000
    ## 63714       63714     1795       179     17         NaN   0.0000000
    ## 63735       63735     1796       179      9         NaN   0.0000000
    ## 63752       63752     1796       179     15 1.000000000   7.0000000
    ## 63814       63814     1794       179      2 0.007194245   0.0000000
    ## 63822       63822     1624       162      1 1.000000000   0.0000000
    ## 63829       63829     1794       179     28 0.250000000   0.0000000
    ## 63842       63842     1791       179      1         NaN   0.0000000
    ## 63935       63935     1796       179     82 0.031250000   0.0000000
    ## 63938       63938     1796       179      4 0.333333333   0.0000000
    ## 63970       63970     1792       179     25 0.142857143   5.0000000
    ## 64002       64002     1792       179      3 0.500000000   0.0000000
    ## 64053       64053     1793       179     87 0.047619048  24.0000000
    ## 64074       64074     1791       179      3 0.014705882   0.0000000
    ## 64119       64119     1797       179     70 0.038461538   0.0000000
    ## 64245       64245     1797       179      1         NaN   0.0000000
    ## 64403       64403     1791       179     21 0.125000000  12.0000000
    ## 64548       64548     1795       179      3 0.006578947   0.0000000
    ## 64823       64823     1794       179     13 0.250000000   0.0000000
    ## 64851       64851     1629       162      1 0.020000000   0.0000000
    ## 64864       64864     1794       179     35 0.500000000   9.0000000
    ## 64915       64915     1791       179      1 1.000000000   0.0000000
    ## 64917       64917     1795       179      7         NaN   0.0000000
    ## 64940       64940     1793       179      7 0.200000000  15.0000000
    ## 64951       64951     1795       179      3 1.000000000   0.0000000
    ## 64992       64992     1799       179      6         NaN   0.0000000
    ## 65031       65031     1796       179     10         NaN   0.0000000
    ## 65111       65111     1623       162     20 1.000000000   0.0000000
    ## 65121       65121     1794       179     23 1.000000000   5.1111111
    ## 65179       65179     1794       179    110 0.062500000  60.5459459
    ## 65260       65260     1794       179      5 0.333333333   0.0000000
    ## 65271       65271     1794       179     19 0.032258065  18.0000000
    ## 65403       65403     1792       179      7 0.100000000   0.0000000
    ## 65454       65454     1793       179      5         NaN   0.0000000
    ## 65474       65474     1796       179      2 0.333333333   0.0000000
    ## 65536       65536     1625       162      1 1.000000000   0.0000000
    ## 65537       65537     1622       162      3 1.000000000   0.0000000
    ## 65601       65601     1797       179     15 0.166666667   0.0000000
    ## 65646       65646     1797       179      3         NaN   0.0000000
    ## 65713       65713     1623       162     12 0.500000000   0.0000000
    ## 65737       65737     1621       162      1 1.000000000   0.0000000
    ## 65757       65757     1797       179      5 0.142857143   0.0000000
    ## 65919       65919     1791       179      7 0.012987013   0.0000000
    ## 65934       65934     1794       179     61 0.004237288   0.0000000
    ## 65962       65962     1623       162      3 0.500000000   0.0000000
    ## 66118       66118     1796       179      3 0.055555556   0.0000000
    ## 66247       66247     1792       179     10         NaN   0.0000000
    ## 66251       66251     1795       179      4         NaN   0.0000000
    ## 66264       66264     1797       179     16         NaN   0.0000000
    ## 66344       66344     1792       179      2         NaN   0.0000000
    ## 66346       66346     1793       179      1 1.000000000   0.0000000
    ## 66387       66387     1797       179      2 0.043478261   0.0000000
    ## 66436       66436     1793       179     75         NaN   0.0000000
    ## 66442       66442     1797       179     28 0.015384615   0.0000000
    ## 66450       66450     1796       179     13 0.012345679   0.0000000
    ## 66503       66503     1795       179      1         NaN   0.0000000
    ## 66582       66582     1792       179     53 0.006666667   0.0000000
    ## 66703       66703     1626       162      1 0.007246377   0.0000000
    ## 66715       66715     1792       179     12         NaN   0.0000000
    ## 66762       66762     1797       179      5 0.083333333   0.0000000
    ## 66769       66769     1797       179      2 0.500000000   0.0000000
    ## 66824       66824     1794       179      3         NaN   0.0000000
    ## 66879       66879     1797       179      2         NaN   0.0000000
    ## 66910       66910     1794       179      5         NaN   0.0000000
    ## 66949       66949     1797       179      4 1.000000000   0.0000000
    ## 66958       66958     1792       179      2 0.500000000   0.0000000
    ## 67021       67021     1794       179      2 0.500000000   0.0000000
    ## 67034       67034     1795       179     35         NaN   0.0000000
    ## 67050       67050     1791       179     39         NaN   0.0000000
    ## 67217       67217     1793       179      4         NaN   0.0000000
    ## 67256       67256     1627       162     22 0.142857143   0.0000000
    ## 67259       67259     1791       179      4 0.250000000   0.0000000
    ## 67300       67300     1799       179     52         NaN   0.0000000
    ## 67331       67331     1796       179      6 0.333333333   3.0000000
    ## 67376       67376     1797       179      7 0.250000000   6.0000000
    ## 67409       67409     1796       179      1 0.333333333   0.0000000
    ## 67478       67478     1791       179      1         NaN   0.0000000
    ## 67487       67487     1792       179      3 0.500000000   0.0000000
    ## 67506       67506     1792       179      2         NaN   0.0000000
    ## 67581       67581     1621       162      7 0.250000000   0.0000000
    ## 67690       67690     1623       162     42         NaN   0.0000000
    ## 67698       67698     1793       179     55 0.014925373   0.0000000
    ## 67731       67731     1621       162      1 1.000000000   0.0000000
    ## 67753       67753     1623       162      1         NaN   0.0000000
    ## 67829       67829     1795       179     62 0.100000000   0.0000000
    ## 67901       67901     1791       179      2 1.000000000   1.0000000
    ## 67973       67973     1795       179      1 1.000000000   0.0000000
    ## 68017       68017     1797       179     15         NaN   0.0000000
    ## 68071       68071     1794       179      6         NaN   0.0000000
    ## 68153       68153     1794       179      2 0.500000000   0.0000000
    ## 68165       68165     1793       179      5 0.500000000   0.0000000
    ## 68166       68166     1625       162     11 1.000000000   0.0000000
    ## 68227       68227     1791       179      2 0.166666667   0.0000000
    ## 68339       68339     1626       162      1 1.000000000   0.0000000
    ## 68384       68384     1792       179      7 0.333333333  24.0000000
    ## 68415       68415     1795       179      1 0.333333333   0.0000000
    ## 68463       68463     1791       179     17         NaN   0.0000000
    ## 68511       68511     1792       179      8 0.500000000   0.0000000
    ## 68546       68546     1793       179      1 1.000000000   0.0000000
    ## 68598       68598     1799       179      8         NaN   0.0000000
    ## 68603       68603     1793       179      3 0.500000000   0.0000000
    ## 68619       68619     1791       179     42         NaN   0.0000000
    ## 68675       68675     1796       179      2 1.000000000   0.0000000
    ## 68678       68678     1794       179     17 1.000000000   1.0000000
    ## 68695       68695     1627       162      7 1.000000000   1.0000000
    ## 68719       68719     1795       179      4         NaN   0.0000000
    ## 68737       68737     1626       162      2 0.333333333   0.0000000
    ## 68788       68788     1791       179      7         NaN   0.0000000
    ## 68815       68815     1795       179      2         NaN   0.0000000
    ## 68915       68915     1795       179      2 0.500000000   0.0000000
    ## 68970       68970     1793       179      5 0.200000000   4.0000000
    ## 69077       69077     1794       179     51 0.500000000  10.2727273
    ## 69096       69096     1794       179      8         NaN   0.0000000
    ## 69098       69098     1797       179    138         NaN   0.0000000
    ## 69099       69099     1792       179      2 0.100000000   0.0000000
    ## 69138       69138     1621       162      3 0.058823529   0.0000000
    ## 69193       69193     1793       179      9 0.008695652   0.0000000
    ## 69209       69209     1792       179      3 0.007246377   0.0000000
    ## 69228       69228     1796       179      3 0.500000000   0.0000000
    ## 69242       69242     1793       179      2         NaN   0.0000000
    ## 69304       69304     1796       179     48 1.000000000   2.0000000
    ## 69378       69378     1795       179      9 0.166666667  89.0000000
    ## 69394       69394     1792       179     11         NaN   0.0000000
    ## 69464       69464     1797       179      1         NaN   0.0000000
    ## 69539       69539     1797       179      1         NaN   0.0000000
    ## 69581       69581     1795       179      1         NaN   0.0000000
    ## 69665       69665     1797       179      5         NaN   0.0000000
    ## 69680       69680     1795       179     10         NaN   0.0000000
    ## 69711       69711     1796       179      3         NaN   0.0000000
    ## 69780       69780     1797       179      1         NaN   0.0000000
    ## 69794       69794     1797       179      4         NaN   0.0000000
    ## 69800       69800     1796       179      3 0.250000000   3.0000000
    ## 69896       69896     1628       162      1         NaN   0.0000000
    ## 69909       69909     1794       179     69 0.029411765   0.0000000
    ## 70017       70017     1625       162      2 0.333333333   0.0000000
    ## 70032       70032     1797       179      4 1.000000000   4.0000000
    ## 70176       70176     1793       179     29         NaN   0.0000000
    ## 70206       70206     1627       162      4         NaN   0.0000000
    ## 70227       70227     1793       179      7         NaN   0.0000000
    ## 70248       70248     1795       179      4 1.000000000   0.0000000
    ## 70268       70268     1795       179      3 0.250000000   0.0000000
    ## 70423       70423     1797       179      2 1.000000000   0.0000000
    ## 70458       70458     1799       179     12 0.500000000   0.0000000
    ## 70610       70610     1796       179     17 0.013698630   0.0000000
    ## 70767       70767     1624       162     12         NaN   0.0000000
    ## 70799       70799     1794       179     18         NaN   0.0000000
    ## 70887       70887     1794       179     10 0.052631579   0.0000000
    ## 71035       71035     1794       179     38 0.038461538   0.0000000
    ## 71086       71086     1794       179      1 0.009433962   0.0000000
    ## 71101       71101     1793       179      1 1.000000000   0.0000000
    ## 71107       71107     1791       179      7         NaN   0.0000000
    ## 71119       71119     1792       179     20 0.009803922 381.0131579
    ## 71142       71142     1792       179     12         NaN   0.0000000
    ## 71143       71143     1796       179     31         NaN   0.0000000
    ## 71174       71174     1792       179     15 0.041666667  53.0000000
    ## 71353       71353     1796       179     11 0.111111111   8.0000000
    ## 71388       71388     1798       179     68 0.250000000  66.5573381
    ## 71447       71447     1791       179     16         NaN   0.0000000
    ## 71557       71557     1791       179      3 0.333333333   0.0000000
    ## 71655       71655     1793       179      8         NaN   0.0000000
    ## 71704       71704     1796       179     10 0.125000000  14.0000000
    ## 71720       71720     1797       179      5 0.071428571   0.0000000
    ## 71762       71762     1794       179     16 1.000000000  25.0000000
    ## 72052       72052     1793       179     31         NaN   0.0000000
    ## 72097       72097     1798       179      4         NaN   0.0000000
    ## 72102       72102     1793       179      4         NaN   0.0000000
    ## 72122       72122     1797       179      4 1.000000000   0.0000000
    ## 72148       72148     1794       179      1         NaN   0.0000000
    ## 72153       72153     1793       179      6         NaN   0.0000000
    ## 72165       72165     1797       179      1 0.055555556   0.0000000
    ## 72332       72332     1797       179      1         NaN   0.0000000
    ## 72514       72514     1797       179      7         NaN   0.0000000
    ## 72524       72524     1793       179      4 1.000000000   6.0000000
    ## 72576       72576     1797       179      4 1.000000000   2.0000000
    ## 72591       72591     1797       179      4 0.100000000  26.0000000
    ## 72613       72613     1796       179      1         NaN   0.0000000
    ## 72638       72638     1796       179      7 0.062500000   0.0000000
    ## 72666       72666     1793       179     28 0.005319149  43.8961593
    ## 72809       72809     1796       179     42 0.041666667   0.0000000
    ## 72855       72855     1795       179      1         NaN   0.0000000
    ## 72941       72941     1626       162      1         NaN   0.0000000
    ## 73074       73074     1791       179      3 0.009615385  41.0000000
    ## 73213       73213     1796       179      1 0.062500000   0.0000000
    ## 73274       73274     1791       179      1 1.000000000   0.0000000
    ## 73327       73327     1796       179      4 0.500000000   0.0000000
    ## 73364       73364     1629       162      5         NaN   0.0000000
    ## 73383       73383     1792       179      2 0.025000000   0.0000000
    ## 73764       73764     1797       179      1         NaN   0.0000000
    ## 73777       73777     1623       162      7 1.000000000   0.0000000
    ## 74334       74334     1795       179      7         NaN   0.0000000
    ## 74342       74342     1791       179     19         NaN   0.0000000
    ## 74347       74347     1794       179      1         NaN   0.0000000
    ## 74579       74579     1794       179     50 0.200000000  14.0000000
    ## 74684       74684     1793       179      7 0.090909091   0.0000000
    ## 74725       74725     1794       179      3 0.333333333   0.0000000
    ## 74727       74727     1797       179      6 0.500000000   0.0000000
    ## 75034       75034     1626       162      3 1.000000000   1.0000000
    ## 75238       75238     1791       179      7         NaN   0.0000000
    ## 75336       75336     1797       179     12 0.333333333   3.0000000
    ## 75341       75341     1792       179     27 0.500000000  34.1428571
    ## 75367       75367     1795       179     23 0.200000000  32.0000000
    ## 75387       75387     1796       179      9 0.038461538   0.0000000
    ## 75406       75406     1791       179     40 0.033333333   0.0000000
    ## 75409       75409     1793       179      7 0.500000000   8.0000000
    ## 75461       75461     1792       179      9 0.043478261   0.0000000
    ## 75530       75530     1795       179      9         NaN   0.0000000
    ## 75592       75592     1792       179     39         NaN   0.0000000
    ## 75718       75718     1796       179      7         NaN   0.0000000
    ## 75774       75774     1793       179     20         NaN   0.0000000
    ## 75864       75864     1796       179     11         NaN   0.0000000
    ## 75933       75933     1795       179     42 0.090909091  40.6071429
    ## 75954       75954     1791       179      7         NaN   0.0000000
    ## 76021       76021     1791       179      5 1.000000000   4.0000000
    ## 76081       76081     1796       179      4 0.066666667   0.0000000
    ## 76154       76154     1791       179     28         NaN   0.0000000
    ## 76320       76320     1797       179     10 0.250000000   0.0000000
    ## 76370       76370     1796       179      3 1.000000000   0.0000000
    ## 76447       76447     1792       179      7 0.032258065   0.0000000
    ## 76469       76469     1793       179      1 1.000000000   0.0000000
    ## 76516       76516     1794       179      7 0.500000000   6.0000000
    ## 76532       76532     1794       179      1 1.000000000   0.0000000
    ## 76583       76583     1791       179     15         NaN   0.0000000
    ## 76622       76622     1792       179      3 0.200000000   0.0000000
    ## 76629       76629     1795       179      4         NaN   0.0000000
    ## 76727       76727     1793       179     18         NaN   0.0000000
    ## 76764       76764     1793       179      4         NaN   0.0000000
    ## 76927       76927     1794       179     21 0.142857143  12.0000000
    ## 76959       76959     1791       179      4 1.000000000   3.0000000
    ## 76979       76979     1792       179      9         NaN   0.0000000
    ## 77112       77112     1793       179     10 0.052631579  20.0000000
    ## 77190       77190     1796       179      3         NaN   0.0000000
    ## 77294       77294     1792       179     38 0.034482759   0.0000000
    ## 77298       77298     1791       179     46 0.038461538   0.0000000
    ## 77348       77348     1626       162      1 1.000000000   0.0000000
    ## 77369       77369     1795       179      4 0.010204082  20.0000000
    ## 77651       77651     1795       179     26 0.250000000   0.0000000
    ## 77743       77743     1791       179      1 0.333333333   0.0000000
    ## 77749       77749     1791       179      2         NaN   0.0000000
    ## 77772       77772     1794       179      3         NaN   0.0000000
    ## 77791       77791     1793       179      7         NaN   0.0000000
    ## 77915       77915     1795       179     14 0.050000000   0.0000000
    ## 77958       77958     1797       179      3 0.090909091  12.0000000
    ## 78003       78003     1793       179      1         NaN   0.0000000
    ## 78051       78051     1797       179      6         NaN   0.0000000
    ## 78056       78056     1796       179      2         NaN   0.0000000
    ## 78144       78144     1795       179      4 0.050000000   0.0000000
    ## 78379       78379     1796       179      7 0.028571429  29.3921569
    ## 78406       78406     1797       179      3 0.083333333   0.0000000
    ## 78568       78568     1791       179     17         NaN   0.0000000
    ## 78715       78715     1795       179      3         NaN   0.0000000
    ## 78807       78807     1792       179     11 1.000000000   0.0000000
    ## 79289       79289     1797       179      1 0.045454545   0.0000000
    ## 79538       79538     1794       179     13 0.333333333   0.0000000
    ## 79564       79564     1793       179     13 0.333333333   0.2000000
    ## 79847       79847     1794       179     34         NaN   0.0000000
    ## 80730       80730     1794       179    128         NaN   0.0000000
    ## 80826       80826     1791       179      2 0.333333333   0.0000000
    ## 80908       80908     1798       179     28         NaN   0.0000000
    ## 81211       81211     1628       162      3         NaN   0.0000000
    ## 81831       81831     1794       179      1 0.333333333   0.0000000
    ## 81865       81865     1623       162      6 1.000000000   0.0000000
    ## 81877       81877     1791       179     12         NaN   0.0000000
    ## 81911       81911     1791       179     17         NaN   0.0000000
    ## 81959       81959     1629       162      3         NaN   0.0000000
    ## 81984       81984     1795       179     14 0.166666667   0.0000000
    ## 82563       82563     1796       179      3 0.024390244   0.0000000
    ## 82735       82735     1792       179     10 0.006993007   0.0000000
    ## 82997       82997     1625       162      4 1.000000000   0.0000000
    ## 83034       83034     1795       179      5         NaN   0.0000000
    ## 83323       83323     1795       179      2         NaN   0.0000000
    ## 83398       83398     1792       179     29 0.007246377   1.1578947
    ## 83475       83475     1797       179     16         NaN   0.0000000
    ## 83597       83597     1793       179      2 0.166666667   0.0000000
    ## 83601       83601     1792       179     11         NaN   0.0000000
    ## 84157       84157     1796       179     46 0.111111111   0.0000000
    ## 84267       84267     1795       179     23         NaN   0.0000000
    ## 84289       84289     1791       179      8 1.000000000   1.0000000
    ## 84609       84609     1792       179      5 1.000000000   0.0000000
    ## 84867       84867     1793       179      1         NaN   0.0000000
    ## 85060       85060     1794       179     10 0.200000000   4.0000000
    ## 85216       85216     1627       162      5         NaN   0.0000000
    ## 85323       85323     1795       179     18         NaN   0.0000000
    ## 85449       85449     1796       179      8         NaN   0.0000000
    ## 85599       85599     1796       179      5         NaN   0.0000000
    ## 85940       85940     1795       179      1         NaN   0.0000000
    ## 86201       86201     1796       179     29 0.071428571   0.0000000
    ## 86212       86212     1796       179      2 1.000000000   0.0000000
    ## 86422       86422     1794       179      4 0.333333333   0.0000000
    ## 86683       86683     1791       179      3         NaN   0.0000000
    ## 86761       86761     1624       162      1 1.000000000   0.0000000
    ## 86928       86928     1794       179      5 1.000000000   0.0000000
    ## 86985       86985     1795       179      1         NaN   0.0000000
    ## 87049       87049     1794       179     18         NaN   0.0000000
    ## 87124       87124     1796       179      5 0.142857143   0.0000000
    ## 87125       87125     1795       179     28 0.052631579  51.0000000
    ## 87309       87309     1624       162      1 0.333333333   0.0000000
    ## 87486       87486     1621       162      5 0.142857143   0.0000000
    ## 87554       87554     1795       179      1 0.045454545   0.0000000
    ## 87571       87571     1795       179      3 0.024390244   0.0000000
    ## 88033       88033     1796       179      5         NaN   0.0000000
    ## 88092       88092     1792       179      3         NaN   0.0000000
    ## 88202       88202     1792       179      6 0.111111111   6.0000000
    ## 88204       88204     1796       179      4 1.000000000   0.0000000
    ## 88508       88508     1621       162      3         NaN   0.0000000
    ## 88509       88509     1794       179      2         NaN   0.0000000
    ## 88730       88730     1792       179      2 1.000000000   1.0000000
    ## 88946       88946     1791       179      1         NaN   0.0000000
    ## 89023       89023     1797       179      5 0.066666667  18.0000000
    ## 89192       89192     1795       179      1         NaN   0.0000000
    ## 89312       89312     1792       179     22         NaN   0.0000000
    ## 89403       89403     1791       179     16 0.058823529   0.0000000
    ## 89539       89539     1793       179    109 0.027777778   0.0000000
    ## 89550       89550     1796       179      8 1.000000000   5.0000000
    ## 89882       89882     1623       162      6 0.333333333   0.0000000
    ## 90168       90168     1794       179      7         NaN   0.0000000
    ## 90201       90201     1792       179     24         NaN   0.0000000
    ## 90241       90241     1795       179      4 0.043478261   0.0000000
    ## 90956       90956     1793       179     85 0.043478261  14.0000000
    ## 91048       91048     1793       179      1         NaN   0.0000000
    ## 91064       91064     1794       179     40         NaN   0.0000000
    ## 91159       91159     1791       179      3         NaN   0.0000000
    ## 91190       91190     1795       179     58         NaN   0.0000000
    ## 91210       91210     1795       179      1 0.142857143   0.0000000
    ## 91233       91233     1792       179      6         NaN   0.0000000
    ## 91352       91352     1798       179     32 0.022727273  17.0000000
    ## 91423       91423     1797       179      4 0.500000000   0.0000000
    ## 91426       91426     1798       179     10 0.250000000   0.0000000
    ## 91747       91747     1627       162      1 1.000000000   0.0000000
    ## 91833       91833     1796       179      2         NaN   0.0000000
    ## 91867       91867     1794       179      5 0.250000000   0.0000000
    ## 91908       91908     1794       179      1 0.333333333   0.0000000
    ## 91956       91956     1629       162      3         NaN   0.0000000
    ## 92108       92108     1795       179      1         NaN   0.0000000
    ## 92143       92143     1791       179     33 0.166666667  48.0000000
    ## 92191       92191     1795       179      1         NaN   0.0000000
    ## 92231       92231     1794       179      1         NaN   0.0000000
    ## 92238       92238     1796       179      4         NaN   0.0000000
    ## 92443       92443     1792       179      7         NaN   0.0000000
    ## 92476       92476     1796       179      3 1.000000000   1.0000000
    ## 92487       92487     1792       179     40         NaN   0.0000000
    ## 92537       92537     1796       179      8 0.142857143   0.0000000
    ## 92612       92612     1792       179      5 1.000000000   2.5000000
    ## 92688       92688     1626       162      3 1.000000000   0.0000000
    ## 92715       92715     1795       179      3 0.500000000   0.0000000
    ## 92733       92733     1793       179      4 0.500000000   0.0000000
    ## 92836       92836     1796       179      5 0.500000000   0.0000000
    ## 92890       92890     1795       179      3         NaN   0.0000000
    ## 92902       92902     1621       162      1         NaN   0.0000000
    ## 93072       93072     1623       162      1 1.000000000   0.0000000
    ## 93178       93178     1625       162      4 0.166666667   0.0000000
    ## 93403       93403     1626       162      9 0.250000000   0.0000000
    ## 93432       93432     1792       179     13 1.000000000   4.0394737
    ## 93486       93486     1794       179      1         NaN   0.0000000
    ## 93553       93553     1791       179      4 0.100000000  30.0000000
    ## 93626       93626     1793       179     13         NaN   0.0000000
    ## 93636       93636     1794       179     11         NaN   0.0000000
    ## 93653       93653     1796       179      1         NaN   0.0000000
    ## 93663       93663     1797       179     16 0.250000000  28.0000000
    ## 93677       93677     1623       162      1         NaN   0.0000000
    ## 93682       93682     1798       179      2 0.333333333   0.0000000
    ## 93715       93715     1793       179      4         NaN   0.0000000
    ## 93725       93725     1797       179      1 1.000000000   0.0000000
    ## 93732       93732     1794       179     29 0.066666667  14.0000000
    ## 93859       93859     1794       179     24 0.166666667  26.3459459
    ## 93896       93896     1792       179    222         NaN   0.0000000
    ## 93909       93909     1791       179      6 0.111111111   0.0000000
    ## 93968       93968     1795       179     34         NaN   0.0000000
    ## 94011       94011     1795       179      2         NaN   0.0000000
    ## 94035       94035     1798       179     16         NaN   0.0000000
    ## 94070       94070     1623       162     10 0.500000000   0.0000000
    ## 94161       94161     1791       179     32 1.000000000  13.0000000
    ## 94211       94211     1792       179      1         NaN   0.0000000
    ## 94238       94238     1793       179      1         NaN   0.0000000
    ## 94251       94251     1791       179      1         NaN   0.0000000
    ## 94260       94260     1797       179     18         NaN   0.0000000
    ## 94301       94301     1792       179      6         NaN   0.0000000
    ## 94340       94340     1792       179      3 0.333333333   0.0000000
    ## 94341       94341     1792       179      4         NaN   0.0000000
    ## 94390       94390     1796       179     32 0.250000000   0.0000000
    ## 94433       94433     1796       179      1 1.000000000   0.0000000
    ## 94466       94466     1795       179      7         NaN   0.0000000
    ## 94543       94543     1796       179      3 0.005813953   0.0000000
    ## 94620       94620     1795       179      2 1.000000000   0.0000000
    ## 94658       94658     1795       179      2 0.142857143   0.0000000
    ## 94737       94737     1796       179      2         NaN   0.0000000
    ## 94774       94774     1797       179     40 0.500000000   0.0000000
    ## 94782       94782     1795       179     53 1.000000000   0.5000000
    ## 94925       94925     1626       162      4         NaN   0.0000000
    ## 94939       94939     1625       162      1 0.200000000   0.0000000
    ## 94954       94954     1794       179      1 1.000000000   0.0000000
    ## 94956       94956     1795       179     10 0.010000000 102.3142857
    ## 94989       94989     1792       179      2 0.250000000   0.0000000
    ## 94994       94994     1792       179      5         NaN   0.0000000
    ## 95010       95010     1795       179      1         NaN   0.0000000
    ## 95065       95065     1795       179      4 0.111111111   0.0000000
    ## 95160       95160     1795       179     11 0.090909091   0.0000000
    ## 95210       95210     1796       179      3         NaN   0.0000000
    ## 95219       95219     1797       179      9         NaN   0.0000000
    ## 95225       95225     1627       162     18 0.031250000   0.0000000
    ## 95299       95299     1795       179      9 0.022222222   0.0000000
    ## 95446       95446     1625       162     17         NaN   0.0000000
    ## 95459       95459     1793       179     26 0.111111111  10.0000000
    ## 95508       95508     1794       179      1         NaN   0.0000000
    ## 95561       95561     1794       179     74 0.041666667  13.0540541
    ## 95721       95721     1794       179      4         NaN   0.0000000
    ## 95782       95782     1794       179      4 1.000000000   0.0000000
    ## 95799       95799     1792       179      9 1.000000000   7.0000000
    ## 95814       95814     1793       179      9         NaN   0.0000000
    ## 95839       95839     1796       179      1 1.000000000   0.0000000
    ## 95860       95860     1792       179      3 0.125000000   5.0000000
    ## 95863       95863     1797       179     10 0.076923077   0.0000000
    ## 95868       95868     1791       179     13         NaN   0.0000000
    ## 95891       95891     1795       179      8         NaN   0.0000000
    ## 95900       95900     1793       179      3         NaN   0.0000000
    ## 95970       95970     1797       179     10 0.040000000   0.0000000
    ## 96068       96068     1793       179      8 0.090909091  22.8000000
    ## 96125       96125     1797       179      4 1.000000000   2.0000000
    ## 96172       96172     1794       179     40 0.052631579   7.0000000
    ## 96234       96234     1792       179      5         NaN   0.0000000
    ## 96267       96267     1793       179      8         NaN   0.0000000
    ## 96304       96304     1796       179      1         NaN   0.0000000
    ## 96371       96371     1793       179      8 0.333333333   2.1578947
    ## 96439       96439     1796       179     25 0.050000000   0.0000000
    ## 96500       96500     1791       179      3 0.142857143   0.0000000
    ## 96532       96532     1796       179     26         NaN   0.0000000
    ## 96556       96556     1791       179     24 0.043478261   0.0000000
    ## 96568       96568     1793       179     11 0.028571429   0.0000000
    ## 96794       96794     1791       179      3         NaN   0.0000000
    ## 96877       96877     1792       179      3         NaN   0.0000000
    ## 96898       96898     1628       162      2         NaN   0.0000000
    ## 97024       97024     1793       179      4 0.250000000   0.0000000
    ## 97083       97083     1795       179     20         NaN   0.0000000
    ## 97187       97187     1797       179      1 1.000000000   0.0000000
    ## 97287       97287     1796       179     17 0.250000000   6.6666667
    ## 97312       97312     1796       179      1 0.333333333   0.0000000
    ## 97404       97404     1792       179      2 0.500000000   0.0000000
    ## 97431       97431     1795       179      2         NaN   0.0000000
    ## 97436       97436     1796       179      1         NaN   0.0000000
    ## 97456       97456     1795       179     11         NaN   0.0000000
    ## 97460       97460     1794       179      6         NaN   0.0000000
    ## 97473       97473     1797       179     26         NaN   0.0000000
    ## 97481       97481     1797       179      2         NaN   0.0000000
    ## 97520       97520     1624       162      1 1.000000000   0.0000000
    ## 97553       97553     1793       179      3 0.250000000   0.0000000
    ## 97560       97560     1797       179      1 1.000000000   0.0000000
    ## 97588       97588     1795       179      1         NaN   0.0000000
    ## 97615       97615     1795       179      1         NaN   0.0000000
    ## 97649       97649     1797       179      2 1.000000000   0.0000000
    ## 97650       97650     1792       179     15 0.125000000   6.8571429
    ## 97657       97657     1791       179     14 1.000000000   1.0000000
    ## 97692       97692     1794       179     36 0.026315789   0.0000000
    ## 97705       97705     1793       179     16         NaN   0.0000000
    ## 97741       97741     1791       179      1 1.000000000   0.0000000
    ## 97745       97745     1792       179      3 0.024390244   0.0000000
    ## 97755       97755     1795       179     22         NaN   0.0000000
    ## 97818       97818     1795       179     20         NaN   0.0000000
    ## 97884       97884     1795       179      1         NaN   0.0000000
    ## 97889       97889     1796       179      2         NaN   0.0000000
    ## 97955       97955     1795       179     12 1.000000000   1.0000000
    ## 97957       97957     1795       179      6 0.333333333   0.0000000
    ## 98014       98014     1791       179      4         NaN   0.0000000
    ## 98045       98045     1793       179     20         NaN   0.0000000
    ## 98098       98098     1796       179      4 1.000000000   1.0000000
    ## 98114       98114     1795       179     25 0.035714286  48.4000000
    ## 98245       98245     1794       179      1 1.000000000   0.0000000
    ## 98253       98253     1794       179      9         NaN   0.0000000
    ## 98297       98297     1792       179    124 0.007299270   0.7894737
    ## 98394       98394     1796       179      3 0.333333333   0.0000000
    ## 98469       98469     1793       179      5         NaN   0.0000000
    ## 98489       98489     1795       179     21 0.009708738   0.0000000
    ## 98546       98546     1794       179     17         NaN   0.0000000
    ## 98558       98558     1797       179     29 0.125000000   8.0000000
    ## 98582       98582     1796       179     24 0.125000000 193.0000000
    ## 98700       98700     1625       162     13 0.250000000   0.0000000
    ## 98705       98705     1796       179      2 0.005617978   0.0000000
    ## 98804       98804     1796       179     20 0.142857143   6.0000000
    ## 98826       98826     1795       179     77 0.018518519 441.8857143
    ## 98834       98834     1795       179     15 0.142857143   0.0000000
    ## 98840       98840     1794       179     11         NaN   0.0000000
    ## 98852       98852     1795       179     27 0.014084507 583.6000000
    ## 98891       98891     1793       179      6 0.029411765  14.0000000
    ## 98943       98943     1793       179      1 1.000000000   0.0000000
    ## 98995       98995     1797       179      2 0.076923077   0.0000000
    ## 99003       99003     1791       179      2         NaN   0.0000000
    ## 99014       99014     1791       179      1 0.333333333   0.0000000
    ## 99047       99047     1627       162      5         NaN   0.0000000
    ## 99106       99106     1791       179      3         NaN   0.0000000
    ## 99191       99191     1621       162      1 1.000000000   0.0000000
    ## 99207       99207     1793       179      6 0.333333333   1.0000000
    ## 99217       99217     1791       179      3 0.024390244   0.0000000
    ## 99218       99218     1797       179      3 0.250000000   0.0000000
    ## 99224       99224     1796       179     15 0.090909091   0.0000000
    ## 99240       99240     1796       179     50         NaN   0.0000000
    ## 99316       99316     1793       179     32 0.066666667   0.0000000
    ## 99424       99424     1625       162      1         NaN   0.0000000
    ## 99457       99457     1792       179     26         NaN   0.0000000
    ## 99465       99465     1791       179      6         NaN   0.0000000
    ## 99484       99484     1794       179      5         NaN   0.0000000
    ## 99518       99518     1794       179     16         NaN   0.0000000
    ## 99520       99520     1794       179      2 1.000000000   0.0000000
    ## 99535       99535     1622       162      2 0.028571429   0.0000000
    ## 99540       99540     1795       179      1         NaN   0.0000000
    ## 99662       99662     1797       179      5 1.000000000   0.0000000
    ## 99725       99725     1798       179     26 0.333333333   0.0000000
    ## 99845       99845     1796       179     22         NaN   0.0000000
    ## 99850       99850     1791       179      3 0.500000000   0.0000000
    ## 99876       99876     1794       179      5         NaN   0.0000000
    ## 99879       99879     1793       179     11 1.000000000   0.0000000
    ## 99929       99929     1792       179      4         NaN   0.0000000
    ## 99930       99930     1797       179    106 0.250000000   0.0000000
    ##        Eigenvector
    ## 59165 1.713008e-05
    ## 59227 6.463529e-06
    ## 59359 7.541575e-06
    ## 59447 1.613129e-06
    ## 59456 2.730313e-05
    ## 59475 1.949299e-05
    ## 59539 2.478662e-08
    ## 59589 3.703910e-11
    ## 59616 3.502601e-08
    ## 59632 1.339735e-09
    ## 59698 8.731996e-09
    ## 59706 2.229079e-05
    ## 59771 7.231733e-06
    ## 59801 6.167868e-08
    ## 59816 4.897235e-08
    ## 59866 9.845149e-06
    ## 59870 1.384775e-07
    ## 59975 4.002013e-04
    ## 59987 2.903611e-05
    ## 60077 4.909672e-07
    ## 60078 2.264190e-05
    ## 60203 5.953953e-04
    ## 60302 5.059030e-17
    ## 60309 2.122823e-07
    ## 60400 5.154729e-08
    ## 60431 4.635321e-09
    ## 60437 1.987517e-04
    ## 60569 9.275475e-05
    ## 60575 4.659769e-05
    ## 60584 1.084123e-07
    ## 60651 1.480506e-06
    ## 60706 5.835379e-08
    ## 60820 2.111278e-08
    ## 60837 3.691897e-04
    ## 60879 5.187498e-07
    ## 60922 1.371745e-04
    ## 60991 2.041753e-07
    ## 61047 1.915624e-04
    ## 61121 1.340248e-10
    ## 61126 1.396675e-04
    ## 61182 1.201950e-08
    ## 61283 1.298493e-07
    ## 61328 9.424012e-03
    ## 61417 0.000000e+00
    ## 61435 8.361664e-07
    ## 61517 1.449611e-06
    ## 61529 0.000000e+00
    ## 61534 4.126540e-06
    ## 61615 1.123983e-05
    ## 61698 6.418802e-04
    ## 61759 2.657956e-05
    ## 61863 1.742243e-06
    ## 61979 2.168218e-05
    ## 62024 6.790085e-05
    ## 62064 4.792656e-02
    ## 62098 1.267315e-04
    ## 62152 1.248774e-05
    ## 62164 1.534140e-03
    ## 62170 1.594975e-05
    ## 62206 3.740343e-09
    ## 62253 7.089251e-15
    ## 62284 1.145832e-02
    ## 62346 2.355576e-06
    ## 62413 3.062388e-06
    ## 62464 1.488594e-10
    ## 62495 1.934313e-06
    ## 62498 4.384253e-05
    ## 62499 6.600407e-04
    ## 62610 9.349433e-10
    ## 62659 8.598949e-06
    ## 62661 0.000000e+00
    ## 62767 2.631838e-06
    ## 62778 9.743815e-05
    ## 62815 4.809903e-02
    ## 62862 1.362345e-09
    ## 62892 4.075857e-04
    ## 62976 5.044284e-05
    ## 62990 6.871383e-10
    ## 63011 9.995957e-08
    ## 63027 4.071202e-06
    ## 63065 1.582016e-06
    ## 63101 4.879116e-05
    ## 63176 6.659157e-05
    ## 63188 5.806845e-03
    ## 63213 4.586136e-04
    ## 63219 4.048587e-05
    ## 63277 1.572600e-07
    ## 63324 1.178316e-06
    ## 63326 2.375107e-08
    ## 63358 6.975289e-07
    ## 63363 5.364688e-07
    ## 63366 3.583390e-06
    ## 63384 1.702505e-11
    ## 63422 7.562095e-06
    ## 63428 3.293397e-08
    ## 63475 8.233827e-10
    ## 63577 1.211932e-06
    ## 63585 5.822794e-08
    ## 63609 7.819133e-07
    ## 63613 9.136896e-07
    ## 63649 1.391473e-08
    ## 63713 8.591290e-06
    ## 63714 5.377855e-04
    ## 63735 2.454523e-06
    ## 63752 2.270121e-06
    ## 63814 4.715368e-04
    ## 63822 0.000000e+00
    ## 63829 2.617739e-04
    ## 63842 1.847043e-07
    ## 63935 1.408679e-03
    ## 63938 9.703245e-06
    ## 63970 3.973896e-06
    ## 64002 4.349156e-07
    ## 64053 8.530185e-06
    ## 64074 1.387181e-06
    ## 64119 1.119992e-05
    ## 64245 2.471419e-10
    ## 64403 1.073077e-04
    ## 64548 1.151564e-03
    ## 64823 3.565046e-05
    ## 64851 2.682296e-06
    ## 64864 3.767246e-05
    ## 64915 3.740343e-09
    ## 64917 1.412732e-06
    ## 64940 7.676035e-04
    ## 64951 2.708461e-05
    ## 64992 1.509739e-06
    ## 65031 0.000000e+00
    ## 65111 0.000000e+00
    ## 65121 9.864787e-04
    ## 65179 1.659988e-03
    ## 65260 2.211247e-07
    ## 65271 3.358465e-04
    ## 65403 3.556691e-06
    ## 65454 2.278218e-07
    ## 65474 6.779503e-08
    ## 65536 0.000000e+00
    ## 65537 0.000000e+00
    ## 65601 5.809960e-09
    ## 65646 2.369222e-09
    ## 65713 1.913451e-14
    ## 65737 0.000000e+00
    ## 65757 3.611515e-07
    ## 65919 6.503523e-06
    ## 65934 5.347274e-04
    ## 65962 1.711207e-05
    ## 66118 6.392033e-06
    ## 66247 1.527129e-05
    ## 66251 2.116311e-03
    ## 66264 1.915490e-07
    ## 66344 3.883131e-08
    ## 66346 2.183713e-07
    ## 66387 1.323815e-05
    ## 66436 8.937882e-04
    ## 66442 3.094428e-08
    ## 66450 2.719668e-04
    ## 66503 1.114733e-07
    ## 66582 4.720633e-04
    ## 66703 4.586136e-04
    ## 66715 4.758723e-04
    ## 66762 1.046421e-05
    ## 66769 9.183904e-10
    ## 66824 1.281208e-05
    ## 66879 9.062727e-08
    ## 66910 2.306481e-06
    ## 66949 5.860111e-09
    ## 66958 4.317167e-08
    ## 67021 1.294480e-05
    ## 67034 1.302530e-06
    ## 67050 3.373582e-05
    ## 67217 2.911997e-07
    ## 67256 0.000000e+00
    ## 67259 1.213498e-06
    ## 67300 2.998067e-07
    ## 67331 6.858217e-06
    ## 67376 7.568196e-07
    ## 67409 1.671791e-07
    ## 67478 1.808335e-07
    ## 67487 4.240033e-07
    ## 67506 7.546092e-07
    ## 67581 0.000000e+00
    ## 67690 2.178120e-13
    ## 67698 9.442698e-05
    ## 67731 0.000000e+00
    ## 67753 0.000000e+00
    ## 67829 4.689616e-05
    ## 67901 1.307306e-05
    ## 67973 2.445829e-08
    ## 68017 6.065538e-09
    ## 68071 3.114499e-05
    ## 68153 1.405656e-05
    ## 68165 1.491075e-05
    ## 68166 2.669333e-11
    ## 68227 4.663688e-06
    ## 68339 0.000000e+00
    ## 68384 3.890914e-06
    ## 68415 4.253730e-10
    ## 68463 1.625824e-05
    ## 68511 1.110561e-06
    ## 68546 7.138395e-06
    ## 68598 3.372666e-09
    ## 68603 1.251580e-05
    ## 68619 8.798781e-05
    ## 68675 2.191135e-06
    ## 68678 1.852961e-03
    ## 68695 0.000000e+00
    ## 68719 2.225457e-07
    ## 68737 4.638038e-07
    ## 68788 1.105198e-06
    ## 68815 1.097352e-09
    ## 68915 1.294480e-05
    ## 68970 1.870069e-05
    ## 69077 1.657410e-03
    ## 69096 4.674793e-06
    ## 69098 2.267083e-07
    ## 69099 1.338121e-07
    ## 69138 6.360123e-07
    ## 69193 2.269688e-05
    ## 69209 1.375841e-03
    ## 69228 1.298910e-05
    ## 69242 4.271158e-06
    ## 69304 4.244259e-06
    ## 69378 3.674562e-07
    ## 69394 1.252082e-07
    ## 69464 1.849200e-10
    ## 69539 8.357421e-08
    ## 69581 1.623657e-07
    ## 69665 2.318807e-09
    ## 69680 9.629448e-07
    ## 69711 8.382428e-09
    ## 69780 3.327679e-11
    ## 69794 3.117381e-07
    ## 69800 2.612189e-08
    ## 69896 0.000000e+00
    ## 69909 2.117352e-03
    ## 70017 7.404159e-11
    ## 70032 1.921060e-07
    ## 70176 8.793758e-05
    ## 70206 0.000000e+00
    ## 70227 1.373030e-07
    ## 70248 3.586998e-07
    ## 70268 1.915107e-04
    ## 70423 6.503072e-08
    ## 70458 1.926341e-08
    ## 70610 7.800924e-06
    ## 70767 4.498133e-07
    ## 70799 1.163425e-02
    ## 70887 4.044701e-04
    ## 71035 4.349351e-05
    ## 71086 5.775749e-08
    ## 71101 6.257499e-06
    ## 71107 2.030667e-06
    ## 71119 5.742236e-02
    ## 71142 2.423490e-05
    ## 71143 3.065879e-06
    ## 71174 7.105167e-06
    ## 71353 1.283090e-06
    ## 71388 4.250884e-03
    ## 71447 2.097633e-09
    ## 71557 4.239599e-07
    ## 71655 7.278451e-06
    ## 71704 2.432276e-06
    ## 71720 2.315357e-08
    ## 71762 4.800628e-02
    ## 72052 2.596596e-05
    ## 72097 8.695979e-07
    ## 72102 8.689002e-06
    ## 72122 3.944821e-10
    ## 72148 3.296616e-07
    ## 72153 1.863567e-06
    ## 72165 9.569926e-10
    ## 72332 5.193712e-09
    ## 72514 1.312332e-07
    ## 72524 7.685213e-04
    ## 72576 2.131107e-08
    ## 72591 7.591560e-07
    ## 72613 3.483787e-07
    ## 72638 1.990351e-06
    ## 72666 1.281437e-04
    ## 72809 4.362001e-05
    ## 72855 4.094619e-08
    ## 72941 8.425447e-18
    ## 73074 1.304784e-06
    ## 73213 6.360784e-06
    ## 73274 5.190921e-09
    ## 73327 7.520481e-08
    ## 73364 0.000000e+00
    ## 73383 6.404399e-08
    ## 73764 4.640225e-11
    ## 73777 1.214458e-14
    ## 74334 4.293626e-08
    ## 74342 1.561264e-04
    ## 74347 1.325778e-05
    ## 74579 9.823024e-02
    ## 74684 5.025263e-06
    ## 74725 2.762680e-05
    ## 74727 5.567236e-09
    ## 75034 0.000000e+00
    ## 75238 4.683230e-07
    ## 75336 8.603561e-08
    ## 75341 4.170833e-04
    ## 75367 1.763842e-06
    ## 75387 6.523775e-07
    ## 75406 4.068692e-05
    ## 75409 4.417456e-06
    ## 75461 1.238760e-05
    ## 75530 2.676816e-05
    ## 75592 2.734196e-05
    ## 75718 1.156237e-06
    ## 75774 1.292940e-05
    ## 75864 2.700699e-06
    ## 75933 9.486527e-06
    ## 75954 2.545138e-07
    ## 76021 1.286351e-05
    ## 76081 5.701683e-07
    ## 76154 6.944937e-06
    ## 76320 4.166540e-09
    ## 76370 8.336352e-10
    ## 76447 1.359869e-05
    ## 76469 1.851128e-11
    ## 76516 3.378307e-05
    ## 76532 1.292323e-05
    ## 76583 1.435886e-05
    ## 76622 1.305322e-06
    ## 76629 5.796550e-04
    ## 76727 1.868874e-05
    ## 76764 2.550084e-07
    ## 76927 4.127646e-05
    ## 76959 6.690214e-08
    ## 76979 5.030573e-06
    ## 77112 7.980228e-04
    ## 77190 3.096131e-07
    ## 77294 2.203917e-05
    ## 77298 1.606481e-03
    ## 77348 0.000000e+00
    ## 77369 3.979421e-06
    ## 77651 1.096184e-06
    ## 77743 3.233474e-07
    ## 77749 1.282474e-08
    ## 77772 1.184430e-06
    ## 77791 5.713292e-06
    ## 77915 1.007444e-06
    ## 77958 1.198237e-07
    ## 78003 7.541575e-07
    ## 78051 5.214662e-09
    ## 78056 6.365338e-06
    ## 78144 1.130714e-05
    ## 78379 1.463182e-05
    ## 78406 4.375354e-08
    ## 78568 2.625980e-06
    ## 78715 5.357100e-06
    ## 78807 1.908415e-14
    ## 79289 1.340418e-09
    ## 79538 1.048661e-04
    ## 79564 5.170142e-05
    ## 79847 1.618099e-03
    ## 80730 1.230253e-03
    ## 80826 7.668211e-04
    ## 80908 1.234814e-08
    ## 81211 0.000000e+00
    ## 81831 4.663390e-06
    ## 81865 1.040955e-14
    ## 81877 1.910755e-05
    ## 81911 2.427460e-06
    ## 81959 0.000000e+00
    ## 81984 2.926315e-07
    ## 82563 6.429732e-06
    ## 82735 2.923593e-02
    ## 82997 3.010660e-04
    ## 83034 2.183917e-07
    ## 83323 3.748891e-07
    ## 83398 1.928562e-01
    ## 83475 5.115226e-07
    ## 83597 3.349921e-06
    ## 83601 1.296048e-06
    ## 84157 2.068153e-05
    ## 84267 1.122807e-05
    ## 84289 3.736463e-08
    ## 84609 8.195545e-06
    ## 84867 3.794489e-06
    ## 85060 5.442667e-05
    ## 85216 0.000000e+00
    ## 85323 1.182284e-05
    ## 85449 2.186501e-07
    ## 85599 1.067473e-06
    ## 85940 8.754865e-09
    ## 86201 2.504978e-05
    ## 86212 1.250983e-07
    ## 86422 3.273334e-05
    ## 86683 6.499478e-07
    ## 86761 1.690228e-05
    ## 86928 2.319321e-04
    ## 86985 1.623657e-07
    ## 87049 8.922448e-05
    ## 87124 1.690588e-07
    ## 87125 2.295928e-05
    ## 87309 3.884662e-04
    ## 87486 1.714301e-15
    ## 87554 1.340418e-09
    ## 87571 6.429732e-06
    ## 88033 6.813890e-08
    ## 88092 1.884513e-05
    ## 88202 1.997955e-04
    ## 88204 9.783316e-08
    ## 88508 1.470712e-07
    ## 88509 4.841704e-07
    ## 88730 3.414649e-06
    ## 88946 5.775749e-08
    ## 89023 1.678318e-07
    ## 89192 2.934753e-09
    ## 89312 5.550763e-05
    ## 89403 2.724485e-05
    ## 89539 2.915884e-04
    ## 89550 2.136585e-06
    ## 89882 9.206700e-06
    ## 90168 2.789547e-06
    ## 90201 1.325465e-05
    ## 90241 1.411538e-06
    ## 90956 1.273011e-05
    ## 91048 7.279992e-08
    ## 91064 2.622393e-04
    ## 91159 7.887027e-08
    ## 91190 5.535801e-06
    ## 91210 3.107546e-08
    ## 91233 1.908947e-05
    ## 91352 2.565415e-07
    ## 91423 2.117994e-09
    ## 91426 3.031114e-05
    ## 91747 1.174610e-09
    ## 91833 0.000000e+00
    ## 91867 5.773024e-05
    ## 91908 3.884662e-04
    ## 91956 0.000000e+00
    ## 92108 7.576580e-08
    ## 92143 6.495193e-05
    ## 92191 1.408723e-08
    ## 92231 2.797413e-10
    ## 92238 6.776899e-07
    ## 92443 3.960645e-06
    ## 92476 2.397912e-07
    ## 92487 1.640241e-05
    ## 92537 1.395741e-05
    ## 92612 2.093226e-05
    ## 92688 2.396002e-02
    ## 92715 1.970562e-05
    ## 92733 1.294257e-05
    ## 92836 0.000000e+00
    ## 92890 5.013987e-07
    ## 92902 0.000000e+00
    ## 93072 6.560215e-08
    ## 93178 2.397326e-02
    ## 93403 1.694714e-15
    ## 93432 4.863930e-02
    ## 93486 2.180612e-07
    ## 93553 2.289560e-06
    ## 93626 1.758688e-05
    ## 93636 8.213951e-06
    ## 93653 1.024762e-08
    ## 93663 2.901863e-07
    ## 93677 8.724826e-18
    ## 93682 6.466948e-07
    ## 93715 1.128838e-06
    ## 93725 7.467082e-12
    ## 93732 1.432407e-04
    ## 93859 2.008624e-03
    ## 93896 1.000000e+00
    ## 93909 1.875987e-05
    ## 93968 5.818402e-05
    ## 94011 3.753527e-07
    ## 94035 1.834340e-07
    ## 94070 2.874748e-08
    ## 94161 5.838963e-04
    ## 94211 3.770216e-06
    ## 94238 2.328821e-06
    ## 94251 5.343255e-10
    ## 94260 1.863848e-08
    ## 94301 3.987552e-04
    ## 94340 5.655548e-08
    ## 94341 5.773599e-04
    ## 94390 1.059348e-06
    ## 94433 3.498803e-08
    ## 94466 1.349030e-06
    ## 94543 2.477774e-05
    ## 94620 1.965126e-05
    ## 94658 2.084877e-07
    ## 94737 2.001604e-07
    ## 94774 8.630825e-08
    ## 94782 1.135362e-04
    ## 94925 6.593120e-18
    ## 94939 1.323720e-05
    ## 94954 6.560215e-08
    ## 94956 3.679280e-05
    ## 94989 4.715829e-06
    ## 94994 6.565919e-06
    ## 95010 5.486758e-10
    ## 95065 6.869891e-08
    ## 95160 1.411941e-05
    ## 95210 1.027110e-07
    ## 95219 2.436307e-08
    ## 95225 1.321751e-06
    ## 95299 2.879848e-03
    ## 95446 4.622077e-10
    ## 95459 4.583712e-06
    ## 95508 1.325778e-05
    ## 95561 1.023023e-03
    ## 95721 1.118965e-09
    ## 95782 8.377679e-06
    ## 95799 2.397266e-02
    ## 95814 1.357398e-03
    ## 95839 1.292323e-05
    ## 95860 1.097752e-07
    ## 95863 1.490499e-05
    ## 95868 3.456890e-06
    ## 95891 5.877889e-04
    ## 95900 5.757820e-04
    ## 95970 1.134639e-05
    ## 96068 8.654705e-06
    ## 96125 6.502973e-07
    ## 96172 1.107262e-04
    ## 96234 6.806779e-06
    ## 96267 9.695831e-06
    ## 96304 6.230343e-08
    ## 96371 2.264549e-05
    ## 96439 2.384658e-05
    ## 96500 1.304206e-07
    ## 96532 1.501100e-05
    ## 96556 2.312656e-05
    ## 96568 2.676993e-07
    ## 96794 8.591354e-07
    ## 96877 7.663995e-07
    ## 96898 0.000000e+00
    ## 97024 8.754890e-07
    ## 97083 1.550404e-03
    ## 97187 1.630681e-09
    ## 97287 3.479217e-06
    ## 97312 1.915134e-09
    ## 97404 6.244515e-08
    ## 97431 1.532882e-06
    ## 97436 3.369181e-07
    ## 97456 5.272657e-05
    ## 97460 1.174335e-06
    ## 97473 3.390353e-08
    ## 97481 2.317772e-09
    ## 97520 3.691497e-12
    ## 97553 1.575846e-05
    ## 97560 1.892220e-11
    ## 97588 2.934753e-09
    ## 97615 1.391473e-08
    ## 97649 3.703910e-11
    ## 97650 7.865240e-04
    ## 97657 1.168010e-04
    ## 97692 1.211174e-01
    ## 97705 3.093432e-05
    ## 97741 5.546695e-08
    ## 97745 9.331322e-06
    ## 97755 5.183494e-06
    ## 97818 9.873104e-07
    ## 97884 3.794489e-06
    ## 97889 1.052128e-06
    ## 97955 2.377205e-07
    ## 97957 1.012133e-07
    ## 98014 2.400437e-07
    ## 98045 7.834925e-04
    ## 98098 5.326035e-08
    ## 98114 7.207443e-02
    ## 98245 1.198879e-07
    ## 98253 1.577356e-03
    ## 98297 9.595599e-01
    ## 98394 1.790606e-07
    ## 98469 7.591754e-06
    ## 98489 5.594517e-06
    ## 98546 1.151984e-04
    ## 98558 2.654473e-05
    ## 98582 7.964247e-04
    ## 98700 2.669665e-11
    ## 98705 5.126814e-06
    ## 98804 1.239345e-04
    ## 98826 2.032958e-05
    ## 98834 2.411888e-02
    ## 98840 2.832777e-05
    ## 98852 4.751026e-04
    ## 98891 1.357414e-05
    ## 98943 1.096594e-09
    ## 98995 4.926809e-06
    ## 99003 1.063690e-06
    ## 99014 3.233474e-07
    ## 99047 0.000000e+00
    ## 99106 7.107453e-07
    ## 99191 1.842111e-08
    ## 99207 2.400053e-05
    ## 99217 8.897702e-07
    ## 99218 3.478761e-10
    ## 99224 4.218503e-05
    ## 99240 3.686830e-05
    ## 99316 9.115174e-06
    ## 99424 2.132128e-13
    ## 99457 3.711865e-05
    ## 99465 5.423982e-07
    ## 99484 4.380801e-06
    ## 99518 1.751788e-03
    ## 99520 1.389039e-08
    ## 99535 2.033425e-07
    ## 99540 4.553748e-09
    ## 99662 9.053226e-09
    ## 99725 3.036895e-08
    ## 99845 4.123615e-06
    ## 99850 2.814627e-07
    ## 99876 3.932764e-05
    ## 99879 2.645902e-05
    ## 99929 6.522619e-06
    ## 99930 2.105924e-07

``` r
applications_centralities <- merge(x=applications, y=centralities, by='examiner_id', all.x=TRUE)

applications_centralities = applications_centralities %>% filter(workgroup==162 | workgroup==179)

applications_centralities <- drop_na(applications_centralities)
```

## Demographic Representations

``` r
# Gender
gender_graph <- ggplot(data=applications_centralities, aes(x=gender)) +
  geom_bar(aes(y = (..count..)/sum(..count..)) )  +
  ylab("Proportion")+
  xlab("Gender")+
  ylim(0,1)+
  ggtitle(paste0("Gender Distribution for Workgroups 162 & 179"))
grid.arrange(gender_graph,ncol=2, widths=c(1,1))
```

![](Exercice-4_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# Race
race_graph <- ggplot(data=applications_centralities, aes(x=race)) +
  geom_bar(aes(y = (..count..)/sum(..count..)) )  +
  ylab("Proportion")+
  xlab("Race")+
  ylim(0,1)+
  ggtitle(paste0("Race Distribution for Workgroups 162 & 179"))
grid.arrange(race_graph,ncol=2, widths=c(1,1))
```

![](Exercice-4_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

## Linear Regression model

``` r
applications_centralities$examiner_art_unit = as.factor(applications_centralities$examiner_art_unit)
applications_centralities$tc = as.factor(applications_centralities$tc)
applications_centralities$gender = as.factor(applications_centralities$gender)
applications_centralities$race = as.factor(applications_centralities$race)

lm = lm(app_proc_time~tc+gender+race+tenure_days+Degree+Closeness+Betweenness, data=applications_centralities)
stargazer(lm, type='text', dep.var.labels=c('Application Process Time'), digits=2)
```

    ## 
    ## =================================================
    ##                          Dependent variable:     
    ##                     -----------------------------
    ##                       Application Process Time   
    ## -------------------------------------------------
    ## tc1700                        165.76***          
    ##                                (3.64)            
    ##                                                  
    ## gendermale                    35.59***           
    ##                                (2.26)            
    ##                                                  
    ## raceblack                     81.76***           
    ##                                (7.95)            
    ##                                                  
    ## raceHispanic                 -351.76***          
    ##                                (8.24)            
    ##                                                  
    ## racewhite                    -153.39***          
    ##                                (2.62)            
    ##                                                  
    ## tenure_days                    0.93***           
    ##                                (0.002)           
    ##                                                  
    ## Degree                         0.48***           
    ##                                (0.06)            
    ##                                                  
    ## Closeness                     -55.77***          
    ##                                (2.91)            
    ##                                                  
    ## Betweenness                    0.20***           
    ##                                (0.02)            
    ##                                                  
    ## Constant                      195.92***          
    ##                                (15.39)           
    ##                                                  
    ## -------------------------------------------------
    ## Observations                   306,946           
    ## R2                              0.39             
    ## Adjusted R2                     0.39             
    ## Residual Std. Error     544.42 (df = 306936)     
    ## F Statistic         21,731.04*** (df = 9; 306936)
    ## =================================================
    ## Note:                 *p<0.1; **p<0.05; ***p<0.01

``` r
lm2 = lm(app_proc_time~tc+gender+race+tenure_days+gender*Degree+gender*Closeness+gender*Betweenness+race*Degree+race*Closeness+race*Betweenness, data=applications_centralities)
stargazer(lm2, type='text', dep.var.labels=c('Application Process Time'), digits=2)
```

    ## 
    ## ======================================================
    ##                               Dependent variable:     
    ##                          -----------------------------
    ##                            Application Process Time   
    ## ------------------------------------------------------
    ## tc1700                             155.65***          
    ##                                     (3.72)            
    ##                                                       
    ## gendermale                           5.99             
    ##                                     (4.13)            
    ##                                                       
    ## raceblack                            43.60            
    ##                                     (29.65)           
    ##                                                       
    ## raceHispanic                      -880.40***          
    ##                                     (18.49)           
    ##                                                       
    ## racewhite                          -59.86***          
    ##                                     (4.92)            
    ##                                                       
    ## tenure_days                         0.93***           
    ##                                     (0.002)           
    ##                                                       
    ## Degree                              1.24***           
    ##                                     (0.18)            
    ##                                                       
    ## Closeness                           19.52**           
    ##                                     (7.70)            
    ##                                                       
    ## Betweenness                         2.09***           
    ##                                     (0.23)            
    ##                                                       
    ## gendermale:Degree                    -0.18            
    ##                                     (0.13)            
    ##                                                       
    ## gendermale:Closeness               72.57***           
    ##                                     (6.12)            
    ##                                                       
    ## gendermale:Betweenness              0.12**            
    ##                                     (0.05)            
    ##                                                       
    ## raceblack:Degree                    7.79**            
    ##                                     (3.50)            
    ##                                                       
    ## raceHispanic:Degree                 8.43***           
    ##                                     (0.38)            
    ##                                                       
    ## racewhite:Degree                   -0.90***           
    ##                                     (0.15)            
    ##                                                       
    ## raceblack:Closeness                 -39.77            
    ##                                     (27.11)           
    ##                                                       
    ## raceHispanic:Closeness            1,021.69***         
    ##                                     (30.55)           
    ##                                                       
    ## racewhite:Closeness               -167.31***          
    ##                                     (7.28)            
    ##                                                       
    ## raceblack:Betweenness                9.96*            
    ##                                     (5.27)            
    ##                                                       
    ## raceHispanic:Betweenness                              
    ##                                                       
    ##                                                       
    ## racewhite:Betweenness              -2.01***           
    ##                                     (0.23)            
    ##                                                       
    ## Constant                           156.52***          
    ##                                     (15.82)           
    ##                                                       
    ## ------------------------------------------------------
    ## Observations                        306,946           
    ## R2                                   0.39             
    ## Adjusted R2                          0.39             
    ## Residual Std. Error          542.42 (df = 306925)     
    ## F Statistic              9,965.49*** (df = 20; 306925)
    ## ======================================================
    ## Note:                      *p<0.1; **p<0.05; ***p<0.01
