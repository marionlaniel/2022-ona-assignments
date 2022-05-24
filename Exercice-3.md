Exercice 3
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
    ## Ncells  4156561 222.0    7281582 388.9         NA  4176104 223.1
    ## Vcells 49053569 374.3   94875321 723.9      16384 79369113 605.6

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
    ## Ncells  4578293 244.6    7281582 388.9         NA  7281582 388.9
    ## Vcells 52860809 403.3   94875321 723.9      16384 94005191 717.3

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
    ## Ncells  4666151 249.2   13124697  701.0         NA  13124697  701.0
    ## Vcells 65344622 498.6  136796462 1043.7      16384 136398269 1040.7

## Select Workgroups 162 & 179

``` r
sample_162 <-applications[substr(applications$examiner_art_unit, 1,3)==162,]
summary(sample_162)
```

    ##  application_number  filing_date         examiner_name_last examiner_name_first
    ##  Length:141390      Min.   :2000-01-03   Length:141390      Length:141390      
    ##  Class :character   1st Qu.:2005-01-18   Class :character   Class :character   
    ##  Mode  :character   Median :2008-11-25   Mode  :character   Mode  :character   
    ##                     Mean   :2008-10-24                                         
    ##                     3rd Qu.:2012-08-23                                         
    ##                     Max.   :2017-05-09                                         
    ##                                                                                
    ##  examiner_name_middle  examiner_id    examiner_art_unit  uspc_class       
    ##  Length:141390        Min.   :59440   Min.   :1620      Length:141390     
    ##  Class :character     1st Qu.:65768   1st Qu.:1624      Class :character  
    ##  Mode  :character     Median :73364   Median :1625      Mode  :character  
    ##                       Mean   :78439   Mean   :1625                        
    ##                       3rd Qu.:93677   3rd Qu.:1626                        
    ##                       Max.   :99990   Max.   :1629                        
    ##                       NA's   :682                                         
    ##  uspc_subclass      patent_number      patent_issue_date   
    ##  Length:141390      Length:141390      Min.   :2000-08-08  
    ##  Class :character   Class :character   1st Qu.:2006-11-07  
    ##  Mode  :character   Mode  :character   Median :2011-04-19  
    ##                                        Mean   :2010-06-28  
    ##                                        3rd Qu.:2014-02-18  
    ##                                        Max.   :2017-06-20  
    ##                                        NA's   :57816       
    ##   abandon_date        disposal_type      appl_status_code appl_status_date  
    ##  Min.   :2000-06-05   Length:141390      Min.   :  1.0    Length:141390     
    ##  1st Qu.:2009-02-18   Class :character   1st Qu.:150.0    Class :character  
    ##  Median :2011-06-27   Mode  :character   Median :150.0    Mode  :character  
    ##  Mean   :2011-01-30                      Mean   :161.3                      
    ##  3rd Qu.:2013-09-09                      3rd Qu.:161.0                      
    ##  Max.   :2017-06-05                      Max.   :454.0                      
    ##  NA's   :97057                           NA's   :262                        
    ##        tc          gender              race           earliest_date       
    ##  Min.   :1600   Length:141390      Length:141390      Min.   :2000-01-03  
    ##  1st Qu.:1600   Class :character   Class :character   1st Qu.:2000-01-07  
    ##  Median :1600   Mode  :character   Mode  :character   Median :2000-02-22  
    ##  Mean   :1600                                         Mean   :2001-06-10  
    ##  3rd Qu.:1600                                         3rd Qu.:2002-10-22  
    ##  Max.   :1600                                         Max.   :2012-07-25  
    ##                                                       NA's   :4389        
    ##   latest_date          tenure_days  
    ##  Min.   :2001-09-23   Min.   : 614  
    ##  1st Qu.:2017-05-19   1st Qu.:5282  
    ##  Median :2017-05-22   Median :6262  
    ##  Mean   :2017-05-03   Mean   :5806  
    ##  3rd Qu.:2017-05-23   3rd Qu.:6340  
    ##  Max.   :2017-11-08   Max.   :6518  
    ##  NA's   :4389         NA's   :4389

``` r
sample_179 <-applications[substr(applications$examiner_art_unit, 1,3)==179,]
summary(sample_179)
```

    ##  application_number  filing_date         examiner_name_last examiner_name_first
    ##  Length:133424      Min.   :2000-01-07   Length:133424      Length:133424      
    ##  Class :character   1st Qu.:2005-03-28   Class :character   Class :character   
    ##  Mode  :character   Median :2006-06-08   Mode  :character   Mode  :character   
    ##                     Mean   :2007-05-22                                         
    ##                     3rd Qu.:2008-02-05                                         
    ##                     Max.   :2017-04-26                                         
    ##                                                                                
    ##  examiner_name_middle  examiner_id    examiner_art_unit  uspc_class       
    ##  Length:133424        Min.   :59012   Min.   :1791      Length:133424     
    ##  Class :character     1st Qu.:65530   1st Qu.:1793      Class :character  
    ##  Mode  :character     Median :73692   Median :1795      Mode  :character  
    ##                       Mean   :77558   Mean   :1794                        
    ##                       3rd Qu.:92733   3rd Qu.:1796                        
    ##                       Max.   :99987   Max.   :1799                        
    ##                       NA's   :568                                         
    ##  uspc_subclass      patent_number      patent_issue_date   
    ##  Length:133424      Length:133424      Min.   :2002-11-12  
    ##  Class :character   Class :character   1st Qu.:2009-02-17  
    ##  Mode  :character   Mode  :character   Median :2010-01-05  
    ##                                        Mean   :2010-06-02  
    ##                                        3rd Qu.:2010-08-31  
    ##                                        Max.   :2017-06-20  
    ##                                        NA's   :69579       
    ##   abandon_date        disposal_type      appl_status_code appl_status_date  
    ##  Min.   :2001-05-12   Length:133424      Min.   :  1.0    Length:133424     
    ##  1st Qu.:2008-07-18   Class :character   1st Qu.:150.0    Class :character  
    ##  Median :2009-04-24   Mode  :character   Median :161.0    Mode  :character  
    ##  Mean   :2009-11-04                      Mean   :155.6                      
    ##  3rd Qu.:2009-12-29                      3rd Qu.:161.0                      
    ##  Max.   :2017-06-05                      Max.   :454.0                      
    ##  NA's   :74525                           NA's   :112                        
    ##        tc          gender              race           earliest_date       
    ##  Min.   :1700   Length:133424      Length:133424      Min.   :2000-01-03  
    ##  1st Qu.:1700   Class :character   Class :character   1st Qu.:2000-01-06  
    ##  Median :1700   Mode  :character   Mode  :character   Median :2000-02-07  
    ##  Mean   :1700                                         Mean   :2001-09-08  
    ##  3rd Qu.:1700                                         3rd Qu.:2003-06-11  
    ##  Max.   :1700                                         Max.   :2012-05-10  
    ##                                                       NA's   :1058        
    ##   latest_date          tenure_days  
    ##  Min.   :2007-11-13   Min.   : 774  
    ##  1st Qu.:2017-05-19   1st Qu.:5080  
    ##  Median :2017-05-22   Median :6304  
    ##  Mean   :2017-04-28   Mean   :5712  
    ##  3rd Qu.:2017-05-23   3rd Qu.:6342  
    ##  Max.   :2017-07-24   Max.   :6391  
    ##  NA's   :1058         NA's   :1058

## Demographic Representations

``` r
# Gender
gg_162 <- ggplot(data=sample_162, aes(x=gender)) +
  geom_bar(aes(y = (..count..)/sum(..count..)) )  +
  ylab("Proportion")+
  xlab("Gender")+
  ylim(0,1)+
  ggtitle(paste0("Gender Distribution for Workgroup 162"))

gg_179 <- ggplot(data=sample_179, aes(x=gender)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  ylab("Proportion")+
  xlab("Gender")+
  ylim(0,1)+
  ggtitle(paste0("Gender Distribution for Workgroup 179"))
grid.arrange(gg_162,gg_179,ncol=2, widths=c(1,1))
```

![](Exercice-3_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Race
gr_162 <- ggplot(data=sample_162, aes(x=race)) +
  geom_bar(aes(y = (..count..)/sum(..count..)) )  +
  ylab("Proportion")+
  xlab("Race")+
  ylim(0,1)+
  ggtitle(paste0("Race Distribution for Workgroup 162"))

gr_179 <- ggplot(data=sample_179, aes(x=race)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  ylab("Proportion")+
  xlab("Race")+
  ylim(0,1)+
  ggtitle(paste0("Race Distribution for Workgroup 179"))
grid.arrange(gr_162,gr_179,ncol=2, widths=c(1,1))
```

![](Exercice-3_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
# Tenure
gt_162 <- ggplot(data=sample_162, aes(round(tenure_days/365,digits=0))) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) + 
          scale_y_continuous(labels=scales::percent) +
          ylab("Relative Frequencies") +
          xlab("Tenure (years)") +
          ylim(0,0.5) +
          ggtitle("Tenure Distribution for Workgroup 162")
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
gt_179 <- ggplot(data=sample_179, aes(round(tenure_days/365,digits=0))) + 
          geom_bar(aes(y = (..count..)/sum(..count..))) + 
          scale_y_continuous(labels=scales::percent) +
          ylab("Relative Frequencies") +
          xlab("Tenure (years)") +
          ylim(0,0.5) +
          ggtitle("Tenure Distribution for Workgroup 179")
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

``` r
grid.arrange(gt_162,gt_179,ncol=2, widths=c(1,1))
```

    ## Warning: Removed 4389 rows containing non-finite values (stat_count).

    ## Warning: Removed 1 rows containing missing values (geom_bar).

    ## Warning: Removed 1058 rows containing non-finite values (stat_count).

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](Exercice-3_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Advice Networks and Centrality Scores

``` r
examiner_art_id = distinct(subset(applications, select=c(examiner_art_unit, examiner_id)))
examiner_art_id$workgroup = substr(examiner_art_id$examiner_art_unit, 1,3)
examiner_art_id = examiner_art_id[examiner_art_id$workgroup==163 | examiner_art_id$workgroup==176,]

df = merge(x=edges, y=examiner_art_id, by.x="ego_examiner_id", by.y="examiner_id", all.x=TRUE)
df = df %>% rename(ego_art_unit=examiner_art_unit, ego_workgroup=workgroup)
df = drop_na(df)

df = merge(x=df, y=examiner_art_id, by.x="alter_examiner_id", by.y="examiner_id", all.x=TRUE)
df = df %>% rename(alter_art_unit=examiner_art_unit, alter_workgroup=workgroup)
df = drop_na(df)

egoNodes = subset(df, select=c(ego_examiner_id,ego_art_unit, ego_workgroup)) %>% rename(examiner_id=ego_examiner_id,art_unit=ego_art_unit,workgroup=ego_workgroup)
alterNodes = subset(df, select=c(alter_examiner_id,alter_art_unit, alter_workgroup))%>% rename(examiner_id=alter_examiner_id,art_unit=alter_art_unit,workgroup=alter_workgroup)
nodes = rbind(egoNodes, alterNodes)
nodes = distinct(nodes)

nodes = nodes %>% group_by(examiner_id) %>% summarise(examiner_id=first(examiner_id), art_unit=first(art_unit), workgroup=first(workgroup))

advNet = graph_from_data_frame(d=df, vertices=nodes, directed=TRUE)

Degree <- degree(advNet, v=V(advNet))
Betweenness <- betweenness(advNet)
Eigenvector <- evcent(advNet)$vector

V(advNet)$size = Degree
V(advNet)$eig = round(Eigenvector,2)
V(advNet)$bet = round(Betweenness,2)

V(advNet)$color = nodes$art_unit

centralities <- cbind(Degree, Eigenvector, Betweenness)
centralities = round(centralities,2)
centralities = data.frame(centralities)

ggraph(advNet, layout="kk") +
  geom_edge_link()+
  geom_node_point(aes(size=size, color=color), show.legend=T)
```

![](Exercice-3_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# Same graph but with ids
ggraph(advNet, layout="kk") +
       geom_edge_link()+
       geom_node_point(aes(size=size, color=color), show.legend=T) +
       geom_node_text(aes(label = name))
```

![](Exercice-3_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

## Discussion

By looking at the graph above, we notice that examiners generally seek
advice from people within their own work group, except for one instance
in work group 179 where examiner 64059 is seeking advice from the other
work group. There also seems to be separate networks with a mix of
examiners coming from both work groups.

``` r
examiner1 <- applications %>% 
  filter(examiner_id==64059)
examiner1
```

    ## # A tibble: 641 × 21
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 10543663           2006-03-03  FINK               BRIEANN            
    ##  2 10548354           2006-11-17  FINK               BRIEANN            
    ##  3 10557173           2005-11-17  FINK               BRIEANN            
    ##  4 10561974           2005-12-22  FINK               BRIEANN            
    ##  5 10563041           2006-01-03  FINK               BRIEANN            
    ##  6 10566424           2007-05-29  FINK               BRIEANN            
    ##  7 10577931           2006-05-02  FINK               BRIEANN            
    ##  8 10578731           2006-05-10  FINK               BRIEANN            
    ##  9 10582628           2007-04-04  FINK               BRIEANN            
    ## 10 10583506           2006-06-16  FINK               BRIEANN            
    ## # … with 631 more rows, and 17 more variables: examiner_name_middle <chr>,
    ## #   examiner_id <dbl>, examiner_art_unit <dbl>, uspc_class <chr>,
    ## #   uspc_subclass <chr>, patent_number <chr>, patent_issue_date <date>,
    ## #   abandon_date <date>, disposal_type <chr>, appl_status_code <dbl>,
    ## #   appl_status_date <chr>, tc <dbl>, gender <chr>, race <chr>,
    ## #   earliest_date <date>, latest_date <date>, tenure_days <dbl>

We can notice that this examiner has been working for the UPSTO for more
than 14 years. Therefore, she could have developed strong links with
people from different work groups.

From looking at the graph, we can also notice that examiners 72253 and
84157 have the highest degree centrality. Let’s look at those examiners.

``` r
examiner2 <- applications %>% 
  filter(examiner_id==72253)
examiner2
```

    ## # A tibble: 399 × 21
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 09242244           2000-02-29  WOITACH            JOSEPH             
    ##  2 09402130           2000-02-02  WOITACH            JOSEPH             
    ##  3 09402527           2000-01-03  WOITACH            JOSEPH             
    ##  4 09403707           2000-03-17  WOITACH            JOSEPH             
    ##  5 09423935           2000-03-13  WOITACH            JOSEPH             
    ##  6 09446717           2000-04-13  WOITACH            JOSEPH             
    ##  7 09463276           2000-05-12  WOITACH            JOSEPH             
    ##  8 09479195           2000-01-07  WOITACH            JOSEPH             
    ##  9 09484629           2000-01-18  WOITACH            JOSEPH             
    ## 10 09484848           2000-01-18  WOITACH            JOSEPH             
    ## # … with 389 more rows, and 17 more variables: examiner_name_middle <chr>,
    ## #   examiner_id <dbl>, examiner_art_unit <dbl>, uspc_class <chr>,
    ## #   uspc_subclass <chr>, patent_number <chr>, patent_issue_date <date>,
    ## #   abandon_date <date>, disposal_type <chr>, appl_status_code <dbl>,
    ## #   appl_status_date <chr>, tc <dbl>, gender <chr>, race <chr>,
    ## #   earliest_date <date>, latest_date <date>, tenure_days <dbl>

``` r
examiner3 <- applications %>% 
  filter(examiner_id==84157)
examiner3
```

    ## # A tibble: 1,188 × 21
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 09424416           2000-02-15  CHOI               LING SIU           
    ##  2 09462095           2000-01-12  CHOI               LING SIU           
    ##  3 09462103           2000-01-13  CHOI               LING SIU           
    ##  4 09463436           2000-01-27  CHOI               LING SIU           
    ##  5 09463999           2000-02-22  CHOI               LING SIU           
    ##  6 09478197           2000-01-05  CHOI               LING SIU           
    ##  7 09479424           2000-01-07  CHOI               LING SIU           
    ##  8 09481047           2000-01-11  CHOI               LING SIU           
    ##  9 09485195           2000-05-04  CHOI               LING SIU           
    ## 10 09485618           2000-02-15  CHOI               LING SIU           
    ## # … with 1,178 more rows, and 17 more variables: examiner_name_middle <chr>,
    ## #   examiner_id <dbl>, examiner_art_unit <dbl>, uspc_class <chr>,
    ## #   uspc_subclass <chr>, patent_number <chr>, patent_issue_date <date>,
    ## #   abandon_date <date>, disposal_type <chr>, appl_status_code <dbl>,
    ## #   appl_status_date <chr>, tc <dbl>, gender <chr>, race <chr>,
    ## #   earliest_date <date>, latest_date <date>, tenure_days <dbl>

Both examiners have worked for the UPSTO for more than 17 years.
Therefore, they are among the examiners with the longest tenure at the
organization. We can deduct from this analysis that employees with the
longest tenure are the one with the highest degree centrality meaning
that they have more connections than other examiners. This explanation
makes sense since working at a place longer means that you connect with
more people.
