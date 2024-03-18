# Pertussis_miniproject

Q1.

``` r
library(datapasta)
library(rvest)
library(ggplot2)
```

``` r
url <- "https://www.cdc.gov/pertussis/surv-reporting/cases-by-year.html"
webpage <- read_html(url)
rows <- html_nodes(webpage, "table")[[1]] %>% html_nodes("tr")
table_data <- lapply(rows, function(row) {
  th_data <- html_nodes(row, "th") %>% html_text()
  td_data <- html_nodes(row, "td") %>% html_text()
  c(th_data, td_data)
})
cdc <- as.data.frame(do.call(rbind, table_data), stringsAsFactors = FALSE)
colnames(cdc) <- cdc[1,]
df_paste(cdc)
```

    data.frame(
                    stringsAsFactors = FALSE,
                         check.names = FALSE,
                                Year = c("Year","1922","1923","1924","1925","1926",
                                         "1927","1928","1929","1930","1931",
                                         "1932","1933","1934","1935","1936",
                                         "1937","1938","1939","1940","1941","1942",
                                         "1943","1944","1945","1946","1947",
                                         "1948","1949","1950","1951","1952",
                                         "1953","1954","1955","1956","1957","1958",
                                         "1959","1960","1961","1962","1963",
                                         "1964","1965","1966","1967","1968","1969",
                                         "1970","1971","1972","1973","1974",
                                         "1975","1976","1977","1978","1979",
                                         "1980","1981","1982","1983","1984","1985",
                                         "1986","1987","1988","1989","1990",
                                         "1991","1992","1993","1994","1995",
                                         "1996","1997","1998","1999","2000","2001",
                                         "2002","2003","2004","2005","2006",
                                         "2007","2008","2009","2010","2011","2012",
                                         "2013","2014","2015","2016","2017",
                                         "2018","2019","2020","2021"),
      `No. Reported Pertussis Cases` = c("No. Reported Pertussis Cases","107,473",
                                         "164,191","165,418","152,003","202,210",
                                         "181,411","161,799","197,371","166,914",
                                         "172,559","215,343","179,135","265,269",
                                         "180,518","147,237","214,652","227,319",
                                         "103,188","183,866","222,202","191,383",
                                         "191,890","109,873","133,792",
                                         "109,860","156,517","74,715","69,479","120,718",
                                         "68,687","45,030","37,129","60,886",
                                         "62,786","31,732","28,295","32,148",
                                         "40,005","14,809","11,468","17,749",
                                         "17,135","13,005","6,799","7,717","9,718",
                                         "4,810","3,285","4,249","3,036","3,287",
                                         "1,759","2,402","1,738","1,010","2,177",
                                         "2,063","1,623","1,730","1,248",
                                         "1,895","2,463","2,276","3,589","4,195",
                                         "2,823","3,450","4,157","4,570","2,719",
                                         "4,083","6,586","4,617","5,137","7,796",
                                         "6,564","7,405","7,298","7,867","7,580",
                                         "9,771","11,647","25,827","25,616",
                                         "15,632","10,454","13,278","16,858",
                                         "27,550","18,719","48,277","28,639","32,971",
                                         "20,762","17,972","18,975","15,609",
                                         "18,617","6,124","2,116")
    )

``` r
cdc$`No. Reported Pertussis Cases` <- as.numeric(gsub(",", "", cdc$`No. Reported Pertussis Cases`))
```

    Warning: NAs introduced by coercion

``` r
cdc$Year <- as.numeric(cdc$Year)
```

    Warning: NAs introduced by coercion

``` r
ggplot(cdc, aes(x = Year, y = `No. Reported Pertussis Cases`)) + geom_point() + geom_line() +
  labs(y = "Number of cases", x = "Year")
```

    Warning: Removed 1 row containing missing values or values outside the scale range
    (`geom_point()`).

    Warning: Removed 1 row containing missing values or values outside the scale range
    (`geom_line()`).

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-4-1.png)

Q2.

``` r
ggplot(cdc, aes(x = Year, y = `No. Reported Pertussis Cases`)) + geom_point() + geom_line() +
  labs(y = "Number of cases", x = "Year") + geom_vline(xintercept = 1946) + 
  geom_vline(xintercept = 1996) 
```

    Warning: Removed 1 row containing missing values or values outside the scale range
    (`geom_point()`).

    Warning: Removed 1 row containing missing values or values outside the scale range
    (`geom_line()`).

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-5-1.png)

Q3.

After the wp vaccine pertussis rates declined, however, after the ap
vaccine in 1996, we see a resurganse of pertussis cases. This could be
due to a decrease in vaccination rates, or an increased sensitivity of
testing.

``` r
library(jsonlite)
```

``` r
subject <- read_json("https://www.cmi-pb.org/api/subject", simplifyVector = TRUE) 
head(subject, 3)
```

      subject_id infancy_vac biological_sex              ethnicity  race
    1          1          wP         Female Not Hispanic or Latino White
    2          2          wP         Female Not Hispanic or Latino White
    3          3          wP         Female                Unknown White
      year_of_birth date_of_boost      dataset
    1    1986-01-01    2016-09-12 2020_dataset
    2    1968-01-01    2019-01-28 2020_dataset
    3    1983-01-01    2016-10-10 2020_dataset

Q4. aP = 60, wP = 58

``` r
table(subject$infancy_vac)
```


    aP wP 
    60 58 

Q5. Female = 79, Male = 39

``` r
table(subject$biological_sex)
```


    Female   Male 
        79     39 

Q6.

``` r
table(subject$race, subject$biological_sex)
```

                                               
                                                Female Male
      American Indian/Alaska Native                  0    1
      Asian                                         21   11
      Black or African American                      2    0
      More Than One Race                             9    2
      Native Hawaiian or Other Pacific Islander      1    1
      Unknown or Not Reported                       11    4
      White                                         35   20

``` r
library(lubridate)
```


    Attaching package: 'lubridate'

    The following objects are masked from 'package:base':

        date, intersect, setdiff, union

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
today()
```

    [1] "2024-03-18"

``` r
today() - ymd("2000-01-01")
```

    Time difference of 8843 days

``` r
time_length( today() - ymd("2000-01-01"),  "years")
```

    [1] 24.21081

Q7.

``` r
subject$age <- today() - ymd(subject$year_of_birth)
ap <- subject %>% filter(infancy_vac == "aP")
round( summary( time_length( ap$age, "years" )))
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
         21      26      26      26      27      30 

``` r
wp <- subject %>% filter(infancy_vac == "wP")
round( summary( time_length( wp$age, "years" )))
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
         28      31      36      37      39      56 

Q8.

``` r
int <- ymd(subject$date_of_boost) - ymd(subject$year_of_birth)
age_at_boost <- time_length(int, "year")
head(age_at_boost)
```

    [1] 30.69678 51.07461 33.77413 28.65982 25.65914 28.77481

Q9.

``` r
ggplot(subject) +
  aes(time_length(age, "year"),
      fill=as.factor(infancy_vac)) +
  geom_histogram(show.legend=FALSE) +
  facet_wrap(vars(infancy_vac), nrow=2) +
  xlab("Age in years")
```

    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-15-1.png)

``` r
specimen <- read_json("https://www.cmi-pb.org/api/specimen", simplifyVector = TRUE) 
titer <- read_json("https://www.cmi-pb.org/api/plasma_ab_titer", simplifyVector = TRUE) 
```

Q9.

``` r
meta <- inner_join(specimen, subject)
```

    Joining with `by = join_by(subject_id)`

``` r
dim(meta)
```

    [1] 939  14

``` r
head(meta)
```

      specimen_id subject_id actual_day_relative_to_boost
    1           1          1                           -3
    2           2          1                            1
    3           3          1                            3
    4           4          1                            7
    5           5          1                           11
    6           6          1                           32
      planned_day_relative_to_boost specimen_type visit infancy_vac biological_sex
    1                             0         Blood     1          wP         Female
    2                             1         Blood     2          wP         Female
    3                             3         Blood     3          wP         Female
    4                             7         Blood     4          wP         Female
    5                            14         Blood     5          wP         Female
    6                            30         Blood     6          wP         Female
                   ethnicity  race year_of_birth date_of_boost      dataset
    1 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    2 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    3 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    4 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    5 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    6 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
             age
    1 13956 days
    2 13956 days
    3 13956 days
    4 13956 days
    5 13956 days
    6 13956 days

Q10.

``` r
abdata <- inner_join(titer, meta)
```

    Joining with `by = join_by(specimen_id)`

``` r
dim(abdata)
```

    [1] 46906    21

Q11.

``` r
table(abdata$isotype)
```


     IgE  IgG IgG1 IgG2 IgG3 IgG4 
    6698 4255 8983 8990 8990 8990 

Q12.

``` r
table(abdata$dataset)
```


    2020_dataset 2021_dataset 2022_dataset 
           31520         8085         7301 

``` r
igg <- abdata %>% filter(isotype == "IgG")
head(igg)
```

      specimen_id isotype is_antigen_specific antigen        MFI MFI_normalised
    1           1     IgG                TRUE      PT   68.56614       3.736992
    2           1     IgG                TRUE     PRN  332.12718       2.602350
    3           1     IgG                TRUE     FHA 1887.12263      34.050956
    4          19     IgG                TRUE      PT   20.11607       1.096366
    5          19     IgG                TRUE     PRN  976.67419       7.652635
    6          19     IgG                TRUE     FHA   60.76626       1.096457
       unit lower_limit_of_detection subject_id actual_day_relative_to_boost
    1 IU/ML                 0.530000          1                           -3
    2 IU/ML                 6.205949          1                           -3
    3 IU/ML                 4.679535          1                           -3
    4 IU/ML                 0.530000          3                           -3
    5 IU/ML                 6.205949          3                           -3
    6 IU/ML                 4.679535          3                           -3
      planned_day_relative_to_boost specimen_type visit infancy_vac biological_sex
    1                             0         Blood     1          wP         Female
    2                             0         Blood     1          wP         Female
    3                             0         Blood     1          wP         Female
    4                             0         Blood     1          wP         Female
    5                             0         Blood     1          wP         Female
    6                             0         Blood     1          wP         Female
                   ethnicity  race year_of_birth date_of_boost      dataset
    1 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    2 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    3 Not Hispanic or Latino White    1986-01-01    2016-09-12 2020_dataset
    4                Unknown White    1983-01-01    2016-10-10 2020_dataset
    5                Unknown White    1983-01-01    2016-10-10 2020_dataset
    6                Unknown White    1983-01-01    2016-10-10 2020_dataset
             age
    1 13956 days
    2 13956 days
    3 13956 days
    4 15052 days
    5 15052 days
    6 15052 days

Q13.

``` r
ggplot(igg, aes(x = MFI_normalised, y = antigen)) + geom_boxplot() + 
  xlim(0,75) + facet_wrap(vars(visit), nrow = 2)
```

    Warning: Removed 5 rows containing non-finite outside the scale range
    (`stat_boxplot()`).

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-22-1.png)

Q14. PT, FIM2/3, and FHA all show differences in levels of IgG antibody
titers over time.

``` r
ggplot(igg) +
  aes(MFI_normalised, antigen, col=infancy_vac ) +
  geom_boxplot(show.legend = FALSE) + 
  facet_wrap(vars(visit), nrow=2) +
  xlim(0,75) +
  theme_bw()
```

    Warning: Removed 5 rows containing non-finite outside the scale range
    (`stat_boxplot()`).

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-23-1.png)

Q15.

``` r
filter(igg, antigen=="OVA") %>%
  ggplot() +
  aes(MFI_normalised, col=infancy_vac) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(vars(visit)) +
  theme_bw()
```

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-24-1.png)

``` r
filter(igg, antigen=="FIM2/3") %>%
  ggplot() +
  aes(MFI_normalised, col=infancy_vac) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(vars(visit)) +
  theme_bw()
```

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-25-1.png)

Q16. The PT levels appear to rise over time in FIM2/3, far exceeding
OVA. It looks like the levels peak at visit 5, before declining. The
trend looks the same for aP and wP.

Q17. I don’t see much of a trend in OVA, but in FIM2/3 the wP seems to
rise before aP initially (until visit 3), then aP rises to briefly
exceed wP during visit 4 before I dont see any trend.

``` r
abdata.21 <- abdata %>% filter(dataset == "2021_dataset")
abdata.21 %>% 
  filter(isotype == "IgG",  antigen == "PT") %>%
  ggplot() +
    aes(x=planned_day_relative_to_boost,
        y=MFI_normalised,
        col=infancy_vac,
        group=subject_id) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_vline(xintercept=14, linetype="dashed") +
  labs(title="2021 dataset IgG PT",
       subtitle = "Dashed lines indicate day 0 (pre-boost) and 14 (apparent peak levels)")
```

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-26-1.png)

Q18. No there was much more vaccination wp vaccination in 2021, but
overall/total vaccination rates declined as well by 2021.

``` r
abdata.20 <- abdata %>% filter(dataset == "2020_dataset")
abdata.20 %>% 
  filter(isotype == "IgG",  antigen == "PT") %>%
  ggplot() +
    aes(x=planned_day_relative_to_boost,
        y=MFI_normalised,
        col=infancy_vac,
        group=subject_id) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_vline(xintercept=14, linetype="dashed") +
  labs(title="2020 dataset IgG PT",
       subtitle = "Dashed lines indicate day 0 (pre-boost) and 14 (apparent peak levels)")
```

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-27-1.png)

``` r
url <- "https://www.cmi-pb.org/api/v2/rnaseq?versioned_ensembl_gene_id=eq.ENSG00000211896.7"
rna <- read_json(url, simplifyVector = TRUE) 
ssrna <- inner_join(rna, meta)
```

    Joining with `by = join_by(specimen_id)`

``` r
ggplot(ssrna, aes(x = visit, y = tpm, group = subject_id)) + geom_point() + 
  geom_line(alpha = 0.2)
```

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-29-1.png)

Q20. The maximum level of this genes expression is at visit 4

Q21. There is a trend where the antibodies slightly peak at visit 4.

``` r
ggplot(ssrna) +
  aes(tpm, col=infancy_vac) +
  geom_boxplot() +
  facet_wrap(vars(visit))
```

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-30-1.png)

``` r
ssrna %>%  
  filter(visit==4) %>% 
  ggplot() +
    aes(tpm, col=infancy_vac) + geom_density() + 
    geom_rug()
```

![](Pertussis_mp_files/figure-commonmark/unnamed-chunk-31-1.png)
