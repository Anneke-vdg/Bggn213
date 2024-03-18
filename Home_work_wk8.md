# Home_work

``` r
library(ggplot2)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
url <- "https://bioboot.github.io/bggn213_W19/class-material/rs8067378_ENSG00000172057.6.txt"
genotypes <- read.table(url, row.names = 1)
```

Determine sample size for each genotype and their corresponding median
expression levels for each of these genotypes?

Sample size: A/A = 108, A/G = 233, G/G = 121

Median: A/A = 31.24847, A/G = 25.06486, G/G = 20.07363

``` r
table(genotypes$geno)
```


    A/A A/G G/G 
    108 233 121 

``` r
Median <- boxplot(genotypes$exp ~ genotypes$geno)
```

![](Home_work_wk8_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
Median
```

    $stats
             [,1]     [,2]     [,3]
    [1,] 15.42908  7.07505  6.67482
    [2,] 26.95022 20.62572 16.90256
    [3,] 31.24847 25.06486 20.07363
    [4,] 35.95503 30.55183 24.45672
    [5,] 49.39612 42.75662 33.95602

    $n
    [1] 108 233 121

    $conf
             [,1]     [,2]     [,3]
    [1,] 29.87942 24.03742 18.98858
    [2,] 32.61753 26.09230 21.15868

    $out
    [1] 51.51787 50.16704 51.30170 11.39643 48.03410

    $group
    [1] 1 1 1 1 2

    $names
    [1] "A/A" "A/G" "G/G"

Generate a boxplot with a box per genotype, what could you infer from
the relative expression value between A/A and G/G displayed in this
plot? Does the SNP effect the expression of ORMDL3?

The genotype A/A vs G/G has strikingly different expression levels,
expression seems to decrease as the genotype moves from being A dominant
to G dominant. Therefore, it would appear that the SNP does impact the
expression of ORMDL3.

``` r
genotypes %>% ggplot(aes(x = geno, y = exp, col = geno)) + geom_boxplot()
```

![](Home_work_wk8_files/figure-commonmark/unnamed-chunk-4-1.png)
