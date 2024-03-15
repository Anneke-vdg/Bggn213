# Lab_5

## Quarto

Quarto enables you to weave together content and executable code into a
finished document. To learn more about Quarto see <https://quarto.org>.

## Running Code

**Q1.** For which phases is data visualization important in our
scientific workflows?

All of the above

- **Q2.** True or False? The ggplot2 package comes already installed
  with R?

  FALSE

**Q3.** Which plot types are typically NOT used to compare distributions
of numeric variables?

Network graphs

**Q4.** Which statement about data visualization with ggplot2 is
incorrect?

ggplot2 is the only way to create plots in R

**Q5.** Which geometric layer should be used to create scatter plots in
ggplot2?

geom_point

**Q6.** Use the `nrow()` function to find out how many genes are in this
dataset. What is your answer?

5196

**Q7.** Use the `colnames()` function and the `ncol()` function on
the `genes` data frame to find out what the column names are (we will
need these later) and how many columns there are. How many columns did
you find?

4

**Q8.** Use the `table()` function on the `State` column of this
data.frame to find out how many 'up' regulated genes there are. What is
your answer?

127

**Q9.** Using your values above and 2 significant figures. What fraction
of total genes is up-regulated in this dataset?

2.44

``` r
library(ggplot2)
ggplot(cars)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-1-1.png)

``` r
ggplot(cars) + aes(x = speed, y = dist)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-2-1.png)

``` r
ggplot(cars) + aes(x = speed, y = dist) + geom_point()
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
ggplot(cars) + aes(x = speed, y = dist) + geom_point() + geom_smooth(method = "lm")
```

    `geom_smooth()` using formula = 'y ~ x'

![](Lab_5_files/figure-commonmark/unnamed-chunk-3-2.png)

``` r
ggplot(cars) + aes(x = speed, y = dist) + geom_point() + geom_smooth(method = "lm", se = FALSE)
```

    `geom_smooth()` using formula = 'y ~ x'

![](Lab_5_files/figure-commonmark/unnamed-chunk-3-3.png)

``` r
ggplot(cars) + aes(x = speed, y = dist) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title="Speed and Stopping Distances of Cars",
       x="Speed (MPH)", 
       y="Stopping Distance (ft)",
       subtitle = "Your informative subtitle text here",
       caption="Dataset: 'cars'") + theme_bw()
```

    `geom_smooth()` using formula = 'y ~ x'

![](Lab_5_files/figure-commonmark/unnamed-chunk-3-4.png)

``` r
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)
```

            Gene Condition1 Condition2      State
    1      A4GNT -3.6808610 -3.4401355 unchanging
    2       AAAS  4.5479580  4.3864126 unchanging
    3      AASDH  3.7190695  3.4787276 unchanging
    4       AATF  5.0784720  5.0151916 unchanging
    5       AATK  0.4711421  0.5598642 unchanging
    6 AB015752.4 -3.6808610 -3.5921390 unchanging

``` r
nrow(genes)
```

    [1] 5196

``` r
colnames(genes)
```

    [1] "Gene"       "Condition1" "Condition2" "State"     

``` r
ncol(genes)
```

    [1] 4

``` r
table(genes$State)
```


          down unchanging         up 
            72       4997        127 

``` r
ggplot(genes) + aes(x = Condition1, y = Condition2) + geom_point()
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-5-1.png)

``` r
p <- ggplot(genes) + 
    aes(x=Condition1, y=Condition2, col=State) +
    geom_point()
p
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-6-1.png)

``` r
p + scale_colour_manual( values=c("blue","gray","red") )
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-7-1.png)

``` r
p + scale_colour_manual( values=c("blue","gray","red") ) + labs(x = "Control (no drug)", y = "Drug Treatment")
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-8-1.png)

``` r
url <- "https://raw.githubusercontent.com/jennybc/gapminder/master/inst/extdata/gapminder.tsv"

gapminder <- read.delim(url)
```

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
gapminder_2007 <- gapminder %>% filter(year==2007)
```

``` r
ggplot(gapminder_2007) + aes(x = gdpPercap, y = lifeExp) + geom_point()
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-11-1.png)

``` r
ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point(alpha=0.5)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-12-1.png)

``` r
ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp, color=continent, size=pop) +
  geom_point(alpha=0.5)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-13-1.png)

``` r
ggplot(gapminder_2007) + 
  aes(x = gdpPercap, y = lifeExp, color = pop) +
  geom_point(alpha=0.8)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-14-1.png)

``` r
ggplot(gapminder_2007) + 
  aes(x = gdpPercap, y = lifeExp, size = pop) +
  geom_point(alpha=0.5)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-15-1.png)

``` r
ggplot(gapminder_2007) + 
  geom_point(aes(x = gdpPercap, y = lifeExp,
                 size = pop), alpha=0.5) + 
  scale_size_area(max_size = 10)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-16-1.png)

``` r
gapminder_1957 <- gapminder %>% filter(year == 1957)
ggplot(gapminder_1957) + geom_point(aes(x = gdpPercap, y = lifeExp, col = continent, size = pop), alpha = 0.7) + scale_size_area(max_size = 15)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-17-1.png)

``` r
gapminder_1957_2007 <- gapminder %>% filter(year==1957 | year==2007)

ggplot(gapminder_1957_2007) + 
  geom_point(aes(x = gdpPercap, y = lifeExp, color=continent,
                 size = pop), alpha=0.7) + 
  scale_size_area(max_size = 10) +
  facet_wrap(~year)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-18-1.png)

``` r
gapminder_top5 <- gapminder %>% 
  filter(year==2007) %>% 
  arrange(desc(pop)) %>% 
  top_n(5, pop)

gapminder_top5
```

            country continent year lifeExp        pop gdpPercap
    1         China      Asia 2007  72.961 1318683096  4959.115
    2         India      Asia 2007  64.698 1110396331  2452.210
    3 United States  Americas 2007  78.242  301139947 42951.653
    4     Indonesia      Asia 2007  70.650  223547000  3540.652
    5        Brazil  Americas 2007  72.390  190010647  9065.801

``` r
ggplot(gapminder_top5) + geom_col(aes(x = country, y = pop))
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-20-1.png)

``` r
gapminder_top5_lE <- gapminder %>% 
  filter(year==2007) %>% 
  arrange(desc(lifeExp)) %>% 
  top_n(5, lifeExp)

ggplot(gapminder_top5_lE) + geom_col(aes(x = country, y = lifeExp))
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-21-1.png)

``` r
ggplot(gapminder_top5) + 
  geom_col(aes(x = country, y = pop, fill = continent))
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-22-1.png)

``` r
ggplot(gapminder_top5) + 
  geom_col(aes(x = country, y = pop, fill = lifeExp))
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-23-1.png)

``` r
ggplot(gapminder_top5) +
  aes(x=country, y=pop, fill=gdpPercap) +
  geom_col()
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-24-1.png)

``` r
ggplot(gapminder_top5) +
  aes(x=reorder(country, -pop), y=pop, fill=gdpPercap) +
  geom_col()
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-25-1.png)

``` r
ggplot(gapminder_top5) +
  aes(x=reorder(country, -pop), y=pop, fill=country) +
  geom_col(col="gray30") +
  guides(fill="none")
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-26-1.png)

``` r
head(USArrests)
```

               Murder Assault UrbanPop Rape
    Alabama      13.2     236       58 21.2
    Alaska       10.0     263       48 44.5
    Arizona       8.1     294       80 31.0
    Arkansas      8.8     190       50 19.5
    California    9.0     276       91 40.6
    Colorado      7.9     204       78 38.7

``` r
USArrests$State <- rownames(USArrests)
ggplot(USArrests) +
  aes(x=reorder(State,Murder), y=Murder) +
  geom_col() +
  coord_flip()
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-28-1.png)

``` r
ggplot(USArrests) +
  aes(x=reorder(State,Murder), y=Murder) +
  geom_point() +
  geom_segment(aes(x=State, 
                   xend=State, 
                   y=0, 
                   yend=Murder), color="blue") +
  coord_flip()
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-29-1.png)

``` r
library(gapminder)
```


    Attaching package: 'gapminder'

    The following object is masked _by_ '.GlobalEnv':

        gapminder

``` r
library(gganimate)

# Setup nice regular ggplot of the gapminder data
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  # Facet by continent
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  shadow_wake(wake_length = 0.1, alpha = FALSE)
```

![](Lab_5_files/figure-commonmark/unnamed-chunk-30-1.gif)

``` r
library(patchwork)

# Setup some example plots 
p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))

# Use patchwork to combine them here:
(p1 | p2 | p3) /
      p4
```

    `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Lab_5_files/figure-commonmark/unnamed-chunk-31-1.png)

``` r
sessionInfo()
```

    R version 4.2.3 (2023-03-15)
    Platform: x86_64-apple-darwin17.0 (64-bit)
    Running under: macOS Big Sur ... 10.16

    Matrix products: default
    BLAS:   /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRblas.0.dylib
    LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib

    locale:
    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
    [1] patchwork_1.2.0 gganimate_1.0.9 gapminder_1.0.0 dplyr_1.1.4    
    [5] ggplot2_3.5.0  

    loaded via a namespace (and not attached):
     [1] pillar_1.9.0      compiler_4.2.3    prettyunits_1.2.0 progress_1.2.3   
     [5] tools_4.2.3       digest_0.6.34     lattice_0.22-5    jsonlite_1.8.8   
     [9] evaluate_0.23     lifecycle_1.0.4   tibble_3.2.1      gtable_0.3.4     
    [13] nlme_3.1-164      mgcv_1.9-1        pkgconfig_2.0.3   rlang_1.1.3      
    [17] Matrix_1.6-4      cli_3.6.2         rstudioapi_0.15.0 yaml_2.3.8       
    [21] xfun_0.42         fastmap_1.1.1     withr_3.0.0       knitr_1.45       
    [25] hms_1.1.3         generics_0.1.3    vctrs_0.6.5       grid_4.2.3       
    [29] tidyselect_1.2.0  glue_1.7.0        R6_2.5.1          gifski_1.12.0-2  
    [33] fansi_1.0.6       rmarkdown_2.26    tweenr_2.0.3      farver_2.1.1     
    [37] magrittr_2.0.3    scales_1.3.0      htmltools_0.5.7   splines_4.2.3    
    [41] colorspace_2.1-0  labeling_0.4.3    utf8_1.2.4        stringi_1.8.3    
    [45] munsell_0.5.0     crayon_1.5.2     
