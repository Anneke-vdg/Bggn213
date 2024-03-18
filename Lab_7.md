# Class_7

``` r
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url)
dim(x)
```

    [1] 17  5

Q1. How many rows and columns are in your new data frame named x? What R
functions could you use to answer this question?

There are 17 rows and 5 columns in this data frame.

You could use dim(), nrow(), or ncol(), to find the number of rows and
columns in this dataframe.

``` r
##preview the first 6 rows
head(x)
```

                   X England Wales Scotland N.Ireland
    1         Cheese     105   103      103        66
    2  Carcass_meat      245   227      242       267
    3    Other_meat      685   803      750       586
    4           Fish     147   160      122        93
    5 Fats_and_oils      193   235      184       209
    6         Sugars     156   175      147       139

``` r
rownames(x) <- x[,1]
x <- x[,-1]
head(x)
```

                   England Wales Scotland N.Ireland
    Cheese             105   103      103        66
    Carcass_meat       245   227      242       267
    Other_meat         685   803      750       586
    Fish               147   160      122        93
    Fats_and_oils      193   235      184       209
    Sugars             156   175      147       139

``` r
#Could also do
#url <- "https://tinyurl.com/UK-foods"
#x <- read.csv(url, row.names = 1)
#head(x)
#Automatically sets the first row as row names
```

``` r
dim(x)
```

    [1] 17  4

Q2. Which approach to solving the “row-names” problem mentioned above do
you prefer and why?

Well if you know apriori that the first row of the imported data will be
the rownames, then I prefer the x \<- read.csv(url, row.names = 1)
method, because I find it shorter and more convenient.

``` r
barplot(as.matrix(x), beside=T, col=rainbow(nrow(x)))
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-5-1.png)

Q3. Changing what optional argument in the above barplot() function
results in the following plot?

You change the beside arguement from T to F to create a stacked barplot.

Q5. Generating all pairwise plots may help somewhat. Can you make sense
of the following code and resulting figure? What does it mean if a given
point lies on the diagonal for a given plot?

The code above does a pairwise comparison for each country across each
variable. If a point lies on the diagonal then it is equal in both
countries, if its above or below then that indicates it is consumed to a
greater degree in the corresponding country.

``` r
#Q3
barplot(as.matrix(x), beside=F, col=rainbow(nrow(x)))
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-6-1.png)

``` r
#Q5
pairs(x, col=rainbow(10), pch=16)
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-6-2.png)

Q6. What is the main difference between N. Ireland and the other
countries of the UK in terms of this data-set?

``` r
pca <- prcomp(t(x))
summary(pca)
```

    Importance of components:
                                PC1      PC2      PC3       PC4
    Standard deviation     324.1502 212.7478 73.87622 4.189e-14
    Proportion of Variance   0.6744   0.2905  0.03503 0.000e+00
    Cumulative Proportion    0.6744   0.9650  1.00000 1.000e+00

Q7. Complete the code below to generate a plot of PC1 vs PC2. The second
line adds text labels over the data points.

``` r
plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab="PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(x))
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-8-1.png)

Q8. Customize your plot so that the colors of the country names match
the colors in our UK and Ireland map and table at the start of this
document.

``` r
plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab="PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(x), col = c("orange", "red", "blue", "darkgreen"))
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-9-1.png)

``` r
v <- round( pca$sdev^2/sum(pca$sdev^2) * 100 )
v
```

    [1] 67 29  4  0

``` r
z <- summary(pca)
z$importance
```

                                 PC1       PC2      PC3          PC4
    Standard deviation     324.15019 212.74780 73.87622 4.188568e-14
    Proportion of Variance   0.67444   0.29052  0.03503 0.000000e+00
    Cumulative Proportion    0.67444   0.96497  1.00000 1.000000e+00

``` r
barplot(v, xlab="Principal Component", ylab= "Percent Variation")
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-11-1.png)

``` r
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,1], las=2 )
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-12-1.png)

Q9. Generate a similar ‘loadings plot’ for PC2. What two food groups
feature prominantly and what does PC2 mainly tell us about?

Fresh potatoes and soft drinks are the two food groups that feature
predominantly in the loadings of PC2. PC2 describes the second most
variation in the data set, in this case on the Y axis, it describes the
pusing of points up and down.

``` r
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,2], las=2 )
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-13-1.png)

``` r
library(ggplot2)

df <- as.data.frame(pca$x)
df_lab <- tibble::rownames_to_column(df, "Country")
ggplot(df_lab) + 
  aes(PC1, PC2, col=Country) + 
  geom_point()
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-14-1.png)

``` r
ggplot(df_lab) + 
  aes(PC1, PC2, col=Country, label=Country) + 
  geom_hline(yintercept = 0, col="gray") +
  geom_vline(xintercept = 0, col="gray") +
  geom_point(show.legend = FALSE) +
  geom_label(hjust=1, nudge_x = -10, show.legend = FALSE) +
  expand_limits(x = c(-300,500)) +
  xlab("PC1 (67.4%)") +
  ylab("PC2 (28%)") +
  theme_bw()
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-15-1.png)

``` r
ld <- as.data.frame(pca$rotation)
ld_lab <- tibble::rownames_to_column(ld, "Food")

ggplot(ld_lab) +
  aes(PC1, Food) +
  geom_col()
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-16-1.png)

``` r
ggplot(ld_lab) +
  aes(PC1, reorder(Food, PC1), bg=PC1) +
  geom_col() + 
  xlab("PC1 Loadings/Contributions") +
  ylab("Food Group") +
  scale_fill_gradient2(low="purple", mid="gray", high="darkgreen", guide=NULL) +
  theme_bw()
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-17-1.png)

``` r
biplot(pca)
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-18-1.png)

``` r
url2 <- "https://tinyurl.com/expression-CSV"
rna.data <- read.csv(url2, row.names=1)
head(rna.data)
```

           wt1 wt2  wt3  wt4 wt5 ko1 ko2 ko3 ko4 ko5
    gene1  439 458  408  429 420  90  88  86  90  93
    gene2  219 200  204  210 187 427 423 434 433 426
    gene3 1006 989 1030 1017 973 252 237 238 226 210
    gene4  783 792  829  856 760 849 856 835 885 894
    gene5  181 249  204  244 225 277 305 272 270 279
    gene6  460 502  491  491 493 612 594 577 618 638

Q10. How many genes and samples are in this dataset?

There are 100 genes and 10 samples in this data set.

``` r
dim(rna.data)
```

    [1] 100  10

``` r
pca <- prcomp(t(rna.data), scale=TRUE)
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2")
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-21-1.png)

``` r
summary(pca)
```

    Importance of components:
                              PC1    PC2     PC3     PC4     PC5     PC6     PC7
    Standard deviation     9.6237 1.5198 1.05787 1.05203 0.88062 0.82545 0.80111
    Proportion of Variance 0.9262 0.0231 0.01119 0.01107 0.00775 0.00681 0.00642
    Cumulative Proportion  0.9262 0.9493 0.96045 0.97152 0.97928 0.98609 0.99251
                               PC8     PC9      PC10
    Standard deviation     0.62065 0.60342 3.348e-15
    Proportion of Variance 0.00385 0.00364 0.000e+00
    Cumulative Proportion  0.99636 1.00000 1.000e+00

``` r
plot(pca, main="Quick scree plot")
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-22-1.png)

``` r
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per
```

     [1] 92.6  2.3  1.1  1.1  0.8  0.7  0.6  0.4  0.4  0.0

``` r
barplot(pca.var.per, main="Scree Plot", 
        names.arg = paste0("PC", 1:10),
        xlab="Principal Component", ylab="Percent Variation")
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-22-2.png)

``` r
colvec <- colnames(rna.data)
colvec[grep("wt", colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"

plot(pca$x[,1], pca$x[,2], col=colvec, pch=16,
     xlab=paste0("PC1 (", pca.var.per[1], "%)"),
     ylab=paste0("PC2 (", pca.var.per[2], "%)"))

text(pca$x[,1], pca$x[,2], labels = colnames(rna.data), pos=c(rep(4,5), rep(2,5)))
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-23-1.png)

``` r
df <- as.data.frame(pca$x)
ggplot(df) + 
  aes(PC1, PC2) + 
  geom_point()
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-24-1.png)

``` r
df$samples <- colnames(rna.data) 
df$condition <- substr(colnames(rna.data),1,2)
p <- ggplot(df) + 
        aes(PC1, PC2, label=samples, col=condition) + 
        geom_label(show.legend = FALSE)
p
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-25-1.png)

``` r
p + labs(title="PCA of RNASeq Data",
       subtitle = "PC1 clealy seperates wild-type from knock-out samples",
       x=paste0("PC1 (", pca.var.per[1], "%)"),
       y=paste0("PC2 (", pca.var.per[2], "%)"),
       caption="Class example data") +
     theme_bw()
```

![](Lab_7_files/figure-commonmark/unnamed-chunk-25-2.png)

``` r
loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores) 
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:10])
top_10_genes
```

     [1] "gene100" "gene66"  "gene45"  "gene68"  "gene98"  "gene60"  "gene21" 
     [8] "gene56"  "gene10"  "gene90" 

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
    [1] ggplot2_3.5.0

    loaded via a namespace (and not attached):
     [1] rstudioapi_0.15.0 knitr_1.45        magrittr_2.0.3    tidyselect_1.2.0 
     [5] munsell_0.5.0     colorspace_2.1-0  R6_2.5.1          rlang_1.1.3      
     [9] fastmap_1.1.1     fansi_1.0.6       dplyr_1.1.4       tools_4.2.3      
    [13] grid_4.2.3        gtable_0.3.4      xfun_0.42         utf8_1.2.4       
    [17] cli_3.6.2         withr_3.0.0       htmltools_0.5.7   yaml_2.3.8       
    [21] digest_0.6.34     tibble_3.2.1      lifecycle_1.0.4   farver_2.1.1     
    [25] vctrs_0.6.5       glue_1.7.0        evaluate_0.23     rmarkdown_2.26   
    [29] labeling_0.4.3    compiler_4.2.3    pillar_1.9.0      generics_0.1.3   
    [33] scales_1.3.0      jsonlite_1.8.8    pkgconfig_2.0.3  
