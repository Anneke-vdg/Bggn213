# Lab_8

``` r
fna.data <- read.csv("~/Desktop/UCSD_courses/BGGN203_bioinf/Bggn213-github/WisconsinCancer.csv")
wisc.df <- data.frame(fna.data, row.names = 1)
```

``` r
# We can use -1 here to remove the first column
wisc.data <- wisc.df[,-1]
wisc.data <- wisc.data[,-31]
# Create diagnosis vector for later 
diagnosis <- as.factor(wisc.df$diagnosis)
```

Q1. How many observations are in this dataset?

There are 569 observations in this dataset

Q2. How many of the observations have a malignant diagnosis?

There are 212 observations that have a malignant diagnosis

Q3. How many variables/features in the data are suffixed with \_mean?

There are 10 variables which contain \_mean as a suffix.

``` r
#Q1
nrow(wisc.df)
```

    [1] 569

``` r
length(diagnosis)
```

    [1] 569

``` r
#Q2
#wisc.df %>% filter(diagnosis == "M") %>% nrow()
table(wisc.df$diagnosis)
```


      B   M 
    357 212 

``` r
#Q3
length(grep("_mean", colnames(wisc.df), value = TRUE))
```

    [1] 10

``` r
# Check column means and standard deviations
colMeans(wisc.data)
```

                radius_mean            texture_mean          perimeter_mean 
               1.412729e+01            1.928965e+01            9.196903e+01 
                  area_mean         smoothness_mean        compactness_mean 
               6.548891e+02            9.636028e-02            1.043410e-01 
             concavity_mean     concave.points_mean           symmetry_mean 
               8.879932e-02            4.891915e-02            1.811619e-01 
     fractal_dimension_mean               radius_se              texture_se 
               6.279761e-02            4.051721e-01            1.216853e+00 
               perimeter_se                 area_se           smoothness_se 
               2.866059e+00            4.033708e+01            7.040979e-03 
             compactness_se            concavity_se       concave.points_se 
               2.547814e-02            3.189372e-02            1.179614e-02 
                symmetry_se    fractal_dimension_se            radius_worst 
               2.054230e-02            3.794904e-03            1.626919e+01 
              texture_worst         perimeter_worst              area_worst 
               2.567722e+01            1.072612e+02            8.805831e+02 
           smoothness_worst       compactness_worst         concavity_worst 
               1.323686e-01            2.542650e-01            2.721885e-01 
       concave.points_worst          symmetry_worst fractal_dimension_worst 
               1.146062e-01            2.900756e-01            8.394582e-02 

``` r
apply(wisc.data,2,sd)
```

                radius_mean            texture_mean          perimeter_mean 
               3.524049e+00            4.301036e+00            2.429898e+01 
                  area_mean         smoothness_mean        compactness_mean 
               3.519141e+02            1.406413e-02            5.281276e-02 
             concavity_mean     concave.points_mean           symmetry_mean 
               7.971981e-02            3.880284e-02            2.741428e-02 
     fractal_dimension_mean               radius_se              texture_se 
               7.060363e-03            2.773127e-01            5.516484e-01 
               perimeter_se                 area_se           smoothness_se 
               2.021855e+00            4.549101e+01            3.002518e-03 
             compactness_se            concavity_se       concave.points_se 
               1.790818e-02            3.018606e-02            6.170285e-03 
                symmetry_se    fractal_dimension_se            radius_worst 
               8.266372e-03            2.646071e-03            4.833242e+00 
              texture_worst         perimeter_worst              area_worst 
               6.146258e+00            3.360254e+01            5.693570e+02 
           smoothness_worst       compactness_worst         concavity_worst 
               2.283243e-02            1.573365e-01            2.086243e-01 
       concave.points_worst          symmetry_worst fractal_dimension_worst 
               6.573234e-02            6.186747e-02            1.806127e-02 

``` r
wisc.hc <- hclust(dist(wisc.data))
plot(wisc.hc)
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-5-1.png)

``` r
# Perform PCA on wisc.data by completing the following code
wisc.pr <- prcomp(wisc.data, scale. = TRUE)
summary(wisc.pr)
```

    Importance of components:
                              PC1    PC2     PC3     PC4     PC5     PC6     PC7
    Standard deviation     3.6444 2.3857 1.67867 1.40735 1.28403 1.09880 0.82172
    Proportion of Variance 0.4427 0.1897 0.09393 0.06602 0.05496 0.04025 0.02251
    Cumulative Proportion  0.4427 0.6324 0.72636 0.79239 0.84734 0.88759 0.91010
                               PC8    PC9    PC10   PC11    PC12    PC13    PC14
    Standard deviation     0.69037 0.6457 0.59219 0.5421 0.51104 0.49128 0.39624
    Proportion of Variance 0.01589 0.0139 0.01169 0.0098 0.00871 0.00805 0.00523
    Cumulative Proportion  0.92598 0.9399 0.95157 0.9614 0.97007 0.97812 0.98335
                              PC15    PC16    PC17    PC18    PC19    PC20   PC21
    Standard deviation     0.30681 0.28260 0.24372 0.22939 0.22244 0.17652 0.1731
    Proportion of Variance 0.00314 0.00266 0.00198 0.00175 0.00165 0.00104 0.0010
    Cumulative Proportion  0.98649 0.98915 0.99113 0.99288 0.99453 0.99557 0.9966
                              PC22    PC23   PC24    PC25    PC26    PC27    PC28
    Standard deviation     0.16565 0.15602 0.1344 0.12442 0.09043 0.08307 0.03987
    Proportion of Variance 0.00091 0.00081 0.0006 0.00052 0.00027 0.00023 0.00005
    Cumulative Proportion  0.99749 0.99830 0.9989 0.99942 0.99969 0.99992 0.99997
                              PC29    PC30
    Standard deviation     0.02736 0.01153
    Proportion of Variance 0.00002 0.00000
    Cumulative Proportion  1.00000 1.00000

``` r
biplot(wisc.pr)
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-6-1.png)

``` r
plot(wisc.pr$x[, 1], wisc.pr$x[, 2], col = diagnosis, xlab = "PC1 (44.27%)", ylab = "PC2 (18.97%)")
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-6-2.png)

``` r
#Scaling is important to normalize and compare varience in the data, so the results arent scued by variables with large variences in their data
sum_pca <- summary(wisc.pr)
barplot(sum_pca$importance[2,])
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-6-3.png)

``` r
library(factoextra)
```

    Loading required package: ggplot2

    Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
fviz_eig(wisc.pr, addlabels = TRUE)
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-7-1.png)

``` r
pcs_res <- as.data.frame(wisc.pr$x)
ggplot(data = pcs_res, aes(x = PC1, y = PC2, color = diagnosis)) + geom_point() + theme_bw() + scale_color_manual(values = c("tan", "black"))
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-8-1.png)

Q4. From your results, what proportion of the original variance is
captured by thee first principle component?

The first component explains 44.27% of the variance in the dataset.

Q5. How many principle components (PCs) are required to describe at
least 70% of the original variance in the data?

Three Pc’s so (PC1, PC2, and PC3) are required to explain at least 70%
of the variance in the original data.

Q6. How many principle components (PCs) are required to describe at
least 90% of the original variance in the data?

Seven PC’s (PC1, PC2, PC3, PC4, PC5, PC6, PC7) are required describe at
least 90% of the variance of the data.

``` r
biplot(wisc.pr)
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-9-1.png)

Q7. What stands out to you about this plot? is it easy or difficult to
understand? Why?

This plot is very messy and confusing. Due to the mess of sample and
variable names it is really difficult to see or understand anything
about what the data looks like, just too many inputs.

Q8. Generate a similar plot for principle components 1 and 3. What do
you notice about these plots?

The points still segregate very well, however, it does seem that there
is more overlap between the groups, as in the groups don’t segregate
very well.

``` r
plot(wisc.pr$x[,1], wisc.pr$x[,3], col = diagnosis, xlab = "PC1", ylab = "PC3")
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-10-1.png)

Q9. For the first principle component, what is the loading vector (ie
wisc.pr\$rotation\[,1\]) for the feature concave.points_mean? This tells
us how much this original feature contributes to the first PC.

It is contributing 1.6% for the first principle component (Dimension 8).

``` r
wisc.pr.hclust <- hclust(dist(wisc.pr$x[,1:3]), method = "ward.D2")
plot(wisc.pr.hclust)
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-11-1.png)

Q.10 Using the plot() and abline() functions, what is the height at
which the clustering model has 4 clusters?

I would say at a height of about 36 the clustering model has 4 clusters.

``` r
plot(wisc.pr.hclust)
abline(h = 36, col = "red")
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-12-1.png)

``` r
plot(wisc.pr.hclust)
abline(h = 80, col = "red")
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-13-1.png)

``` r
grps <- cutree(wisc.pr.hclust, h = 80)
table <- grps
table(grps, diagnosis)
```

        diagnosis
    grps   B   M
       1  24 179
       2 333  33

Q11. Can you find a better cluster vs diagnoses match by cutting into a
different number of clusters between 2 and 10? How do you judge the
quality of your result in each case?

The better cluster vs diagnosis match would be if you cut at two
clusters. The quality of the result can be determined by evaluating the
degree of overlap between the clustered groups and the diagnosis groups.

Q12. Which method gives your favorite results for the same data.dist
dataset? Explain your reasoning.

the ward.D2 method would be my preferred method, because it minimizes
variance within clusters.

Q13. How well does the newly created model with two clusters separate
out the two diagnoses?

The new model with the two clusters separates well and aligns nicely
with the diagnoses results.

Q14. How well do the hierarchical clustering models you created in
previous sections do in terms of separating the diagnoses? Again, use
the table() function to compare the output of each model with the vector
containing the actual diagnoses.

It looks like the hierarchical clustering models do a fairly good job of
separating the results with the appropriately selected K’s.

Q15. Which of your analysis procedures resulted in a clustering model
with the best specificity? How about sensitivity?

``` r
url <- "https://tinyurl.com/new-samples-CSV"
new <- read.csv(url)
npc <- predict(wisc.pr, newdata=new)
npc
```

               PC1       PC2        PC3        PC4       PC5        PC6        PC7
    [1,]  2.576616 -3.135913  1.3990492 -0.7631950  2.781648 -0.8150185 -0.3959098
    [2,] -4.754928 -3.009033 -0.1660946 -0.6052952 -1.140698 -1.2189945  0.8193031
                PC8       PC9       PC10      PC11      PC12      PC13     PC14
    [1,] -0.2307350 0.1029569 -0.9272861 0.3411457  0.375921 0.1610764 1.187882
    [2,] -0.3307423 0.5281896 -0.4855301 0.7173233 -1.185917 0.5893856 0.303029
              PC15       PC16        PC17        PC18        PC19       PC20
    [1,] 0.3216974 -0.1743616 -0.07875393 -0.11207028 -0.08802955 -0.2495216
    [2,] 0.1299153  0.1448061 -0.40509706  0.06565549  0.25591230 -0.4289500
               PC21       PC22       PC23       PC24        PC25         PC26
    [1,]  0.1228233 0.09358453 0.08347651  0.1223396  0.02124121  0.078884581
    [2,] -0.1224776 0.01732146 0.06316631 -0.2338618 -0.20755948 -0.009833238
                 PC27        PC28         PC29         PC30
    [1,]  0.220199544 -0.02946023 -0.015620933  0.005269029
    [2,] -0.001134152  0.09638361  0.002795349 -0.019015820

``` r
plot(wisc.pr$x[,1:2], col=diagnosis)
points(npc[,1], npc[,2], col="blue", pch=16, cex=3)
text(npc[,1], npc[,2], c(1,2), col="white")
```

![](Lab_8_files/figure-commonmark/unnamed-chunk-16-1.png)

Q16. Which of these new patients should we prioritize for follow up
based on your results? Patient 2 should be followed up, more similar to
malignant cancer patients.

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
    [1] factoextra_1.0.7 ggplot2_3.5.0   

    loaded via a namespace (and not attached):
     [1] Rcpp_1.0.12       pillar_1.9.0      compiler_4.2.3    ggpubr_0.6.0     
     [5] tools_4.2.3       digest_0.6.34     jsonlite_1.8.8    evaluate_0.23    
     [9] lifecycle_1.0.4   tibble_3.2.1      gtable_0.3.4      pkgconfig_2.0.3  
    [13] rlang_1.1.3       cli_3.6.2         rstudioapi_0.15.0 ggrepel_0.9.5    
    [17] yaml_2.3.8        xfun_0.42         fastmap_1.1.1     withr_3.0.0      
    [21] dplyr_1.1.4       knitr_1.45        generics_0.1.3    vctrs_0.6.5      
    [25] grid_4.2.3        tidyselect_1.2.0  glue_1.7.0        R6_2.5.1         
    [29] rstatix_0.7.2     fansi_1.0.6       rmarkdown_2.26    carData_3.0-5    
    [33] farver_2.1.1      car_3.1-2         tidyr_1.3.1       purrr_1.0.2      
    [37] magrittr_2.0.3    backports_1.4.1   scales_1.3.0      htmltools_0.5.7  
    [41] abind_1.4-5       colorspace_2.1-0  ggsignif_0.6.4    labeling_0.4.3   
    [45] utf8_1.2.4        munsell_0.5.0     broom_1.0.5      
