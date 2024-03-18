# Class_11:Alpha_fold

AlphaFold is a cool new bioinformatics method for structure prediction
from sequence.

``` r
pth <- "HIV1dimer_23119/"
list.files(path = pth)
```

     [1] "cite.bibtex"                                                                   
     [2] "config.json"                                                                   
     [3] "HIV1dimer_23119_coverage.png"                                                  
     [4] "HIV1dimer_23119_env"                                                           
     [5] "HIV1dimer_23119_pae.png"                                                       
     [6] "HIV1dimer_23119_plddt.png"                                                     
     [7] "HIV1dimer_23119_predicted_aligned_error_v1.json"                               
     [8] "HIV1dimer_23119_scores_rank_001_alphafold2_multimer_v3_model_1_seed_000.json"  
     [9] "HIV1dimer_23119_scores_rank_002_alphafold2_multimer_v3_model_5_seed_000.json"  
    [10] "HIV1dimer_23119_scores_rank_003_alphafold2_multimer_v3_model_4_seed_000.json"  
    [11] "HIV1dimer_23119_scores_rank_004_alphafold2_multimer_v3_model_2_seed_000.json"  
    [12] "HIV1dimer_23119_scores_rank_005_alphafold2_multimer_v3_model_3_seed_000.json"  
    [13] "HIV1dimer_23119_unrelaxed_rank_001_alphafold2_multimer_v3_model_1_seed_000.pdb"
    [14] "HIV1dimer_23119_unrelaxed_rank_002_alphafold2_multimer_v3_model_5_seed_000.pdb"
    [15] "HIV1dimer_23119_unrelaxed_rank_003_alphafold2_multimer_v3_model_4_seed_000.pdb"
    [16] "HIV1dimer_23119_unrelaxed_rank_004_alphafold2_multimer_v3_model_2_seed_000.pdb"
    [17] "HIV1dimer_23119_unrelaxed_rank_005_alphafold2_multimer_v3_model_3_seed_000.pdb"
    [18] "HIV1dimer_23119.a3m"                                                           
    [19] "HIV1dimer_23119.csv"                                                           
    [20] "HIV1dimer_23119.done.txt"                                                      
    [21] "log.txt"                                                                       

The multiple sequence allignment (msa) is contained in the “a3m” file of
our AlphaFold output.

``` r
aln.files <- list.files(path = pth, pattern = ".a3m", full.names = TRUE)
```

``` r
library(bio3d)
aln <- read.fasta(aln.files, to.upper = TRUE)
```

    [1] " ** Duplicated sequence id's: 101 **"
    [2] " ** Duplicated sequence id's: 101 **"

``` r
attributes(aln)
```

    $names
    [1] "id"   "ali"  "call"

    $class
    [1] "fasta"

This is a big alignment – almost too big to look at

``` r
dim(aln$ali)
```

    [1] 5378  132

Lets calculate sum summary info such as conservation scores,

``` r
sim <- conserv(aln)
```

``` r
plot(sim, type = 'h')
```

![](CLass_11_files/figure-commonmark/unnamed-chunk-7-1.png)

We can summarize these conserved columns (the ones with high scores) via
a consensus sequence

``` r
consensus(aln, cutoff = 0.9)$seq
```

      [1] "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-"
     [19] "-" "-" "-" "-" "-" "-" "D" "T" "G" "A" "-" "-" "-" "-" "-" "-" "-" "-"
     [37] "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-"
     [55] "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-"
     [73] "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-"
     [91] "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-"
    [109] "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-" "-"
    [127] "-" "-" "-" "-" "-" "-"

Read all our structure models into R

Read the PAE (predicted aligned error)

``` r
library(jsonlite)
```

Find json files

``` r
pae.files <- list.files(path = pth, pattern = "000.json", full.names = TRUE)
```

``` r
pae5 <- read_json(pae.files[5], simplifyVector = TRUE)
pae1 <- read_json(pae.files[1], simplifyVector = TRUE)
```

``` r
dim(pae1$pae)
```

    [1] 198 198

``` r
plot.dmat(pae5$pae)
```

![](CLass_11_files/figure-commonmark/unnamed-chunk-13-1.png)

``` r
plot.dmat(pae1$pae)
```

![](CLass_11_files/figure-commonmark/unnamed-chunk-14-1.png)
