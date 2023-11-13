<p align="justify">
<!-- README.md is generated from README.Rmd. Please edit that file -->

In Experiment 1, we’re looking at differences in the microbiome between
coffee drinkers and non-coffee drinkers.

``` r
source("scripts/ex1_baseline_differences_CD_vs_NCD.R")
```

    ## Loading required package: permute

    ## Loading required package: lattice

    ## This is vegan 2.6-4

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ tidyr::expand() masks Matrix::expand()
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ tidyr::pack()   masks Matrix::pack()
    ## ✖ tidyr::unpack() masks Matrix::unpack()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    ## 
    ## Attaching package: 'scales'
    ## 
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

    ## [1] "Using the following formula: x ~ Legend_ex1"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ Legend_ex1"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ Legend_ex1"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ Legend_ex1"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."

``` r
ex1pca + ex1alpha / ex1DA + plot_layout(guides = 'collect')
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ex1DA_metab
```

![](README_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

In Experiment 2, we’re looking at differences in the microbiome between
coffee drinkers and non-coffee drinkers.

``` r
source("scripts/ex1_baseline_differences_CD_vs_NCD.R")
```

    ## [1] "Using the following formula: x ~ Legend_ex1"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ Legend_ex1"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ Legend_ex1"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ Legend_ex1"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."

``` r
ex1pca + ex1alpha / ex1DA + plot_layout(guides = 'collect')
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ex1DA_metab
```

![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

In Experiment 2B, we’re looking at features that return to non-coffee
drinker levels post washout (coffee abstinence).

``` r
source("scripts/ex_RESTORATION_coffee_analysis_post_washout_normalisation.R")
```

    ## [1] "Using the following formula: x ~ Legend_ex_REST + (1 | participant_ID)"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ Legend_ex_REST + (1 | participant_ID)"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ Legend_ex_REST + (1 | participant_ID)"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ Legend_ex_REST + (1 | participant_ID)"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."

``` r
ex_RESTpca + ex_RESTalpha / ex_RESTDA + plot_layout(guides = 'collect')
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ex1DA_metab
```

![](README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

In Experiment 3, we’re looking at the effects of going back on either
caffeinated or decaffeinated coffee post-washout (coffee abstinence).

``` r
source("scripts/ex3_intervention_v3_vs_v4.R")
```

    ## [1] "Using the following formula: x ~ visit + Treatment + (1 | participant_ID) + visit:Treatment"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ visit + Treatment + (1 | participant_ID) + visit:Treatment"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ visit + Treatment + (1 | participant_ID) + visit:Treatment"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."
    ## [1] "Using the following formula: x ~ visit + Treatment + (1 | participant_ID) + visit:Treatment"
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
ex3pca + ex3alpha + plot_layout(guides = 'collect')
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.
    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.
    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ex3metab
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->
