<p align="justify">
<!-- README.md is generated from README.Rmd. Please edit that file -->

In Experiment 1, we’re looking at differences in the microbiome between
coffee drinkers and non-coffee drinkers.

``` r
source("scripts/ex1_baseline_differences_CD_vs_NCD.R")
```

``` r
ex1_top
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ex1_metab_forest_a + ex1_metab_forest_b  + plot_layout(guides = 'collect') + 
  plot_annotation(title = "Differentially abundant faecal metabolites between non-coffee drinkers (L) and coffee drinkers (R)") 
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ex1_top + ex1_metab_forest + plot_layout(guides = 'collect', heights = c(1, 1, 2))
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

In Experiment 2B, we’re looking at features that return to non-coffee
drinker levels post washout (coffee abstinence).

``` r
source("scripts/ex_RESTORATION_coffee_analysis_post_washout_normalisation.R")
```

``` r
ex_RESTpca + ex_RESTalpha / ex_RESTDA + plot_layout(guides = 'collect')
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ex_REST_metab_forest_a + ex_REST_metab_forest_b + plot_layout(guides = 'collect') + 
  plot_annotation(title = "Differentially abundant faecal metabolites between non-coffee drinkers and coffee drinkers post-washout (L) vs baseline coffee drinker levels (R)")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

In Experiment 3, we’re looking at the effects of going back on either
caffeinated or decaffeinated coffee post-washout (coffee abstinence).

``` r
source("scripts/ex3_intervention_v3_vs_v4.R")
```

``` r
ex3pca + ex3alpha + plot_layout(guides = 'collect')
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ex3metab
```

![](README_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

In this additional microbiome experiment, we’re looking at the acute
effects of coffee abstinence (coffee withdrawal).

``` r
source("scripts/ex_washout_acute_withdrawal.R")
```

``` r
ex_withdrawpca + ex_withdrawalpha + plot_layout(guides = 'collect')
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

In this additional microbiome experiment, we’re looking at the acute
effects of going back on either caffeinated or decaffeinated coffee
post-washout (coffee abstinence).

``` r
source("scripts/ex_INTERVENTION_acute_recaf_or_decaf.R")
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

    ## [1] "Using the following formula: x ~ Legend_ex_INTERVENTION + Treatment + (1 | participant_ID) + "
    ## [2] "Using the following formula:     Legend_ex_INTERVENTION:Treatment"                            
    ## [1] "Adjusting for FDR using Benjamini & Hochberg's procedure."

![](README_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
ex_INTERVENTIONpca + ex_INTERVENTIONalpha + ex_INTERVENTIONDA_GBM + ex_INTERVENTIONDA_GMM +  plot_layout(guides = 'collect', heights = c(1,1,2,2))
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
