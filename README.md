<p align="justify">
<!-- README.md is generated from README.Rmd. Please edit that file -->

    ## Loading required package: permute

    ## Loading required package: lattice

    ## This is vegan 2.6-2

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.4
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ tidyr::expand() masks Matrix::expand()
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ tidyr::pack()   masks Matrix::pack()
    ## ✖ tidyr::unpack() masks Matrix::unpack()

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

Fig. 1: Cognitive effects of coffee.

``` r
source("new_scripts/fig_1_behav_cog_health_data.R")
```

``` r
(plot_cog_NCD | plot_cog_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/unnamed-chunk-2-1.svg)<!-- -->

Fig 2b Microbiome

``` r
source("new_scripts/fig_2_gi_microbiome_data.R")
```

``` r
(plot_mb_NCD |  plot_mb_CD) + 
  plot_layout(guides = 'collect', widths = c(1,8))
```

![](README_files/figure-gfm/unnamed-chunk-4-1.svg)<!-- -->

Fig 2b Metabolome

``` r
source("new_scripts/fig_2_gi_metabolome_data.R")
```

``` r
(plot_mt_NCD | plot_mt_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/unnamed-chunk-6-1.svg)<!-- -->
