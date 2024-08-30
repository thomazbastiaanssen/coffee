<p align="justify">
<!-- README.md is generated from README.Rmd. Please edit that file -->

Fig. 1a: Cognitive effects of coffee.

``` r
source("new_scripts/fig_1_behav_cog_health_data.R")
```

``` r
(plot_cog_NCD | plot_cog_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/unnamed-chunk-2-1.svg)<!-- -->

``` r
plot_cog_craving
```

![](README_files/figure-gfm/unnamed-chunk-3-1.svg)<!-- -->

``` r
design_1 <- c("AAAAAAAAAAAABBCCCCCC
               AAAAAAAAAAAABBCCCCCC
               AAAAAAAAAAAABBCCCCCC
               DDDDDDDDDDDDBBCCCCCC
               DDDDDDDDDDDDBBCCCCCC
               DDDDDDDDDDDDBBCCCCCC")
(plot_spacer() + plot_cog_NCD + plot_cog_CD + plot_cog_craving) + plot_layout(design = design_1, guides = 'collect') & theme(legend.position = 'left')
```

![](README_files/figure-gfm/fig_1_full-1.svg)<!-- -->

Fig 2a Microbiome

``` r
source("new_scripts/fig_2_gi_microbiome_data.R")
```

``` r
(plot_mb_NCD |  plot_mb_CD) + 
  plot_layout(guides = 'collect', widths = c(1,8))
```

![](README_files/figure-gfm/unnamed-chunk-5-1.svg)<!-- -->

Fig 2b Metabolome

``` r
source("new_scripts/fig_2_gi_metabolome_data.R")
```

``` r
(plot_mt_NCD | plot_mt_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/unnamed-chunk-7-1.svg)<!-- -->

Fig 2b2 Urine Metabolome

``` r
source("new_scripts/fig_2_urine_metabolome_data_reclassed.R")
```

``` r
(plot_urmet_reclass_NCD | plot_urmet_reclass_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/unnamed-chunk-9-1.svg)<!-- -->

``` r
#Sometimes bugs out, so alternatively:

# ggsave(plot = plot_urmet_NCD + guides(fill= "none"), width = 11,  height = 45, units = "cm", device = "svg", filename = "new_stats/urine_plot_a.svg")
# ggsave(plot = plot_urmet_CD + guides(fill= "none"), width = 8,  height = 45, units = "cm", device = "svg", filename = "new_stats/urine_plot_b.svg")
```

Fig 2c Cytokines

``` r
source("new_scripts/fig_2_cytokine_data.R")
```

``` r
(plot_cyt_stim_NCD | plot_cyt_stim_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/unnamed-chunk-11-1.svg)<!-- -->

``` r
(plot_cyt_unstim_NCD | plot_cyt_unstim_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/unnamed-chunk-12-1.svg)<!-- -->

``` r
source("new_scripts/fig_final_integration.R")
```

``` r
fig_int
```

![](README_files/figure-gfm/unnamed-chunk-14-1.svg)<!-- -->

``` r
#source("new_scripts/generate_stats_tables.R")
```
