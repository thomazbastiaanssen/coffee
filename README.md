<p align="justify">
<!-- README.md is generated from README.Rmd. Please edit that file -->

Fig. 1a: Cognitive effects of coffee.

``` r
source("scripts/fig_1_behav_cog_health_data.R")
```

``` r
(plot_cog_NCD | plot_cog_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/plot_cog-1.svg)<!-- -->

``` r
plot_cog_craving
```

![](README_files/figure-gfm/plot_cog_craving-1.svg)<!-- -->

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
source("scripts/fig_2_gi_microbiome_data.R")
```

``` r
(plot_mb_NCD |  plot_mb_CD) + 
  plot_layout(guides = 'collect', widths = c(1,8))
```

![](README_files/figure-gfm/plot_MB-1.svg)<!-- -->

Fig 2b Metabolome

``` r
source("scripts/fig_2_gi_metabolome_data.R")
```

``` r
(plot_mt_NCD | plot_mt_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/plot_MT-1.svg)<!-- -->

Fig 2b2 Urine Metabolome

``` r
source("scripts/fig_2_urine_metabolome_data_reclassed.R")
```

``` r
(plot_urmet_reclass_NCD  + plot_urmet_reclass_CD) + 
  plot_layout(widths = c(1,3), guides = 'collect')
```

![](README_files/figure-gfm/plot_met_urine-1.svg)<!-- -->

``` r
#Sometimes bugs out, so alternatively:

# ggsave(plot = plot_urmet_NCD + guides(fill= "none"), width = 11,  height = 45, units = "cm", device = "svg", filename = "stats/urine_plot_a.svg")
# ggsave(plot = plot_urmet_CD + guides(fill= "none"), width = 8,  height = 45, units = "cm", device = "svg", filename = "stats/urine_plot_b.svg")
```

Fig 2bx Targeted faecal metabolomics

``` r
source("scripts/fig_2_faecal_metabolome_data_reclassed.R")
```

``` r
(plot_fecmet_reclass_NCD | plot_fecmet_reclass_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/plot_met_fec-1.svg)<!-- -->

Fig 2c Cytokines

``` r
source("scripts/fig_2_cytokine_data.R")
```

``` r
(plot_cyt_stim_NCD | plot_cyt_stim_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/plot_cyt_stim-1.svg)<!-- -->

``` r
(plot_cyt_unstim_NCD | plot_cyt_unstim_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))
```

![](README_files/figure-gfm/plot_cyt_unstim-1.svg)<!-- -->

``` r
source("scripts/fig_final_integration.R")
```

``` r
fig_int
```

![](README_files/figure-gfm/plot_sankey_MB_MT_cog-1.svg)<!-- -->

``` r
source("scripts/fig_final_integration_MB_fecmet_cog.R")
```

``` r
fig_int_mb_fecmet_cog
```

![](README_files/figure-gfm/plot_sankey_MB_fecmet_cog-1.svg)<!-- -->

``` r
source("scripts/fig_final_integration_MB_urmet_cog.R")
```

``` r
fig_int_mb_urmet_cog
```

![](README_files/figure-gfm/plot_sankey_MB_urmet_cog-1.svg)<!-- -->

\#Microbiome in the middle

``` r
source("scripts/fig_final_integration_fecmet_MB_behav.R")
```

``` r
fig_int_fecmet_MB_cog
```

![](README_files/figure-gfm/plot_sankey_fecmet_MB_cog-1.svg)<!-- -->

``` r
source("scripts/fig_final_integration_urmet_MB_behav.R")
```

``` r
fig_int_urmet_MB_cog
```

![](README_files/figure-gfm/fig_int_urmet_MB_cog-1.svg)<!-- -->

``` r
#source("scripts/generate_stats_tables.R")
```
