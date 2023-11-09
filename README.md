<!-- README.md is generated from README.Rmd. Please edit that file -->

# [AFSC RACE GAP Bering Sea Survey Data Report](link_repo) <img src="https://avatars.githubusercontent.com/u/91760178?s=96&amp;v=4" alt="Logo." align="right" width="139" height="139"/>

> This code is always in development. Find code used for various reports
> in the code
> [releases](https://github.com/afsc-gap-products/gap_bs_data_report/releases).

This repository was previously named `AFSCDataReport` and
`GAP_BS_DataReport`.

The scripts therein reproducibly produce our annual data reports,
presentations, and other outreach documents from data to data product.

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov;
[@EmilyMarkowitz-NOAA](https://github.com/EmilyMarkowitz-NOAA))

**Liz Dawson** (Liz.Dawson AT noaa.gov;
[@liz-dawson-NOAA](https://github.com/liz-dawson-NOAA))

**Chris Anderson** (Christopher.Anderson AT noaa.gov;
[@ChrisAnderson-NOAA](https://github.com/ChrisAnderson-NOAA))

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

## This code and the associated releases were used to develop the following reports, outreach documents, and presentations:

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-2021NBSCommunity" class="csl-entry">

Britt, L. L., Markowitz, E. H., Dawson, E. J., Charriere, N. E.,
Prohaska, B. K., Rohan, S. K., Stevenson, D. E., and Britt, L. L.
(2021). *2021 northern Bering Sea groundfish and crab trawl survey
highlights* \[Outreach\]. https://youtu.be/putpYJtPRF8.

</div>

<div id="ref-2022NEBS2023" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Anderson, A. B., Rohan, S. K.,
Charriere, N. E., Prohaska, B. K., and Stevenson, D. E. (2023). *Results
of the 2022 eastern and northern Bering Sea continental shelf bottom
trawl survey of groundfish and invertebrate fauna* (NOAA Tech. Memo.
NMFS-AFSC-469; p. 213). U.S. Dep. Commer.

</div>

<div id="ref-2022NBSCommunity" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Anderson, C., Charriere, N. E., Richar,
J. I., Rohan, S. K., Prohaska, B. K., Haehn, R. A., and Stevenson, D. E.
(2022). *2022 northern Bering Sea groundfish and crab trawl survey
highlights* \[Outreach\]. University of Alaska Fairbanks Strait Science
Seminar; https://www.youtube.com/watch?v=TGXN2pIDhfc.

</div>

<div id="ref-2018EBS2022" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Charriere, N. E., Prohaska, B. K.,
Rohan, S. K., Haehn, R. A., Stevenson, D. E., and Britt, L. L. (2022).
*Results of the 2018 eastern Bering Sea continental shelf bottom trawl
survey of groundfish and invertebrate fauna* (NOAA Tech. Memo.
NMFS-AFSC-450; p. 183). U.S. Dep. Commer.
<https://doi.org/10.25923/m4pw-t510>

</div>

<div id="ref-2019NEBS2022" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Charriere, N. E., Prohaska, B. K.,
Rohan, S. K., Stevenson, D. E., and Britt, L. L. (2022a). *Results of
the 2019 eastern and northern Bering Sea continental shelf bottom trawl
survey of groundfish and invertebrate fauna* (NOAA Tech. Memo.
NMFS-AFSC-451; p. 225). U.S. Dep. Commer.
<https://doi.org/10.25923/d641-xb21>

</div>

<div id="ref-2021NEBS2022" class="csl-entry">

Markowitz, E. H., Dawson, E. J., Charriere, N. E., Prohaska, B. K.,
Rohan, S. K., Stevenson, D. E., and Britt, L. L. (2022b). *Results of
the 2021 eastern and northern Bering Sea continental shelf bottom trawl
survey of groundfish and invertebrate fauna* (NOAA Tech. Memo.
NMFS-AFSC-452; p. 227). U.S. Dep. Commer.
<https://doi.org/10.25923/g1ny-y360>

</div>

</div>

<!-- Use .bib file to cite reports in subsection titles -->

**Annual Bering Sea Data Report and presentations** \[Markowitz et al.
([2023](#ref-2022NEBS2023)); Markowitz, Dawson, Charriere, Prohaska,
Rohan, Stevenson, et al. ([2022b](#ref-2021NEBS2022)); Markowitz,
Dawson, Charriere, Prohaska, Rohan, Stevenson, et al.
([2022a](#ref-2019NEBS2022)); Markowitz, Dawson, Charriere, Prohaska,
Rohan, Haehn, et al. ([2022](#ref-2018EBS2022));
2021NEBS2022PlanTeamPres; 2022NEBS2022PlanTeamPres\]

**Northern Bering Sea Groundfish and Crab Trawl Survey Highlights**
([Britt et al., 2021](#ref-2021NBSCommunity); [Markowitz, Dawson,
Anderson, et al., 2022](#ref-2022NBSCommunity))

> This document is for informational purposes only and does not
> necessarily represent the views or official position of the Department
> of Commerce, the National Oceanic and Atmospheric Administration, or
> the National Marine Fisheries Service. Not to be cited without
> permission from the authors.

- [11/4/2021 Presentation at the University of Alaska Fairbanks Strait
  Science Seminar](https://youtu.be/putpYJtPRF8)
- [11/4/2022 Presentation at the University of Alaska Fairbanks Strait
  Science Seminar](https://www.youtube.com/watch?v=TGXN2pIDhfc)

## Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/afsc-gap-products/gap_bs_data_report/pulls),
[submit an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/afsc-gap-products/gap_bs_data_report/issues).

# R Version Metadata

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## time zone: America/Los_Angeles
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] RODBC_1.3-21      XML_3.99-0.14     flextable_0.9.4   httr_1.4.7        pingr_2.0.2       scales_1.2.1     
    ##  [7] raster_3.6-26     sp_2.1-1          ps_1.7.5          ggsn_0.5.0        digest_0.6.33     ggspatial_1.1.9  
    ## [13] shadowtext_0.1.2  rlist_0.4.6.2     coldpool_3.2-2    reshape2_1.4.4    lubridate_1.9.3   fields_15.2      
    ## [19] spam_2.10-0       ggthemes_4.2.4    akgfmaps_3.2.0    terra_1.7-55      stars_0.6-4       abind_1.4-5      
    ## [25] sf_1.0-14         gstat_2.1-1       classInt_0.4-10   readtext_0.90     stringr_1.5.0     janitor_2.2.0    
    ## [31] viridis_0.6.4     viridisLite_0.4.2 here_1.0.1        readxl_1.4.3      tidyr_1.3.0       readr_2.1.4      
    ## [37] magrittr_2.0.3    googledrive_2.1.1 dplyr_1.1.3       plyr_1.8.9        ggridges_0.5.4    ggpubr_0.6.0     
    ## [43] extrafont_0.19    magick_2.8.1      png_0.1-8         cowplot_1.1.1     ggplot2_3.4.4     officedown_0.3.1 
    ## [49] officer_0.6.3     rmarkdown_2.25    knitr_1.44       
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] rstudioapi_0.15.0       jsonlite_1.8.7          farver_2.1.1            fs_1.6.3               
    ##   [5] ragg_1.2.6              vctrs_0.6.4             memoise_2.0.1           askpass_1.2.0          
    ##   [9] rstatix_0.7.2           htmltools_0.5.6.1       curl_5.1.0              broom_1.0.5            
    ##  [13] cellranger_1.1.0        KernSmooth_2.23-22      zoo_1.8-12              cachem_1.0.8           
    ##  [17] uuid_1.1-1              maptools_1.1-8          mime_0.12               lifecycle_1.0.3        
    ##  [21] pkgconfig_2.0.3         R6_2.5.1                fastmap_1.1.1           shiny_1.7.5.1          
    ##  [25] snakecase_0.11.1        colorspace_2.1-0        rprojroot_2.0.3         textshaping_0.3.7      
    ##  [29] labeling_0.4.3          fansi_1.0.5             timechange_0.2.0        compiler_4.3.1         
    ##  [33] gargle_1.5.2            proxy_0.4-27            intervals_0.15.4        bit64_4.0.5            
    ##  [37] fontquiver_0.2.1        withr_2.5.1             backports_1.4.1         carData_3.0-5          
    ##  [41] DBI_1.1.3               RgoogleMaps_1.4.5.3     maps_3.4.1              Rttf2pt1_1.3.12        
    ##  [45] ggsignif_0.6.4          openssl_2.1.1           rappdirs_0.3.3          gfonts_0.2.0           
    ##  [49] tools_4.3.1             units_0.8-4             foreign_0.8-85          zip_2.3.0              
    ##  [53] httpuv_1.6.12           extrafontdb_1.0         glue_1.6.2              promises_1.2.1         
    ##  [57] generics_0.1.3          gtable_0.3.4            tzdb_0.4.0              class_7.3-22           
    ##  [61] data.table_1.14.8       hms_1.1.3               xml2_1.3.5              car_3.1-2              
    ##  [65] utf8_1.2.4              pillar_1.9.0            vroom_1.6.4             later_1.3.1            
    ##  [69] lattice_0.22-5          bit_4.0.5               FNN_1.1.3.2             tidyselect_1.2.0       
    ##  [73] rvg_0.3.3               fontLiberation_0.1.0    fontBitstreamVera_0.1.1 gridExtra_2.3          
    ##  [77] crul_1.4.0              xfun_0.40               stringi_1.7.12          ggmap_3.0.2            
    ##  [81] yaml_2.3.7              evaluate_0.22           codetools_0.2-19        httpcode_0.3.0         
    ##  [85] gdtools_0.3.4           tibble_3.2.1            cli_3.6.1               xtable_1.8-4           
    ##  [89] systemfonts_1.0.5       processx_3.8.2          munsell_0.5.0           spacetime_1.3-0        
    ##  [93] Rcpp_1.0.11             parallel_4.3.1          ellipsis_0.3.2          dotCall64_1.1-0        
    ##  [97] jpeg_0.1-10             bitops_1.0-7            xts_0.13.1              e1071_1.7-13           
    ## [101] purrr_1.0.2             crayon_1.5.2            rlang_1.1.1

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
