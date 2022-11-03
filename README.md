<!-- README.md is generated from README.Rmd. Please edit that file -->

# [AFSC RACE GAP Bering Sea Survey Data Report](%60r%20link_repo%60) <img src="https://avatars.githubusercontent.com/u/91760178?s=96&amp;v=4" alt="Logo." align="right" width="139" height="139"/>

This repository was previously named `AFSCDataReport` and
`GAP_BS_DataReport`.

The scripts therein reproducibly produce our annual data reports,
presentations, and other outreach documents from data to data product.

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov;
[@EmilyMarkowitz-NOAA](https://github.com/EmilyMarkowitz-NOAA))

**Liz Dawson** (Liz.Dawson AT noaa.gov;
[@liz-dawson-NOAA](https://github.com/liz-dawson-NOAA))

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

> This code is always in development. Find code used for various reports
> in the code
> [releases](https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report/releases).

## This code and the associated releases were used to develop the following reports, outreach documents, and presentations:

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-2021Community" class="csl-entry">

Britt, L. L., E. H. Markowitz, E. J. Dawson, N. Charriere, B. Prohaska,
S. Rohan, D. E. Stevenson, and L. L. Britt. 2021. 2021 northern Bering
Sea groundfish and crab trawl survey highlights. Outreach.

</div>

<div id="ref-2022Community" class="csl-entry">

Markowitz, E. H., E. J. Dawson, A. Anderson, N. Charriere, J. Richar, S.
Rohan, B. Prohaska, R. Haehn, and D. E. Stevenson. 2022a. 2022 northern
Bering Sea groundfish and crab trawl survey highlights. Outreach.

</div>

<div id="ref-2018EBS2022" class="csl-entry">

Markowitz, E. H., E. J. Dawson, N. Charriere, B. Prohaska, S. Rohan, R.
Haehn, D. E. Stevenson, and L. L. Britt. 2022b. Results of the 2018
eastern Bering Sea continental shelf bottom trawl survey of groundfish
and invertebrate fauna. U.S. Dep. Commer., NOAA Tech. Memo.

</div>

<div id="ref-2019NEBS2022" class="csl-entry">

Markowitz, E. H., E. J. Dawson, N. Charriere, B. Prohaska, S. Rohan, D.
E. Stevenson, and L. L. Britt. 2022e. Results of the 2019 eastern and
northern Bering Sea continental shelf bottom trawl survey of groundfish
and invertebrate fauna. U.S. Dep. Commer., NOAA Tech. Memo.

</div>

<div id="ref-2021NEBS2022" class="csl-entry">

Markowitz, E. H., E. J. Dawson, N. Charriere, B. Prohaska, S. Rohan, D.
E. Stevenson, and L. L. Britt. 2022d. Results of the 2021 eastern and
northern Bering Sea continental shelf bottom trawl survey of groundfish
and invertebrate fauna. U.S. Dep. Commer., NOAA Tech. Memo.

</div>

<div id="ref-2022NEBS2022" class="csl-entry">

Markowitz, E. H., E. J. Dawson, N. Charriere, B. Prohaska, S. Rohan, D.
E. Stevenson, and L. L. Britt. 2022c. Results of the 2022 eastern and
northern Bering Sea continental shelf bottom trawl survey of groundfish
and invertebrate fauna. U.S. Dep. Commer., NOAA Tech. Memo.

</div>

</div>

<!-- Use .bib file to cite reports in subsection titles -->

**Annual Bering Sea Data Report** (Markowitz et al. 2022c, 2022d, 2022e,
2022b)

-   09/20/21-09/24/21 North Pacific Fisheries Management Council
    Groundfish Plan Team Meeting
    [webpage](https://meetings.npfmc.org/Meeting/Details/2427) and
    [presentation](https://meetings.npfmc.org/CommentReview/DownloadFile?p=f48aa839-fdd1-4000-a556-2a3aa4521d8b.pdf&fileName=2021_EBSsurvey%20PRESENTATION.pdf)

-   09/19/22-09/23/22 North Pacific Fisheries Management Council
    Groundfish Plan Team Meeting
    [webpage](https://meetings.npfmc.org/Meeting/Details/2949) and
    [presentation](https://meetings.npfmc.org/CommentReview/DownloadFile?p=02e397c4-a1cc-46eb-b2ae-1c3cc368e682.pdf&fileName=2022_EBSsurvey_planteam.pdf)

**Northern Bering Sea Groundfish and Crab Trawl Survey Highlights**
(Britt et al. 2021; Markowitz et al. 2022a)

> This document is for informational purposes only and does not
> necessarily represent the views or official position of the Department
> of Commerce, the National Oceanic and Atmospheric Administration, or
> the National Marine Fisheries Service. Not to be cited without
> permission from the authors.

-   [11/4/2021 Presentation at the University of Alaska Fairbanks Strait
    Science Seminar](https://youtu.be/putpYJtPRF8)
-   11/4/2022 Presentation at the University of Alaska Fairbanks Strait
    Science Seminar (coming soon!)

## Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report/pulls),
[submit an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report/issues).

# R Version Metadata

``` r
sessionInfo()
```

    ## R version 4.2.0 (2022-04-22 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19044)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] RODBC_1.3-19           XML_3.99-0.12          flextable_0.8.2        httr_1.4.4             pingr_2.0.2            scales_1.2.1           reshape_0.8.9         
    ##  [8] ps_1.7.2               ggsn_0.5.0             digest_0.6.30          rlist_0.4.6.2          coldpool_2.1           reshape2_1.4.4         rgdal_1.5-32          
    ## [15] lubridate_1.8.0        fields_14.1            viridis_0.6.2          viridisLite_0.4.1      spam_2.9-1             here_1.0.1             ggthemes_4.2.4        
    ## [22] akgfmaps_2.2.1         stars_0.5-6            abind_1.4-5            shadowtext_0.1.2       sf_1.0-8               raster_3.6-3           sp_1.5-0              
    ## [29] gstat_2.1-0            ggspatial_1.1.6        classInt_0.4-8         readtext_0.81          stringr_1.4.1          readxl_1.4.1           tidyr_1.2.1           
    ## [36] readr_2.1.3            magrittr_2.0.3         googledrive_2.0.0      dplyr_1.0.10           plyr_1.8.7             knitcitations_1.0.12   NMFSReports_0.0.1.3   
    ## [43] ggridges_0.5.4         nmfspalette_0.0.0.9000 ggpubr_0.4.0           extrafont_0.18         magick_2.7.3           png_0.1-7              cowplot_1.1.1         
    ## [50] ggplot2_3.3.6          officedown_0.2.4       officer_0.4.4          rmarkdown_2.17         knitr_1.40            
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] uuid_1.1-0          backports_1.4.1     systemfonts_1.0.4   lwgeom_0.2-9        htmltools_0.5.3     fansi_1.0.3         memoise_2.0.1       tzdb_0.3.0         
    ##  [9] vroom_1.6.0         xts_0.12.2          extrafontdb_1.0     askpass_1.1         jpeg_0.1-9          colorspace_2.0-3    rappdirs_0.3.3      xfun_0.34          
    ## [17] crayon_1.5.2        jsonlite_1.8.3      zoo_1.8-11          glue_1.6.2          rvg_0.2.5           gtable_0.3.1        gargle_1.2.1        car_3.1-1          
    ## [25] Rttf2pt1_1.3.11     maps_3.4.0          DBI_1.1.3           bibtex_0.5.0        rstatix_0.7.0       Rcpp_1.0.9          units_0.8-0         bit_4.0.4          
    ## [33] foreign_0.8-83      proxy_0.4-27        dotCall64_1.0-2     intervals_0.15.2    RColorBrewer_1.1-3  FNN_1.1.3.1         wk_0.7.0            ellipsis_0.3.2     
    ## [41] farver_2.1.1        pkgconfig_2.0.3     janitor_2.1.0       utf8_1.2.2          tidyselect_1.2.0    rlang_1.0.6         munsell_0.5.0       cellranger_1.1.0   
    ## [49] tools_4.2.0         cachem_1.0.6        cli_3.4.1           generics_0.1.3      ggmap_3.0.0         broom_1.0.1         evaluate_0.17       fastmap_1.1.0      
    ## [57] yaml_2.3.6          RefManageR_1.4.0    bit64_4.0.5         processx_3.8.0      fs_1.5.2            zip_2.2.2           s2_1.1.0            purrr_0.3.5        
    ## [65] RgoogleMaps_1.4.5.3 xml2_1.3.3          compiler_4.2.0      rstudioapi_0.14     curl_4.3.3          e1071_1.7-12        ggsignif_0.6.4      spacetime_1.2-8    
    ## [73] tibble_3.1.8        stringi_1.7.8       gdtools_0.2.4       lattice_0.20-45     vctrs_0.5.0         pillar_1.8.1        lifecycle_1.0.3     data.table_1.14.4  
    ## [81] bitops_1.0-7        maptools_1.1-5      R6_2.5.1            KernSmooth_2.23-20  gridExtra_2.3       codetools_0.2-18    assertthat_0.2.1    openssl_2.0.4      
    ## [89] rprojroot_2.0.3     rjson_0.2.21        withr_2.5.0         parallel_4.2.0      hms_1.1.2           terra_1.6-17        class_7.3-20        snakecase_0.11.0   
    ## [97] carData_3.0-5       base64enc_0.1-3

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
