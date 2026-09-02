<!-- README.md is generated from README.Rmd. Please edit that file -->

# [AFSC RACE GAP Bering Sea Survey Data Report](link_repo) <img src="https://avatars.githubusercontent.com/u/91760178?s=96&amp;v=4" alt="Logo." align="right" width="139" height="139"/>

> This code is always in development. Find code used for various reports
> in the code
> [releases](https://github.com/afsc-gap-products/gap_bs_data_report/releases).

This repository was previously named `AFSCDataReport` and
`GAP_BS_DataReport`.

The scripts therein reproducibly produce our annual data reports,
presentations, and other outreach documents from data to data product.

## This code is primarily maintained by:

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

# Table of contents

``` r
toc <- strsplit(x = readtext::readtext(file = "./README.Rmd", verbosity = 0)[[2]], split = "\n")
toc <- toc[[1]][substr(x = toc[[1]], start = 1, stop = 1) == "#"]
toc <- toc[-c(1:3)]
toc_list <- toc
toc_list <- gsub(pattern = "### ", replacement = ">      - [*", x = toc_list, fixed = TRUE)
toc_list <- gsub(pattern = "## ", replacement = ">    - [*", x = toc_list, fixed = TRUE)
toc_list <- gsub(pattern = "# ", replacement = ">  - [*", x = toc_list, fixed = TRUE)
toc_link <- tolower(gsub(pattern = " ", replacement = "-", 
                          x = gsub(pattern = "#", replacement = "", 
                                   x = gsub(pattern = "# ", replacement = "", 
                                            x = toc, fixed = TRUE), fixed = TRUE)))
toc <- paste0(toc_list, "*](#", toc_link, ")", collapse = "\n")
```

> - [*This code and the associated releases were used to develop the
>   following reports, outreach documents, and
>   presentations:*](#this-code-and-the-associated-releases-were-used-to-develop-the-following-reports,-outreach-documents,-and-presentations:)
> - [*Suggestions and Comments*](#suggestions-and-comments)
> - [*R Version Metadata*](#r-version-metadata)
>   - [*NOAA README*](#noaa-readme)
>   - [*NOAA License*](#noaa-license)

## This code and the associated releases were used to develop the following reports, outreach documents, and presentations:

<div id="refs">

</div>

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

    ## R version 4.6.1 (2026-06-24 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ##   LAPACK version 3.12.1
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    
    ## 
    ## time zone: America/Los_Angeles
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] RODBC_1.3-26.2     XML_3.99-0.24      flextable_0.10.0   httr_1.4.8         pingr_2.0.5        scales_1.4.0       raster_3.6-32     
    ##  [8] sp_2.2-3           ps_1.9.3           digest_0.6.39      ggspatial_1.1.10   shadowtext_0.1.6   rlist_0.4.6.2      crabpack_1.0.0    
    ## [15] gapindex_3.1.0     coldpool_3.5-3     reshape2_1.4.5     lubridate_1.9.5    fields_17.3        RColorBrewer_1.1-3 spam_2.11-4       
    ## [22] gstat_2.1-6        ggthemes_6.0.0     akgfmaps_4.2.1     terra_1.9-46       stars_0.7-3        abind_1.4-8        sf_1.1-2          
    ## [29] readtext_0.92.1    stringr_1.6.0      janitor_2.2.1      viridis_0.6.5      viridisLite_0.4.3  here_1.0.2         readxl_1.5.0      
    ## [36] tidyr_1.3.2        readr_2.2.0        googledrive_2.1.2  dplyr_1.2.1        plyr_1.8.9         ggridges_0.5.7     ggpubr_1.0.0      
    ## [43] magick_2.9.1       png_0.1-9          cowplot_1.2.0      ggplot2_4.0.3      officedown_0.4.1   officer_0.7.6      rmarkdown_2.31    
    ## [50] knitr_1.51        
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] jsonlite_2.0.0          rstudioapi_0.19.0       magrittr_2.0.5          farver_2.1.2            fs_2.1.0               
    ##  [6] ragg_1.5.2              vctrs_0.7.3             memoise_2.0.1           sampling_2.11           askpass_1.2.1          
    ## [11] rstatix_1.1.0           htmltools_0.5.9         curl_8.0.0              broom_1.0.13            cellranger_1.1.0       
    ## [16] Formula_1.2-6           KernSmooth_2.23-27      htmlwidgets_1.6.4       keyring_1.4.1           zoo_1.9-0              
    ## [21] cachem_1.1.0            uuid_1.2-2              lifecycle_1.0.5         pkgconfig_2.0.3         R6_2.6.1               
    ## [26] fastmap_1.2.0           snakecase_0.11.1        rprojroot_2.1.1         textshaping_1.0.5       timechange_0.4.0       
    ## [31] compiler_4.6.1          gargle_1.6.1            proxy_0.4-29            intervals_0.15.5        bit64_4.8.4            
    ## [36] fontquiver_0.2.1        withr_3.0.3             S7_0.2.2                backports_1.5.1         carData_3.0-6          
    ## [41] DBI_1.3.0               maps_3.4.3              ggsignif_0.6.4          MASS_7.3-66             openssl_2.4.2          
    ## [46] rappdirs_0.3.4          classInt_0.4-11         tools_4.6.1             units_1.0-1             otel_0.2.0             
    ## [51] odbc_1.7.0              zip_3.0.2               glue_1.8.1              grid_4.6.1              getPass_0.2-4          
    ## [56] lpSolve_5.6.23          generics_0.1.4          gtable_0.3.6            tzdb_0.5.0              class_7.3-24           
    ## [61] data.table_1.18.6.1     hms_1.1.4               xml2_1.6.0              car_3.1-5               pillar_1.11.1          
    ## [66] vroom_1.7.1             lattice_0.23-1          FNN_1.1.4.1             bit_4.6.0               tidyselect_1.2.1       
    ## [71] rvg_0.4.2               fontLiberation_0.1.0    fontBitstreamVera_0.1.1 gridExtra_2.3.1         xfun_0.60              
    ## [76] stringi_1.8.9           yaml_2.3.12             evaluate_1.0.5          codetools_0.2-20        gdtools_0.5.1          
    ## [81] tibble_3.3.1            cli_3.6.6               systemfonts_1.3.2       processx_3.9.0          spacetime_1.3-4        
    ## [86] Rcpp_1.1.2              parallel_4.6.1          blob_1.3.0              dotCall64_1.2           ggiraph_0.9.6          
    ## [91] xts_0.14.2              e1071_1.7-17            crayon_1.5.3            purrr_1.2.2             rlang_1.3.0

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
