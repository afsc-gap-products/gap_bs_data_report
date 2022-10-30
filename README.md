<!-- README.md is generated from README.Rmd. Please edit that file -->

# [AFSC RACE GAP Bering Sea Survey Data Report](%60r%20link_repo%60) <img src="https://avatars.githubusercontent.com/u/91760178?s=96&amp;v=4" alt="Logo." align="right" width="139" height="139"/>

This repository was previously named `AFSCDataReport` and
`GAP_BS_DataReport`.

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov;
[@EmilyMarkowitz-NOAA](https://github.com/EmilyMarkowitz-NOAA))

**Liz Dawson** (Liz.Dawson AT noaa.gov;
[@liz-dawson-NOAA](https://github.com/liz-dawson-NOAA))

Alaska Fisheries Science Center,

National Marine Fisheries Service,

National Oceanic and Atmospheric Administration,

Seattle, WA 98195

> This document is for informational purposes only and does not
> necessarily represent the views or official position of the Department
> of Commerce, the National Oceanic and Atmospheric Administration, or
> the National Marine Fisheries Service. Not to be cited without
> permission from the authors.

> This code is always in development. Find code used for various reports
> in the code
> [releases](https://github.com/EmilyMarkowitz-NOAA/gap_bs_data_report/releases).

## This code and the associated releases were used to develop the following reports, outreach documents, and presentations:

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-2021Community" class="csl-entry">

Britt, L. L., E. H. Markowitz, E. J. Dawson, N. Charriere, B. Prohaska,
S. Rohan, D. E. Stevenson, and L. L. Britt. 2021. Results of the 2021
eastern and northern bering sea continental shelf bottom trawl survey of
groundfish and invertebrate fauna. U.S. Dep. Commer., NOAA Tech. Memo.

</div>

<div id="ref-2022Community" class="csl-entry">

Markowitz, E. H., E. J. Dawson, A. Anderson, N. Charriere, J. Richar, S.
Rohan, B. Prohaska, R. Haehn, and D. E. Stevenson. 2022a. Results of the
2021 eastern and northern bering sea continental shelf bottom trawl
survey of groundfish and invertebrate fauna. U.S. Dep. Commer., NOAA
Tech. Memo.

</div>

<div id="ref-2018EBS2022" class="csl-entry">

Markowitz, E. H., E. J. Dawson, N. Charriere, B. Prohaska, S. Rohan, R.
Haehn, D. E. Stevenson, and L. L. Britt. 2022b. Results of the 2018
eastern bering sea continental shelf bottom trawl survey of groundfish
and invertebrate fauna. U.S. Dep. Commer., NOAA Tech. Memo.

</div>

<div id="ref-2019NEBS2022" class="csl-entry">

Markowitz, E. H., E. J. Dawson, N. Charriere, B. Prohaska, S. Rohan, D.
E. Stevenson, and L. L. Britt. 2022e. Results of the 2019 eastern and
northern bering sea continental shelf bottom trawl survey of groundfish
and invertebrate fauna. U.S. Dep. Commer., NOAA Tech. Memo.

</div>

<div id="ref-2021NEBS2022" class="csl-entry">

Markowitz, E. H., E. J. Dawson, N. Charriere, B. Prohaska, S. Rohan, D.
E. Stevenson, and L. L. Britt. 2022d. Results of the 2021 eastern and
northern bering sea continental shelf bottom trawl survey of groundfish
and invertebrate fauna. U.S. Dep. Commer., NOAA Tech. Memo.

</div>

<div id="ref-2022NEBS2022" class="csl-entry">

Markowitz, E. H., E. J. Dawson, N. Charriere, B. Prohaska, S. Rohan, D.
E. Stevenson, and L. L. Britt. 2022c. Results of the 2022 eastern and
northern bering sea continental shelf bottom trawl survey of groundfish
and invertebrate fauna. U.S. Dep. Commer., NOAA Tech. Memo.

</div>

</div>

<!-- Use .bib file to cite reports in subsection titles -->

**Annual Bering Sea Data Reports** (Markowitz et al. 2022c, 2022d,
2022e, 2022b)

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

-   [11/4/2021 Presentation at the University of Alaska Fairbanks Strait
    Science Seminar](https://youtu.be/putpYJtPRF8)
-   11/4/2022 Presentation at the University of Alaska Fairbanks Strait
    Science Seminar (coming soon!)

## Suggestions and Comments

If you feel that the data or metadata can be improved, please create a
pull request, [submit an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](%60r%20paste0(link_repo,%20%22/issues%22)%60).

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
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.2.0  magrittr_2.0.3  fastmap_1.1.0   cli_3.4.1      
    ##  [5] tools_4.2.0     htmltools_0.5.3 rstudioapi_0.14 yaml_2.3.5     
    ##  [9] stringi_1.7.8   rmarkdown_2.17  knitr_1.40      stringr_1.4.1  
    ## [13] xfun_0.33       digest_0.6.29   rlang_1.0.6     evaluate_0.17

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
