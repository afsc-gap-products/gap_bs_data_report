# AFSC Survey Data Reports

Here are the basic files you need to create reproducible repeatable (e.g., annual) reports in R Markdown for AFSC Surveys! 

> Code is still in development

**Emily Markowitz** (Emily.Markowitz AT noaa.gov)

Alaska Fisheries Science Center, 

National Marine Fisheries Service, 

National Oceanic and Atmospheric Administration,

Seattle, WA 98195


## How to download this:

```r
install.packages("usethis")

library(usethis) # Automate package and project setup tasks that are otherwise performed manually.

usethis::use_course(url = 'https://github.com/emilyhmarkowitz/AFSCDataReport/archive/master.zip', 
                    destdir = "your/local/directory/")
```

## What you'll need: 

```r
library(devtools)

devtools::install_github("emilyhmarkowitz/RMarkReports")

devtools::install_github("nmfs-general-modeling-tools/nmfspalette")

```

## For creating similar, but totally-different-and-your-own reproducible R Markdown Tech Memos: 

```r
install.packages("usethis")

library(usethis) # Automate package and project setup tasks that are otherwise performed manually.

usethis::use_course(url = 'https://github.com/emilyhmarkowitz/NOAATechMemoStarterKit/archive/master.zip', 
                    destdir = "your/local/directory/")

```


## How this is designed to work: 

Once you have built this reproducible report, the objective to to only ever run the "run.R" file. Think of the run.R file as your skeleton, such that it connects all other scripts. Beyond minor changes or improvements to the report, nothing outside of the run.R file should change. Things kept in the run.R file include (for example):

 - Author names, year of the report, etc. (since those might change from year to year)
 - Commands that run other scripts (e.g., source() or rmarkdown::render())
 - Anything that is way easier to run from the run file because of random technical bugs. 

Consider all else sacred and only to be touched by someone who really knows what they are doing. 

## How this is structured

Main file

 - **/citationStyles**, contains possible citation style libraries (*.cls) to choose from (aka, how your citations will be styled.)
 - **/img**, images that need to be in the report. Below I've already added the images that go into the frontm atter of the report. 
    - **DeptOfCommerce.jpg**
    - **noaa-gray.png**
 - **/IncludeInGitHub** As a NOAA user, you MUST include these files in your GitHub repositories. I've added them here for you:
    - **README.md**
    - **LICENSE.md** 
 - **/output** Where all of your content from your run is saved. It's organized like this:
    - **/YYY-MM-DD** 
       - **/chapters**, where all content going directly into your report will be saved (e.g., .docx, .pdf).
       - **/metadata**, where information about your run and the packages you used is saved. 
       - **/plots**, where .rdata and other versions of your plots will be saved (additonal to those saved for the report in the '/chapters' folder).
       - **/rawdata**, where all raw data you used in your report will be saved. Just good house keeping.
       - **/rscripts**, where all the rscripts you used in your run are saved
       - **/tables**, where additional verisons of your tables will be saved. I like to keep "raw" (nothing rounded or made pretty) and "print" (basically what the table would look like in the report) version of my tables so I can refer to them later or share them with people checking my work/who need table verisons of the data. 
 - **/packrat**, A backup of all of the r packages (managed by the "packrat" package) used to create this starter kit. A good way to make sure your code will always work!
 - **/reference**, Where useful files providing context for choices are kept, e.g., NOAA TM Style Guide PDF, etc.
 - **/rscripts**, Where all of the rscripts and other programming files are kept. 
    - **run.R**, the skeleton of the report where EVERYTHING is run. 
    - **funcitons.R**, loads packages, saves files, creates file structure for the "**/output**" folder, houses local functions for the report.
    - **dataDownload.R**; NEVER SHARE THIS FILE TO GITHUB ETC. IF IT HAS PASSWORDS IN IT.
    - **data.R**, where data is loaded and generally manipulated/wrangled.
    - **[].Rmd**, the rmarkdown files used to actually generate the report's content into word.
    - **header.yaml**, a nifty thing that makes sure all of the same bibliographys, styles, etc. are used throughout the report for all of the RMarkdown files. 
    - **word-styles-reference.docx**, defines all of the style guide stuff (e.g., H1, p, footnotes) for the word document. 
    
## Tips and Tricks

**1. On your keyboard, press 'Shift' + 'Control' + 'O'.** This will open the report outline to the right of this window. Here you can see the complete layout of this script (and all scripts) and thus, this report you are creating!

## Citations/Work Cited

Here (in the "reference"" folder) is an EndNote Style Guides that follow the rules and guidelines listed in the "Tech Memo Guidelines.PDF" document from the NOAA scientific publications office. Add these to your EndNote library so you can export your citations in the correct format. 

NOAA Tech Memo Style Guide.ens (https://github.com/emilyhmarkowitz/NOAATechMemoStarterKit/blob/main/reference/NOAA%20Tech%20Memo%20Style%20Guide.ens)

Learn more about how to add these style guides to your EndNote database here: Video: https://www.youtube.com/watch?v=lQJzPjpgAcE; Article: https://utas.libguides.com/EndNote/Styles

Learn more about how to add citations to your report in R Markdown here: https://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html

## NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.
