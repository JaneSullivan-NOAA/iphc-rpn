# Project description
Use International Pacific Halibut Commission (IPHC) setline survey effort data to calculate Relative Population Number (RPN) indices by species/complex.

# Session info

```
> devtools::session_info()
- Session info -------------------------------------------------------------------------------------------------------------------
 setting  value                       
 version  R version 3.6.3 (2020-02-29)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/Anchorage           
 date     2020-07-20                  

- Packages -----------------------------------------------------------------------------------------------------------------------
 package     * version date       lib source        
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.3)
 backports     1.1.7   2020-05-13 [1] CRAN (R 3.6.3)
 blob          1.2.1   2020-01-20 [1] CRAN (R 3.6.3)
 broom         0.7.0   2020-07-09 [1] CRAN (R 3.6.3)
 callr         3.4.3   2020-03-28 [1] CRAN (R 3.6.3)
 cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.6.3)
 cli           2.0.2   2020-02-28 [1] CRAN (R 3.6.3)
 colorspace    1.4-1   2019-03-18 [1] CRAN (R 3.6.3)
 crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.3)
 DBI           1.1.0   2019-12-15 [1] CRAN (R 3.6.3)
 dbplyr        1.4.4   2020-05-27 [1] CRAN (R 3.6.3)
 desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.3)
 devtools    * 2.3.0   2020-04-10 [1] CRAN (R 3.6.3)
 digest        0.6.25  2020-02-23 [1] CRAN (R 3.6.3)
 dplyr       * 1.0.0   2020-05-29 [1] CRAN (R 3.6.3)
 ellipsis      0.3.1   2020-05-15 [1] CRAN (R 3.6.3)
 fansi         0.4.1   2020-01-08 [1] CRAN (R 3.6.3)
 forcats     * 0.5.0   2020-03-01 [1] CRAN (R 3.6.3)
 fs          * 1.4.2   2020-06-30 [1] CRAN (R 3.6.3)
 generics      0.0.2   2018-11-29 [1] CRAN (R 3.6.3)
 ggplot2     * 3.3.2   2020-06-19 [1] CRAN (R 3.6.3)
 glue          1.4.1   2020-05-13 [1] CRAN (R 3.6.3)
 gtable        0.3.0   2019-03-25 [1] CRAN (R 3.6.3)
 haven         2.3.1   2020-06-01 [1] CRAN (R 3.6.3)
 hms           0.5.3   2020-01-08 [1] CRAN (R 3.6.3)
 httr          1.4.1   2019-08-05 [1] CRAN (R 3.6.3)
 jsonlite      1.7.0   2020-06-25 [1] CRAN (R 3.6.3)
 lifecycle     0.2.0   2020-03-06 [1] CRAN (R 3.6.3)
 lubridate     1.7.9   2020-06-08 [1] CRAN (R 3.6.3)
 magrittr      1.5     2014-11-22 [1] CRAN (R 3.6.3)
 memoise       1.1.0   2017-04-21 [1] CRAN (R 3.6.3)
 modelr        0.1.8   2020-05-19 [1] CRAN (R 3.6.3)
 munsell       0.5.0   2018-06-12 [1] CRAN (R 3.6.3)
 pillar        1.4.6   2020-07-10 [1] CRAN (R 3.6.3)
 pkgbuild      1.1.0   2020-07-13 [1] CRAN (R 3.6.3)
 pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 3.6.3)
 pkgload       1.1.0   2020-05-29 [1] CRAN (R 3.6.3)
 prettyunits   1.1.1   2020-01-24 [1] CRAN (R 3.6.3)
 processx      3.4.3   2020-07-05 [1] CRAN (R 3.6.3)
 ps            1.3.3   2020-05-08 [1] CRAN (R 3.6.3)
 purrr       * 0.3.4   2020-04-17 [1] CRAN (R 3.6.3)
 R6            2.4.1   2019-11-12 [1] CRAN (R 3.6.3)
 Rcpp          1.0.5   2020-07-06 [1] CRAN (R 3.6.3)
 readr       * 1.3.1   2018-12-21 [1] CRAN (R 3.6.3)
 readxl      * 1.3.1   2019-03-13 [1] CRAN (R 3.6.3)
 remotes       2.1.1   2020-02-15 [1] CRAN (R 3.6.3)
 reprex        0.3.0   2019-05-16 [1] CRAN (R 3.6.3)
 rlang         0.4.7   2020-07-09 [1] CRAN (R 3.6.3)
 rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.3)
 rstudioapi    0.11    2020-02-07 [1] CRAN (R 3.6.3)
 rvest         0.3.5   2019-11-08 [1] CRAN (R 3.6.3)
 scales        1.1.1   2020-05-11 [1] CRAN (R 3.6.3)
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.3)
 stringi       1.4.6   2020-02-17 [1] CRAN (R 3.6.2)
 stringr     * 1.4.0   2019-02-10 [1] CRAN (R 3.6.3)
 testthat      2.3.2   2020-03-02 [1] CRAN (R 3.6.3)
 tibble      * 3.0.3   2020-07-10 [1] CRAN (R 3.6.3)
 tidyr       * 1.1.0   2020-05-20 [1] CRAN (R 3.6.3)
 tidyselect    1.1.0   2020-05-11 [1] CRAN (R 3.6.3)
 tidyverse   * 1.3.0   2019-11-21 [1] CRAN (R 3.6.3)
 tinytex       0.24    2020-06-20 [1] CRAN (R 3.6.3)
 usethis     * 1.6.1   2020-04-29 [1] CRAN (R 3.6.3)
 utf8          1.1.4   2018-05-24 [1] CRAN (R 3.6.3)
 vctrs         0.3.1   2020-06-05 [1] CRAN (R 3.6.3)
 withr         2.2.0   2020-04-20 [1] CRAN (R 3.6.3)
 xfun          0.15    2020-06-21 [1] CRAN (R 3.6.3)
 xml2          1.3.2   2020-04-23 [1] CRAN (R 3.6.3)

[1] C:/Users/Jane.Sullivan/Work/R/win-library/3.6
[2] C:/Program Files/R/R-3.6.3/library
```