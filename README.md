# Condemned To Leisure: UK Time Use in Lockdown 2020
 Online Code and Appendices for M2 dissertation of James Alster, submitted to the EHESS in autumn 2021.

The dissertation itself can be found in folder Dissertation/ .
Appendix C is provided as an .odt document.

# Instructions to reproduce the analysis: 
 
 1. Obtain the dataset from the UK data service (institutional account required) at https://beta.ukdataservice.ac.uk/datacatalogue/doi/?id=8741#0

 2. Save the dataset (stata format) to data/uk_3wave_caddi_data.dta 

 3. Execute file optimal_matching.R (this writes a clustered version of the dataset to folder data/)

 4. The code to reproduce the plots found in the dissertation, as well as quoted figures, can be found in the other .R files, which are labelled by section and subdivided according to the sections of the text.

 5. Appendix C is printed from the model caches (not provided here). Executing all the code will fill the folder cache/ with cache files that can be printed with the relevant RMarkdown file in folder print_tables_appendix_C/. The tables can also be printed in .docx format using this methods.

Note that due to the random MCMC sampling of the models, model output will marginally vary from the values provided in the original text.

File global_variables.R contains colour palettes and convenience functions.

File process_repsondent_data.R processes the raw data and exposes re-coded variabels as objects used in the other files.

Folders cache/ and data/ are intentionally empty, they store objects created in the running of the analysis.

Stan Version: 2.12.0

# Output of sessioninfo(): 

R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252 
[2] LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252
[4] LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] patchwork_1.1.1   lubridate_1.7.10  brms_2.15.0       Rcpp_1.0.6       
 [5] tidybayes_2.3.1   colorspace_2.0-1  dendextend_1.15.1 cluster_2.1.2    
 [9] TraMineR_2.2-1    haven_2.3.1       forcats_0.5.1     stringr_1.4.0    
[13] dplyr_1.0.6       purrr_0.3.4       readr_1.4.0       tidyr_1.1.3      
[17] tibble_3.1.2      ggplot2_3.3.3     tidyverse_1.3.1  

loaded via a namespace (and not attached):
  [1] readxl_1.3.1         backports_1.2.1      Hmisc_4.5-0         
  [4] plyr_1.8.6           igraph_1.2.6         splines_4.0.5       
  [7] svUnit_1.0.6         crosstalk_1.1.1      rstantools_2.1.1    
 [10] inline_0.3.17        digest_0.6.27        htmltools_0.5.1.1   
 [13] viridis_0.6.1        rsconnect_0.8.17     fansi_0.4.2         
 [16] magrittr_2.0.1       checkmate_2.0.0      modelr_0.1.8        
 [19] RcppParallel_5.1.4   matrixStats_0.58.0   xts_0.12.1          
 [22] prettyunits_1.1.1    jpeg_0.1-8.1         rvest_1.0.0         
 [25] ggdist_2.4.0         xfun_0.23            callr_3.7.0         
 [28] crayon_1.4.1         jsonlite_1.7.2       lme4_1.1-27         
 [31] survival_3.2-11      zoo_1.8-9            glue_1.4.2          
 [34] gtable_0.3.0         V8_3.4.2             distributional_0.2.2
 [37] pkgbuild_1.2.0       rstan_2.21.2         abind_1.4-5         
 [40] scales_1.1.1         mvtnorm_1.1-1        DBI_1.1.1           
 [43] miniUI_0.1.1.1       viridisLite_0.4.0    xtable_1.8-4        
 [46] htmlTable_2.1.0      foreign_0.8-81       Formula_1.2-4       
 [49] StanHeaders_2.21.0-7 stats4_4.0.5         DT_0.18             
 [52] htmlwidgets_1.5.3    httr_1.4.2           threejs_0.3.3       
 [55] arrayhelpers_1.1-0   RColorBrewer_1.1-2   ellipsis_0.3.2      
 [58] pkgconfig_2.0.3      loo_2.4.1            farver_2.1.0        
 [61] nnet_7.3-16          dbplyr_2.1.1         utf8_1.2.1          
 [64] tidyselect_1.1.1     rlang_0.4.11         reshape2_1.4.4      
 [67] later_1.2.0          munsell_0.5.0        cellranger_1.1.0    
 [70] tools_4.0.5          cli_2.5.0            generics_0.1.0      
 [73] broom_0.7.6          ggridges_0.5.3       fastmap_1.1.0       
 [76] processx_3.5.2       knitr_1.33           fs_1.5.0            
 [79] nlme_3.1-152         mime_0.10            projpred_2.0.2      
 [82] xml2_1.3.2           compiler_4.0.5       bayesplot_1.8.0     
 [85] shinythemes_1.2.0    rstudioapi_0.13      curl_4.3.1          
 [88] gamm4_0.2-6          png_0.1-7            reprex_2.0.0        
 [91] stringi_1.5.3        ps_1.6.0             Brobdingnag_1.2-6   
 [94] lattice_0.20-41      Matrix_1.3-3         nloptr_1.2.2.2      
 [97] markdown_1.1         shinyjs_2.0.0        vctrs_0.3.8         
[100] pillar_1.6.1         lifecycle_1.0.0      bridgesampling_1.1-2
[103] data.table_1.14.0    httpuv_1.6.1         R6_2.5.0            
[106] latticeExtra_0.6-29  promises_1.2.0.1     gridExtra_2.3       
[109] codetools_0.2-18     boot_1.3-28          colourpicker_1.1.0  
[112] MASS_7.3-54          gtools_3.8.2         assertthat_0.2.1    
[115] withr_2.4.2          shinystan_2.5.0      mgcv_1.8-35         
[118] parallel_4.0.5       hms_1.0.0            grid_4.0.5          
[121] rpart_4.1-15         coda_0.19-4          minqa_1.2.4         
[124] shiny_1.6.0          base64enc_0.1-3      dygraphs_1.1.1.6 
