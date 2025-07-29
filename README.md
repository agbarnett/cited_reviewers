# Cited reviewers study

Research Questions: 

1. Do peer reviewers give a more or less favourable recommendation when they are cited in the article? PICO: Peer reviewers (Population), Cited (Intervention), Not Cited (Control), Favourable recommendation (Outcome).

2. Do peer reviewers give a more or less their favourable recommendation when their review includes a citation to their own research? PICO: Peer reviewers (Population), Self-citation included (Intervention), No self-citation (Control), Changed recommendation (Outcome).

Data from open peer reviews published on [F1000Research](https://f1000research.com/), [Wellcome Open Research](https://wellcomeopenresearch.org/), [Gates Open Research](https://gatesopenresearch.org/) and [Open Research Europe](https://open-research-europe.ec.europa.eu/). Bibliographic data were downloaded from [_OpenAlex_](https://openalex.org/).

The _R_ files are in order of execution with a prefix of:

* 0_ searching the four journals' APIs for potentially relevant articles
* 1_ reporting the search results
* 2_ get paper-specific data from the journals' APIs
* 3_ add reviewers' country
* 4_ get the reviewers' papers from _Open Alex_
* 5_ match the papers in the papers' reference lists and reviewers' papers to find self-citations and if reviewers were cited (analysis data)
* 6_ run the statistical models and summaries
* 7_ report on the manual checks of the data; text analysis of reviewers' comments
* 99_ other files, including the simulated sample size calculation

The subfolders are:

* `R` for R functions.
* `figures` for figures.
* `checks` for random checks of the data.
* `as_predicted` PDF submitted to the [As Predicted](https://aspredicted.org/rn8vg.pdf) site in May 2024.
* `data` data in _R_ format.

The data does not include the review text as this made the file too large for _GitHub_.

<details><summary>R version and packages</summary>
<p>

```
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 22631)

Matrix products: default


locale:
[1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8   
[3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                      
[5] LC_TIME=English_Australia.utf8    

time zone: Australia/Brisbane
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] gridExtra_2.3             ggplot2_3.5.1             glmnet_4.1-8             
[4] Matrix_1.7-0              quanteda.textstats_0.97.2 quanteda_4.1.0           
[7] tidytext_0.4.2            stringr_1.5.1             dplyr_1.1.4              

loaded via a namespace (and not attached):
 [1] janeaustenr_1.0.0  utf8_1.2.4         generics_0.1.3     shape_1.4.6.1      stringi_1.8.4     
 [6] lattice_0.22-6     digest_0.6.36      magrittr_2.0.3     evaluate_0.24.0    grid_4.4.1        
[11] iterators_1.0.14   pkgload_1.4.0      fastmap_1.2.0      foreach_1.5.2      survival_3.6-4    
[16] stopwords_2.3      fansi_1.0.6        scales_1.3.0       codetools_0.2-20   cli_3.6.3         
[21] rlang_1.1.4        tokenizers_0.3.0   munsell_0.5.1      splines_4.4.1      withr_3.0.1       
[26] yaml_2.3.10        tools_4.4.1        colorspace_2.1-1   fastmatch_1.1-4    vctrs_0.6.5       
[31] R6_2.6.1           lifecycle_1.0.4    pkgconfig_2.0.3    pillar_1.9.0       gtable_0.3.5      
[36] TeachingDemos_2.13 rsconnect_1.3.1    glue_1.7.0         Rcpp_1.0.13        xfun_0.47         
[41] tibble_3.2.1       tidyselect_1.2.1   rstudioapi_0.16.0  knitr_1.48         farver_2.1.2      
[46] htmltools_0.5.8.1  SnowballC_0.7.1    labeling_0.4.3     rmarkdown_2.28     compiler_4.4.1    
[51] nsyllable_1.0.1 
```

</p>
</details>