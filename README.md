
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multides

<!-- badges: start -->
<!-- badges: end -->

The `multides` package is a companion R package of the project
“MULTI-DES: Multilevel Design Parameters and Effect Size Benchmarks for
Students’ Competencies,” as well as its follow-up project “MULTI-DES 2:
Multilevel Design Parameters for Sample Size Planning of Randomized
Intervention Studies in Preschool, Elementary and Secondary School.”

`multides` compiles tools that were developed to facilitate the analyses
conducted within the MULTI-DES project framework. The functions may be
used to replicate the R code that accompanies the manuscripts prepared
within MULTI-DES. All R scripts are shared via the Open Science
Framework (OSF; see below).

Nevertheless, the application scope of `multides` is not limited to the
analyses specific to MULTI-DES. The functions provided can also be used
in other contexts of (multilevel) data analysis, for instance, to easily
generate overviews of descriptive statistics, single- and multilevel
correlation matrices, or–most importantly–to calculate (multilevel)
design parameters such as intraclass correlation coefficients and
explained variances by covariates at each hierarchical level with
corresponding standard errors, based on the variance components
estimated from single- or multilevel models.

## The MULTI-DES Project

MULTI-DES is funded by the German Research Foundation (DFG) and aims at
investigating (1) single- and multilevel design parameters that are
needed to efficiently plan adequately powered individually and cluster
randomized trials on various outcomes (viz., achievement/cognitive
outcomes and socio-emotional learning outcomes) in preschool, elementary
and secondary school, as well as (2) effect size benchmarks in terms of
academic growth and performance gaps between schools or policy-relevant
groups to appropriately interpret and communicate the results of such
studies. In MULTI-DES, rich data from several German (longitudinal)
large-scale assessments were used to apply multilevel and structural
equation modeling. For more information on MULTI-DES, visit
<https://www.uni-potsdam.de/en/quantmethoden/forschung>.

### Publications and Preprints

Brunner, M., Stallasch, S. E., Artelt, C., Hedges, L. V., & Lüdtke, O.
(2024). An individual participant data meta-analysis to support power
analyses for randomized intervention studies in preschool: Cognitive and
socio-emotional learning outcomes. PsyArXiv.
<https://doi.org/10.31234/osf.io/dkw42> <br>
[Preprint](https://osf.io/preprints/psyarxiv/dkw42)

Brunner, M., Stallasch, S. E., & Lüdtke, O. (2023). Empirical benchmarks
to interpret intervention effects on student achievement in elementary
and secondary school: Meta-analytic results from Germany. *Journal of
Research on Educational Effectiveness*, *17*(1), 1–39.
<https://doi.org/10.1080/19345747.2023.2175753> <br> [R
code](https://osf.io/wp5rf)

Stallasch, S. E., Lüdtke, O., Artelt, C., & Brunner, M. (2021).
Multilevel design parameters to plan cluster-randomized intervention
studies on student achievement in elementary and secondary school.
*Journal of Research on Educational Effectiveness*, *14*, 172–206.
<https://doi.org/10.1080/19345747.2020.1823539> <br> [R
code](https://osf.io/2w8nt)

Stallasch, S. E., Lüdtke, O., Artelt, C., Hedges, L. V., & Brunner, M.
(2024). Single- and multilevel perspectives on covariate selection in
randomized intervention studies on student achievement. *Educational
Psychology Review*, *36*, 112.
<https://doi.org/10.1007/s10648-024-09898-7> <br> [R
code](https://osf.io/nhx4w)

## Installation

You can install the development version of `multides` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sophiestallasch/multides")
```

## Usage

``` r
library(multides)

# calculate a range of descriptive statistics for a series of numeric variables,
# grouped by school type
describe_stats(studach, gender:read, ts_name)
#> # A tibble: 20 × 14
#>    ts_name      variable     n missings   mean    sd   min    p25 median     p75
#>    <fct>        <chr>    <dbl>    <dbl>  <dbl> <dbl> <dbl>  <dbl>  <dbl>   <dbl>
#>  1 vocational   gender    5410   0       0.496 0.500   0    0      0      1     
#>  2 vocational   ses       5168   0.0447 -1.97  5.15  -21.1 -5.48  -1.90   1.56  
#>  3 vocational   math      5148   0.0484 -4.84  4.71  -21.7 -7.97  -4.76  -1.71  
#>  4 vocational   read      5147   0.0486 -4.66  5.18  -22.2 -8.18  -4.70  -1.05  
#>  5 intermediate gender    5372   0       0.500 0.500   0    0      0      1     
#>  6 intermediate ses       5090   0.0525 -1.81  5.10  -22.8 -5.19  -1.79   1.63  
#>  7 intermediate math      5081   0.0542 -4.37  4.63  -19.6 -7.50  -4.37  -1.23  
#>  8 intermediate read      5106   0.0495 -3.78  5.02  -21.2 -7.15  -3.76  -0.354 
#>  9 multitrack   gender    5228   0       0.503 0.500   0    0      1      1     
#> 10 multitrack   ses       4979   0.0476 -1.31  5.06  -19.9 -4.61  -1.28   2.08  
#> 11 multitrack   math      4992   0.0451 -3.32  4.67  -21.2 -6.44  -3.30  -0.0765
#> 12 multitrack   read      4980   0.0474 -2.75  5.05  -19.1 -6.15  -2.76   0.721 
#> 13 comprehensi… gender    5366   0       0.510 0.500   0    0      1      1     
#> 14 comprehensi… ses       5091   0.0512 -0.795 5.18  -18.8 -4.36  -0.855  2.69  
#> 15 comprehensi… math      5116   0.0466 -2.04  4.75  -19.0 -5.28  -2.09   1.32  
#> 16 comprehensi… read      5095   0.0505 -0.982 5.23  -19.8 -4.56  -0.983  2.61  
#> 17 academic     gender    5494   0       0.509 0.500   0    0      1      1     
#> 18 academic     ses       5214   0.0510  1.20  5.12  -17.2 -2.21   1.24   4.68  
#> 19 academic     math      5248   0.0448  2.06  4.77  -16.4 -1.14   2.10   5.24  
#> 20 academic     read      5230   0.0481  4.16  5.25  -12.2  0.443  4.05   7.77  
#> # ℹ 4 more variables: max <dbl>, mean_h <dbl>, skewness <dbl>, kurtosis <dbl>

# calculate multilevel correlations between group means at the school level
correlate_ml(studach, gender:read, id_sch)
#> Correlation computed with
#> • Method: 'pearson'
#> • Missing treated using: 'pairwise.complete.obs'
#> # A tibble: 4 × 5
#>   variable  gender     ses    math    read
#>   <chr>      <dbl>   <dbl>   <dbl>   <dbl>
#> 1 gender   NA      -0.0475 -0.0323  0.0129
#> 2 ses      -0.0475 NA       0.884   0.886 
#> 3 math     -0.0323  0.884  NA       0.974 
#> 4 read      0.0129  0.886   0.974  NA
```

## Help & Suggestions

If you encounter a bug, have questions or suggestions for improvement,
please file an issue on
[GitHub](https://github.com/sophiestallasch/multides/issues) or [email
me](mailto:sophie.stallasch@gmail.com), possibly including a minimal
reproducible example.
