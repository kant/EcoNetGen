EcoNetGen
================

[![Travis-CI Build
Status](https://travis-ci.org/cboettig/EcoNetGen.svg?branch=master)](https://travis-ci.org/cboettig/EcoNetGen)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/cboettig/EcoNetGen?branch=master&svg=true)](https://ci.appveyor.com/project/cboettig/EcoNetGen)
[![Coverage
Status](https://img.shields.io/codecov/c/github/cboettig/EcoNetGen/master.svg)](https://codecov.io/github/cboettig/EcoNetGen?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/EcoNetGen)](https://cran.r-project.org/package=EcoNetGen)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![DOI](https://zenodo.org/badge/116610054.svg)](https://zenodo.org/badge/latestdoi/116610054)

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Installation

You can install EcoNetGen from github with:

``` r
# install.packages("devtools")
devtools::install_github("cboettig/EcoNetGen")
```

## Example

This is a basic example which generates a network. See `?netgen` for
documentation describing the parameter arguments.

``` r
library(EcoNetGen)
network <- netgen(n_modav = c(250, 20), 
                  cutoffs = c(50, 5), 
                  net_type = 41, 
                  net_degree = 10,
                  net_rewire = c(0.07,0.2),
                  mod_probs = c(0.2, 0.0, 0.3, 0.3, 0.2, 0.0, 0.0))
#> 
#> module count = 4 
#> average degree = 8.736 
#> average module size = 62.5 
#> number of components = 1 
#> size of largest component = 250
```

We can plot the resulting `igraph` as an adjacency matrix:

``` r
adj_plot(network)
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

Network `igraph` objects can also be plotted using the standard `igraph`
plotting routines, for example:

``` r
library(igraph)
#> 
#> Attaching package: 'igraph'
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
plot(network, vertex.size= 0, vertex.label=NA, 
     edge.color = rgb(.22,0,1,.02), vertex.shape="none", 
     edge.curved =TRUE, layout = layout_with_kk)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

And we can compute common statistics from igraph as well. Here we
confirm that clustering by “edge betweeness” gives us the expected
number of modules:

``` r
community <- cluster_edge_betweenness(as.undirected(network))
length(groups(community))
#> [1] 4
```

We can check the size of each module as well:

``` r
module_sizes <- sapply(groups(community), length)
module_sizes
#>  1  2  3  4 
#> 53 52 63 82
mean(module_sizes)
#> [1] 62.5
```

``` r
mean(degree(as.undirected(network)))
#> [1] 8.752
```
