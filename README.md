<!-- README.md is generated from README.Rmd. Please edit that file -->
asus
====
[![Build Status](https://travis-ci.org/trambakbanerjee/asus.svg?branch=master)](https://travis-ci.org/trambakbanerjee/asus)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/asus)](https://cran.r-project.org/package=asus)
![](http://cranlogs.r-pkg.org/badges/grand-total/asus)

The R-package `asus` implements the ASUS (Adpative SURE thresholding with Side Information) procedure which is an adaptive and robust methodology for leveraging auxiliary data to improve the accuracy of existing methods for estimating a high-dimensional sparse parameter. ASUS is built to estimate a high-dimensional sparse parameter when along with the primary data we can also gather side information from secondary data sources. It is adaptive to both the unknown sparsity of the parameter and the degree of informativeness in the associated side information, and is robust in performance when pooling non-informative auxiliary data.

Installation
-----------
You can:

1. install the development version of `asus`

 ```R
   devtools::install_github("trambakbanerjee/asus")
   ```

Usage
-------

See the included vignette [`demo-asus`](http://htmlpreview.github.com/?https://github.com/trambakbanerjee/asus/blob/master/demo-asus.html) for illustrative examples.

References
--------
[Adaptive Sparse Estimation with Side Information](http://www-bcf.usc.edu/~wenguans/Papers/ASUS.pdf).                                     
Banerjee, T., Mukherjee, G. and Sun, W.
