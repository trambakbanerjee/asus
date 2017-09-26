<!-- README.md is generated from README.Rmd. Please edit that file -->
asus
====
[![Build Status](https://travis-ci.org/trambakbanerjee/asus.svg?branch=master)](https://travis-ci.org/trambakbanerjee/asus)

The goal of `asus` is to estimate a high-dimensional sparse parameter when along with the primary data we can also gather side information from secondary data sources. ASUS (Adpative SURE thresholding with Side Information) is an adaptive and robust methodology for leveraging the auxiliary data to improve the accuracy of existing methods. It is adaptive to both the unknown sparsity of the parameter and the degree of informativeness in the associated side information, and is robust in performance when pooling non-informative auxiliary data.

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
Adaptive SURE Thresholding with Side Information. Banerjee, T., Mukherjee, G. and Sun, W.
