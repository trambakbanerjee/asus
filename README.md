<!-- README.md is generated from README.Rmd. Please edit that file -->
asus
====
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/asus)](https://cran.r-project.org/package=asus)
![](http://cranlogs.r-pkg.org/badges/grand-total/asus)

The R-package `asus` implements the ASUS (Adpative SURE thresholding with Side Information) procedure that estimates a high-dimensional sparse parameter when along with the primary data we can also gather side information from secondary data sources. ASUS is an adaptive and robust methodology for leveraging auxiliary data to improve the accuracy of existing methods for estimating a high-dimensional sparse parameter. It is adaptive to both the unknown sparsity of the parameter and the degree of informativeness in the associated side information, and is robust in performance when pooling non-informative auxiliary data.

See the reference for more details around the ASUS procedure.

Installation
-----------
You can:

1. install the release version of `asus` from [CRAN](https://CRAN.R-project.org/package=asus) with `install.packages("asus")`.

2. install the development version of `asus`

 ```R
   devtools::install_github("trambakbanerjee/asus")
   ```

Usage
-------

See the included vignette [`demo-asus`](http://htmlpreview.github.com/?https://github.com/trambakbanerjee/asus/blob/master/demo-asus.html) for illustrative examples.

References
--------
[Banerjee, T., Mukherjee, G. and Sun, W. Adaptive Sparse Estimation with Side Information. Journal of the American Statistical Association (2020), Volume 115, no. 532, Pages 2053-2067](https://www.tandfonline.com/doi/abs/10.1080/01621459.2019.1679639).                                     
 
