# Variable Penalty Dynamic Time Warping


<!-- badges: start -->
  [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/VPdtw)](https://cran.r-project.org/package=VPdtw)
  [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/VPdtw)](https://cran.r-project.org/package=VPdtw)
  [![metacran downloads](https://cranlogs.r-pkg.org/badges/last-month/VPdtw)](https://cran.r-project.org/package=VPdtw)

  <!-- badges: end -->
  
Variable Penalty Dynamic Time Warping for aligning GC-MS chromatograms to a master signal and more. With the appropriate penalty this method performs good alignment without altering the shape of peaks in chromatography data.

## Installation
Please install the package in R directly using the commands:

```R
install.packages("devtools")
devtools::install_github("david-clifford/VPdtw")
```

## References
Clifford, D., Stone, G., Montoliu, I., Rezzi, S., Martin, F. P., Guy, P., ... & Kochhar, S. (2009). Alignment using variable penalty dynamic time warping. Analytical chemistry, 81(3), 1000-1007. doi:http://dx.doi.org/10.1021/ac802041e

Clifford, D., & Stone, G. (2012). Variable Penalty Dynamic Time Warping Code for Aligning Mass Spectrometry Chromatograms in R. Journal of Statistical Software, 47(8), 1 - 17. doi:http://dx.doi.org/10.18637/jss.v047.i08

---

This package was created by [David Clifford](https://github.com/david-clifford/VPdtw) and Glenn Stone,
but is now maintained by [Ethan Bass](https://github.com/ethanbass).
