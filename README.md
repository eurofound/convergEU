  <!-- badges: start -->
  [![CRAN status](https://www.r-pkg.org/badges/version/convergEU)](https://cran.r-project.org/package=convergEU)
<!-- badges: end -->

![](inst/logoConvergEU9_github.png)  

# R package *convergEU*

Indicators and measures by country and time describe
what happens at economic and social levels. This package provides
functions to calculate several measures of convergence after imputing
missing values. The automated download of Eurostat data,
followed by the production of country fiches and indicator fiches,
makes possible to  automate the production of reports.
This is the development release.  

Some references  are:   

  *  [tutorial-conv.html](https://www.eurofound.europa.eu/system/files/2022-04/introduction-to-the-convergeu-package-0.6.4-tutorial-v2-apr2022.pdf)   
  *  [Eurofound working paper](https://www.eurofound.europa.eu/en/publications/eurofound-paper/2020/monitoring-upward-convergence-eu-r-convergeu-package) 
  *  [Eurofound research report](https://www.eurofound.europa.eu/en/publications/2018/upward-convergence-eu-concepts-measurements-and-indicators)       
  *  To prepare fiches without any R code, please visit the Eurofound official  webapge of the [**convergEU app**](https://www.eurofound.europa.eu/en/resources/convergence-monitoring-hub/perform-convergence-analysis-eu-using-convergeu-app)

The stable release of the R package is available at https://CRAN.R-project.org/package=convergEU

This project is a joint work with  **Eurofound** developed under **contract &#8470;
 18-3030-42**.<br>
<img src="inst/EF2015_Logo_Colour_rid.png" width="113"  height="75">  


<br>  

January 2024 -The package has been updated to add patterns (from 39 to 59) to the country dynamics analysis, maps have also been added to the fiches.

You may use  *devtools* to  install the current version under development:   

```
install.packages("devtools")
library(devtools)
install_github(repo="eurofound/convergEU",
              build_vignettes= TRUE,
              force= TRUE)
```


