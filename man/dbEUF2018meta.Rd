% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbEUF2018meta.R
\docType{data}
\name{dbEUF2018meta}
\alias{dbEUF2018meta}
\title{Metainformation on Eurofound dataset}
\format{
A dataset  with 13 rows and 10 columns
}
\source{
\url{https://www.eurofound.europa.eu/en/surveys/about-eurofounds-surveys/data-availability}
}
\usage{
data(dbEUF2018meta)
}
\description{
Metainformation about data provided by Eurofound currently up to 2018.
Metainformation is provided for two dimensions: quality of life and working conditions.
For each dimension, metainformation for several indicators is reported, e.g. coding in database, official code,
measurement unit, source organization, disaggregation and bookmark URL.
Variable names often end with characters denoting scales:
The following convention holds for names of variables:
"_p"	percentage, "_i" index, "_pop" persons, "_h" hours,
"_eur" euros, "_pps"	purchasing power standards,
"_y"	years.
}
\examples{

data(dbEUF2018meta)
names(dbEUF2018meta)

\dontrun{
View(dbEUF2018meta)
}

# Visualize metainformation on the indicators stored in the dataset:
dbEUF2018meta$INDICATOR

# Visualize the indicators coding in database:
dbEUF2018meta$Code_in_database

# Visuazlize the indicators official code:
dbEUF2018meta$Official_code


}
\references{
{\url{https://www.eurofound.europa.eu/system/files/2022-04/introduction-to-the-convergeu-package-0.6.4-tutorial-v2-apr2022.pdf}}
}
\keyword{datasets}
