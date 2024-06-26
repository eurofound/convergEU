% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dev_mean_plot.R
\name{dev_mean_plot}
\alias{dev_mean_plot}
\title{Plot of deviations from the mean}
\usage{
dev_mean_plot(
  myTB,
  timeName = "time",
  time_0 = NA,
  time_t = NA,
  countries = c(NA, NA),
  indiType = "highBest",
  displace = 0.25,
  axis_name_y = "Countries",
  val_alpha = 0.95,
  debug = FALSE
)
}
\arguments{
\item{myTB}{a dataset time by countries}

\item{timeName}{name of the variable representing time}

\item{time_0}{starting time}

\item{time_t}{ending time}

\item{countries}{selection of countries to display; NA means all countries}

\item{indiType}{the type of indicator  "highBest" or "lowBest"}

\item{displace}{graphical displacement}

\item{axis_name_y}{name of the axis}

\item{val_alpha}{transparency value in (0,1].}

\item{debug}{a flag to get debug information as msg component}
}
\value{
a list with ggplot2 graphical object within res component
}
\description{
Negative deviations and positive deviations are added over years
and plotted by country.
}
\examples{

\dontrun{
# Example 1
# A dataset in the format time by countries:
require(tibble)
testTB <- dplyr::tribble(
~time, ~countryA ,  ~countryB,  ~countryC,
2000,     0.8,   2.7,    3.9,
2001,     1.2,   3.2,    4.2,
2002,     0.9,   2.9,    4.1,
2003,     1.3,   2.9,    4.0,
2004,     1.2,   3.1,    4.1,
2005,     1.2,   3.0,    4.0
)
# Plot the deviations from the mean for all countries:
resDMP <- dev_mean_plot(testTB,
                        timeName="time",
                        displace = 0.25,
                        axis_name_y = "Countries")
resDMP

# Plot by considering only some of the years:
resDMP1 <- dev_mean_plot(testTB,
                       timeName="time",
                       time_0 = 2002,
                       time_t = 2004,
                       displace = 0.25,
                       axis_name_y = "Countries")
resDMP1

# Example 2
# The Eurofound dataset "emp_20_64_MS":
myTB1 <- emp_20_64_MS

# Plot the deviations from the mean only for some of the member states:
resDMP2 <- dev_mean_plot(myTB1,
                       timeName="time",
                       time_0 = 2005,
                       time_t = 2010,
                       countries= c("AT","BE","IT"),
                       displace = 0.25,
                       axis_name_y = "Countries")
resDMP2
}

}
\references{
{\url{https://www.eurofound.europa.eu/system/files/2022-04/introduction-to-the-convergeu-package-0.6.4-tutorial-v2-apr2022.pdf}}
}
