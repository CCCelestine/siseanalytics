% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univarie.r
\name{khi2}
\alias{khi2}
\title{Chi-Square Test}
\usage{
khi2(x, y)
}
\arguments{
\item{x}{categorical variable}

\item{y}{clusters variable}
}
\value{
A list with class "htest"
}
\description{
Chi-Square Test
}
\examples{
khi2(df_test$sexe,df_test$val_pred)
}
