% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multivarie.R
\name{AOV2}
\alias{AOV2}
\title{ANOVA}
\usage{
AOV2(X, y)
}
\arguments{
\item{X}{data.frame, with quantitative variable}

\item{y}{integer,  with class variable}
}
\value{
aov, multivariate variance analysis
}
\description{
ANOVA
}
\examples{
X=data.frame(fromage["calories"],fromage["magnesium"],fromage["lipides"],fromage["retinol"])
y=fromage$groupes.cah
AOV2(X,y)
}
