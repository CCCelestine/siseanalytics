% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mesuresEval.R
\name{print.metrics}
\alias{print.metrics}
\title{Overload of print for displaying the "metrics" object}
\usage{
\method{print}{metrics}(obj)
}
\arguments{
\item{obj}{The print function will display the confusion matrix and the indicators when it receives a parameter of type metrics}
}
\description{
Overload of print for displaying the "metrics" object
}
\examples{
Obj2c <- EvalMetrics(pred_reel_2c$val_reel,pred_reel_2c$val_pred)
print(Obj2c)
}
