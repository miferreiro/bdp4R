% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TeeCSVFromSynsetFeatureVectorPipe.R
\docType{class}
\name{TeeCSVFromSynsetFeatureVectorPipe}
\alias{TeeCSVFromSynsetFeatureVectorPipe}
\title{Class to complete the csv with the preprocessed instance and synsets}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
TeeCSVFromSynsetFeatureVectorPipe$new(propertyName = "",
                                      alwaysBeforeDeps = list(),
                                      notAfterDeps = list())
}
\arguments{
\item{propertyName}{(character) Name of the property associated with the pipe.}

\item{alwaysBeforeDeps}{(list) The dependences alwaysBefore (pipes that must
be executed before this one).}

\item{notAfterDeps}{(list) The dependences notAfter (pipes that cannot be
executed after this one).}
}
\description{
Complete the csv with the preprocessed instance and synsets.
}
\details{
It is necessary to identify the properties associated with the
synsets that the instance will have, so as not to include them in the
data.frame.
}
\section{Inherit}{

This class inherit from \code{\link{PipeGeneric}} and implements the
\code{pipe} abstract function.
}

\section{Methods}{

\itemize{
\item{\bold{pipe}}{
Function that complete the csv with the preprocessed instance and synsets.
\itemize{
\item{\emph{Usage}}{

\code{pipe(instance, withData = TRUE, withSource = TRUE,
listPropertySynsets = c("synsetVector", "synsetFeatureVector"))}
}
\item{\emph{Value}}{

The instance with the modifications that have occurred in the pipe.
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{instance}}{
(Instance) Instance to preproccess.
}
\item{\strong{withData}}{
(logical) Indicate if the data is added to csv.
}
\item{\strong{withSource}}{
(logical) Indicate if the source is added to csv.
}
\item{\strong{listPropertySynsets}}{
(character) vector indicating properties related to synsets.
}
}
}
}
}
}
}

\seealso{
\code{\link{PipeGeneric}}, \code{\link{Instance}}
}
\keyword{datasets}
