% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ContractionsPipe.R
\docType{class}
\name{ContractionsPipe}
\alias{ContractionsPipe}
\title{Class to find and/or replace the contractions on the data of a instance}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
ContractionsPipe$new(propertyName = "contractions",
                     propertyLanguageName = "language",
                     alwaysBeforeDeps = list("GuessLanguagePipe"),
                     notAfterDeps = list())
}
\arguments{
\item{propertyName}{(character) Name of the property associated with the pipe.}

\item{propertyLanguageName}{(character) Name of the language property.}

\item{alwaysBeforeDeps}{(list) The dependences alwaysBefore (pipes that must
be executed before this one).}

\item{notAfterDeps}{(list) The dependences notAfter (pipes that cannot be
executed after this one).}
}
\description{
This class allows you to preprocess the data of an instance to
find the contractions that are in it. Optionally, you can decide whether to
replace the data contractions or not.
}
\details{
This class needs files in json format that will contain the
contractions to be located and the string that will replace them. For this
it is necessary that the instance contains a property that indicates the
language of the data to be able to correctly choose the list of contractions
that apply to the data. The format of the file names of the resources has to
be: contr.xxx.json (Being xxx the value of the language property of the
instance).

To indicate the path where the associated resources are located, the
configuration file is used. It is necessary to indicate in the section called
resourcesPath, the path of resourcesContractionsPath.

The pipe will invalidate the instance in the moment that the resulting data is
empty.
}
\section{Inherit}{

This class inherits from \code{\link{PipeGeneric}} and implements the
\code{pipe} abstract function.
}

\section{Methods}{

\itemize{
\item{\bold{pipe}}{
Function that preprocesses the instance to obtain/replace the contractions.
The contractions found in the pipe are added to the list of properties of
the Instance. If the replaceContractions parameter is TRUE, the instance
data will be modified by replacing the contractions found.
\itemize{
\item{\emph{Usage}}{

\code{pipe(instance, replaceContractions = TRUE)}
}
\item{\emph{Value}}{

The instance with the modifications that have occurred in the pipe.
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{instance}}{
(Instance) Instance to preproccess.
}
\item{\strong{replaceContractions}}{
(logical) Indicates if the contractions are replace or not.
}
}
}
}
}

\item{\bold{findContraction}}{
Function that checks if the contractions is in the data.
\itemize{
\item{\emph{Usage}}{

\code{findContraction(data, contraction)}
}
\item{\emph{Value}}{

TRUE or FALSE depending on whether the contraction is on the data.
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{data}}{
(character) Text in which the contraction is searched.
}
\item{\strong{contraction}}{
(character) Indicates the contraction to find.
}
}
}
}
}

\item{\bold{replaceContraction}}{
Function that replaces the contraction in the data for the extendedContraction.
\itemize{
\item{\emph{Usage}}{

\code{replaceContraction(contraction, extendedContraction, data)}
}
\item{\emph{Value}}{

The data with the contractions replaced.
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{contraction}}{
(character) Indicates the contraction to remove.
}
\item{\strong{extendedContraction}}{
(character) Indicates the string to replace for the contraction found.
}
\item{\strong{data}}{
(character) Text in which contractions will be replaced.
}
}
}
}
}

\item{\bold{getPropertyLanguageName}}{
Getter of name of property language.
\itemize{
\item{\emph{Usage}}{

\code{getPropertyLanguageName()}
}
\item{\emph{Value}}{

Value of name of property language.
}
}
}

\item{\bold{getResourcesContractionsPath}}{
Getter of path of contractions resources.
\itemize{
\item{\emph{Usage}}{

\code{getResourcesContractionsPath()}
}
\item{\emph{Value}}{

Value of path of contractions resources.
}
}
}
}
}

\section{Private fields}{

\itemize{
\item{\bold{propertyLanguageName}}{
 (character) The name of property about language.
}
\item{\bold{resourcesContractionsPath}}{
 (character) The path where are the resources.
}
}
}

\seealso{
\code{\link{PipeGeneric}}, \code{\link{Instance}},
\code{\link{ResourceHandler}}
}
\keyword{datasets}
