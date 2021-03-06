% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FindUrlPipe.R
\docType{class}
\name{FindUrlPipe}
\alias{FindUrlPipe}
\title{Class to find and/or remove the urls on the data}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
FindUrlPipe$new(propertyName = "URLs",
                alwaysBeforeDeps = list(),
                notAfterDeps = list("FindUrlPipe"))
}
\arguments{
\item{propertyName}{(character) Name of the property associated with the pipe.}

\item{alwaysBeforeDeps}{(list) The dependences alwaysBefore (pipes that must
be executed before this one).}

\item{notAfterDeps}{(list) The dependences notAfter (pipes that cannot be
executed after this one).}
}
\description{
This class allows you to preprocess the data of an instance to
find the urls that are in it. Optionally, you can decide whether to
remove the data urls or not.
}
\details{
The regular expressions indicated in the \code{URLPatterns}
variable are used to identify urls.

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
Function that preprocesses the instance to obtain/remove the users.
\itemize{
\item{\emph{Usage}}{

\code{pipe(instance,
     removeUrl = TRUE,
     URLPatterns = list(self$URLPattern, self$EmailPattern),
     namesURLPatterns = list("UrlPattern","EmailPattern"))}
}
\item{\emph{Value}}{

The instance with the modifications that have occurred in the pipe.
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{instance}}{
(Instance) Instance to preproccess.
}
\item{\strong{removeUrl}}{
(logical) Indicates if the urls are removed.
}
\item{\strong{URLPatterns}}{
(list) The regex to find urls.
}
\item{\strong{namesURLPatterns}}{
(list) The names of regex.
}
}
}
}
}

\item{\bold{findUrl}}{
Function that find the urls in the data.
\itemize{
\item{\emph{Usage}}{

\code{findHashtag(pattern, data)}
}
\item{\emph{Value}}{

List with urls found.
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{pattern}}{
(character) Regex to find urls.
}
\item{\strong{data}}{
(character) Text to find urls.
}
}
}
}
}

\item{\bold{removeUrl}}{
Function that removes the urls in the data.
\itemize{
\item{\emph{Usage}}{

\code{removeUrl(pattern, data)}
}
\item{\emph{Value}}{

The data with urls removed.
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{pattern}}{
(character) Regex to find urls.
}
\item{\strong{data}}{
(character) Text in which hashtags will be urls.
}
}
}
}
}

\item{\bold{putNamesURLPattern}}{
Set the names to url patterns result.
\itemize{
\item{\emph{Usage}}{

\code{putNamesURLPattern(resultOfURLPatterns)}
}
\item{\emph{Value}}{

Value of resultOfURLPatterns variable with the names of url pattern.
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{resultOfURLPatterns}}{
(list) List with urls found.
}
}
}
}
}

\item{\bold{getURLPatterns}}{
Getter of url patterns.
\itemize{
\item{\emph{Usage}}{

\code{getURLPatterns()}
}
\item{\emph{Value}}{

Value of url patterns.
}
}
}

\item{\bold{getNamesURLPatterns}}{
Getter of name of urls.
\itemize{
\item{\emph{Usage}}{

\code{getNamesURLPatterns()}
}
\item{\emph{Value}}{

Value of name of urls.
}
}
}
}
}

\section{Public fields}{

\itemize{
\item{\bold{URLPattern}}{
 (character) Regular expression to detect urls.
}
\item{\bold{EmailPattern}}{
 (character) Regular expression to detect emails.
}
}
}

\section{Private fields}{

\itemize{
\item{\bold{URLPatterns}}{
 (list) Regular expressions used to detect urls.
}
\item{\bold{namesURLPatterns}}{
 (list) Names of regular expressions that are used to identify urls.
}
}
}

\seealso{
\code{\link{PipeGeneric}}, \code{\link{Instance}}
}
\keyword{datasets}
