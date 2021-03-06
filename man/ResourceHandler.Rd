% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ResourceHandler.R
\docType{class}
\name{ResourceHandler}
\alias{ResourceHandler}
\title{Class to manage differents types of resources}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
ResourceHandler$new(propertyName,
                alwaysBeforeDeps,
                notAfterDeps)
}
\arguments{
\item{propertyName}{(character) Name of the property associated with the pipe.}

\item{alwaysBeforeDeps}{(list) The dependences alwaysBefore (pipes that must
be executed before this one).}

\item{notAfterDeps}{(list) The dependences notAfter (pipes that cannot be
executed after this one).}
}
\description{
Class to manage different types of resources.
}
\details{
It is a class that allows you to store the resources that are needed
in the pipes to avoid having to repeatedly read from the file. File resources
of type json are read and stored in memory.
}
\section{Methods}{

\itemize{
\item{\bold{isLoadResource}}{
From the resource path, it is checked if they have already been loaded. In
this case, the list of the requested resource is returned. Otherwise, the
resource variable is added to the list of resources, and the resource list is
returned. In the event that the resource file does not exist, NULL is returned.
\itemize{
\item{\emph{Usage}}{

\code{isLoadResource(pathResource)}
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{pathResource}}{
(character) Resource file path.
}
}
}
}
}

\item{\bold{getResources}}{
Getter of resources variable.
\itemize{
\item{\emph{Usage}}{

\code{getResources()}
}
\item{\emph{Value}}{

Value of resources variable.
}
}
}

\item{\bold{setResources}}{
Setter of resources.
\itemize{
\item{\emph{Usage}}{

\code{setResources(resources)}
}
\item{\emph{Arguments}}{
\itemize{
\item{\strong{resources}}{
(list) The new value of resources.
}
}
}
}
}

\item{\bold{getNamesResources}}{
Getter of names of resources.
\itemize{
\item{\emph{Usage}}{

\code{getNamesResources()}
}
\item{\emph{Value}}{

Value of names of resources.
}
}
}
}
}

\section{Private fields}{

\itemize{
\item{\bold{resources}}{
 (list) Variable that stores the lists of the different types of resources.
}
}
}

\keyword{datasets}
