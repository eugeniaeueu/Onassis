% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/similarity-methods.R
\docType{methods}
\name{similarityInstance}
\alias{similarityInstance}
\alias{similarityInstance<-}
\alias{similarityInstance,Similarity-method}
\alias{similarityInstance<-,Similarity-method}
\title{\code{similarityInstance}}
\usage{
similarityInstance(object)

similarityInstance(object) <- value

\S4method{similarityInstance}{Similarity}(object)

\S4method{similarityInstance}{Similarity}(object) <- value
}
\arguments{
\item{object}{Instance of the class \code{\link{Similarity-class}}}

\item{value}{the reference to a java Similarity object}
}
\value{
The java reference to an object of class Similarity

The object of class Similarity with a new instance of the java Similarity class
}
\description{
This method retrieves the java object referencing the Similarity class
}
\examples{
sim <- new('Similarity')
similarityInstance(sim)
sim <- new('Similarity')
similarityInstance(sim) <-.jnew('iit/comp/epigen/nlp/similarity/Similarity')
}
