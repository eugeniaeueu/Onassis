% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/EntityFinder-methods.R
\docType{methods}
\name{annotateDF}
\alias{annotateDF}
\alias{annotateDF,EntityFinder,data.frame,character,CMoptions-method}
\title{\code{annotateDF}}
\usage{
annotateDF(object, descr_df, outDir = tempdir(), configOpt, cmDict)

\S4method{annotateDF}{EntityFinder,data.frame,character,CMoptions}(object,
  descr_df, outDir = tempdir(), configOpt, cmDict)
}
\arguments{
\item{object}{Instance of class \code{\link{EntityFinder-class}}}

\item{descr_df}{the table of text to annotate. The data frame should have identifiers in the first column and descriptions or text in the rest of the columns.}

\item{outDir}{the output directory}

\item{configOpt}{instance of class \code{\link{CMoptions-class}}}

\item{cmDict}{Object of type \code{\link{CMdictionary-class}} containing the reference to a previously created Conceptmapper dictionary. Alternatively the path to a Conceptmapper xml file can be passed.}
}
\value{
A data frame of annotations containing the sample name, the id of the OBO concept, the corresponding name, the part of the text containing the annotation
}
\description{
Method to find concepts from a Conceptmapper Dictionary of type \code{\link{CMdictionary}} contained in a given data frame, with a specified configuration of type \code{\link{CMoptions}}. This is a method of the \code{\link{EntityFinder-class}}
}
\examples{
obo <- system.file('extdata', 'sample.cs.obo', package='OnassisJavaLibs')
dict <- CMdictionary(inputFileOrDb=obo, outputDir=getwd(), synonymType='ALL')
opts <- CMoptions()
ef <- new('EntityFinder')
methylation <- readRDS(system.file('extdata', 'vignette_data',
'GEOmethylation.rds', package='Onassis'))
annotations <- annotateDF(ef, methylation[1:10, ], getwd(), opts, dict)
}
