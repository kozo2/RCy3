% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StyleValues.R
\name{getNodePosition}
\alias{getNodePosition}
\title{Get Node Position}
\usage{
getNodePosition(node.names = NULL, network = NULL, base.url = .defaultBaseUrl)
}
\arguments{
\item{node.names}{List of node names. Default is NULL for all nodes.}

\item{network}{(optional) Name or SUID of the network. Default is the 
"current" network active in Cytoscape.}

\item{base.url}{(optional) Ignore unless you need to specify a custom domain,
port or version to connect to the CyREST API. Default is http://localhost:1234
and the latest version of the CyREST API supported by this version of RCy3.}
}
\value{
Dataframe of x, y values
}
\description{
Retrieve the actual x,y position of specified nodes.
}
\examples{
\donttest{
getNodePosition()
}
}
