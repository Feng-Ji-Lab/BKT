% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models-Model.R
\name{fetch_dataset}
\alias{fetch_dataset}
\title{Fetch a dataset}
\usage{
fetch_dataset(object, link, loc)
}
\arguments{
\item{object}{A BKT model object. The model can use the fetched dataset for fitting or other tasks.}

\item{link}{Character. The URL where the dataset is located. This must be a publicly accessible URL.}

\item{loc}{Character. The local file path where the dataset will be saved. The dataset will
be stored at this location after download.}
}
\value{
None. The function downloads the data file to the specified location.
}
\description{
Fetch a dataset from an online source.
This function downloads a dataset from a provided URL and saves it to a specified location
on the local system. The dataset must be publicly accessible, without requiring any
password or authentication. It can then be used for further analysis or modeling.
}
\examples{
\donttest{
model <- bkt()
fetch_dataset(model, "http://example.com/dataset.csv", "data.csv")
}
}
