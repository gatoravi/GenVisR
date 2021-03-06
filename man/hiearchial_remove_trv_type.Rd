% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/hiearchial_remove_trv_type.R
\name{hiearchial_remove_trv_type}
\alias{hiearchial_remove_trv_type}
\title{Hiearchical removal of MAF entries}
\usage{
hiearchial_remove_trv_type(data_frame, file_type)
}
\arguments{
\item{data_frame}{a data frame in long format with columns sample, gene, trv_type}

\item{file_type}{The type of file to act on one of 'MAF" or 'TGI'}
}
\value{
a data frame with multiple mutations in the same sample/gene collapsed on the most deleterious
}
\description{
Remove MAF entries with the same gene/sample in an ordered fashion such that the most deleterious are retained
}

