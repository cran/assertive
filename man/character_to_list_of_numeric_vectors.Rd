\name{character_to_list_of_numeric_vectors}
\alias{character_to_list_of_numeric_vectors}
\title{Convert a character vector to a list of numeric vectors.}
\usage{
  character_to_list_of_numeric_vectors(x)
}
\arguments{
  \item{x}{Input to convert.}
}
\value{
  A list of numeric vectors.
}
\description{
  Split strings by character, then convert to numeric.
}
\examples{
\dontrun{
character_to_list_of_numeric_vectors(c("123", "4567a"))
}
}
\seealso{
  \code{\link[base]{strsplit}} and
  \code{\link[base]{as.numeric}}.
}

