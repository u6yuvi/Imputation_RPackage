\name{Impute}
\alias{Impute}
\title{Missing values Imputation }
\description{
This function does the imputation of dataset containing missing values in a manually or automated manner.
}
\usage{
Impute(data)
}

\arguments{
  \item{data}{
 A complete data matrix or dataframe. Variables can be numeric or  categorical.
}
}
\examples{
require(mtcars)
result = Impute(mtcars)

}


