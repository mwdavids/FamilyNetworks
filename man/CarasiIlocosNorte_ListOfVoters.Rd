\name{CARASI List of Voters}
\docType{data}
\title{
An old list of registered voters from a Philippine Municipality
}
\description{
An old list of registered voters for a single municipality with three barangay (communities/villages). 
}
\usage{data("CARASI")}
\format{
  A data.table of 1286 individuals (observations) with columns containing an arbitrary ID, their names (complete, first, middle, last, suffix), their precinct, barangay, municipality and province.
  % \describe{
  %   \item{\code{x}}{a numeric vector}
  %   \item{\code{y}}{a numeric vector}
  % }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Philippines Commission on Elections, 2013.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(CARASI)

str(CARASI)
}
\keyword{datasets}
