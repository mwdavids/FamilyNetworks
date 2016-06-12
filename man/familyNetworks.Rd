\name{familyNetworks}
\docType{package}
\title{
familyNetworks
}
\description{
This package facilitates the construction of inter-marriage networks according to the naming convention commonly used in the Philippines. See Cruz, Labonne and Querubin (2016) for more information on this convention and an example of its proper use to construct networks of families. The package also constructs networks of individuals (\code{buildFamilyNetworks_Ind}) and networks of communities (\code{buildFamilyNetwork_Community}) based on the same conventions.
}
\details{
To build the different types of family networks these functions construct, the user needs a \code{data.frame} or \code{data.table} with individuals on the rows and characteristics of the individuals on the columns. At a minimum the user needs the individuals middle and last names. 
}
\author{
Author: Michael Davidson mwdavidson@ucsd.edu

Maintainer: Michael Davidson mwdavidson@ucsd.edu
}
\references{
Cruz, C., Labonne, J. & Querubin, P. 2016. Politician Family Networks and Electoral Outcomes.
}

\keyword{ Family }
\keyword{ Network }
\keyword{ Marriage }
% \seealso{
% ~~ Optional links to other man pages, e.g. ~~
% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
% }
\examples{
# Load the sample data
data(CARASI)

# Build a network of individuals in the same community (barangay)
buildFamilyNetwork_Fam(data = CARASI, lastName = "lastName", middleName = "middleName", area = "BARANGAY", graph = TRUE)

# Build a network of families who reside in the same municipalities
buildFamilyNetwork_Fam(data = CARASI, lastName = "lastName", middleName = "middleName", area = "MUNICIPALITY", graph = TRUE)

# Build a network of communities (barangay) within municipalities
buildFamilyNetwork_Community(data = CARASI, lastName = "lastName", middleName = "middleName", ID = "ID", byArea = "BARANGAY", withinArea = "MUNICIPALITY", graph = TRUE)
}
