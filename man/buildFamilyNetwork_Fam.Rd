\name{buildFamilyNetwork_Fam}
\title{
Build Family Name-Level Family Networks
}
\description{
This function produces a family-level marriage network following the rules of name inheritance used in the Philippines. These rules were employed by Cruz, Labonne, and Querubin (2016) to construct family networks where family names are the nodes and marriages are the Edges.
}
\usage{
buildFamilyNetwork_Fam(data, lastName, middleName, area = NULL, graph = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame with individuals on the rows and attributes on the columns.
}
  \item{lastName}{
Character string corresponding to the column in data of last names.
}
  \item{middleName}{
Character string corresponding to the column in data of middle names.}
  \item{area}{
Character string corresponding to the name of a vector in data that distinguishes an area within which network ties should be drawn. Vector is coerced to factor. Default is NULL.
}
  \item{graph}{
Logical. Should an igraph object be produced. If false, an edge list is returned. Default is TRUE.
}
}
\details{
This function constructs family networks, with family names serving as nodes in the network, following the naming convention first exploited by Cruz, Labonne, and Querubin (2016). See their paper for details on the inheritance rules that make this possible.
}
\value{
Either an igraph object or edge list (as data.table) with two columns containing ego and alter ids.}
\references{
Cruz, C., Labonne, J. & Querubin, P. 2016. Politician Family Networks and Electoral Outcomes.
}
\author{
Michael Davidson mwdavidson@ucsd.edu
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{buildFamilyNetwork_Ind}}, 
\code{\link{buildFamilyNetwork_Community}}
}
\examples{
# Load provided sample data
data(CARASI)

# Build the network producing an Igraph object
buildFamilyNetwork_Fam(data = CARASI, lastName = "lastName", middleName = "middleName", area = "BARANGAY", graph = TRUE)

# Build the network producing an edge list
buildFamilyNetwork_Fam(data = CARASI, lastName = "lastName", middleName = "middleName", area = "BARANGAY", graph = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Family }
\keyword{ Network }
