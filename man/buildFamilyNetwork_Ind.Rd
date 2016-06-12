\name{buildFamilyNetwork_Ind}
\title{
Build Individual-Level Family Networks
}
\description{
This function produces an individual-level family network following the rules of name inheritance used in the Philippines. These rules were employed by Cruz, Labonne, and Querubin (2016) to construct family-level networks (where family names are the nodes; see the function buildFamilyNetwork_Fam to replicate their work).
}
\usage{
buildFamilyNetwork_Ind(data, ID, lastName, middleName, area = NULL, graph = TRUE, metadata = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame with individuals on the rows and attributes on the columns. One row per individual.
}
  \item{ID}{
A character string corresponding to the name of a unique identifier for individuals in data. Numeric identifiers work best. Default is NULL. If NULL, unique IDs generated.
}
  \item{lastName}{
Character string corresponding to the column in data of last names.
}
  \item{middleName}{
Character string corresponding to the name of the vector of middle names in data.
}
  \item{area}{
Character string corresponding to the name of a vector in data that distinguishes an area within which network ties should be drawn. Vector is coerced to factor. Default is NULL.
}
  \item{graph}{
Logical. Should an igraph object be produced. If false, a edge list is returned. Default is TRUE.
}
  \item{metadata}{
Logical. Should other variables in data be included in the igraph object as attributes. Default is TRUE unless graph equals FALSE.
}
}
\details{
This function constructs family networks, with individual people serving as nodes in the network, following the naming convention first exploited by Cruz, Labonne, and Querubin (2016). See their paper for details on the inheritance rules that make this possible. See Davidson, Hicken and Ravanilla (2016) for an example of this individual-level network.
}
\value{
Either an igraph object or edge list with two columns containing ego and alter ids.
}
\references{
Cruz, C., Labonne, J. & Querubin, P. 2016. Politician Family Networks and Electoral Outcomes.

Davidson, M., Hicken, H., & Ravanilla, Nico. 2016. Family Networks and Voting Behavior: Evidence from the Philippines.
}
\author{
Michael Davidson mwdavidson@ucsd.edu
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{buildFamilyNetwork_Fam}}, 
\code{\link{buildFamilyNetwork_Community}}
}
\examples{
# Load sample data
data(CARASI)

# Build the network producing an Igraph object with meta data
buildFamilyNetwork_Ind(data = CARASI, ID = "ID", lastName = "lastName", middleName = "middleName", area = "BARANGAY", graph = TRUE, metadata = TRUE)

# Build the network producing an Igraph object without meta data
buildFamilyNetwork_Ind(data = CARASI, ID = "ID", lastName = "lastName", middleName = "middleName", area = "BARANGAY", graph = TRUE, metadata = FALSE)

# Build the network producing an edge list
buildFamilyNetwork_Ind(data = CARASI, ID = "ID", lastName = "lastName", middleName = "middleName", area = "BARANGAY", graph = FALSE, metadata = FALSE)
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Network }
\keyword{ Family }
