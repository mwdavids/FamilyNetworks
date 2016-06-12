\name{buildFamilyNetwork_Community}
\title{
Build Community-Level Family Networks
}
\description{
This function produces a community-level network following the rules of name inheritance used in the Philippines. These rules were employed by Cruz, Labonne, and Querubin (2016) to construct family-level networks (where family names are the nodes; see the function buildFamilyNetwork_Fam to replicate their work). The nodes in the networks produced by this function are communities and the edges are weighted according to the total number of relatives individuals in either community have in the other.
}
\usage{
buildFamilyNetwork_Community(data, lastName, middleName, ID, byArea, withinArea, graph=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame with individuals on the rows and attributes on the columns. One row per individual.
}
  \item{lastName}{
Character string corresponding to the column in data of last names.
}
  \item{middleName}{
Character string corresponding to the name of the vector of middle names in data.
}
  \item{ID}{
Character string corresponding to the name of a unique identifier for individuals in data. Numeric identifiers work best. Default is NULL. If NULL, unique IDs generated.
}
  \item{byArea}{
Character string corresponding to the name of a vector in data that distinguishes an area that will constitute the nodes in the network. Vector is coerced to factor. Default is NULL. See details for more information.
}
  \item{withinArea}{
Character string corresponding to the name of a vector in data that distinguishes the area within which the nodes may be connected. Vector is coerced to factor. Default is NULL. If you wish to construct the network within the entire range of byAreas in data, point withinArea to a column that is constant across all these byAreas.  See details for more information.
}
  \item{graph}{
Logical. Should an igraph object be produced. If false, a edge list is returned. Default is TRUE.
}
}
\details{
This function constructs family networks, with communities serving as nodes in the network. The function uses the naming convention first exploited by Cruz, Labonne, and Querubin (2016) to identify relatives, but does so looking at inter-community relatives. See their paper for details on the inheritance rules that make this possible.

The byArea is the community that serves as a node in the network produced by this function. Ties between byAreas are only constructed between byAreas that share a withinArea.
}
\value{
Either an igraph object with weights or an edge list with three columns. The first two contain community identifiers (corresponding with unique values in byArea). The third column has the count of individuals in either community who have a relative in the other.
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

\section{Warning }{
The function works best when the input values are character strings. The function gets especially confused when byArea and withinArea are factors where every level is not represented in the data. If you wish to use a factor in this case, use droplevels on the vector first. 	
}

\seealso{
\code{\link{buildFamilyNetwork_Ind}}, 
\code{\link{buildFamilyNetwork_Family}}
}
\examples{
# Load sample data
data(CARASI)

# Igraph Object
buildFamilyNetwork_Community(data=CARASI, lastName="lastName", middleName="middleName", ID="ID", byArea="BARANGAY", withinArea="MUNICIPALITY", graph=TRUE)

# Edgelist
buildFamilyNetwork_Community(data=CARASI, lastName="lastName", middleName="middleName", ID="ID", byArea="BARANGAY", withinArea="MUNICIPALITY", graph=FALSE)

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Network }
\keyword{ Family }
