# Family Networks

<img align="right" src="http://pages.ucsd.edu/~mwdavids/img/Bon-Ot.png" width=250>

R package for constructing family networks following the naming convention used in the Philippines. This method is used and described in [Cruz, Labonne, and Querubin (2016)](http://cesicruz.com/papers/FamilyNetworksCLQ.pdf) & [Querubin (2011)](https://sites.google.com/site/pabloquerubin/research/Querubin_Term_Limits.pdf?attredirects=0).

## Rules

It is possible to approximate actual family networks in the Philippines because of strict patterns of name inheritance. Everyone is born with a given (first) name and two surnames--their mother and father's family names (which in turn came from their fathers). When women marry, they take their husband's family name (the husband's father's family name) and retain their father's family name.

## Functions
I provide three functions for constructing different types of networks. 

  - *buildFamilyNetwork_Ind* builds *individual* level networks, meaning that individuals are the nodes in the network.
  
  - *buildFamilyNetwork_Fam* builds *family* level networks, so that each family name is a node in the network.
  
  - *buildFamilyNetwork_Community* builds *community* level networks. Communities, such as barangay, are the nodes in the network and relations between individuals in the different communities determine the presence or absence, and strength of ties between them.

## Installation
To install the current version of FamilyNetworks, run the following code.

```R
library(devtools)
install_github("mwdavids/FamilyNetworks")
```

## References

Cruz, Cesi, Julien Labonne, and Pablo Querubin (2016). “Politician Family Networks and Electoral Outcomes”. Revise and Resubmit at *American Economic Review*.

Querubin, Pablo (2011). “Political Reform and Elite Persistence: Term Limits and Political Dynasties in the Philippines”. *Working paper*.
