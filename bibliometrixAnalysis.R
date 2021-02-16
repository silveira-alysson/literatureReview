#install.packages('bibliometrix', dependencies=TRUE)
#install.packages('rstatix', dependencies=TRUE)

library(bibliometrix)

file <- "C:/Users/alyss/Downloads/scopus (3).bib"
M <- convert2df(file = file, dbsource = "scopus", format = "bibtex")


#Prints the summary and plot graphs 
results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)



#Summary of Most Cited Authors
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])


#Summary of Authors and Papers most cited by other authors that are included in this collection
CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]
CR$Papers[1:10,]


#Author's dominance factor 
DF <- dominance(results, k = 10)
DF


# Bornmann's impact indices:
authors=gsub(","," ",names(results$Authors)[1:10])
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
indices$H


#Top-Authors' Productivity over the Time
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)


# Create a co-citation network
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)


# Conceptual Structure using keywords (method="CA")
CS <- conceptualStructure(M,field="ID", method="CA", minDegree=10, clust=5, stemming=FALSE, labelsize=10, documents=10)

# Create a historical citation network
options(width=130)
histResults <- histNetwork(M, min.citations = 50, sep = ";")
net <- histPlot(histResults, n=300, size = 5, labelsize=5)


