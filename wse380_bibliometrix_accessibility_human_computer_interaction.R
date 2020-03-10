install.packages("bibliometrix")
library(bibliometrix)
#getwd()
#getwd("C:/Users/dtsstudent/Downloads")    #  "C:/Users/dtsstudent/Documents"    
#to read in as biblio...
D <- readFiles("C:/Users/dtsstudent/Documents/savedrecs.bib")
View(D)
M <- convert2df(D,dbsource="isi", format="bibtex") 
###---BiblioAnalysis
install.packages('mice')
install.packages('rtools')
library('rtools')
library('mice') ##for imputing data sets (mutiple imput for computing)
###---BiblioAnalysis
DEresults <- biblioAnalysis(M, sep = ";")
##Summary Generic Function__
options(width=100)
SDE <-summary(object=DEresults, K=10, pause=FALSE)
##---Plot generic function, fig.width=7---
plot(x=DEresults, k=10, pause=FALSE)
##---M$CR[1]
##---Article Citation---
CDEa <-citations(M, field ="article", sep=";")
cbind(CDEa$Cited[1:10])
##Author Citation---
CDEA <-citations(M, field="author", sep=";")
cbind(CDEA$Cited[1:10])
### Local Author Citation
#CDE <- localCitations(M, sep=";")
CDE$Authors[1:10,]
CDE$Papers[1:10, ]
###Dominance Ranking
DDE <-dominance(DEresults, k=10)
DDE
###H Index---
DEHi<- Hindex(M, field="author", elements="FALLAN K", sep=";", years=20)
##Fallan's impact indices:
DEHi$H
##Fallan's Citations
DEHi$CitationList
##--h - index 10 authors
DEauthors=gsub(","," ",names(DEresults$Authors)[1:10])
DEindices<-Hindex(M, field="author",elements=DEauthors, sep = ";", years = 50)
DEindices$H
##AuthorProdOverTime, fig.height=6, fig.width=8
topAU<-authorProdOverTime(M, k = 10, graph=TRUE)
##Table: Author's Productivity per year 
head(topAU$dfAU)
topAU$dfAU
##Table: Author's Documents lIst
head(topAU$dfPapersAU)
##Lotka Law---
L <- lotka(DEresults)
L
## Author Productivity. Empirical Distribution
L$AuthorProd
## BEta coefficient Estimate
L$Beta
##Constant
L$C
#Goodness of Fit
L$R2  
#P-value of K-S two sample test
L$p.value
#####################Code below direct from Help..._
## ----Lotka law comparison, out.width='300px', dpi=200-----------------------------------------------------------------
# Observed distribution
Observed=L$AuthorProd[,3]
# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))
plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")
## ## ----Bipartite network------------------------------------------------------------------------------------------------
A <- cocMatrix(M, Field = "SO", sep = ".")
A
## ----Most relevant sources--------------------------------------------------------------------------------------------
sort(Matrix::colSums(A), decreasing = FALSE)[1:5]
##########Using their code above I get error message
## another assignment
A <- cocMatrix(M, Field = "CR", sep = ".  ")
## ---------------------------------------------------------------------------------------------------------------------
A <- cocMatrix(M, Field = "AU", sep = ";")
DEmeta<-metaTagExtraction(M, Field = "AU_CO", sep = ";")  
A <- cocMatrix(DEmeta, Field = "AU_CO", sep = ";")
#can change field to =  "DE", "ID", "AU"
#----version on references
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")
#---network based on authors
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
#----------
#code below from help, but is not working
net=networkPlot(NetMatrix, normalize = "salton",weighted=NULL, n=100, 
                Title="Authors' Coupling", 
                type="fruchterman",
                size=5, size.cex = T, 
                remove.multiple = TRUE, 
                labelsize=0.8, label.n=50, 
                label.cex = F)
#------------------------
# An example of a classical keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
##  Plot by Keywords
net=networkPlot(NetMatrix, normalize = "salton",weighted=NULL, n=100, 
                Title="Keword Co-Occurrance", 
                type="fruchterman",
                size=5, size.cex = T, 
                remove.multiple = TRUE, 
                labelsize=1, label.n=30, 
                label.cex = F)
## --------------------------------------------------
#NETWORK STATISTICS... 
netstat <- networkStat(NetMatrix)
netstat
##
names(netstat$network)
names(netstat$vertex)
summary(netstat, k=10)
###------------------------------
## ----Country collaboration, fig.height=7, fig.width=7, warning=FALSE--------------------------------------------------
# Create a country collaboration network
#name mtag for metaTagExtraction
Mtag <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(Mtag, analysis = "collaboration", network = "countries", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "fruchterman", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")
##(can also say type = circle.. )
#----------
## Co-citation network, fig.height=7, fig.width=7, warning=FALSE----------------------------------------------------
# Create a co-citation network
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)
##-------
## ----Keyword c-occurrences, fig.height=7, fig.width=7, warning=FALSE--------------------------------------------------
# Create keyword co-occurrences network
NetMatrix <- biblioNetwork(MJDH, analysis = "co-occurrences", network = "keywords", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
## Co-Word Analysis, fig.height=9, fig.width=9, warning=FALSE-------------------------------------------------------
# Conceptual Structure using keywords (method="CA")
CS <- conceptualStructure(M, field="ID", method="CA", minDegree=4, k.max=8, stemming=FALSE, labelsize=10, documents=10)
###########Right now the above not working...
#----
## ----Historical Co-citation network, fig.height=7, fig.width=10, warning=FALSE--------------------------------------------------
# Create a historical citation network
options(width=130)
histResults <- histNetwork(M, min.citations = 3, sep = ";")
# Plot a historical co-citation network
net <- histPlot(histResults, n=100, size =30, labelsize=10, size.cex=TRUE, arrowsize = 0.5, color = TRUE)
###END
