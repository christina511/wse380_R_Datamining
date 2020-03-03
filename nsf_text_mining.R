#This does minor TEXTMINING  I begin with looking at award amounts
#improve by loading a dataset with out oceonagrpahic research
#install.packages("qdap") #applied directly to text vectors
library(qdap)
###install.packages("tm") #works on text corpus object
library(tm)
###install.packages("wordcloud") #works on text corpus object
library(wordcloud)
#read.csv("C:/Users/Daniene/Downloads/NSF Awards in ethics and autonomous vehicles.csv")
# home path   NSFdata<-read.csv("C:/Users/Daniene/Downloads/NSF Awards in ethics and autonomous vehicles.csv")
# on other computer path: NSFdata<- read.csv("E:/NSF Awards Ethics and Autonomous Vehicles.csv")
#load to read csv   library(readr)
#using path below data loads as factors...
NSFdata <- read.csv("C:/Users/dtsstudent/Downloads/Awards.csv")
#this loaded everything as character(read_csv loads as character; read.csv loads as factor) (at home loads as factor)
View(NSFdata)
str(NSFdata)#RA added shows loaded correctly)
typeof(NSFdata)
class(NSFdata)
#to understand data
plot(NSFdata$AwardedAmountToDate) #not working here - works at home
summary(NSFdata)#summarizes all giving first few data col
nlevels(NSFdata$Organization) #give number groups awarded
org<-(NSFdata$Organization) #names this element
summary(org) #summarizes awards to each group
so<-summary(org)
plot(so)
hist(so)#just shows most groups get under 200 grants one group got 750
###a line of code from hodan to read file
#nsfd<-read.table(file.choose(NSFdata))
#the issue here is converting the data set from factor so I can get dollar amounts
#read in differently or find a way to convert it
###
# in call to read csv set... stringsAsFactors=F (this was for home...at school loaded this way)
##
#Function to convert from factor to numeric
convertCurrency <-function(currency){
  currency1<- sub ('$','', as.character(currency), fixed=TRUE)
  currency2 <-as.numeric(gsub('\\,','',as.character(currency1)))
  currency2
}
NSFdata$funds<-convertCurrency(NSFdata$AwardedAmountToDate)
head(NSFdata$funds)
head(NSFdata$AwardedAmountToDate)#this checks conversion work.
hist(NSFdata$funds)
plot(NSFdata$funds)
########################################## Start Text mining ####on NSF Abstracts
#create a new column that is a cleaned version of NSFdata$Abstract
#cleaned abstract will be called CAbs to start make a col and then extract as X2
NSFdata$CAbs<-NSFdata$Abstract
X2<-NSFdata$CAbs #this is the vector with abstracts
class(X2)
#note the class and type of data matters, this order and code works to transform as needed
#qdap cleaning function 
qdap_clean <-function (X){
  X<-replace_abbreviation(X)
  X<-replace_contraction(X)
  X<-replace_number(X)
  X<-replace_ordinal(X)
  X<-replace_symbol(X)
  X<-tolower(X)
  return(X)
}
###runs as character and then make tdm as factor...
X3<-qdap_clean(X2) #uses "qdap_clean" function to clean my corpus & changes to character
class(X3)
#### maybe try later...mycorpus<-iconv(mycorpus, to ="utf-8")#added this to try and get to turn into matrix 
#call(fun(tm,"tm")) #error here didn't work  what is this trying to do?
X4<-VCorpus(VectorSource(X3)) #changes X3 into a corpus
class(X4)
typeof(X4)
#use the tm_map function to apply cleaning functions to a corpus
clean_corpus<-function(corpus){
  corpus<-tm_map(corpus, content_transformer(tolower)) 
  corpus<-tm_map(corpus, PlainTextDocument)
  corpus<-tm_map(corpus, removeWords,
                 c(stopwords("en"),"ocean", "will", "the", "project", "this","also", "two","one","can"))
  corpus<-tm_map(corpus, stripWhitespace)
  corpus<-tm_map(corpus, removePunctuation)
}
X5<-clean_corpus(X4)
class(X5)
typeof(X5)
#generate a TDM (or term  document matrix/ this has all terms as rows and documents as columns)
#note  a TMD (or document term matrix, rows=documents, terms =columns)
#I will use a TDM because I have (maybe) more rows than columns
X5tdm<-TermDocumentMatrix(
  X5,control = list(Token_Tokenizer(X5)))
#create ethics_P_tdm_m MATRIX
X5tdmm <- as.matrix(X5tdm)
#create ethics_p_freq
X5freq<-rowSums(X5tdmm)
#prep to barplot: Sort term frequency in descending order
X5freq<-sort(X5freq, decreasing=TRUE)
#plot a barchart-of the 20 most common words
barplot(X5freq[1:20], col="red", las=2)
#wordCloud
wordcloud(names(X5freq),X5freq, max.words = 75, color="blue")

