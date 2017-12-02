# Load
#install.packages("NLP")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("RColorBrewer")
#install.packages("wordcloud")

library("NLP")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library(stringr)

data <- read.csv(file="CLEANdataset.csv", header=TRUE,stringsAsFactors = FALSE, sep=",")
data<-as.data.frame(data)
head(data["InvestorsName"])
#remove spaces that seperate two related words 
data[,"InvestorsName"]<-str_replace_all(data[,"InvestorsName"], fixed(" "), "")

#writing to a text file for clean up

l <- data$InvestorsName
i <- 1
 sink("investors.txt")
 min<-min(data$AmountInUSD)
 while(i<=nrow(data))
{
cat(rep(toString(l[i]),(data[i,"AmountInUSD"]/(10*min))))
cat("\n")
i <- i + 1
}
sink()

file.show("investors.txt")

text <- readLines("investors.txt")



#setting the frequency of each word based on amount invested
#i<-1
#d<-c()
#min<-min(data$AmountInUSD)
#while(i<=nrow(data))
#{
#d<- cbind(d,rep(data[i,"InvestorsName"],(data[i,"AmountInUSD"]/(10*min))))
#i<-i+1
#}
#length(d)


# Load the data as a corpus
docs = Corpus(VectorSource(text))
#inspect(docs)

# Remove numbers
docs <- tm_map(docs, removeNumbers)

 # Replace @UserName
usrName <- content_transformer(function (text) gsub("@,", " ", text))
docs <- tm_map(docs, usrName)


#to build a term document matrix

dtm <- TermDocumentMatrix(docs,control=list(tolower=FALSE))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
png(filename="D:/DA/InvestorWordCloud.png")
#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(10, "Dark2"))
dev.off()


