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

data <- read.csv(file="foundercompanyskill.csv", header=TRUE,stringsAsFactors = FALSE, sep=",")
data<-as.data.frame(data)

##############################################################################
#storing all skills and markets into one data frame
skills<-data.frame(strsplit(data[1,"skills"],';'))
names(skills)<-c("skills")
i<-2
while(i<=nrow(data))
{
d<-data.frame(strsplit(data[i,"skills"],';'))
names(d)<-c("skills")
skills<-rbind(skills,d)
d<-data.frame(strsplit(data[i,"markets"],';'))
names(d)<-c("skills")
skills<-rbind(skills,d)
i<-i+1
}
#removing na values
skills<-na.omit(skills)

##############################################################################
#Some of the rows have location instead of skill or market (scraping issue)
#We want to remove these rows so that the word cloud only has founder skills
#CLEAN LOCATION
library(stringr)
locdata <- read.csv(file="startup_funding.csv", header=TRUE, sep=",")
locdata<-as.data.frame(locdata)

rows<-nrow(locdata)
last_sl<-locdata[rows,]$SNo
j=last_sl+1

new_data<-data.frame()

for(i in 1:nrow(locdata)){
  row2<-locdata[i,]
  row1<-locdata[i,]
  loc<-toString(locdata[i,]$CityLocation)
  loc<-str_replace_all(loc, fixed(" "), "")
  loc<-paste0(toupper(substr(loc, 1, 1)), substr(loc, 2, nchar(loc)))
  if(grepl("/",loc,fixed=TRUE)==TRUE){
    #print(loc)
    two_locs<-str_split(loc,"/")
    locA<-two_locs[[1]][1]
    locB<-two_locs[[1]][2]
    if(locA=="NewDelhi"){
      locA<-"New Delhi"
    }
    if(locB=="NewDelhi"){
      locB<-"New Delhi"
    }
    
    if(locA=="SanMateo"){
      locA<-"San Mateo"
    }
    if(locB=="SanMateo"){
      locB<-"San Mateo"
    }
    if(locA=="PaloAlto"){
      locA<-"Palo Alto"
    }
    if(locB=="PaloAlto"){
      locB<-"Palo Alto"
    }
    if(locA=="bangalore"){
      locA<-"Bangalore"
    }
    if(locB=="bangalore"){
      locB<-"Bangalore"
    }
    row1$CityLocation<-locA
    row2$CityLocation<-locB
    row1$SNo<-j
    #print(j)
    j<-j+1
    row2$SNo<-j
    #print(j)
    j<-j+1
    new_data<-rbind(new_data,row1)
    new_data<-rbind(new_data,row2)
    
  }
  else{
    row1$SNo<-j
    #print(j)
    j<-j+1
    new_data<-rbind(new_data,row1)
  }
}
names(new_data)<-names(locdata)
locdata<-new_data
locdata[,"CityLocation"]<-tolower(locdata[,"CityLocation"])
cities <-unique(locdata["CityLocation"])
i<-1
while(i<=nrow(data))
{
for(city in cities$CityLocation)
{
if(is.na(str_locate(tolower(toString(skills[i,"skills"])),toString(city))[1,]["start"])==FALSE)
{
if(tolower(toString(skills[i,"skills"])) %in% cities)
print(skills[i,"skills"])
}
}
i<-i+1
}
###########################################################################################
#remove spaces that seperate two related words 
skills[,"skills"]<-str_replace_all(skills[,"skills"], fixed(" "), "")
l<-as.character(skills[,"skills"])
df=as.data.frame(l)
# Load the data as a corpus
docs = Corpus(VectorSource(df$l))
#inspect(docs)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#inspect(docs)

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c(unlist(cities)))
docs <- tm_map(docs, removeWords, c("bengaluru","asia","newyorkcity","sanfrancisco"))
 
#to build a term document matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

png(filename="D:/DA/skillsWordCloud.png")
#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(10, "Set2"))
dev.off()


