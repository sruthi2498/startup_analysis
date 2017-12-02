data <- read.csv(file="startup_funding.csv", header=TRUE, sep=",")
mydata<- as.data.frame(data)
##########################################################################
#CLEAN DATE COLUMN
head(mydata)
sapply(mydata, function(x) sum(is.na(x)))
library(stringr)

#Number of startups funded per month - hist
Date<-function(string){
  return(str_sub(string,1,2))
}
Month<-function(string){
  return(str_sub(string,4,5))
}
Year<-function(string){
  return(str_sub(string,7,10))
}
MonthYear<-function(string){
  m<-Month(string)
  y<-Year(string)
  my<-paste(m,y,sep="-")
  return(my)
}

New_Date<-data.frame()
j=1
for(i in 1:nrow(mydata)){
  d<-mydata[i,]
  currdate<-toString(d$Date)
  #print(str_length(currdate))
  if(str_length(currdate)!=10){
    #print(d)
    fourth<-str_sub(currdate,4,4)
    if(fourth!='0'){
      currdate<-paste(str_sub(currdate,1,3),'0',str_sub(currdate,4,9),sep="")
    }
    if(str_length(currdate)!=10){
      currdate<-paste(str_sub(currdate,1,6),str_sub(currdate,8,11),sep="")
      #print(currdate)
    }
    
  }
  
  new_row<-data.frame(currdate)
  New_Date<-rbind(New_Date,new_row)
  
}
names(New_Date)<-c("New_Date")
mydata<-cbind(mydata,New_Date)
head(mydata)
DDMMYYYY<-data.frame()
for(i in 1:nrow(mydata)){
  d<-mydata[i,]
  currdate<-toString(d$New_Date)
  
  date<-Date(currdate)
  mon<-Month(currdate)
  year<-Year(currdate)
  monyear<-MonthYear(currdate)
  new_row<-data.frame(date,mon,year,monyear)
  names(new_row)<-c("dd","mm","yyyy","mmyyyy")
  DDMMYYYY<-rbind(DDMMYYYY,new_row)
}
nrow(DDMMYYYY)
names(DDMMYYYY)<-c("dd","mm","yyyy","mmyyyy")
#head(DDMMYYYY)
mydata<-cbind(mydata,DDMMYYYY,row.names=NULL)
head(mydata)
tail(mydata)

##########################################################################
#CLEAN LOCATION COLUMN
library(stringr)
rows<-nrow(mydata)
last_sl<-mydata[rows,]$SNo
j=last_sl+1

new_data<-data.frame()

for(i in 1:nrow(mydata)){
  row2<-mydata[i,]
  row1<-mydata[i,]
  loc<-toString(mydata[i,]$CityLocation)
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
names(new_data)<-names(mydata)
mydata<-new_data
#######################################################################
#CLEAN AMOUNT IN USD COLUMN

fund<-mydata["AmountInUSD"] 
i<-1
d<-data.frame()
while(i<=nrow(mydata))
{
  f<-toString(mydata[i,]$AmountInUSD)
  f<-str_replace_all(f, ",", "")
  d[i,"AmountInUSD"]<-as.numeric(f)
  i<-i+1
  
}
mydata["AmountInUSD"]<-d["AmountInUSD"]
nrow(mydata)
mydata<-na.omit(mydata)
nrow(mydata)
sapply(mydata, function(x) sum(is.na(x)))

###########################################################################
#FUNDING TYPE CLEAN
for(i in 1:nrow(mydata)){
  row1<-mydata[i,]
  it<-toString(row1$InvestmentType)
  if(it=="PrivateEquity")mydata[i,]$InvestmentType<-"Private Equity"
  else if(it=="SeedFunding")mydata[i,]$InvestmentType<-"Seed Funding"
  else if(it=="Crowd funding")mydata[i,]$InvestmentType<-"Crowd Funding"
}
as.vector(unique(mydata$InvestmentType))

###########################################################################

#CATEGORIZE INDUSTRY VERTICAL
# CREATING INDUSTRY MAIN COLUMN BY TAKING FREQUENTLY OCCURING INDUSTRIES

library(stringr)
#length(IV)[1] 516
fc<-c()
hc<-c()
edc<-c()
tc<-c()
gc<-c()
hoc<-c()
fac<-c()
bc<-c()
ec<-c()
vc<-c()
mc<-c()
bic<-c()
fic<-c()
fuc<-c()
for(i in 1:nrow(mydata)){
  iv1<-toString(mydata[i,]$IndustryVertical)
  if(is.na(str_locate(iv1,"health")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Health")[1,]["start"])==FALSE ||
     is.na(str_locate(iv1,"pharmacy")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Pharmacy")[1,]["start"])==FALSE ||
     is.na(str_locate(iv1,"doctor")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Doctor")[1,]["start"])==FALSE){
    hc<-c(hc,iv1)
  }
  else if(is.na(str_locate(iv1,"food")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Food")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"Restaurant")[1,]["start"])==FALSE || is.na(str_locate(iv1,"restaurant")[1,]["start"])==FALSE){
    fc<-c(fc,iv1)
  }
  else if(is.na(str_locate(iv1,"education")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Education")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"learning")[1,]["start"])==FALSE || is.na(str_locate(iv1,"E-learning")[1,]["start"])==FALSE ){
    edc<-c(edc,iv1)
  }
  else if(is.na(str_locate(iv1,"travel")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Travel")[1,]["start"])==FALSE){
    tc<-c(tc,iv1)
  }
  else if(is.na(str_locate(iv1,"grocery")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Grocery")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"grocers")[1,]["start"])==FALSE ){
    gc<-c(gc,iv1)
  }
  else if(is.na(str_locate(iv1,"hotel")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Hotel")[1,]["start"])==FALSE){
    hoc<-c(hoc,iv1)
  }
  else if(is.na(str_locate(iv1,"fashion")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Fashion")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"apparel")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Apparel")[1,]["start"])==FALSE){
    fac<-c(fac,iv1)
  }
  else if(is.na(str_locate(iv1,"beauty")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Beauty")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"spa")[1,]["start"])==FALSE){
    bc<-c(bc,iv1)
  }
  else if(is.na(str_locate(iv1,"ecommerce")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Ecommerce")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"eCommerce")[1,]["start"])==FALSE ||  is.na(str_locate(iv1,"E-Commerce")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"ECommerce")[1,]["start"])==FALSE ){
    ec<-c(ec,iv1)
  }
  else if(is.na(str_locate(iv1,"vehicle")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Vehicle")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"car")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Car")[1,]["start"])==FALSE){
    vc<-c(vc,iv1)
  }
  else if(is.na(str_locate(iv1,"mobile")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Mobile")[1,]["start"])==FALSE ){
    mc<-c(mc,iv1)
  }
  else if(is.na(str_locate(iv1,"big data")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Big data")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"analytics")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Analytics")[1,]["start"])==FALSE){
    bic<-c(bic,iv1)
  }
  else if(is.na(str_locate(iv1,"finance")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Finance")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"FinTech")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Financial")[1,]["start"])==FALSE){
    fic<-c(fic,iv1)
  }
  else if(is.na(str_locate(iv1,"home")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Home")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"decor")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Decor")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"furniture")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Furniture")[1,]["start"])==FALSE){
    fuc<-c(fuc,iv1)
  }
}

#mydata["IndustryMain"]<-mydata["IndustryVertical"]
d<-data.frame()
for(i in 1:nrow(mydata)){
  iv1<-toString(mydata[i,]$IndustryVertical)
  #if(is.na(iv1)==FALSE && iv1!=""){
  #print(iv1)
  if(iv1 %in% hc){
    d[i,"IndustryMain"]<-as.character("Healthcare")
  }
  else if(iv1 %in% fc){
    d[i,"IndustryMain"]<-as.character("Food")
    #mydata[i,]$IndustryMain<-"Food"
  }
  else if(iv1 %in% edc){
    d[i,"IndustryMain"]<-as.character("Education")
    #mydata[i,]$IndustryMain<-"Education"
  }
  else if(iv1 %in% tc){
    d[i,"IndustryMain"]<-as.character("Travel")
    #mydata[i,]$IndustryMain<-"Travel"
  }
  else if(iv1 %in% gc){
    d[i,"IndustryMain"]<-as.character("Grocery")
    #mydata[i,]$IndustryMain<-"Grocery"
  }
  else if(iv1 %in% hoc){
    d[i,"IndustryMain"]<-as.character("Hotel")
    #mydata[i,]$IndustryMain<-"Hotel"
  }
  else if(iv1 %in% fac){
    d[i,"IndustryMain"]<-as.character("Fashion")
    # mydata[i,]$IndustryMain<-"Fashion"
  }
  else if(iv1 %in% bc){
    d[i,"IndustryMain"]<-as.character("Beauty")
    #  mydata[i,]$IndustryMain<-"Beauty"
  }
  else if(iv1 %in% ec){
    d[i,"IndustryMain"]<-as.character("Ecommerce")
    # mydata[i,]$IndustryMain<-"Ecommerce"
  }
  else if(iv1 %in% vc){
    d[i,"IndustryMain"]<-as.character("Vehicle")
    # mydata[i,]$IndustryMain<-"Vehicle"
  }
  else if(iv1 %in% mc){
    d[i,"IndustryMain"]<-as.character("Mobile")
    # mydata[i,]$IndustryMain<-"Mobile"
  }
  else if(iv1 %in% bic){
    d[i,"IndustryMain"]<-as.character("Big Data and Analytics")
    # mydata[i,]$IndustryMain<-"Big Data and Analytics"
  }
  else if(iv1 %in% fic){
    d[i,"IndustryMain"]<-as.character("Finance")
    # mydata[i,]$IndustryMain<-"Finanace"
  }
  else if(iv1 %in% fuc){
    d[i,"IndustryMain"]<-as.character("Furniture")
    # mydata[i,]$IndustryMain<-"Furniture"
  }
  else{
    d[i,"IndustryMain"]<-as.character(iv1)
  }
  # }
  if(is.na( d[i,"IndustryMain"]))print(i)
  # print(mydata[i,]$IndustryMain)
  
}
mydata["IndustryMain"]<-d
unique(mydata$IndustryMain)
table(mydata$IndustryMain)

mydata["CityEncoded"]<-NA
cities<-as.vector(unique(mydata$CityLocation))
for(i in 1:nrow(mydata)){
  j<-which(cities==toString(mydata[i,]$CityLocation))
  mydata[i,]$CityEncoded<-j
}

iv<-as.vector(unique(mydata$IndustryMain))
mydata["IndustryMainEncoded"]<-NA
for(i in 1:nrow(mydata)){
  j<-which(iv==toString(mydata[i,]$IndustryMain))
  mydata[i,]$IndustryMainEncoded<-j
}

###########################################################################
#CLEANING FOUNDER EDUCATION COLUMN 
# ADDING A NEW DESIGNATION COLUMN
founders<-read.csv(file="foundercompanyskill.csv", header=TRUE, sep=",")
fdata<- as.data.frame(founders)


for(i in 1:nrow(fdata)){
  u<-toString(fdata[i,]$education)
  us<-strsplit(u,";")[[1]]
  if(length(us)>1){
    print(i)
    fdata[i,]$education<-toString(us[1])
    for(j in 2:length(us)){
      r<-as.data.frame(fdata[i,])
      r$education<-toString(us[j])
      fdata<-rbind(fdata,r)
    }
  }
}

for(i in 1:nrow(fdata)){
  u<-toString(fdata[i,]$education)
  us<-strsplit(u,";")[[1]]
  if(length(us)>1){
    print(i)
  }
}

fd<-data.frame()
for(i in 1:nrow(fdata)){
  desg<-toString(fdata[i,]$designation)
  if(is.na(str_locate(desg,"Founder")[1,]["start"])==FALSE  ||is.na(str_locate(desg,"founder")[1,]["start"])==FALSE ||
     is.na(str_locate(desg,"CEO")[1,]["start"])==FALSE || is.na(str_locate(desg,"CTO")[1,]["start"])==FALSE ||
     is.na(str_locate(desg,"Entrepreneur")[1,]["start"])==FALSE ||  is.na(str_locate(desg,"entrepreneur")[1,]["start"])==FALSE )
    fd[i,"Designation"]<-"Founder"
  else fd[i,"Designation"]<-"Employee"
}
fdata["desgrev"]<-fd["Designation"]
