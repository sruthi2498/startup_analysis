
# YEAR WISE STARTUPP COUNT
library(plotly)

years<-as.vector(unique(mydata$yyyy))
year_df<-data.frame()
for(y in years){
  r<-data.frame(y,0)
  year_df<-rbind(year_df,r)
}
names(year_df)<-c("year","startup_count")
year_df
for(i in 1:nrow(mydata)){
  y<-toString(mydata[i,]$yyyy)
  i<-y==as.vector(year_df$year)
  year_df[i,]$startup_count=year_df[i,]$startup_count+1
}
year_df
p1<-plot_ly(x=year_df$year,y=year_df$startup_count,type="bar",
            name='YEAR WISE startup count') %>%
  layout(title='YEAR WISE startup count')
  

p1
#####################################################################################
# MONTH WISE STARTUPP COUNT

mon_years<-as.vector(unique(mydata$mmyyyy))
monyear_df<-data.frame()
for(y in length(mon_years):1){
  r<-data.frame(mon_years[y],0)
  monyear_df<-rbind(monyear_df,r)
}
names(monyear_df)<-c("month_year","startup_count")
monyear_df
for(i in nrow(mydata):1){
  y<-toString(mydata[i,]$mmyyyy)
  i<-y==as.vector(monyear_df$month_year)
  #print(i)
  monyear_df[i,]$startup_count=monyear_df[i,]$startup_count+1
}
monyear_df
p2<-plot_ly(x=monyear_df$month_year,y=monyear_df$startup_count,type="scatter",mode='lines',
            name='MONTH WISE startup count',margin=list(b=300))%>%
  layout(title='MONTH WISE startup count',xaxis=list(title='Month Year'),yaxis=list(title='Number of startups'),
         margin=list(b=120) )
p2
  #p2
subplot(p1,p2)
######################################################################################

#INDUSTRY VERTICAL SPLIT UP IN THE YEARS

df2015<-subset(mydata,mydata$yyyy==2015)
df2016<-subset(mydata,mydata$yyyy==2016)
df2017<-subset(mydata,mydata$yyyy==2017)

df2015<-as.data.frame(table(df2015$IndustryMain))
df2015<-df2015[-1,]
names(df2015)<-c("Industry","Number_of_startups")
df2015<-subset(df2015,df2015$Number_of_startups>2)

df2016<-as.data.frame(table(df2016$IndustryMain))
df2016<-df2016[-1,]
names(df2016)<-c("Industry","Number_of_startups")
df2016<-subset(df2016,df2016$Number_of_startups>2)

df2017<-as.data.frame(table(df2017$IndustryMain))
df2017<-df2017[-1,]
names(df2017)<-c("Industry","Number_of_startups")
df2017<-subset(df2017,df2017$Number_of_startups>2)

plot_ly(type='pie',data=df2015,labels=df2015$Industry,values=df2015$Number_of_startups,name='2015',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        domain = list(x = c(0, 0.3), y = c(0, 1)),showlegend=FALSE,title='2015')%>%
add_trace(type='pie',data=df2016,labels=df2016$Industry,values=df2016$Number_of_startups,name='2016',
          domain = list(x = c(0.35, 0.65), y = c(0, 1)),showlegend=FALSE)%>%
  add_trace(type='pie',data=df2017,labels=df2017$Industry,values=df2017$Number_of_startups,name='2017',
            domain = list(x = c(0.7, 1), y = c(0, 1)),showlegend=FALSE)%>%
  layout(title='Industry vertical split up')





  
  
  
  
  ###########################################################################################


# AVERAGE AND MAXIMUM FUNDING IN THE THREE YEARS

d<-data.frame()
x<-as.vector(unique(mydata$mmyyyy))
for(i in length(x):1){
  s<-subset(mydata,mydata$mmyyyy==x[i])
  avg<-mean(s$AmountInUSD,na.rm=TRUE)
  m<-max(s$AmountInUSD,na.rm=TRUE)
  n<-data.frame(toString(s[1,]$mmyyyy),avg,m)
  d<-rbind(d,n)
}

names(d)<-c("Month-Year","Average_funding","Max_funding")
plot_ly(type='scatter',mode='lines',x=d$`Month-Year`,y=d$Average_funding,name='Average funding') %>%
  add_trace(type='scatter',mode='lines',x=d$`Month-Year`,y=d$Max_funding,name='Maximum funding') %>%
  layout(margin=list(b=100),xaxis=list(title='Month Year'),yaxis=list(title='Amount in funding USD'),title='Funding in USD')

s<-subset(mydata,mydata$mmyyyy=="09-2015")
y<-subset(s,s$AmountInUSD==max(s$AmountInUSD))
#FLIPKART - FROM MICROSOFT AND EBAY 1.4 BILL /3 2017
#PAYTM - FROM SOFTBANK GROUP 1.4 BILL /5 2017

#FLIPKART -Steadview Capital and existing investors 700 Mill /7 2015
#PAYTM - FROM Alibaba Group, Ant Financial 600 Mill /9 2015


