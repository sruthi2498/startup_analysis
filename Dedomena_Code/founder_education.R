
#ANALYSING FOUNDER CREDENTIALS
# UNIVERSITIES THAT PRODUCE TOP STARTUPS
# FOUNDERS AND ENGINEERS PRODUCED BY TOP UNOVERSITIES

library(stringr)
library(plotly)


t<-as.data.frame(table(fdata$education))
t<-t[-1,]
names(t)<-c("ed","count")
edplusdesg<-data.frame()
t<-subset(t,t$count>10)
for(i in 1:nrow(t)){
  s<-subset(fdata,fdata$education==t[i,]$ed)
  x<-table(s$desgrev)
  r<-data.frame(toString(t[i,]$ed),as.numeric(x[1]),as.numeric(x[2]))
  edplusdesg<-rbind(edplusdesg,r)
}
names(edplusdesg)<-c("ed","emp","founder")
  plot_ly(edplusdesg,x=edplusdesg$ed,y=edplusdesg$emp,type='bar',name='Employees')%>%
  add_trace(y=~founder,name='Founders')%>%
  layout(title='Founders vs Employees in top Universities',
         yaxis = list(title = 'Count'),margin=list(b=200),barmode = 'stack')


t<-as.data.frame(table(fdata$education))
t<-t[-1,]
names(t)<-c("education","count")
t<-subset(t,t$count!=0)
m<-as.vector(t$education)
n<-as.vector(t$count)
plot_ly(type='bar',x=m,y=n)%>%
    layout(title='Universities that produce startups',xaxis=list(tickangle=85),margin=list(b=300))
  
  
iit<-data.frame()
  for(i in 1:nrow(t)){
    if( is.na(str_locate(toString(t[i,]$education),"Indian Institute Of Technology")[1,]['start'])==FALSE  ){
      iit<-rbind(iit,t[i,])
    }
  }
  names(iit)<-names(t)
  plot_ly(type='bar',x=iit$education,y=iit$count,marker=list())%>%
    layout(title='IITs that produce startups',xaxis=list(tickangle=85),margin=list(b=200))

