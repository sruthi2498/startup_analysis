
# INTERACTIVE PLOT WITH MAJOR INDUSTRIES AND TOTAL AMT INVESTED BY TOP INVESTORS IN THEM
library(plotly)
mydata<-mydata[order(-mydata$AmountInUSD),]
p1<-subset(mydata,mydata$IndustryMainEncoded==1,select = c("IndustryMain","InvestorsName","AmountInUSD"))
p1<-p1[1:(0.2*nrow(p1)),]
p2<-subset(mydata,mydata$IndustryMainEncoded==2,select = c("IndustryMain","InvestorsName","AmountInUSD"))
p2<-p2[1:(0.2*nrow(p2)),]
p3<-subset(mydata,mydata$IndustryMainEncoded==3,select = c("IndustryMain","InvestorsName","AmountInUSD"))
p3<-p1[1:(0.2*nrow(p3)),]
p4<-subset(mydata,mydata$IndustryMainEncoded==5,select = c("IndustryMain","InvestorsName","AmountInUSD"))
p4<-p4[1:(0.2*nrow(p4)),]
p5<-subset(mydata,mydata$IndustryMainEncoded==6,select = c("IndustryMain","InvestorsName","AmountInUSD"))
p5<-p5[1:(0.3*nrow(p5)),]
p6<-subset(mydata,mydata$IndustryMainEncoded==7,select = c("IndustryMain","InvestorsName","AmountInUSD"))
p6<-p6[6:(0.3*nrow(p6)),]
p7<-subset(mydata,mydata$IndustryMainEncoded==8,select = c("IndustryMain","InvestorsName","AmountInUSD"))
p7<-p7[1:(0.3*nrow(p7)),]
p8<-subset(mydata,mydata$IndustryMainEncoded==9,select = c("IndustryMain","InvestorsName","AmountInUSD"))
p8<-p8[1:(0.3*nrow(p8)),]
p9<-subset(mydata,mydata$IndustryMainEncoded==10,select = c("IndustryMain","InvestorsName","AmountInUSD"))
p9<-p9[1:(0.5*nrow(p9)),]



p<-plot_ly(type='bar')%>%
  add_trace (data = p1, x= p1$InvestorsName, y= p1$AmountInUSD , name =iv ,visible=F) %>%
  add_trace (data = p2, x= p2$InvestorsName, y= p2$AmountInUSD , name =iv ,visible=F) %>%
  add_trace (data = p3, x= p3$InvestorsName, y= p3$AmountInUSD , name =iv ,visible=F) %>%
  add_trace (data = p4, x= p4$InvestorsName, y= p4$AmountInUSD , name =iv ,visible=F) %>%
  add_trace (data = p5, x= p5$InvestorsName, y= p5$AmountInUSD , name =iv ,visible=F) %>%
  add_trace (data = p6, x= p6$InvestorsName, y= p6$AmountInUSD , name =iv ,visible=F) %>%
  add_trace (data = p7, x= p7$InvestorsName, y= p7$AmountInUSD , name =iv ,visible=F) %>%
  add_trace (data = p8, x= p8$InvestorsName, y= p8$AmountInUSD , name =iv ,visible=F) %>%
  add_trace (data = p9, x= p9$InvestorsName, y= p9$AmountInUSD , name =iv ,visible=F) %>%
  layout(
  title = "Top Investors in major Industry Verticals", showlegend=FALSE,
  xaxis=list(title="Investors", tickangle = 45),
  yaxis=list(title="Amount invested (in USD)"),
  margin=list(b=200),
  updatemenus=list(
    list(
      active=-1,
      buttons=list(
        list(
          label="Consumer Internet",
          method="update",
          args=list ( list (visible=c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                      list(title="Consumer Internet"))
          ),
         list(
          label="Technology",
          method="update",
          args=list ( list (visible=c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                      list(title="Technology"))
        ),
       list(
          label="Ecommerce",
          method="update",
          args=list ( list (visible=c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                      list(title="Ecommerce"))
        ),
     list(
          label="Food",
          method="update",
          args=list ( list (visible=c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                      list(title="Food"))
        ),
      list(
          label="Mobile",
          method="update",
          args=list ( list (visible=c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)),
                      list(title="Mobile"))
        ),
      list(
          label="Healthcare ",
          method="update",
          args=list ( list (visible=c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)),
                      list(title="Healthcare "))
        ),
     list(
          label=" Education",
          method="update",
          args=list ( list (visible=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)),
                      list(title=" Education"))
        ),
     list(
          label="Vehicle",
          method="update",
          args=list ( list (visible=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)),
                      list(title="Vehicle"))
        ),
      list(
          label="Logistics",
          method="update",
          args=list ( list (visible=c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)),
                      list(title="Logistics"))
        )
      )
    )
  )
)
p

