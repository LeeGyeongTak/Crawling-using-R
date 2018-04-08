########셀레늄을 이용한 구글 play store 리뷰 크롤링 코드입니다.



library(RSelenium)

library(jsonlite)

rD <- rsDriver(browser="fire",port=1006L)



remDr <- rD[["client"]]

## 크롤링 하고자 하는 앱 주소 

b<-"https://play.google.com/store/apps/details?id=com.nate.android.portalmini#details-reviews"

remDr$navigate(b)







  k<-1

comment_list<-list()

while(T){

  cat("\n",k)

  

  webElem <- remDr$findElement(using = 'xpath', "//*/div[@data-load-more-docid='com.nate.android.portalmini']/button[2]")

  webElem$clickElement()

  Sys.sleep(0.3)

  webElem <- remDr$findElement(using = 'css selector', "div[data-load-more-docid='com.nate.android.portalmini']")

  comment_list[[k]]<-unlist(webElem$getElementText())                        

  # Sys.sleep(1)

  k<-k+1

}



library(stringr)

head(comment_list)

length(comment_list)

col<-do.call("rbind",comment_list)

col2<-str_split(col,"\n")

length(unlist(col2))

col3<-matrix(unlist(col2),ncol=2,byrow=T)





data<-c(col3[str_detect(col3[,1],"2014|2015|2016|2017|2018"),2],

        col3[str_detect(col3[,2],"2014|2015|2016|2017|2018"),1])

data2<-data[!str_detect(data,"2014|2015|2016|2017|2018")]

length(data2)

head(data2)

# save(col3,file="col3.RData")

write.csv(data2,"nate1.csv",row.names=F)


