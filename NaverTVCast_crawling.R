

#########셀레늄을 이용한 크롤링 코드 입니다.

library(RSelenium)

library(rvest)



RSelenium::checkForServer()





rD <- rsDriver(browser="fire",port=1006L)



library(rvest)



setwd("D:\\개인폴더\\크롤링\\셀레늄\\네이버티비캐스트댓글")



bu<-"http://tv.naver.com/r/"



top3<- read_html(bu) %>% html_nodes("div.top_flick div.inner a") %>% html_attr("href")

last97<-read_html(bu) %>% html_nodes("div.cds div.cds_type  dt.title a") %>% html_attr("href")



url_100<-c(top3,last97)



top3_title<-read_html(bu) %>% html_nodes("div.top_flick div.inner span.ch") %>% html_text()

last97_title<-read_html(bu) %>% html_nodes("div.cds div.cds_type  dd.chn") %>% html_text()

title_100<-c(top3_title,last97_title)

title_100<-gsub("\n|\t","",title_100)





for(k in 1:100){

remDr$navigate("http://tv.naver.com/r/")



Sys.sleep(1.5)

  

url<-url_100[k]



webElem <- remDr$findElement(using = 'css', paste0("a[href='",url,"']"))

webElem$clickElement()

Sys.sleep(1.5)



webElem <- remDr$findElement(using = 'css', "a[href='#comment_focus']")

webElem$clickElement()

Sys.sleep(1.5)





webElem <- remDr$findElement(using = 'css', "span[class='play']")

webElem$getElementText() ##조회수 

Sys.sleep(1.5)

webElem <- remDr$findElement(using = 'css', "em[class='_commentCount']")

webElem$getElementText() ##댓글수 

Sys.sleep(1.5)



webElem <- remDr$findElement(using = 'css', "ul[class='u_cbox_list']")



comments<-unlist(webElem$getElementText())

library(stringr)



cos<-str_split(comments,"\n")

coments_df<-matrix(unlist(cos),ncol=11,byrow=T)



coments_df2<-coments_df[,c(1,2,6,9,11)]



Sys.sleep(1.5)

write.csv(coments_df2,paste0(title_100[k],"-",k,"위.csv"),row.names=F)



cat("\n",k)



}


