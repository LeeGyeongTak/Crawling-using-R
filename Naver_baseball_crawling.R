
###############네이버 야구 뉴스 크롤링하기 

x<-1



final_url<-NULL



for(x in 0:3){ ##3일치만...

dadate<-as.Date(Sys.time())-x

dadate2<-gsub("-","",dadate) ### 야구뉴스 페이지에 날짜가 들어가기 때문에 필요합니다.

i<-1

m_url<-NULL

for(i in 1:5){

day_url<-paste0("http://sports.news.naver.com/kbaseball/news/index.nhn?date=",dadate2,"&page=",i) ## 주소설정

b<-readLines(day_url,encoding="UTF-8")

b2<-gsub("\t\tnewsListModel:","",b[str_detect(b,"newsListModel")]) ## 원하는 정보가 있는 곳 찾기 

b3<-fromJSON(b2) ## json을 list로 반환



b3$list[[1]]$oid

b3$list[[1]]$aid





oid<-sapply(b3$list,function(x){x$oid}) ##url을 만들기 위한 정보

aid<-sapply(b3$list,function(x){x$aid})

datedate<-sapply(b3$list,function(x){x$datetime})



new_url<-paste0("http://sports.news.naver.com/kbaseball/news/read.nhn?oid=",oid,"&aid=",aid)



ddd<-cbind(datedate,new_url)

cat("\n",x,"-",i)

m_url<-rbind(m_url,ddd)



}



final_url<-rbind(final_url,m_url)

}









library(stringr)

urlist<-final_url[,2] ## 뉴스기사 url만 받아옵니다.

p<-1

cont<-NULL

for(p in 1:length(urlist)){

  b<-readLines(urlist[p],encoding = "UTF-8")

  tp<-b[str_detect(b,"<h4 class=\"title\">")] ## title의 위치를 가져옴

  title<-str_sub(str_extract(tp,perl("(?<=title).*(?=</h4>)")),3)    ## title만 추출

  b2<-b[which(str_detect(b,"newsEndContents")): which(str_detect(b,"news_end_btn"))] ## 기사내용이 있는 곳을 가져옴

  b3<-paste(b2,collapse = "")  

  b4<- gsub("<.*?>|\t|nbsp;","",b3)  

  cont<-rbind(cont,c(title,b4))

  cat("\n",p)

}

  



final_data<-cbind(final_url,cont)
