blog_crawl<-function(keyword,number_of_blog,fromdate=NULL,dateto=NULL){
nc<-number_of_blog
keyword<-iconv(keyword,from="CP949",to="UTF-8")
Encoding(keyword)
keyword2<-URLencode(keyword)
if(is.null(fromdate)){
  url<-paste0("https://search.naver.com/search.naver?sm=tab_hty.top&where=post&ie=utf8&query=",keyword2)
}else{
url<-paste0("https://search.naver.com/search.naver?where=post&query=",keyword2,"&ie=utf8&st=sim&sm=tab_opt&date_from=",fromdate,"&date_to=",dateto,"&date_option=8&srchby=all&dup_remove=1&from",fromdate,"to",dateto,"&mson=0")
}
b<-readLines(url,encoding='UTF-8')

library(stringr)

b2<-b[str_detect(b,"blog_terms")]

to<-sapply(str_split(str_extract(b2,perl("(?<=title_num\">).*(?=건</span> </div>)")),"/"),function(x){x[2]})
to<-gsub(",","",to)
total<-as.numeric(to)

if(total<nc){
  nc<-total
}

tp<-round(nc / 10)
r<-2
final_url<-NULL
for(r in 1:tp){
  if(is.null(fromdate)){
    url<-paste0("https://search.naver.com/search.naver?sm=tab_hty.top&where=post&ie=utf8&query=",keyword2,"&start=",10*(r-1)+1)
    
  }else{
    url<-paste0("https://search.naver.com/search.naver?where=post&query=",keyword2,"&ie=utf8&st=sim&sm=tab_opt&date_from=",fromdate,"&date_to=",dateto,"&date_option=8&srchby=all&dup_remove=1&from",fromdate,"to",dateto,"&mson=0&start=",10*(r-1)+1)
  }

if(class(try(b<-readLines(url,encoding='UTF-8')))=='try-error'){
  cat("\n 에러발생 좀만기달 ㅠㅠ")
  b<-readLines(url,encoding='UTF-8')
}
b2<-b[str_detect(b,"blog_terms")]
b3<-str_split(b2,"<div class=\"thumb\"|<div class=\"thumb thumb-rollover\">")
length(b3)

b4<-str_sub(sapply(b3,str_extract,regex("(?<=<a href=).*(?=target=\"_blank\" onclick=\"retur)")),2,end=-3)
n_blog<-b4[str_detect(b4,"naver")][-1]

blogid<-str_sub(str_extract(n_blog,regex("(?<=naver.com/).*(?=Redirect)")),end=-2)
logno<-str_extract(n_blog,regex("(?<=logNo=).*(?=)"))


blogurl<- paste0("http://blog.naver.com/PostView.nhn?blogId=",blogid,"&logNo=",logno,"&beginTime=0&jumpingVid=&from=search&redirect=Log&widgetTypeCall=true")
i<-1
final_url<-c(final_url,blogurl)

cat("\n",r,"page blog url 수집중")

}


blog<-NULL
for(j in 1:length(final_url)){
  ke<-1
  while(ke < 4){
    if(class(try(b<-readLines(final_url[j],encoding="EUC-KR"))) !="try-error"){
     Sys.sleep(0.5)
      break;
    }
   
  }
  
  if(sum(str_detect(b,"post-view")) == 0 | sum(str_detect(b,"post_footer")) == 0){
    next;
  } 
  
  start<-which(str_detect(b,"post-view"))
  last<-which(str_detect(b,"post_footer")) 
  
  if(length(which(str_detect(b,"모바일에서 작성된 글입니다"))) > 0){
    last<-which(str_detect(b,"모바일에서 작성된 글입니다"))-1
  }
  
  content<-b[start:last]
  lastcon<-c()        
  lastcon<-paste0(content,collapse="")
  lastcon<-gsub("<.*?>","",lastcon)
  lastcon<-gsub("\t"," ",lastcon)
  lastcon<-gsub("&nbsp;"," ",lastcon)
  

  
  
  blog<-c(blog,lastcon)
  
  cat("\n 총",length(final_url),"블로그 중 ",j,"번째 블로그 수집 중")
  
}

dir.create("C://blogcrwal")
cat("\n 데이터수집 완료, 경로 C://blogcrwal 생성")
setwd("C://blogcrwal")

write.csv(as.matrix(blog),paste0(keyword,"블로그.csv"),row.names=F)
cat(paste0("\n",getwd()," 경로에 ",paste0(keyword,"블로그.csv 파일명으로 "), "블로그 저장"))
}

####사용방법
## 날짜 지정해서 수집하고 싶은 경우
## blog_crawl(키워드,수집 하고자 하는 블로그 수,시작날짜,끝날짜) 
## 날짜 상관없이 수집하고 싶은 경우
## blog_crawl(키워드,수집 하고자 하는 블로그 수)
blog_crawl("빅데이터",100,20160101,20170102)
