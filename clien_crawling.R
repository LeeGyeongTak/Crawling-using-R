

########클리앙의 모두의 공원 게시글을 수집하는 코드 입니다.

########

########크롤링은 html소스를 보면서 하셔야 이해가 빠릅니다.

########긁고자 하는 사이트에서 오른쪽 마우스 클릭-> 소스 보기를 하시면 소스를 보실수 있고

########그 소스내에 내가 원하는 정보가 있는 위치를 찾고, 규칙을 찾아 원하는 정보만 추출합니다.

######## 예를 들어 게시판의 제목이 <h2>제목</h2> 이렇게 만 나온다고한다면

######## <h2>가 있는 line만 찾아서 뽑으면 될것입니다.





library(stringr)



final<-NULL ## 제목, 조회수, url 만 긁습니다. 내용에 대한 크롤링은 게시글 url에 다시 접근해서 긁습니다.

i<-1

for(i in 1:10){

  

base_url<-paste0("http://www.clien.net/cs2/bbs/board.php?bo_table=park&page=",i-1,"&page=",i) ##게시판 url 설정



b<-readLines(base_url,encoding='UTF-8')  ## readLines. 일반적으로 html소스 전체를 불러들입니다.(모든 홈페이지가 다 적용되는 것은 아닙니다만, 가장 기본적인 형태 입니다)

head(b)

tail(b)



b2<-b[str_detect(b,"post_subject")] ##post_subject가 들어가있는 line을 뽑습니다.

tt<-str_extract(b2,perl("(?<=page).*(?=</a>)")) ## page로 시작하고 </a> 끝나는 가운데걸 뽑습니다.



#### 제목

title<-str_sub(tt,6)[-c(1:3)] ## str_sub을 통해 앞 6글자를 제거합니다. 위에 1~3개는 공지이기 때문에 제거합니다.





###조회수

mm<-b[which(str_detect(b,"post_subject")) +3] ##조회수는 post_subject가 있는 line보다 항상 3줄 밑에 있습니다.

mm2<-str_extract(mm,perl("(?<=<td>).*(?=</td>)")) ## 정규표현식을 통해 <td>와 </td> 사이에있는것을 뽑습니다.

## 매우 자주쓰이는 정규표현식이니 항상 어딘가에 저장 해두시길 권장합니다. 

mm3<-as.numeric(mm2[-c(1:3)])



###url

uu<-b[which(str_detect(b,"post_subject")) -1] ## url은 post_subject가 있는 line보다 항상 1줄 위에 있습니다.

uu2<-str_extract(uu,perl("(?<=<td>).*(?=</td>)"))

uu3<-as.numeric(uu2[-c(1:3)])

cont_url<-paste0("http://www.clien.net/cs2/bbs/board.php?bo_table=park&wr_id=",uu3,"&page=",i) ##전체 url설정

data<-cbind(title,mm3,cont_url)

final<-rbind(final,data)

cat("\n",i)



}



head(final)

dim(final)

tail(final)

final<-data.frame(final)

colnames(final)<-c("제목","조회수","url")

write.csv(final,"final.csv",row.names=F)



url_list <- as.character(final$url) ## 게시글 url만 뽑기





con_data<-NULL ## 내용이 들어갈 dataframe빈값으로 선언

for(j in 1:length(url_list)){

b<-readLines(url_list[j],encoding='UTF-8')





## 게시글은 항상 !--EAP_CONTENT-- 와 !--/EAP_CONTENT--가 들어가 있는 line사이에 있습니다.

b2<-b[which(str_detect(b,"!--EAP_CONTENT--")):which(str_detect(b,"<!--/EAP_CONTENT-->"))] 

### 여기서 뽑힌 것들을 하나로 합칩니다. 구분자는 없이 ("") 붙입니다.

b3<-paste(b2,collapse = "")

b4<-gsub("<.*?>|&nbsp;|\t","",b3) ## 보통 html 소스는 <>안에 있기 때문에 < >안에있는 것들을 제거하고 그외에도 다른 html소스들을 제거합니다. 

## gsub함수는 문자열 제거 또는 대체 위한 함수로서 gsub("제거할것","대체할것",대상)으로 씁니다.

con_data<-c(con_data,b4) 

cat("\n",j)

}

url_list[5]





final_data<-cbind(final,con_data)



#fianl_data에는 클리앙 모두의 공원의 제목, 조회수, url, 내용이 들어가있습니다.
