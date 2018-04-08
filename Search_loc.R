

################

### Naver지도에서 쿼리를 날려 위도 경도 받아오는 함수 짜기



library(RJSONIO)




search_loc<-function(keyword){

loc<-iconv(keyword,from="cp949",to="UTF-8") ## 키워드 인코딩 변환

juso<-URLencode(loc)



url<-paste0("https://m.map.naver.com/search2/searchMore.nhn?query=",juso,"&sm=clk&page=1&displayCount=75&type=SITE_1") ## 모바일 네이버 지도 활용(네이버지도는 막혀있음)

## 여기서도 ctrl +shit +j로 진짜 주소를 찾아올 수 있습니다.

b<-readLines(url,encoding="UTF-8") 

head(b)

b2<-paste(b,collapse = " ") ##JSON형태라 R에서 List로 바꿔서 써야합니다.

head(b2)

b3<-fromJSON(b2) # list로 변환

head(b3)

## JSON안에있는 요소들을 $로 접근이 가능합니다.

## b4$result$site$list[[1]]은 검색결과 맨위에 나오는 위치의 정보가 들어가 있습니다.

## b4$result$site$list[[1]]은 두번째 결과이겠죠

wg<-c(b3$result$site$list[[1]]$x,b3$result$site$list[[1]]$y)

wg



}



search_loc("연세대")

