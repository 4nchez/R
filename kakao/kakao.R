useSejongDic()
mergeUserDic(data.frame(readLines("C:\\Shin_Python\\R분석\\ka.txt"),"ncn"))
txt=readLines("C:\\Shin_Python\\R분석\\kakao.txt")
txt

kko1=sapply(txt, extractNoun, USE.NAMES = F)
kko1
kko2=unlist(txt)
kko2=unlist(kko1)
kko2
head(unlist(kko2),30)
kko2=gsub(" ","",kko2)
kko2 <- gsub('[ㄱ-ㅎ]','',kko2)
kko2 <- gsub('\\(사진\\)','',kko2)
kko2 <- gsub('\\(이모티콘\\)','',kko2)
kko2 <- gsub('[0-9]','',kko2)
kko2 <- gsub('\\/','',kko2)
kko2 <- gsub('\\[','',kko2)
kko2 <- gsub('\\]','',kko2)
kko3=str_replace_all(kko2,"[^[:alpha:]]","")#한글 영어 외에 모든것 삭제

txt2=readLines("C:\\Shin_Python\\R분석\\kagsub.txt")#데이터에 커서가 있을 경우 에러 발생
txt2
i=1
for(i in 1:length(txt2)){
  kko3=gsub((txt2[i]),"",kko3)
}
kko3=Filter(function(x){
  nchar(x)>=2
},kko3
)
kko3
write(unlist(kko3),"C:\\Shin_Python\\R분석\\kakao2.txt")
kko4=read.table("C:\\Shin_Python\\R분석\\kakao2.txt")
kko4
nrow(kko4)

#6. 단어 빈도수 저장
wc=table(kko4)
wc
head(sort(wc,decreasing = T),20)

op=par(oma=c(1,0.5,3,1), mfrow=c(1,1))
dev.new()
#7. wordcloud 출력
pal=brewer.pal(9,'Set3')
wordcloud(names(wc),freq = wc, scale = c(5,1), rot.per = 0.25, min.freq = 1, random.order = F, random.color = T,colors = pal)
legend(0.3,1,"카톡방",cex=0.8,fill=NA, border=NA, bg='white', text.col='red', text.font=2,box.col='red')

bchart=head(sort(wc, decreasing = T),10)
bb=c(bchart)
colors=c()
i=1
bchart[i]
for(i in 1:length(bchart)){
  if (bchart[i]>35) {
    colors=c(colors,'red')
  }else if(bchart[i]>16){
    colors=c(colors,'yellow')
  }else if(bchart[i]>14){
    colors=c(colors,'blue')
  }else{
    colors=c(colors,'pink')
  }
}
top10=head(sort(wc, decreasing = T),10)
#top10
#bar
bp=barplot(bchart, main = "카톡방 키워드 top10",col=colors, cex.names=1, las=1, ylim=c(0,150))
text(x=bp,y=bchart*0.95, labels = paste(bchart,"건"), col = "black", cex=1)
legend(5,75,"12월 29일 ~ 12월 31일",cex=0.8,fill=NA, border=NA, bg='white', text.col='red', text.font=1,box.col='red')
