# This script pulls all the videos and associated details for provided channel ID's and plots
# below graphs

library(tuber)
library(dplyr)
library(plyr)
library(gridExtra)
library(ggplot2)
library(writexl)
library(readxl)
library(tm)
library(wordcloud)
library(RColorBrewer)

# My YouTube Oauth credntials

yt_oauth(app_id = "672457131068-fhqsi5fcdn1un01hbuj835dqetaneliv.apps.googleusercontent.com",
         app_secret = "TOyAaxGjxF5COZuS_49dgH19",token = "")#done
yt_oauth(app_id = "863216899807-866730r6aeuuf7p68vk2ck5n78dss6pk.apps.googleusercontent.com",
         app_secret = "HM8HMczhZi3DuTg92ch4CvQv",token = "")#done
yt_oauth(app_id = "578726141081-l7cs5vpn7ikipg8vilu63lcgnprp07hh.apps.googleusercontent.com",
         app_secret = "VtjVTxjsISLctpK_0QwDBa2F",token = "")#done
yt_oauth(app_id = "1072518033012-js3mqs2u31enr8psn66fkivl2k96q3lr.apps.googleusercontent.com",
         app_secret = "HQRNLUB1tXj9sv2kd93HAA0P",token = "")#pending
yt_oauth(app_id = "1067825464099-du88kfpa81g0vpjecdq54d9um4l197rg.apps.googleusercontent.com",
         app_secret = "CIW3YFNdRlG-PxqEqKW296vT",token = "")#done
yt_oauth(app_id = "42851752837-272quo54qjh51tmj3l6h57q6bn98coep.apps.googleusercontent.com",
         app_secret = "aDgypc0CuXjr0ORNDIC6s3_9",token = "")#pending
yt_oauth(app_id = "112138201772-neqo1mcamo0m6j54ksqklvnteacsdss0.apps.googleusercontent.com",
         app_secret = "WhAlOLu08cx-4crkQc0fOCyG",token = "")#pending
yt_oauth(app_id = "369326405510-a7l9k6mdvpm0p0be51n57q3lp94skoc5.apps.googleusercontent.com",
         app_secret = "T1WUDLd0ZD4WJ3VpYCcF8-2L",token = "")#done
yt_oauth(app_id = "84729495698-55l08juvudavpkodm01oi2d14m7agqnq.apps.googleusercontent.com",
         app_secret = "ZPkBWuGxg4bC_oNX6wIrXUiO",token = "")#done

#Prepare a list of channelId's from excel sheet
channel_id<- read_excel("channelanalysis.xlsx","Input")
channel_analysis=data.frame(channel_id,stringsAsFactors = TRUE)

#Pull all channel Stats
channel_analysis=lapply(as.character(channel_analysis$channel_id), function(x){
  get_channel_stats(channel_id =x)
})
channel_analysis <- ldply(channel_analysis,data.frame)
channel_analysis_table <- cbind.data.frame("Channel Title"=channel_analysis$snippet.title,
                                           "Country"=(ifelse(is.na(channel_analysis$snippet.country)==TRUE,"Not Provided",as.character(channel_analysis$snippet.country))),"View Count"=channel_analysis$statistics.viewCount,
                                          "Subscriber Count"=(ifelse((channel_analysis$statistics.hiddenSubscriberCount=="TRUE"),"Hidden",as.character(channel_analysis$statistics.subscriberCount)))
                                          ,"Video Count"=channel_analysis$statistics.videoCount)
channel_analysis_table=channel_analysis_table[order(-as.numeric(as.character(channel_analysis_table$`View Count`))),]
kable_styling(kable(channel_analysis_table,"html",row.names = FALSE))
channel_analysis$id=as.character(channel_analysis$id) 
channel_analysis$snippet.title=as.character(channel_analysis$snippet.title) 

#Pulling all video stats for all videos in the channels and adding to excel sheet.
videostats_All=data.frame(matrix(ncol = 0,nrow = 0))
videostats=data.frame(ncol=0,nrow=0)
videostats=get_all_channel_video_stats(channel_analysis$id[7])
videostats$chid[1:nrow(videostats)]=channel_analysis$id[7]
videostats$chtitle[1:nrow(videostats)]=channel_analysis$snippet.title[7]


videostats_All=rbind(videostats_All,videostats)

# Write to excel sheet to avoid loss of data
#write.csv(videostats,"videostats.csv")
write.csv(videostats_All,"videostats.csv",append = TRUE,row.names = FALSE,col.names = FALSE)

#Remove duplicate entries because it happens sometimes and rename ID colomn to Vid ( Video ID )
names(videostats_All)[names(videostats_All) == 'id'] <- 'vid'

# Keep only the videos from 2019 and 2020 and required colomns
videostats_All$date=as.Date(videostats_All$publication_date)
videostats_All$viewCount=as.numeric(as.character(videostats_All$viewCount))
videostats_All$chid=as.character(videostats_All$chid)
channel_analysis$snippet.title=as.character(channel_analysis$snippet.title)
videostats_All=subset(videostats_All,videostats_All$date >='2019-01-01')
videostats_All=subset(videostats_All,select = c("title","publication_date","viewCount","likeCount","chtitle","date","chid"))

#Publish date vs View Count Plot for Videos#


plot_views_all = ggplot() 
videostats=tail(videostats_All[order(videostats_All$viewCount),],200)
plot_views_all=ggplot(videostats, aes(x=date, y=viewCount, group=chtitle)) +
  geom_point(aes(color=chtitle),size=2.5)+theme_bw()+
  theme(legend.position="none",axis.title=element_text(size=10,face="bold"))+scale_y_continuous(name="View Count", labels = scales::comma)+
  scale_x_date(name="Published Date",date_breaks = "months" , date_labels = "%b-%y")+ggtitle("Distribution of top 200 viewed videos in 2019 and 2020")

# For 2020 top 100 videos 
videostats=subset(videostats_All,videostats_All$date >='2020-01-01')
plot_views_tw = ggplot() 
videostats=tail(videostats[order(videostats$viewCount),],200)
plot_views_tw=ggplot(videostats, aes(x=date, y=viewCount, group=chtitle)) +
  geom_point(aes(color=chtitle),size=2.5)+theme_bw()+
  theme(legend.position = "none",axis.title=element_text(size=10,face="bold"))+scale_y_continuous(name="View Count", labels = scales::comma)+
  scale_x_date(name="Published Date",date_breaks = "months" , date_labels = "%b-%y")+ggtitle("Distribution of top 200 viewed videos in 2020")

#For 2019 top 100 videos
videostats=subset(videostats_All,videostats_All$date <'2020-01-01')
plot_views_nt = ggplot() 
videostats=tail(videostats[order(videostats$viewCount),],200)
plot_views_nt=ggplot(videostats, aes(x=date, y=viewCount, group=chtitle)) +
  geom_point(aes(color=chtitle),size=2.5)+theme_bw()+
  theme(legend.background =element_rect(color = "steel"),legend.position="bottom",axis.title=element_text(size=10,face="bold"))+scale_y_continuous(name="View Count", labels = scales::comma)+
  scale_x_date(name="Published Date",date_breaks = "months" , date_labels = "%b-%y")+ggtitle("Distribution of top 200 viewed videos in 2019")+
  scale_color_discrete("Channel Name")

grid.arrange(plot_views_all,plot_views_tw,plot_views_nt)

videostatsp=subset(videostats,videostats$date <'2020-01-01')
videostatsp=tail(videostatsp[order(videostatsp$date,videostatsp$viewCount),],50)
videostatsp=subset(videostatsp,select = c("title","date","chtitle"))
videostatsp=kable_styling(kable(videostatsp,"html"))
#Generating word cloud using topic
videostats$title=as.character(videostats$title)
for(i in 1:500)
videostats$title[i]=gsub("<([a-zA-Z0-9]|[_]|[+]|[>])+" , "", videostats_All$title[i])

alldesc = VCorpus(VectorSource(videostats$title))
#Step 1: cleaning
corp = tm_map(alldesc, removePunctuation) 
corp = tm_map(corp, removeNumbers) 
corp = tm_map(corp, content_transformer(tolower) ,lazy=TRUE) 
corp = tm_map(corp, content_transformer(removeWords), c("TIL") ,lazy=TRUE)
corp = tm_map(corp, removeWords, c("the", "for", "is", "and","what","which","with","you","can","will","these","i'm"))

dtm = DocumentTermMatrix(corp)
dtms = removeSparseTerms(dtm, 0.983)
dim(dtm)
dtms_m = as.matrix(dtms)

# colSums adds up value over all of the Columns in a matrix
# rowSums(m) is the equivalent over rows
word.freq = colSums(dtms_m)
word.freq = sort(word.freq, decreasing=T)
d <- data.frame(word = names(word.freq),freq=word.freq)

#create wordcloud
top_Viewed_Wordcloud_topic=wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                                     max.words=200, random.order=FALSE, rot.per=0.35, 
                                     colors=brewer.pal(8, "Dark2"))
  
# Top 20 topics w