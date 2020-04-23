#This script lists the channels associated with the keyword "STOCK MARKET TUTORIALS 
#on youtube.
#Search and get list of youtube videos.
#Step1: Perform Oauth authentication for access to Youtube API using Console
#yt_oauth(app_id = 'your client id' ,app_secret = 'your client secret',token = '')
#Reference:https://www.storybench.org/how-to-download-youtube-data-in-r-using-tuber-and-purrr/
# yt_oauth(app_id = "578726141081-l7cs5vpn7ikipg8vilu63lcgnprp07hh.apps.googleusercontent.com",
# app_secret = "VtjVTxjsISLctpK_0QwDBa2F",token = "")

library(tuber)
library(dplyr)
library(plyr)
library(gridExtra)
library(ggplot2)
library(writexl)



yt_oauth(app_id = "672457131068-fhqsi5fcdn1un01hbuj835dqetaneliv.apps.googleusercontent.com",
         app_secret = "TOyAaxGjxF5COZuS_49dgH19",token = "")
#Prepare a list of channelId's
channel_id<- c("UCwAdQUuPT6laN-AQR17fe1g","UCe3qdG0A_gr-sEdat5y2twQ","UCbOkZlwRtOP3EGGAYd-Zi1Q")
channel_analysis=data.frame(channel_id,stringsAsFactors = TRUE)
channel_analysis=lapply(as.character(channel_analysis$channel_id), function(x){
  get_channel_stats(channel_id =x)
})
channel_analysis <- ldply(channel_analysis,data.frame)
channel_analysis$id=as.character(channel_analysis$id) 

#excelsheet=write_xlsx(channel_analysis,"channelanalysis.xlsx")
#To view channel analysis in a neat format
channel_analysis_tab=data.frame("Title"=channel_analysis$snippet.title,
                                "View Count"=channel_analysis$statistics.viewCount,
                                "Subscriber Count"=channel_analysis$statistics.subscriberCount,
                                "Video Count"=channel_analysis$statistics.videoCount)
kable_styling(kable(channel_analysis_tab))

#Top 20 videos in all channel and top 3 in each channel colr scheme channel
videostats_All=data.frame(matrix(ncol = 0,nrow = 0))
for(i in 1:3) {
  videostats=get_all_channel_video_stats(channel_analysis$id[i])
  videostats$chid[1:nrow(videostats)]=channel_analysis$id[i]
  videostats$chtitle[1:nrow(videostats)]=channel_analysis$id[i]
  videostats_All=rbind(videostats_All,videostats)
  } 
videostats_All=videostats_All[!duplicated(videostats_All[,c('id')]),]
excelsheet=write_xlsx(list(videostats_sheet=videostats_All,channelstats_sheet=channel_analysis),excelsheet)
videostats_All <- merge(videostats_All,channel_analysis[,c("id","snippet.title")], by.x = "channelid",by.y = "id",sort = F, all.x = T)

videostats_All$likeCount=as.numeric(videostats_All$likeCount)
videostats_All$viewCount=as.numeric(videostats_All$viewCount)

Top_Liked_all=tail(videostats_All[order(videostats_All$likeCount),],20)
Top_Viewed_all=tail(videostats_All[order(videostats_All$viewCount),],20)
Top_Viewed_all$channelid=as.factor(Top_Viewed_all$channelid)
Viewedplot=ggplot(Top_Viewed_all, aes(x=viewCount, y=title, fill=snippet.title)) +
  geom_bar(stat="identity", width=0.75,position = position_dodge(width=0.05))+theme_minimal()
Viewedplot=Viewedplot + theme(axis.text = element_text(size=7.5) ,axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank())+ labs(x ="Number of Views",fill="CHANNEL TITLE")+scale_fill_brewer(palette = "Set2")
Viewedplot
#Word cloud for channel