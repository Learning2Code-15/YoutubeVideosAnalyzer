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



yt_oauth(app_id = "863216899807-866730r6aeuuf7p68vk2ck5n78dss6pk.apps.googleusercontent.com",
         app_secret = "HM8HMczhZi3DuTg92ch4CvQv",token = "")
#Prepare a list of channelId's
channel_id<- c("UCwAdQUuPT6laN-AQR17fe1g","UCe3qdG0A_gr-sEdat5y2twQ","UCbOkZlwRtOP3EGGAYd-Zi1Q")
channel_analysis=data.frame(channel_id,stringsAsFactors = TRUE)
channel_analysis=lapply(as.character(channel_analysis$channel_id), function(x){
  get_channel_stats(channel_id =x)
})
channel_analysis <- ldply(channel_analysis,data.frame)
channel_analysis_res$id=as.character(channel_analysis_res$id) 