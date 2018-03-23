#DATA WRANGLING
#R VERSION 3.4.3

#LOAD IN PACKAGES
{
install.packages('devtools')
install.packages('jsonlite')
install.packages('ggplot2')
install.packages('magrittr')
install.packages('dplyr')
install.packages('anytime')
  
require(jsonlite)
require(ggplot2)
require(magrittr)
require(dplyr)
require(plyr)
require(anytime)
}
#LOAD DATA -----------------------------------------------
{
json_file = "~/playlist data challenge/data_science_challenge.json"
json_data = fromJSON(json_file, flatten = TRUE)

col_names = json_data$columns
raw_data = as.data.frame(json_data$data) 
colnames(raw_data) = col_names      

# glimpse(data) 
}
#DATA WRANGLING  -----------------------------------------------
{
working_data = raw_data %>% mutate(
  
  #change time from POSIX to YYYY:MM:DD HH:MM:SS
  ts = as.POSIXct(as.numeric(as.character(ts)), origin = '1970-01-01'),
  
  #change time from POSIX to YYYY:MM:DD HH:MM:SS
  track_upload_date = as.POSIXct(as.numeric(as.character(track_upload_date)), origin = '1970-01-01'),
  
  #CREATE new metric track duration time in seconds
  track_duration_seconds = as.numeric(as.character(track_duration)) / 1000,
  
  #change time from POSIXct to MM:SS
  track_duration = format(as.POSIXct(Sys.Date())+as.numeric(track_duration)/1000, "%M:%S"),
  
  #CREATE new metric listen_duration time in seconds
  listen_duration_seconds = as.numeric(as.character(listen_duration)) / 1000,
  
  #change time from POSIXct to MM:SS
  listen_duration = format(as.POSIXct(Sys.Date())+as.numeric(listen_duration)/1000, "%M:%S"),
  
  #change time from POSIX to YYYY:MM:DD HH:MM:SS
  listener_signup_date = as.POSIXct(as.numeric(as.character(listener_signup_date)), origin = '1970-01-01'),
  
  #CREATE new metric listener_prev_month time in hours
  listener_prev_month_listening_time_hours = as.numeric(as.character(listener_prev_month_listening_time)) / 1000 /60 /60,
  
  #change time from POSIXct to MM:SS
  listener_prev_month_listening_time = format(as.POSIXct(Sys.Date())+as.numeric(listener_prev_month_listening_time)/1000, "%M:%S"),
  
  #CREATE new metric percentage_listened
  percentage_listened = listen_duration_seconds / track_duration_seconds
 
)
    
}
#INITIAL DATA EXPLORATION/VISUALIZATION -----------------------------------------------
{
qplot(working_data$ts, bins = 1000)
qplot(working_data$country_code)
qplot(working_data$client_version)
qplot(working_data$listening_context)
qplot(working_data$recommender_algorithm_name)
#qplot(working_data$track_id) // too many to run
qplot(working_data$track_genre_category)
qplot(working_data$track_upload_date, bins = 1000)
qplot(working_data$track_duration_seconds, bins = 1000)
qplot(working_data$listen_duration_seconds, bins = 1000)
#qplot(working_data$listener_id) // too many to run 
qplot(working_data$listener_signup_date, bins = 1000)
qplot(working_data$listener_top_genre_category_listened)
qplot(working_data$listener_prev_month_listening_time, bins = 1000)
qplot(working_data$listener_prev_month_listening_time_hours)


}
#A CLOSER LOOK AT TRACK AND LISTEN DURATIONS  -----------------------------------------------
{
#TRACK DURATION

track_duration = working_data$track_duration_seconds
summary(track_duration)

qplot(working_data$track_duration, xlab = c(0,1000), bins = 10)
qplot(working_data$track_duration_seconds, xlab = c(0,1000))
qplot(log(working_data$track_duration_seconds), bins = 100) #[FIGURE 1]


#LISTEN DURATION
listen_duration_seconds = working_data$listen_duration_seconds
summary(working_data$listen_duration_seconds)

qplot(log(working_data$listen_duration_seconds), bins = 100) #[FIGURE 2]


#PERCENTAGE LISTENED 
percentage_listened = working_data$percentage_listened
summary(percentage_listened)

qplot(working_data$percentage_listened, bins = 1000)
qplot(working_data$percentage_listened, xlim = c(0,1.1), ylim = c(0,1e6), bins = 100) #[FIGURE 3] #zoom on in 0 to 100%
qplot(working_data$percentage_listened, xlim = c(0,.3), bins = 1000) #zoom in on 0 to 30%
qplot(working_data$percentage_listened, xlim = c(0,.1), bins = 1000) #zoom in on 0 to 10%

percentage_listened_under100 = subset(percentage_listened, percentage_listened < 1) #About 350k (42%)
percentage_listened_at100 = subset(percentage_listened, percentage_listened == 1) #About 460K (55%)
percentage_listened_over100 = subset(percentage_listened, percentage_listened > 1) #About 13K (1.6%)

percentage_listened_under5 = subset(percentage_listened, percentage_listened < .05) #About 145k (18%)
percentage_listened_under15 = subset(percentage_listened, percentage_listened < .15) #About 215k rows (26%)
percentage_listened_under30 = subset(percentage_listened, percentage_listened < .3) #About 260k rows (31%)

#Why are there Inf data in percentage listened?
percentage_inf = subset(percentage_listened, is.infinite(percentage_listened)) #there are 629 Inf points. lets keep these

#Why are there NA data in percentage listened?
percentage_NA = subset(percentage_listened, is.na(percentage_listened)) #there are only 10 NA points in an 800k dataset. probably a data recording issue
took_rec = ifelse(percentage_listened<.05,0,1)


}






