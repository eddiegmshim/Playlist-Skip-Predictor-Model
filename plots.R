
install.packages('ggplot2')
require(ggplot2)


#SCATTER GRAPHS  -----------------------------------------------

#LISTENER_PREV_MONTH_LISTENING_TIME_HOURS #[FIGURE 5]
qplot(pred, listener_prev_month_listening_time_hours, data = output, color = combined_result, alpha = I(0.3), size = I(0.5)) +
  scale_color_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                            'Model = Skip, Actual = Listened',
                                                                                            'Model = Listened, Actual = Skip',
                                                                                            'Model = Listened, Actual = Listened')) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle(' Listener’s previous month listening time hours ' ) +
  labs(x = 'Model Probability of Approval') +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size=10), legend.title = element_blank())


#LISTENER_PREV_MONTH_AVG_DAILY_TRACKS_LISTENED #[FIGURE 6]
qplot(pred, as.numeric(listener_prev_month_avg_daily_tracks_listened), data = output, color = combined_result, alpha = I(0.3), size = I(0.5)) +
  scale_color_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                            'Model = Skip, Actual = Listened',
                                                                                            'Model = Listened, Actual = Skip',
                                                                                            'Model = Listened, Actual = Listened')) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('Listener’s previous month average daily track listened ' ) +
  labs(x = 'Model Probability of Approval') + 
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size=10), legend.title = element_blank())


#LISTENING_CONTEXT #[FIGURE 7]
qplot(pred, listening_context, data = output, color = combined_result, alpha = I(0.3), size = I(0.5)) +
  scale_color_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                            'Model = Skip, Actual = Listened',
                                                                                            'Model = Listened, Actual = Skip',
                                                                                            'Model = Listened, Actual = Listened')) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('Listening context' ) +
  labs(x = 'Model Probability of Approval') + 
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size=10), legend.title = element_blank())


#TS_HOUR #[FIGURE 8]
qplot(pred, as.numeric(ts_hour), data = output, color = combined_result, alpha = I(0.3), size = I(0.5)) +
  scale_color_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                            'Model = Skip, Actual = Listened',
                                                                                            'Model = Listened, Actual = Skip',
                                                                                            'Model = Listened, Actual = Listened')) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('Timestamp (Hour)' ) +
  labs(x = 'Model Probability of Approval') + 
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size=10), legend.title = element_blank())




#LISTENER_SIGNUP_DAY #[FIGURE 9]
qplot(pred, listener_signup_day, data = output, color = combined_result, alpha = I(0.3), size = I(0.5)) +
  scale_color_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                            'Model = Skip, Actual = Listened',
                                                                                            'Model = Listened, Actual = Skip',
                                                                                            'Model = Listened, Actual = Listened')) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('Listener signup date (Day)' ) +
  labs(x = 'Model Probability of Approval') + 
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size=10), legend.title = element_blank())




#LISTENER_TOP_GENRE_CATEGORY_LISTENED #[FIGURE 10]
qplot(pred, listener_top_genre_category_listened, data = output, color = combined_result, alpha = I(0.3), size = I(0.5)) +
  scale_color_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                            'Model = Skip, Actual = Listened',
                                                                                            'Model = Listened, Actual = Skip',
                                                                                            'Model = Listened, Actual = Listened')) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('Listener top genre listened' ) +
  labs(x = 'Model Probability of Approval') + 
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size=10), legend.title = element_blank())





#BAR GRAPHS  -----------------------------------------------


#LISTENER_PREV_MONTH_AVG_DAILY_TRACKS_LISTENED #[FIGURE 11]
ggplot(output, aes(x= listener_prev_month_avg_daily_tracks_listened, fill = combined_result)) + geom_bar(position = 'dodge') + 
  scale_fill_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                           'Model = Skip, Actual = Listened',
                                                                                           'Model = Listened, Actual = Skip',
                                                                                           'Model = Listened, Actual = Listened'))+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('Average daily tracks listened prev month' )


#LISTENING_CONTEXT #[FIGURE 11]
ggplot(output, aes(x= listening_context, fill = combined_result)) + geom_bar(position = 'dodge') + 
  scale_fill_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                            'Model = Skip, Actual = Listened',
                                                                                            'Model = Listened, Actual = Skip',
                                                                                            'Model = Listened, Actual = Listened'))+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('Listening context' )


#TS_HOUR #[FIGURE 12]
ggplot(output, aes(x= ts_hour, fill = combined_result)) + geom_bar(position = 'dodge') + 
  scale_fill_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                           'Model = Skip, Actual = Listened',
                                                                                           'Model = Listened, Actual = Skip',
                                                                                           'Model = Listened, Actual = Listened'))+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('TS hour' )

#TS_DAY #[FIGURE 13]
ggplot(output, aes(x= ts_day, fill = combined_result)) + geom_bar(position = 'dodge') + 
  scale_fill_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                           'Model = Skip, Actual = Listened',
                                                                                           'Model = Listened, Actual = Skip',
                                                                                           'Model = Listened, Actual = Listened'))+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('TS day' )



#LISTENER_TOP_GENRE_CATEGORY_LISTENED #[FIGURE 14]
ggplot(output, aes(x= listener_top_genre_category_listened, fill = combined_result)) + geom_bar(position = 'dodge') + 
  scale_fill_manual(values = c('grey', 'firebrick1', 'royalblue', 'limegreen'), labels = c('Model = Skip, Actual = Skip',
                                                                                           'Model = Skip, Actual = Listened',
                                                                                           'Model = Listened, Actual = Skip',
                                                                                           'Model = Listened, Actual = Listened'))+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 10))) + theme_minimal() + ggtitle('Listener top genre listened' )
