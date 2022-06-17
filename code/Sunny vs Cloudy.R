weather_data<-weather_data %>% mutate(new_date=ymd(weather_data$date))

average_sunny_and_cloudy<-function(){

    a<-weather_data %>% na.omit() %>%
        filter(new_date>="2017-01-01") %>%
        summarise(sunny=round(mean(sunshine),2),cloudy=round(mean(cloud_cover),2))

    a

}


last_five_years_weekly<-function(){

    g<-weather_data %>%
        group_by(week=week(new_date)) %>%
        filter(new_date>="2017-01-01") %>%
        mutate(weekly_sunshine=mean(sunshine)) %>%
        mutate(weekly_cloudy=mean(cloud_cover)) %>% #takes average by week and creates column with weekly average displayed for each day
        mutate(weekday=wday(new_date)) %>% #not elegant, but trying to not spend too long on figuring out the perfect way since i couldnt get summarise to give me what i was looking for (kept just giving me only 2021)
        filter(weekday==7) %>%
        ggplot(aes(x=new_date,y=weekly_sunshine))+
        geom_point(color="yellow", alpha=0.7)+
        geom_point(aes(y=weekly_cloudy), color="blue", alpha=0.07)+
        geom_smooth(color="yellow")+
        geom_smooth(aes(y=weekly_cloudy))+
        theme_bw()+
        labs(title="Weekly Sunshine and Cloudy Scores",
             subtitle = "London 2017-2021",
             x="Date",
             y="Score")

    g

}

