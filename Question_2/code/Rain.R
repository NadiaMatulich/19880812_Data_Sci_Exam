
rain<-function(){
    g<-weather_data %>%
        group_by(week=week(new_date)) %>%
        filter(new_date>="2020-01-01") %>%
        mutate(weekly_rain=mean(precipitation)) %>%
        replace(is.na(.),0) %>%
        mutate(weekday=wday(new_date)) %>%
        filter(weekday==7) %>%
        ggplot(aes(x=new_date, y=weekly_rain))+
        geom_area(alpha=0.5 , fill="dark blue" )+
        theme_bw()+
        labs(title="Weekly Precipitation",
             subtitle = "London 2020",
             x="Date",
             y="Precipitation")


    g

}
