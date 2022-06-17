
mean_temp<-function(){

    g<-weather_data %>%
        filter(new_date>="2020-01-01") %>%
        ggplot(aes(x=new_date, y=mean_temp))+
        geom_point(alpha=0.5)+
        geom_smooth(color="dark grey")+
        theme_bw()+
        labs(title="Average Temperature",
             subtitle = "London 2020",
             x="Date",
             y="Temperature")

    g

}

mean_temp()


min_and_max_temp<-function(){

    g<-weather_data %>%
        group_by(week=week(new_date)) %>%
        filter(new_date>="2020-01-01") %>%
        ggplot(aes(x=new_date, y=max_temp))+
        geom_area(alpha=0.5 , fill="dark blue" )+
        geom_area(aes(y=min_temp), color="dark blue", fill="dark blue")+
        theme_bw()+
        labs(title="London Minimum and Maximum Temperatures",
             subtitle = "London 2020",
             x="Date",
             y="Temperature")

    g

}
