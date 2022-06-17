#Comparing Deaths and Cases by Regions

regions_compare<-function(vjust_input, size_input, round_input){

    deaths_by_region <- covid_data %>%
        filter(date==last(date)) %>%
        filter(continent==c("Africa","Europe","South America","North America", "Oceania", "Asia")) %>% #sorry I couldn't think of a more elegant way to get rid of the world averages OWID included with blank (But not NA?) continents
        group_by(continent) %>%
        summarise(total_deaths=sum(x=total_deaths_per_million, na.rm=T))

    cases_by_region <- covid_data %>%
        filter(date==last(date)) %>%
        filter(continent==c("Africa","Europe","South America","North America", "Oceania", "Asia")) %>%
        group_by(continent) %>%
        summarise(total_cases=sum(x=total_cases_per_million, na.rm=T))

    cases_and_deaths<-left_join(deaths_by_region, cases_by_region, by='continent')

    g<- cases_and_deaths %>%
        ggplot(aes(x=reorder(continent,total_deaths,decreasing=TRUE), y=total_cases, fill=factor(ifelse(continent=="Africa", "Africa","Other Regions"))))+
        geom_bar(stat="identity")+
        geom_text(aes(label=round(total_deaths,round_input)),vjust=vjust_input, size=size_input)+
        theme_bw()+
        theme(legend.position="none")+
        labs(title="Total Cases Per Million",
             subtitle="with Total Deaths Per Million",
             x="Region",
             y="Total",)+
        scale_fill_brewer(palette="Purples")
    g

}


regions_compare(-0.5,3,0)

regions_compare_2<-function(){

    deaths_by_region <- covid_data %>%
        filter(date==last(date)) %>%
        filter(continent==c("Africa","Europe","South America","North America", "Oceania", "Asia")) %>%
        group_by(continent) %>%
        summarise(total_deaths=sum(x=total_deaths_per_million, na.rm=T))

    cases_by_region <- covid_data %>%
        filter(date==last(date)) %>%
        filter(continent==c("Africa","Europe","South America","North America", "Oceania", "Asia")) %>%
        group_by(continent) %>%
        summarise(total_cases=sum(x=total_cases_per_million, na.rm=T))

    cases_and_deaths<-left_join(deaths_by_region, cases_by_region, by='continent')

    cases_and_deaths<-cases_and_deaths %>% mutate(., new_var=total_deaths/total_cases)

    h<- cases_and_deaths %>%
        ggplot(aes(x=reorder(continent,new_var,decreasing=TRUE), y=new_var, fill=factor(ifelse(continent=="Africa", "Africa","Other Regions"))))+
        geom_bar(stat="identity")+
        theme_bw()+
        theme(legend.position="none")+
        labs(title="Proportion of Deaths",
             x="Region",
             y="Total",)+
        scale_fill_brewer(palette="Purples")
    h
}

regions_compare_2()


```