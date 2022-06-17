
deaths_by_cardio<-function(){

    #need to filter by last date and country
    #severity of covid measured by deaths per population

    deaths_by_country <- covid_data %>%
        filter(date==last(date)) %>%
        group_by(location) %>%
        summarise(deaths_per_1000000=total_deaths/(population/100000))

    co_morbidities <- covid_data %>%
        filter(date==last(date)) %>%
        select(c(location, aged_70_older,gdp_per_capita,extreme_poverty,cardiovasc_death_rate,diabetes_prevalence,female_smokers, male_smokers, life_expectancy, human_development_index))

    #cannot feasibly show all of these in 15 hours, so for now consider only co-morbidity data

    co_morbidities <- covid_data %>%
        filter(date==last(date)) %>%
        select(c(location, cardiovasc_death_rate,diabetes_prevalence,male_smokers,female_smokers))

    morbidity_deaths<-left_join(deaths_by_country,co_morbidities, by='location')

    i<- morbidity_deaths %>%
        ggplot(aes(x=cardiovasc_death_rate, y=deaths_per_1000000))+
        geom_point()+
        geom_smooth()+
        theme_bw()+
        labs(title="Deaths per 1000000 Population",
             subtitle = "By Cardio Vascular Death Prevalence",
             x="Cardio Vascular Death Prevalence",
             y="Deaths")
    i

}



#sorry this is really inelegant, did not know how to make a function print multiple outputs so in the interest of time I am doing it the inelegant way
#the first wrangling chunk is the same in each of them

deaths_by_diabetes<-function(){

    #need to filter by last date and country
    #severity of covid measured by deaths per population

    deaths_by_country <- covid_data %>%
        filter(date==last(date)) %>%
        group_by(location) %>%
        summarise(deaths_per_1000000=total_deaths/(population/100000))

    co_morbidities <- covid_data %>%
        filter(date==last(date)) %>%
        select(c(location, aged_70_older,gdp_per_capita,extreme_poverty,cardiovasc_death_rate,diabetes_prevalence,female_smokers, male_smokers, life_expectancy, human_development_index))

    co_morbidities <- covid_data %>%
        filter(date==last(date)) %>%
        select(c(location, cardiovasc_death_rate,diabetes_prevalence,male_smokers,female_smokers))

    morbidity_deaths<-left_join(deaths_by_country,co_morbidities, by='location')

    j<- morbidity_deaths %>%
        ggplot(aes(x=diabetes_prevalence, y=deaths_per_1000000))+
        geom_point()+
        geom_smooth()+
        theme_bw()+
        labs(title="Deaths per 1000000 Population",
             subtitle = "By Diabetes Prevalence",
             x="Diabetes Prevalence",
             y="Deaths")
    j
}



deaths_by_smoking<-function(){

    #need to filter by last date and country
    #severity of covid measured by deaths per population

    deaths_by_country <- covid_data %>%
        filter(date==last(date)) %>%
        group_by(location) %>%
        summarise(deaths_per_1000000=total_deaths/(population/100000))

    co_morbidities <- covid_data %>%
        filter(date==last(date)) %>%
        select(c(location, aged_70_older,gdp_per_capita,extreme_poverty,cardiovasc_death_rate,diabetes_prevalence,female_smokers, male_smokers, life_expectancy, human_development_index))

    #cannot feasibly show all of these in 15 hours, so for now consider only co-morbidity data

    co_morbidities <- covid_data %>%
        filter(date==last(date)) %>%
        select(c(location, cardiovasc_death_rate,diabetes_prevalence,male_smokers,female_smokers))

    morbidity_deaths<-left_join(deaths_by_country,co_morbidities, by='location')


    k<- morbidity_deaths %>%
        ggplot(aes(y=deaths_per_1000000))+
        geom_point(aes(x=male_smokers), color="blue")+
        geom_point(aes(x=female_smokers),colour="pink")+
        geom_smooth(aes(x=female_smokers), colour="pink")+
        geom_smooth(aes(x=male_smokers))+
        theme_bw()+
        labs(title="Deaths per 1000000 Population",
             subtitle = "By Smoking Prevalence",
             x="Smoking Prevalence",
             y="Deaths")
    k
}

deaths_by_smoking()