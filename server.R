server <- function(input, output, session) {
  # Loading Data ----
  
  args <- commandArgs(TRUE)
  args <- ifelse(length(args)==0,"../COVIDiSTRESS_April_27_clean.csv",args)
  data = read.csv(args, header=T, stringsAsFactors=F)
  
  data = data %>%
    mutate(Country=recode(Country,"Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))
  
  
  # Creating Variables ----
  
  if(file.exists("Unique_CountryName_full.csv"))
  {
    Unique_CountryName_full <- read.csv("Unique_CountryName_full.csv", header = T, stringsAsFactors = F)$x
  }else{
    Unique_CountryName_full <- unique(toTitleCase(tolower(data$Country)))
    Unique_CountryName_full <- sort(Unique_CountryName_full[Unique_CountryName_full!=""])
    Unique_CountryName_full <- c(Unique_CountryName_full, "World")
    #execute the following line to speed up the process (not generate country names at every run)
    #write.csv(Unique_CountryName_full,"Unique_CountryName_full.csv", row.names = TRUE)
  }
  
  updateSelectizeInput(session, 'CountryChoice', choices = Unique_CountryName_full, server = TRUE,
                       selected=c("France", "Italy"))
  
  #Functions ----
  get_world_map <- function(s){
    world_map <- map_data(s)
    world_map$country = world_map$region
    
    world_map[world_map$region == "Grenadines","country"] <- "Saint Vincent and the Grenadines"
    world_map[world_map$region == "Saint Vincent","country"] <- "Saint Vincent and the Grenadines"
    world_map[world_map$region == "Antigua","country"] <- "Antigua and Barbuda"
    world_map[world_map$region == "Barbuda","country"] <- "Antigua and Barbuda"
    
    world_map[world_map$region == "Aruba","country"] <- "Netherlands"
    world_map[world_map$region == "Curacao","country"] <- "Netherlands"
    world_map[world_map$region == "Bonaire","country"] <- "Netherlands"
    world_map[world_map$region == "Sint Eustatius","country"] <- "Netherlands"
    world_map[world_map$region == "Saba","country"] <- "Netherlands"
    world_map[world_map$region == "Sint Maarten","country"] <- "Netherlands"
    
    world_map[world_map$region == "Anguilla","country"] <- "UK"
    world_map[world_map$region == "Bermuda","country"] <- "UK"
    world_map[world_map$region == "Falkland Islands","country"] <- "UK"
    world_map[world_map$region == "Chagos Archipelago","country"] <- "UK"
    world_map[world_map$region == "Pitcairn Islands","country"] <- "UK"
    world_map[world_map$region == "South Sandwich Islands","country"] <- "UK"
    world_map[world_map$region == "Saint Helena","country"] <- "UK"
    world_map[world_map$region == "Ascension Island","country"] <- "UK"
    world_map[world_map$region == "Turks and Caicos Islands","country"] <- "UK"
    
    world_map[world_map$region == "French Southern and Antarctic Lands","country"] <- "France"
    world_map[world_map$region == "Saint Barthelemy","country"] <- "France"
    world_map[world_map$region == "Reunion","country"] <- "France"
    world_map[world_map$region == "Mayotte","country"] <- "France"
    world_map[world_map$region == "French Guiana","country"] <- "France"
    world_map[world_map$region == "Martinique","country"] <- "France"
    world_map[world_map$region == "Guadeloupe","country"] <- "France"
    world_map[world_map$region == "Saint Martin","country"] <- "France"
    world_map[world_map$region == "New Caledonia","country"] <- "France"
    world_map[world_map$region == "French Polynesia","country"] <- "France"
    world_map[world_map$region == "Saint Pierre and Miquelon","country"] <- "France"
    world_map[world_map$region == "Wallis and Futuna","country"] <- "France"
    
    world_map[world_map$region == "Canary Islands","country"] <- "Spain"
    world_map[world_map$region == "Montserrat","country"] <- "Spain"
    
    world_map[world_map$region == "Azores","country"] <- "Portugal"
    
    world_map[world_map$region == "Guam","country"] <- "USA"
    world_map[world_map$region == "Puerto Rico","country"] <- "USA"
    
    world_map[world_map$region == "Heard Island","country"] <- "Australia"
    world_map[world_map$region == "Cocos Islands","country"] <- "Australia"
    world_map[world_map$region == "Christmas Island","country"] <- "Australia"
    world_map[world_map$region == "Norfolk Island","country"] <- "Australia"
    
    world_map[world_map$region == "Siachen Glacier","country"] <- "India"
    
    world_map[world_map$region == "Trinidad","country"] <- "Trinidad and Tobago"
    world_map[world_map$region == "Tobago","country"] <- "Trinidad and Tobago"
    return(world_map)
  }
  
  
  #Beginning of the dynamic part ----
  observeEvent({
    input$CountryChoice
    input$MapRegionChoice
    input$ConcernChoice
  },{
    
    country_list <- input$CountryChoice
    #country_list <- c("France", "Italy")
    
    #Generating Gender 100% barplot (by @ggautreau) ---- 
    
    genders <- c("Female","Male","Other/would rather not say")
    
    processed_data = data %>%
      filter(Country%in%country_list,Dem_gender != "") %>%
      group_by(Country,Dem_gender) %>%
      summarise(nb_surveyed=n()) %>%
      ungroup() %>%
      group_by(Country) %>%
      mutate(perc_surveyed_by_country = (nb_surveyed / sum(nb_surveyed))) %>%
      ungroup() %>%
      mutate(country_gender_text = paste0(
        "Country: ", Country, "\n",
        "Gender: ", Dem_gender, "\n",
        "# of surveyed: ", nb_surveyed, "\n",
        "% of surveyed: ", round(perc_surveyed_by_country*100, 2), "%\n"))
    processed_data$Country <- factor(processed_data$Country, levels = rev(country_list))
    processed_data$Dem_gender <- factor(processed_data$Dem_gender, levels = rev(genders))
    pGender100 <- ggplot(data = processed_data) +
      geom_bar(aes(x = Country, y = perc_surveyed_by_country, fill = Dem_gender, text = country_gender_text), stat="identity", size=0.5, color="grey20") +
      scale_fill_manual(name="Gender", values=c("Female" = "#00c7b8ff", "Male" = "#31233bff","Other/would rather not say" = "#fbedcdff")) +
      coord_flip() +
      scale_y_continuous(breaks=seq(0,1,0.1),labels = scales::percent_format(accuracy = 1),expand=c(0,0))+
      scale_x_discrete(expand=c(0,0))+
      labs(x = "Country", y = "% Gender") +
      theme_classic() +
      theme(legend.position="top")
    
    #Generating Age Pyramid (by @ggautreau) ---- 
    
    processed_data <- data[data$Country%in%country_list,]
    processed_data$Dem_age_sliced <- cut(as.numeric(processed_data$Dem_age), breaks = seq(0, 100, 5), right = FALSE)
    processed_data <- processed_data[!is.na(processed_data$Dem_age_sliced),]
    
    label_ages <- function(x){x <- str_replace(x,"\\)", "[")
    str_replace(x,",", "-")}
    
    pAge <- ggplot(data = processed_data, aes(x = Dem_age_sliced, fill=Dem_gender)) +
      geom_bar(data = subset(processed_data, Dem_gender == "Female"), aes(y = ..count.. * (-1), text = ..count..)) +
      geom_bar(data = subset(processed_data, Dem_gender == "Male"), aes(y = ..count.. , text = ..count..)) +
      scale_fill_manual(name="Gender", values = c("Female" = "#00c7b8ff", "Male" = "#31233bff")) +
      scale_y_continuous(labels = abs) +
      scale_x_discrete(labels = label_ages) +
      geom_hline(yintercept=0, size=0.1) +
      coord_flip() +
      labs(x = "Age ranges", y = "# of surveyed") +
      theme_classic()
    
    #Generating Education Distribution Plot (by @ggautreau) ---- 
    
    education <- c("- PhD/Doctorate", "- College degree, bachelor, master", "- Some College, short continuing education or equivalent", "- Up to 12 years of school ", "- Up to 9 years of school", "- Up to 6 years of school", "- None")
    
    processed_data = data %>%
      filter(Country%in%country_list,Dem_edu %in% education) %>%
      group_by(Country,Dem_edu) %>%
      summarise(nb_surveyed=n()) %>%
      ungroup() %>%
      group_by(Country) %>%
      mutate(perc_surveyed_by_country = (nb_surveyed / sum(nb_surveyed))) %>%
      ungroup() %>%
      mutate(country_edu_text = paste0(
        "Country: ", Country, "\n",
        "Education: ", Dem_edu, "\n",
        "# of surveyed: ", nb_surveyed, "\n",
        "% of surveyed: ", round(perc_surveyed_by_country*100, 2), "%\n"))
    processed_data$Country <- factor(processed_data$Country, levels = rev(country_list))
    processed_data$Dem_edu <- factor(processed_data$Dem_edu, levels = rev(education))
    
    pEdu <- ggplot(data = processed_data) +
      geom_bar(aes(x = Country, y = perc_surveyed_by_country, fill = Dem_edu, text = country_edu_text), stat="identity", size=0.5, color="grey20") +
      scale_fill_manual(name="Education", values=c("- PhD/Doctorate" = "#31233bff", "- College degree, bachelor, master" = "#50456cff","- Some College, short continuing education or equivalent" = "#6b6099ff","- Up to 12 years of school " = "#9392b7ff","- Up to 9 years of school" = "#b0b0d1ff","- Up to 6 years of school" = "#bec0d4ff", "- None" = "#f8f8ffff")) +
      coord_flip() +
      scale_y_continuous(breaks=seq(0,1,0.1),labels = scales::percent_format(accuracy = 1),expand=c(0,0))+
      scale_x_discrete(expand=c(0,0))+
      labs(x = "Country", y = "% Education") +
      theme_classic() +
      theme(legend.position="top")
    
    #Generating Isolation Map Plot (by @ggautreau) ---- 
    isolation_map <- function(world="world"){
      #world = "world" means a atlantic-centred map
      #world = "world2" means a pacific-centred map
      data = data %>%
        mutate(Country=recode(Country,"- other"="NA","Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))
      
      processed_data = data %>%
        filter(Country!="") %>%
        mutate(isolation_score=recode(Dem_islolation,"Life carries on as usual"=0,"Life carries on with minor changes"=1,"Isolated"=2,"Isolated in medical facility of similar location"=3,.default=as.numeric(NA))) %>%
        group_by(Country) %>%
        summarise(mean_isolation_score = mean(isolation_score,na.rm=T),sd_isolation_score = sd(isolation_score,na.rm=T),
                  nb_answers = n())
      
      processed_world_map = get_world_map(world)
      processed_world_map = left_join(processed_world_map,processed_data, by=c("country"="Country")) %>%
        mutate(country_text = paste0(
          "Country: ", country, "\n",
          "Region: ", region, "\n",
          "Mean isolation score: ", round(mean_isolation_score,2), "\n",
          "Std dev. isolation score: ", round(sd_isolation_score,2), "\n",
          "# of answers: ", nb_answers))
      
      p <- ggplot(as.data.frame(processed_world_map)) +
        geom_polygon(aes( x = long, y = lat, group = group, fill = mean_isolation_score, text = country_text), colour = "black", size = 0.2)+
        scale_fill_distiller(palette="RdYlBu", name = "isolation score", limits = c(0, 3), breaks = c(0,1,2,3), labels= c("0 - Life carries on as usual","1 - Life carries on with minor changes","2 - Isolated","3 - Isolated in medical facility of similar location"),values=c(0,0.45,0.55,1))+
        theme_void()
      
    }
    
    #Function for Generating Stress Map Plot (by @ggautreau) ---- 
    stress_map <- function(world="world"){
      #world = "world" means a atlantic-centred map
      #world = "world2" means a pacific-centred map
      data = data %>%
        mutate(Country=recode(Country,"- other"="NA","Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))
      
      processed_data = data %>%
        filter(Country!="") %>%
        mutate(score_Scale_PSS10_UCLA_1=recode(Scale_PSS10_UCLA_1,"Never"=0,"Almost never"=1,"Sometimes"=2,"Fairly often"=3,"Very often"=4,.default=as.numeric(NA))) %>%
        mutate(score_Scale_PSS10_UCLA_2=recode(Scale_PSS10_UCLA_2,"Never"=0,"Almost never"=1,"Sometimes"=2,"Fairly often"=3,"Very often"=4,.default=as.numeric(NA))) %>%
        mutate(score_Scale_PSS10_UCLA_3=recode(Scale_PSS10_UCLA_3,"Never"=0,"Almost never"=1,"Sometimes"=2,"Fairly often"=3,"Very often"=4,.default=as.numeric(NA))) %>%
        mutate(score_Scale_PSS10_UCLA_4=recode(Scale_PSS10_UCLA_4,"Never"=4,"Almost never"=3,"Sometimes"=2,"Fairly often"=1,"Very often"=0,.default=as.numeric(NA))) %>%
        mutate(score_Scale_PSS10_UCLA_5=recode(Scale_PSS10_UCLA_5,"Never"=4,"Almost never"=3,"Sometimes"=2,"Fairly often"=1,"Very often"=0,.default=as.numeric(NA))) %>%
        mutate(score_Scale_PSS10_UCLA_6=recode(Scale_PSS10_UCLA_6,"Never"=0,"Almost never"=1,"Sometimes"=2,"Fairly often"=3,"Very often"=4,.default=as.numeric(NA))) %>%
        mutate(score_Scale_PSS10_UCLA_7=recode(Scale_PSS10_UCLA_7,"Never"=4,"Almost never"=3,"Sometimes"=2,"Fairly often"=1,"Very often"=0,.default=as.numeric(NA))) %>%
        mutate(score_Scale_PSS10_UCLA_8=recode(Scale_PSS10_UCLA_8,"Never"=4,"Almost never"=3,"Sometimes"=2,"Fairly often"=1,"Very often"=0,.default=as.numeric(NA))) %>%
        mutate(score_Scale_PSS10_UCLA_9=recode(Scale_PSS10_UCLA_9,"Never"=0,"Almost never"=1,"Sometimes"=2,"Fairly often"=3,"Very often"=4,.default=as.numeric(NA))) %>%
        mutate(score_Scale_PSS10_UCLA_10=recode(Scale_PSS10_UCLA_10,"Never"=0,"Almost never"=1,"Sometimes"=2,"Fairly often"=3,"Very often"=4,.default=as.numeric(NA))) %>%
        mutate(stress_score=score_Scale_PSS10_UCLA_1+score_Scale_PSS10_UCLA_2+score_Scale_PSS10_UCLA_3+score_Scale_PSS10_UCLA_4+score_Scale_PSS10_UCLA_5+score_Scale_PSS10_UCLA_6+score_Scale_PSS10_UCLA_7+score_Scale_PSS10_UCLA_8+score_Scale_PSS10_UCLA_9+score_Scale_PSS10_UCLA_10) %>%
        group_by(Country) %>%
        summarise(mean_stress_score = mean(stress_score,na.rm=T),sd_stress_score = sd(stress_score,na.rm=T),
                  nb_answers = n())
      
      processed_world_map = get_world_map(world)
      processed_world_map = left_join(processed_world_map,processed_data, by=c("country"="Country")) %>%
        mutate(country_text = paste0(
          "Country: ", country, "\n",
          "Region: ", region, "\n",
          "Mean stress score: ", round(mean_stress_score,2), "\n",
          "Std dev. stress score: ", round(sd_stress_score,2), "\n",
          "# of answers: ", nb_answers))
      
      p <- ggplot(as.data.frame(processed_world_map)) +
        geom_polygon(aes( x = long, y = lat, group = group, fill = mean_stress_score, text = country_text), colour = "black", size = 0.2)+
        scale_fill_distiller(palette="RdYlBu", name = "stress score")+
        theme_void()
    }
    
    #Function for Generating Trust Map Plot (by @ggautreau) ---- 
    trust_map <- function(world="world"){
      #world = "world" means a atlantic-centred map
      #world = "world2" means a pacific-centred map
      data = data %>%
        mutate(Country=recode(Country,"- other"="NA","Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))
      
      processed_data = data %>%
        filter(Country!="") %>%
        mutate(trust_score=recode(Trust_countrymeasure,"Too little"=0,"1"=1,"2"=2,"3"=3,"4"=4,"Appropriate"=5,"6"=6,"7"=7,"8"=8,"9"=9,"Too much"=10,.default=as.numeric(NA))) %>%
        group_by(Country) %>%
        summarise(mean_trust_score = mean(trust_score,na.rm=T), sd_trust_score = sd(trust_score,na.rm=T),
                  nb_answers = n())
      
      processed_world_map = get_world_map(world)
      processed_world_map = left_join(processed_world_map,processed_data, by=c("country"="Country")) %>%
        mutate(country_text = paste0(
          "Country: ", country, "\n",
          "Region: ", region, "\n",
          "Mean trust score: ", round(mean_trust_score,2), "\n",
          "Std dev. trust score: ", round(sd_trust_score,2), "\n",
          "# of answers: ", nb_answers))
      
      p <- ggplot(as.data.frame(processed_world_map)) +
        geom_polygon(aes( x = long, y = lat, group = group, fill = mean_trust_score, text = country_text), colour = "black", size = 0.2)+
        scale_fill_distiller(palette="RdYlBu", name = "All things considered, do you believe that\nthe government has taken the appropriate measures\nin response to Coronavirus ?", limits = c(0, 10), breaks = 0:10, labels= c("0 - Too little","1","2","3","4","5 - Appropriate","6","7","8","9","10 - Too mush"))+
        theme_void()
    }
    
    #Function for Generating Concern Map Plot (by @ggautreau) ---- 
    concern_map <- function(world="world", who="himself"){
      #world = "world" means a atlantic-centred map
      #world = "world2" means a pacific-centred map
      #who="himself" 
      #who="family" 
      #who="friends" 
      #who="country"
      #who="othercountries"
      data = data %>%
        mutate(Country=recode(Country,"- other"="NA","Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))
      
      processed_data = data %>%
        filter(Country!="") %>%
        group_by(Country) %>%
        mutate(score_Corona_concerns_himself=recode(Corona_concerns_1,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%
        mutate(score_Corona_concerns_friend=recode(Corona_concerns_2,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%
        mutate(score_Corona_concerns_family=recode(Corona_concerns_3,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%  
        mutate(score_Corona_concerns_country=recode(Corona_concerns_4,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%     
        mutate(score_Corona_concerns_othercountry=recode(Corona_concerns_5,"Strongly disagree"=0,"Disagree"=1,"Slightly disagree"=2,"Slightly agree"=3,"Agree"=4,"Strongly agree"=5,.default=as.numeric(NA))) %>%                   
        summarise(mean_concern_score_himself = mean(score_Corona_concerns_himself, na.rm=T),
                  sd_concern_score_himself = sd(score_Corona_concerns_himself,na.rm=T),
                  mean_concern_score_friend = mean(score_Corona_concerns_friend, na.rm=T),
                  sd_concern_score_friend = sd(score_Corona_concerns_friend, na.rm=T),
                  mean_concern_score_family = mean(score_Corona_concerns_family, na.rm=T),
                  sd_concern_score_family = sd(score_Corona_concerns_family, na.rm=T),
                  mean_concern_score_country = mean(score_Corona_concerns_country, na.rm=T),
                  sd_concern_score_country = sd(score_Corona_concerns_country, na.rm=T),
                  mean_concern_score_othercountries = mean(score_Corona_concerns_othercountry, na.rm=T),
                  sd_concern_score_othercountries = sd(score_Corona_concerns_othercountry, na.rm=T),
                  nb_answers = n())
      
      processed_world_map = get_world_map("world")
      processed_world_map = left_join(processed_world_map,processed_data, by=c("country"="Country")) %>%
        mutate(country_text = paste0(
          "Country: ", country, "\n",
          "Region: ", region, "\n",
          ifelse(who=="himself","<b>",""), "Mean concern score for herself/himself: ", round(mean_concern_score_himself,2), "\n",
          "Std dev. concern score for herself/himself: ", round(sd_concern_score_himself,2), ifelse(who=="himself","</b>","") ,"\n",
          ifelse(who=="friend","<b>",""),"Mean concern score for close friends: ", round(mean_concern_score_friend,2), "\n",
          "Std dev. concern score for close friends: ", round(sd_concern_score_friend,2), ifelse(who=="friend","</b>",""), "\n",
          ifelse(who=="family","<b>",""),"Mean concern score for family: ", round(mean_concern_score_family,2), "\n",
          "Std dev. concern score for family: ", round(sd_concern_score_family,2), ifelse(who=="family","</b>",""), "\n",
          ifelse(who=="country","<b>",""),"Mean concern score for his country: ", round(mean_concern_score_country,2), "\n",
          "Std dev. concern score for his country: ", round(sd_concern_score_country,2), ifelse(who=="country","</b>",""), "\n",
          ifelse(who=="orthercountries","<b>",""),"Mean concern score for other countries: ", round(mean_concern_score_othercountries,2), "\n",
          "Std dev. concern score for other countries: ", round(sd_concern_score_othercountries,2), ifelse(who=="othercountries","</b>",""), "\n",
          "# of answers: ", nb_answers))
      
      p <- ggplot(as.data.frame(processed_world_map))+
        switch(who,
               himself=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_himself, text = country_text), colour = "black", size = 0.2),
               friend=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_friend, text = country_text), colour = "black", size = 0.2),
               family=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_family, text = country_text), colour = "black", size = 0.2),
               country=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_country, text = country_text), colour = "black", size = 0.2),
               othercountries=geom_polygon(aes( x = long, y = lat, group = group, fill = mean_concern_score_othercountries, text = country_text), colour = "black", size = 0.2))+
        scale_fill_distiller(palette="RdYlBu", name = "Concerned about consequences of the coronavirus ?", limits = c(0, 5), breaks = c(0,1,2,3,4,5), labels= c("0 - Strongly disagree","1 - Disagree","2 - Slightly disagree","3 - Slightly agree","4 - Agree","5 - Strongly agree"))+#,values=c(0,0.45,0.55,1)
        theme_void()
    }
    
    
    concern_switch <- 
      switch(input$ConcernChoice, "1"="himself", "2"="family", "3"="friends", "4"="country", "5"="othercountries")
    
    # Sending plots to ui ----
    
    output$PlotlyGender100<-renderPlotly({ ggplotly(pGender100, tooltip = "text") })
    output$PlotlyAge<-renderPlotly({ ggplotly(pAge, tooltip = "text") })
    output$PlotlyEdu<-renderPlotly({ ggplotly(pEdu, tooltip = "text") })
    if(TRUE) {
    switch(input$MapRegionChoice,
           "1" = { #1 = World
             output$PlotlyStressMap <- renderPlotly({ggplotly(stress_map(world="world"), tooltip="text")} )
             output$PlotlyIsolationMap<-renderPlotly({ggplotly(isolation_map(world="world"), tooltip="text")})
             output$PlotlyTrustMap <- renderPlotly({ggplotly(trust_map(world="world"), tooltip="text")} )
             output$PlotlyCoronaConcernMap<-renderPlotly({ggplotly(concern_map(world="world", who=concern_switch), tooltip="text")})
           },
           "2" = { #2 Africa 
             output$PlotlyStressMap <- renderPlotly({ ggplotly(stress_map(), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-25,60)),yaxis=list(range = c(-40,40))) })
             output$PlotlyIsolationMap<-renderPlotly({ ggplotly(isolation_map(), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-25,60)),yaxis=list(range = c(-40,40))) })
             output$PlotlyTrustMap <- renderPlotly({ggplotly(trust_map(), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-25,60)),yaxis=list(range = c(-40,40))) })
             output$PlotlyCoronaConcernMap <- renderPlotly({ggplotly(concern_map(who=concern_switch), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-25,60)),yaxis=list(range = c(-40,40))) })
           },
           "3" = { #3 America
             output$PlotlyStressMap <- renderPlotly({ ggplotly(stress_map(), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-180,-20)),yaxis=list(range =  c(-60,80))) })
             output$PlotlyIsolationMap<-renderPlotly({ ggplotly(isolation_map(), tooltip="text") %>%
                 layout(xaxis=list(range = c(-180,-20)),yaxis=list(range =  c(-60,80)))  })
             output$PlotlyTrustMap <- renderPlotly({ggplotly(trust_map(), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-180,-20)),yaxis=list(range =  c(-60,80))) })                 
             output$PlotlyCoronaConcernMap <- renderPlotly({ggplotly(concern_map(who=concern_switch), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-180,-20)),yaxis=list(range =  c(-60,80))) })                     
           },
           "4" = { #4 Asia
             output$PlotlyStressMap <- renderPlotly({ ggplotly(stress_map(), tooltip="text") %>%
                 layout(xaxis=list(range = c(25,191)),yaxis=list(range = c(-15,90))) })
             output$PlotlyIsolationMap<-renderPlotly({ ggplotly(isolation_map(), tooltip="text") %>% 
                 layout(xaxis=list(range = c(25,191)),yaxis=list(range = c(-15,90))) })
             output$PlotlyTrustMap <- renderPlotly({ggplotly(trust_map(), tooltip="text") %>% 
                 layout(xaxis=list(range = c(25,191)),yaxis=list(range = c(-15,90))) })
             output$PlotlyCoronaConcernMap <- renderPlotly({ggplotly(concern_map(who=concern_switch), tooltip="text") %>% 
                 layout(xaxis=list(range = c(25,191)),yaxis=list(range = c(-15,90))) })
           },
           "5" = { #5 Europe
             output$PlotlyStressMap <- renderPlotly({ ggplotly(stress_map(), tooltip="text") %>%
                 layout(xaxis=list(range = c(-25,50)),yaxis=list(range = c(33,72))) })
             output$PlotlyIsolationMap<-renderPlotly({ ggplotly(isolation_map(), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-25,50)),yaxis=list(range = c(33,72))) })
             output$PlotlyTrustMap <- renderPlotly({ggplotly(trust_map(), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-25,50)),yaxis=list(range = c(33,72))) })
             output$PlotlyCoronaConcernMap <- renderPlotly({ggplotly(concern_map(who=concern_switch), tooltip="text") %>% 
                 layout(xaxis=list(range = c(-25,50)),yaxis=list(range = c(33,72))) })
           },
           "6" = { #Oceania
             output$PlotlyStressMap <- renderPlotly({ ggplotly(stress_map(world="world2"), tooltip="text") %>% 
                 layout(xaxis=list(range = c(100,300)),yaxis=list(range = c(-80,80))) })
             output$PlotlyIsolationMap <- renderPlotly({ggplotly(isolation_map(world="world2"), tooltip="text") %>%
                 layout(xaxis=list(range = c(100,300)),yaxis=list(range = c(-80,80))) })
             output$PlotlyTrustMap <- renderPlotly({ggplotly(trust_map(world="world2"), tooltip="text") %>% 
                 layout(xaxis=list(range = c(100,300)),yaxis=list(range = c(-80,80))) })
             output$PlotlyCoronaConcernMap <- renderPlotly({ggplotly(concern_map(world="world2", who=concern_switch), tooltip="text") %>% 
                 layout(xaxis=list(range = c(100,300)),yaxis=list(range = c(-80,80))) })        
           }
    )
    }
  })
}