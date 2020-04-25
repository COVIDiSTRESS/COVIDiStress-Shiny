server <- function(input, output, session) {
  # Loading Data ----
  data <- read.csv("../covid_06042020_choice_values.csv", header = T, stringsAsFactors = F)
  data <- data[3:nrow(data),]
  
  # Creating Variables ----
  
  if(file.exists("../Unique_CountryName_full.csv"))
  {
    Unique_CountryName_full <- read.csv("../Unique_CountryName_full.csv", header = T, stringsAsFactors = F)$x
  }else{
    Unique_CountryName_full <- unique(toTitleCase(tolower(data$Country)))
    Unique_CountryName_full <- sort(Unique_CountryName_full[Unique_CountryName_full!=""])
    Unique_CountryName_full <- c(Unique_CountryName_full, "World")
    #execute the following line to speed up the process (not generate country names at every run)
    #write.csv(Unique_CountryName_full,"Unique_CountryName_full.csv", row.names = TRUE)
  }
  
  updateSelectizeInput(session, 'CountryChoice', choices = Unique_CountryName_full, server = TRUE,
                       selected=c("France", "Italy"))
  
  #Beginning of the dynamic part ----
  observeEvent({
    input$CountryChoice
    input$MapRegionChoice
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
    
    if(TRUE){
      worldmap_from_data <- function(data){
        world_map <- map_data("world")
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
      
      
      isolation_map <- function(country_list = setdiff(world_map$country,c("")), x_continent=c(-200,200), y_continent=c(-80,100))
      {
        data = data %>%
          mutate(Country=recode(Country,"- other"="NA","Cabo Verde"="Cape Verde","Congo, Democratic Republic of the"="Democratic Republic of the Congo","Congo, Republic of the"="Republic of Congo","Côte d’Ivoire"="Ivory Coast","East Timor (Timor-Leste)"="Timor-Leste","Korea, North"="North Korea","Korea, South"="South Korea","Micronesia, Federated States of"="Micronesia","Sudan, South"="South Sudan","The Bahamas"="Bahamas","United Kingdom"="UK","United States"="USA"))
        
        processed_world_map = world_map %>%
          filter(country%in%country_list)
        
        processed_data = data %>%
          filter(Country%in%country_list) %>%
          mutate(isolation_score=replace(Dem_islolation,Dem_islolation=="1",NA)) %>%
          mutate(isolation_score=replace(isolation_score,isolation_score=="",NA)) %>%
          mutate(isolation_score=replace(isolation_score,isolation_score=="Life carries on as usual",0)) %>%
          mutate(isolation_score=replace(isolation_score,isolation_score=="Life carries on with minor changes",1)) %>%
          mutate(isolation_score=replace(isolation_score,isolation_score=="Isolated", 2)) %>%
          mutate(isolation_score=replace(isolation_score,isolation_score=="Isolated in medical facility of similar location",3)) %>%
          mutate(isolation_score=as.numeric(isolation_score)) %>%
          group_by(Country) %>%
          summarise(mean_isolation_score = mean(isolation_score,na.rm=T),sd_isolation_score = sd(isolation_score,na.rm=T),
                    nb_answers = n())
        
        processed_world_map = left_join(processed_world_map,processed_data, by=c("country"="Country")) %>%
          mutate(country_text = paste0(
            "Country: ", country, "\n",
            "Region: ", region, "\n",
            "Mean isolation score: ", round(mean_isolation_score,2), "\n",
            "Std dev. isolation score: ", round(sd_isolation_score,2), "\n",
            "# of answers: ", nb_answers))
        
        p <- ggplot(as.data.frame(processed_world_map)) +
          geom_polygon(aes( x = long, y = lat, group = group, fill = mean_isolation_score, text = country_text), colour = "black", size = 0.2)+
          scale_fill_distiller(palette="RdYlBu", name = "isolation score", limits = c(0, 3), breaks = c(0,1,2,3), labels= c("0 - Life carries on as usual","1 - Life carries on with minor changes","2 - Isolated","3 - Isolated in medical facility of similar location"),values=c(0,0.45,0.55,1))+#breaks=c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000), guide=guide_colorbar(barwidth = 0.8, barheight = 18), trans = "log", limits=c(10, NA), direction=-1
          scale_x_continuous(limits=x_continent)+
          scale_y_continuous(limits=y_continent)+
          theme_void()
      }
      world_map <- worldmap_from_data(data)
      if(input$MapRegionChoice==1)
      {
        pIsolationMap <- isolation_map()  
      }else{
        x_europe = c(-25,50)
        y_europe = c(35,80)
        europe = c("Netherlands","UK","Albania","Finland","Andorra","France","Austria","Belgium","Bulgaria","Bosnia and Herzegovina","Switzerland","Czech Republic","Germany","Denmark","Spain","Estonia","Faroe Islands","Georgia","Guernsey","Greece","Croatia","Hungary","Iceland","Italy","San Marino","Jersey","Lithuania","Monaco","Luxembourg","Moldova","Macedonia","Malta","Norway","Portugal","Romania","Serbia","Slovakia","Sweden","Slovenia","Turkey","Ukraine","Vatican","Ireland","Poland","Cyprus","Russia","Belarus","Latvia","Albania","Montenegro","Kosovo")  
        pIsolationMap <- isolation_map(europe,x_europe,y_europe)  
      }
      
      
    }
    
    # Sending plots to ui ----
    
    output$PlotlyGender100<-renderPlotly({ ggplotly(pGender100, tooltip = "text") })
    output$PlotlyAge<-renderPlotly({ ggplotly(pAge, tooltip = "text") })
    output$PlotlyEdu<-renderPlotly({ ggplotly(pEdu, tooltip = "text") })
    output$PlotlyIsolationMap<-renderPlotly({ ggplotly(pIsolationMap, tooltip = "text") })
    
    
  })
}