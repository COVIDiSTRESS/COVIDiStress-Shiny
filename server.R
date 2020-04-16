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
    group_by(Dem_edu) %>%
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
  
  # Sending plots to ui ----
  
  
  output$PlotlyGender100<-renderPlotly({ ggplotly(pGender100, tooltip = "text") })
  output$PlotlyAge<-renderPlotly({ ggplotly(pAge, tooltip = "text") })
  output$PlotlyEdu<-renderPlotly({ ggplotly(pEdu, tooltip = "text") })
  
  
  })
}