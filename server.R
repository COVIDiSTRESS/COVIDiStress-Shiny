args <- commandArgs(TRUE)
args <- ifelse(length(args)==0,"COVIDiSTRESS_clean.dat",args)
load(args)

# Creating Variables ----
Unique_CountryName_full <- c()
if(file.exists("Unique_CountryName_full.csv"))
{
  Unique_CountryName_full <- read.csv("Unique_CountryName_full.csv", header = T, stringsAsFactors = F)$x
}else{
  Unique_CountryName_full <- unique(toTitleCase(tolower(data$Country)))
  Unique_CountryName_full[Unique_CountryName_full=="Usa"] <- "USA"
  Unique_CountryName_full <- sort(Unique_CountryName_full[Unique_CountryName_full!=""])
  Unique_CountryName_full <- c(Unique_CountryName_full, "World")
  #execute the following line to speed up the process (not generate country names at every run)
  #write.csv(Unique_CountryName_full,"Unique_CountryName_full.csv", row.names = TRUE)
}

server <- function(input, output, session) {
  
  withProgress(message = 'Loading maps', value = 0,{
  # Loading Data ----
   updateSelectizeInput(session, 'CountryChoice', choices = Unique_CountryName_full, server = TRUE,
                       selected=c("France", "Italy"))

  
  # Creating map functions and loading maps ----
  incProgress(1/6, detail = "Creating Maps")
  #Generating Isolation Map Plot (by @ggautreau)
  isolation_map <- function(){
    
    processed_data = data %>%
      mutate(isolation_score=recode(Dem_islolation,"Life carries on as usual"=0,"Life carries on with minor changes"=1,"Isolated"=2,"Isolated in medical facility of similar location"=3,.default=as.numeric(NA))) %>%
      group_by(Country) %>%
      summarise(mean_isolation_score = mean(isolation_score,na.rm=T),sd_isolation_score = sd(isolation_score,na.rm=T),
                nb_answers = n()) %>%
      mutate(
        Country_iso3c = countrycode(
          sourcevar = Country,
          origin = "country.name.en",
          destination = "iso3c",
          custom_match = c(
            "Sudan, South" = "SSD",
            "other" = NA,
            "Kosovo" = NA
          )
        ),
        Country_code = countrycode(
          sourcevar = Country_iso3c,
          origin = "iso3c",
          destination = "un",
          custom_match = c("SSD" = 728, "TWN" = 158)
        ),
        population_2020 = pop[match(Country_code, pop$country_code), "2020"] * 1000,
        life_expectancy_M = e0M[match(Country_code, e0M$country_code), "2015-2020"],
        life_expectancy_F = e0F[match(Country_code, e0F$country_code), "2015-2020"],
      )
    
    
    fig <- plot_geo(processed_data) %>%
      add_trace(
        locations =  ~ Country_iso3c,
        z = ~ mean_isolation_score,
        color = ~ mean_isolation_score,
        hovertemplate = ~ paste0(
          "<b>Mean isolation score: ",
          round(mean_isolation_score, 2),
          "\n",
          "Std dev. isolation score: ",
          round(sd_isolation_score, 2),
          "</b>\n",
          "Population size: ",
          format(round(population_2020), big.mark = " "),
          "\n",
          "Life expectancy: Male=", round(life_expectancy_M,1), " ; Female=", round(life_expectancy_F,1),
          "\n",
          "# of answers: ",
          nb_answers,
          '<extra>',
          Country,
          '</extra>'
        ),
        colorscale = list(c(0, 0.5, 1), c('rgb(255, 255, 77)', 'rgb(255, 210, 77)', 'rgb(230, 0, 0)')),
        zmin = 0,
        zmax = 3,
        colorbar = list(
          title = 'Isolation score',
          tickvals = 0:3,
          ticktext = c(
            "0 - Life carries on as usual",
            "1 - Life carries on with minor changes",
            "2 - Isolated",
            "3 - Isolated in medical facility of similar location"
          )
        )
      ) 
    
    return(fig)
    
  }
  fig_isolation = isolation_map()
  
    #Function for Generating Stress Map Plot (by @ggautreau)
  stress_map <- function(){
    
    processed_data = data %>%
      group_by(Country) %>%
      summarise(mean_stress_score = mean(PSS10_avg,na.rm=T),sd_stress_score = sd(PSS10_avg,na.rm=T),
                nb_answers = n()) %>%
      mutate(
        Country_iso3c = countrycode(
          sourcevar = Country,
          origin = "country.name.en",
          destination = "iso3c",
          custom_match = c(
            "Sudan, South" = "SSD",
            "other" = NA,
            "Kosovo" = NA
          )
        ),
        Country_code = countrycode(
          sourcevar = Country_iso3c,
          origin = "iso3c",
          destination = "un",
          custom_match = c("SSD" = 728, "TWN" = 158)
        ),
        population_2020 = pop[match(Country_code, pop$country_code), "2020"] * 1000,
        life_expectancy_M = e0M[match(Country_code, e0M$country_code), "2015-2020"],
        life_expectancy_F = e0F[match(Country_code, e0F$country_code), "2015-2020"],
      )
    
    fig <- plot_geo(processed_data) %>%
      add_trace(
        locations =  ~ Country_iso3c,
        z = ~ mean_stress_score,
        color = ~ mean_stress_score,
        hovertemplate = ~ paste0(
          "<b>Mean stress score: ",
          round(mean_stress_score, 2),
          "\n",
          "Std dev. stress score: ",
          round(sd_stress_score, 2),
          "</b>\n",
          "Population size: ",
          format(round(population_2020), big.mark = " "),
          "\n",
          "Life expectancy: Male=", round(life_expectancy_M,1), " ; Female=", round(life_expectancy_F,1),
          "\n",
          "# of answers: ",
          nb_answers,
          '<extra>',
          Country,
          '</extra>'
        ),
        colorscale = 'RdBu',
        colorbar = list(title = 'Stress score')) 
    
    return(fig)
    
  }
  fig_stress = stress_map()
  
  #Function for Generating Trust Map Plot (by @ggautreau)
  trust_map <- function(){
    
    processed_data = data %>%
      group_by(Country) %>%
      summarise(mean_trust_score = mean(Trust_countrymeasure,na.rm=T), sd_trust_score = sd(Trust_countrymeasure,na.rm=T),
                nb_answers = n()) %>%
      mutate(
        Country_iso3c = countrycode(
          sourcevar = Country,
          origin = "country.name.en",
          destination = "iso3c",
          custom_match = c(
            "Sudan, South" = "SSD",
            "other" = NA,
            "Kosovo" = NA
          )
        ),
        Country_code = countrycode(
          sourcevar = Country_iso3c,
          origin = "iso3c",
          destination = "un",
          custom_match = c("SSD" = 728, "TWN" = 158)
        ),
        population_2020 = pop[match(Country_code, pop$country_code), "2020"] * 1000,
        life_expectancy_M = e0M[match(Country_code, e0M$country_code), "2015-2020"],
        life_expectancy_F = e0F[match(Country_code, e0F$country_code), "2015-2020"],
      )
    
    fig <- plot_geo(processed_data) %>%
      add_trace(
        locations =  ~ Country_iso3c,
        z = ~ mean_trust_score,
        color = ~ mean_trust_score,
        hovertemplate = ~ paste0(
          "<b>Mean trust score: ",
          round(mean_trust_score, 2),
          "\n",
          "Std dev. trust score: ",
          round(sd_trust_score, 2),
          "</b>\n",
          "Population size: ",
          format(round(population_2020), big.mark = " "),
          "\n",
          "Life expectancy: Male=", round(life_expectancy_M,1), " ; Female=", round(life_expectancy_F,1),
          "\n",
          "# of answers: ",
          nb_answers,
          '<extra>',
          Country,
          '</extra>'
        ),
        colorscale = list(c(0, 0.5, 1), c('rgb(51, 153, 255)', 'rgb(128, 255, 128)', 'rgb(255, 51, 51)')),
        zmin = 0,
        zmax = 10,
        colorbar = list(
          title = 'All things considered, do you believe that\nthe government has taken the appropriate\n measures in response to Coronavirus ?',
          tickvals = 0:10,
          ticktext = c(
            "0 - Too little",
            "1",
            "2",
            "3",
            "4",
            "5 - Appropriate",
            "6",
            "7",
            "8",
            "9",
            "10 - Too much"
          )
        )
      )
    
    return(fig)
  }
  fig_trust = trust_map()
  
  #Function for Generating Concern Map Plot (by @ggautreau) 
  concern_map <- function() {
    processed_data = data %>%
      group_by(Country) %>%
      summarise(
        mean_concern_score_themself = mean(Corona_concerns_1, na.rm = T),
        sd_concern_score_themself = sd(Corona_concerns_1, na.rm =
                                         T),
        mean_concern_score_friend = mean(Corona_concerns_2, na.rm =
                                           T),
        sd_concern_score_friend = sd(Corona_concerns_2, na.rm =
                                       T),
        mean_concern_score_family = mean(Corona_concerns_3, na.rm =
                                           T),
        sd_concern_score_family = sd(Corona_concerns_3, na.rm =
                                       T),
        mean_concern_score_country = mean(Corona_concerns_4, na.rm =
                                            T),
        sd_concern_score_country = sd(Corona_concerns_4, na.rm =
                                        T),
        mean_concern_score_othercountries = mean(Corona_concerns_5, na.rm =
                                                   T),
        sd_concern_score_othercountries = sd(Corona_concerns_5, na.rm =
                                               T),
        nb_answers = n()
      ) %>%
      mutate(
        Country_iso3c = countrycode(
          sourcevar = Country,
          origin = "country.name.en",
          destination = "iso3c",
          custom_match = c(
            "Sudan, South" = "SSD",
            "other" = NA,
            "Kosovo" = NA
          )
        ),
        Country_code = countrycode(
          sourcevar = Country_iso3c,
          origin = "iso3c",
          destination = "un",
          custom_match = c("SSD" = 728, "TWN" = 158)
        ),
        population_2020 = pop[match(Country_code, pop$country_code), "2020"] * 1000,
        life_expectancy_M = e0M[match(Country_code, e0M$country_code), "2015-2020"],
        life_expectancy_F = e0F[match(Country_code, e0F$country_code), "2015-2020"],
      )
    
    
    fig <- plot_geo(processed_data) %>%
      add_trace(
        locations =  ~ Country_iso3c,
        z = ~ mean_concern_score_themself,
        color = ~ mean_concern_score_themself,
        hovertemplate = ~ paste0(
          "<b>Mean concern score for themself: ",
          round(mean_concern_score_themself, 2),
          "\n",
          "Std dev. concern score for themself: ",
          round(sd_concern_score_themself, 2),
          "</b>\n",
          "Mean concern score for close friends: ",
          round(mean_concern_score_friend, 2),
          "\n",
          "Std dev. concern score for close friends: ",
          round(sd_concern_score_friend, 2),
          "\n",
          "Mean concern score for family: ",
          round(mean_concern_score_family, 2),
          "\n",
          "Std dev. concern score for family: ",
          round(sd_concern_score_family, 2),
          "\n",
          "Mean concern score for his country: ",
          round(mean_concern_score_country, 2),
          "\n",
          "Std dev. concern score for his country: ",
          round(sd_concern_score_country, 2),
          "\n",
          "Mean concern score for other countries: ",
          round(mean_concern_score_othercountries, 2),
          "\n",
          "Std dev. concern score for other countries: ",
          round(sd_concern_score_othercountries, 2),
          "\n",
          "Population size: ",
          format(round(population_2020), big.mark = " "),
          "\n",
          "Life expectancy: Male=", round(life_expectancy_M,1), " ; Female=", round(life_expectancy_F,1),
          "\n",
          "# of answers: ",
          nb_answers,
          '<extra>',
          Country,
          '</extra>'
        ),
        colorscale = 'RdBu',
        zmin = 1,
        zmax = 6,
        colorbar = list(
          title = 'Concern for themself',
          tickvals = 1:6,
          ticktext = c(
            "1 - Strongly disagree",
            "2 - Disagree",
            "3 - Slightly disagree",
            "4 - Slightly agree",
            "5 - Agree",
            "6 - Strongly agree"
          )
        )
      ) %>%
      add_trace(
        locations =  ~ Country_iso3c,
        z = ~ mean_concern_score_friend,
        color = ~ mean_concern_score_friend,
        hovertemplate = ~ paste0(
          "Mean concern score for themself: ",
          round(mean_concern_score_themself, 2),
          "\n",
          "Std dev. concern score for themself: ",
          round(sd_concern_score_themself, 2),
          "\n",
          "<b>Mean concern score for close friends: ",
          round(mean_concern_score_friend, 2),
          "\n",
          "Std dev. concern score for close friends: ",
          round(sd_concern_score_friend, 2),
          "</b>\n",
          "Mean concern score for family: ",
          round(mean_concern_score_family, 2),
          "\n",
          "Std dev. concern score for family: ",
          round(sd_concern_score_family, 2),
          "\n",
          "Mean concern score for his country: ",
          round(mean_concern_score_country, 2),
          "\n",
          "Std dev. concern score for his country: ",
          round(sd_concern_score_country, 2),
          "\n",
          "Mean concern score for other countries: ",
          round(mean_concern_score_othercountries, 2),
          "\n",
          "Std dev. concern score for other countries: ",
          round(sd_concern_score_othercountries, 2),
          "\n",
          "Population size: ",
          format(round(population_2020), big.mark = " "),
          "\n",
          "Life expectancy: Male=", round(life_expectancy_M,1), " ; Female=", round(life_expectancy_F,1),
          "\n",
          "# of answers: ",
          nb_answers,
          '<extra>',
          Country,
          '</extra>'
        ),
        colorscale = 'RdBu',
        zmin = 1,
        zmax = 6,
        visible = FALSE,
        colorbar = list(
          title = 'Concern for their friends',
          tickvals = 1:6,
          ticktext = c(
            "1 - Strongly disagree",
            "2 - Disagree",
            "3 - Slightly disagree",
            "4 - Slightly agree",
            "5 - Agree",
            "6 - Strongly agree"
          )
        )
      ) %>%
      add_trace(
        locations =  ~ Country_iso3c,
        z = ~ mean_concern_score_family,
        color = ~ mean_concern_score_family,
        hovertemplate = ~ paste0(
          "Mean concern score for themself: ",
          round(mean_concern_score_themself, 2),
          "\n",
          "Std dev. concern score for themself: ",
          round(sd_concern_score_themself, 2),
          "\n",
          "Mean concern score for close friends: ",
          round(mean_concern_score_friend, 2),
          "\n",
          "Std dev. concern score for close friends: ",
          round(sd_concern_score_friend, 2),
          "\n",
          "<b>Mean concern score for family: ",
          round(mean_concern_score_family, 2),
          "\n",
          "Std dev. concern score for family: ",
          round(sd_concern_score_family, 2),
          "</b>\n",
          "Mean concern score for his country: ",
          round(mean_concern_score_country, 2),
          "\n",
          "Std dev. concern score for his country: ",
          round(sd_concern_score_country, 2),
          "\n",
          "Mean concern score for other countries: ",
          round(mean_concern_score_othercountries, 2),
          "\n",
          "Std dev. concern score for other countries: ",
          round(sd_concern_score_othercountries, 2),
          "\n",
          "Population size: ",
          format(round(population_2020), big.mark = " "),
          "\n",
          "Life expectancy: Male=", round(life_expectancy_M,1), " ; Female=", round(life_expectancy_F,1),
          "\n",
          "# of answers: ",
          nb_answers,
          '<extra>',
          Country,
          '</extra>'
        ),
        colorscale = 'RdBu',
        zmin = 1,
        zmax = 6,
        visible = FALSE,
        colorbar = list(
          title = 'Concern for their family',
          tickvals = 1:6,
          ticktext = c(
            "1 - Strongly disagree",
            "2 - Disagree",
            "3 - Slightly disagree",
            "4 - Slightly agree",
            "5 - Agree",
            "6 - Strongly agree"
          )
        )
      ) %>%
      add_trace(
        locations =  ~ Country_iso3c,
        z = ~ mean_concern_score_country,
        color = ~ mean_concern_score_country,
        hovertemplate = ~ paste0(
          "Mean concern score for themself: ",
          round(mean_concern_score_themself, 2),
          "\n",
          "Std dev. concern score for themself: ",
          round(sd_concern_score_themself, 2),
          "\n",
          "Mean concern score for close friends: ",
          round(mean_concern_score_friend, 2),
          "\n",
          "Std dev. concern score for close friends: ",
          round(sd_concern_score_friend, 2),
          "\n",
          "Mean concern score for family: ",
          round(mean_concern_score_family, 2),
          "\n",
          "Std dev. concern score for family: ",
          round(sd_concern_score_family, 2),
          "\n",
          "<b>Mean concern score for his country: ",
          round(mean_concern_score_country, 2),
          "\n",
          "Std dev. concern score for his country: ",
          round(sd_concern_score_country, 2),
          "</b>\n",
          "Mean concern score for other countries: ",
          round(mean_concern_score_othercountries, 2),
          "\n",
          "Std dev. concern score for other countries: ",
          round(sd_concern_score_othercountries, 2),
          "\n",
          "Population size: ",
          format(round(population_2020), big.mark = " "),
          "\n",
          "Life expectancy: Male=", round(life_expectancy_M,1), " ; Female=", round(life_expectancy_F,1),
          "\n",
          "# of answers: ",
          nb_answers,
          '<extra>',
          Country,
          '</extra>'
        ),
        colorscale = 'RdBu',
        zmin = 1,
        zmax = 6,
        visible = FALSE,
        colorbar = list(
          title = 'Concern for their country',
          tickvals = 1:6,
          ticktext = c(
            "1 - Strongly disagree",
            "2 - Disagree",
            "3 - Slightly disagree",
            "4 - Slightly agree",
            "5 - Agree",
            "6 - Strongly agree"
          )
        )
      ) %>%
      add_trace(
        locations =  ~ Country_iso3c,
        z = ~ mean_concern_score_othercountries,
        color = ~ mean_concern_score_othercountries,
        hovertemplate = ~ paste0(
          "Mean concern score for themself: ",
          round(mean_concern_score_themself, 2),
          "\n",
          "Std dev. concern score for themself: ",
          round(sd_concern_score_themself, 2),
          "\n",
          "Mean concern score for close friends: ",
          round(mean_concern_score_friend, 2),
          "\n",
          "Std dev. concern score for close friends: ",
          round(sd_concern_score_friend, 2),
          "\n",
          "Mean concern score for family: ",
          round(mean_concern_score_family, 2),
          "\n",
          "Std dev. concern score for family: ",
          round(sd_concern_score_family, 2),
          "\n",
          "Mean concern score for his country: ",
          round(mean_concern_score_country, 2),
          "\n",
          "Std dev. concern score for his country: ",
          round(sd_concern_score_country, 2),
          "\n",
          "<b>Mean concern score for other countries: ",
          round(mean_concern_score_othercountries, 2),
          "\n",
          "Std dev. concern score for other countries: ",
          round(sd_concern_score_othercountries, 2),
          "</b>\n",
          "Population size: ",
          format(round(population_2020), big.mark = " "),
          "\n",
          "Life expectancy: Male=", round(life_expectancy_M,1), " ; Female=", round(life_expectancy_F,1),
          "\n",
          "# of answers: ",
          nb_answers,
          '<extra>',
          Country,
          '</extra>'
        ),
        colorscale = 'RdBu',
        zmin = 1,
        zmax = 6,
        visible = FALSE,
        colorbar = list(
          title = 'Concern for other countries',
          tickvals = 1:6,
          ticktext = c(
            "1 - Strongly disagree",
            "2 - Disagree",
            "3 - Slightly disagree",
            "4 - Slightly agree",
            "5 - Agree",
            "6 - Strongly agree"
          )
        )
      ) %>%
      layout(
        title = "Concern map",
        geo = list(showland = T,
                   landcolor = toRGB("grey50")),
        updatemenus = list(list(
          buttons = list(
            list(
              method = 'restyle',
              args = list("visible", list(TRUE, FALSE, FALSE, FALSE, FALSE)),
              label = "Themself"
            ),
            list(
              method = 'restyle',
              args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE)),
              label = "Friends"
            ),
            list(
              method = 'restyle',
              args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE)),
              label = "Family"
            ),
            list(
              method = 'restyle',
              args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE)),
              label = "Country"
            ),
            list(
              method = 'restyle',
              args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE)),
              label = "Other countries"
            )
          )
        ))
      )
    
    return(fig)
    
  }
  fig_concern = concern_map()
  
  })
  
  #Dynamic Part for the "Sample Description" tab ----
  observeEvent({
    input$CountryChoice
  },{
    
    withProgress(message = 'Computing Results and Creating plots', value = 0,{
    country_list <- input$CountryChoice
    #country_list <- c("France", "Italy")
    
    incProgress(1/5, detail = "Creating Gender Plot")
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
    incProgress(1/5, detail = "Creating Age Plot")
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
    incProgress(1/5, detail = "Creating Education Plot")
    education <- c("PhD/Doctorate", "College degree, bachelor, master", "Some College, short continuing education or equivalent", "Up to 12 years of school ", "Up to 9 years of school", "Up to 6 years of school", "None")
    
    processed_data = data %>%
      filter(Country%in%country_list, Dem_edu %in% education) %>%
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
      scale_fill_manual(name="Education", values=c("PhD/Doctorate" = "#31233bff", "College degree, bachelor, master" = "#50456cff","Some College, short continuing education or equivalent" = "#6b6099ff","Up to 12 years of school " = "#9392b7ff","Up to 9 years of school" = "#b0b0d1ff","Up to 6 years of school" = "#bec0d4ff", "None" = "#f8f8ffff"))+
      coord_flip() +
      scale_y_continuous(breaks=seq(0,1,0.1),labels = scales::percent_format(accuracy = 1),expand=c(0,0))+
      scale_x_discrete(expand=c(0,0))+
      labs(x = "Country", y = "% Education") +
      theme_classic() +
      theme(legend.position="top")
    
    
    # Sending plots to ui ----
    
    incProgress(1/5, detail = "Creating outputs")
    output$PlotlyGender100<-renderPlotly({ ggplotly(pGender100, tooltip = "text") })
    output$PlotlyAge<-renderPlotly({ ggplotly(pAge, tooltip = "text") })
    output$PlotlyEdu<-renderPlotly({ ggplotly(pEdu, tooltip = "text") })
    })
  })
  
  
  # Dynamic Part for the "Results" tab ----
  observeEvent({
    input$MapRegionChoice
  },{
    
    #Creating Concern maps
    withProgress(message = 'Plotting maps', value = 0,{
    
    # Sending plots to ui
    incProgress(1/3, detail = "Sending plots to UI")
    if(TRUE) {
      switch(input$MapRegionChoice,
             "1" = { #1 = World
               output$PlotlyIsolationMap     <- renderPlotly({ fig_isolation %>% layout(height = 800)})
               output$PlotlyStressMap        <- renderPlotly({ fig_stress    %>% layout(height = 800)})
               output$PlotlyTrustMap         <- renderPlotly({ fig_trust     %>% layout(height = 800)})
               output$PlotlyCoronaConcernMap <- renderPlotly({ fig_concern   %>% layout(height = 800)})
             },
             "2" = { #2 Africa 
               output$PlotlyStressMap        <- renderPlotly({ fig_stress    %>% layout(height = 800, geo = list(scope = 'africa')) })
               output$PlotlyIsolationMap     <- renderPlotly({ fig_isolation %>% layout(height = 800, geo = list(scope = 'africa')) })
               output$PlotlyTrustMap         <- renderPlotly({ fig_trust     %>% layout(height = 800, geo = list(scope = 'africa')) })
               output$PlotlyCoronaConcernMap <- renderPlotly({ fig_concern   %>% layout(height = 800, geo = list(scope = 'africa')) })
             },
             "3" = { #3 Asia
               output$PlotlyStressMap        <- renderPlotly({fig_stress    %>% layout(height = 800, geo = list(scope = 'asia')) })
               output$PlotlyIsolationMap     <- renderPlotly({fig_isolation %>% layout(height = 800, geo = list(scope = 'asia')) })
               output$PlotlyTrustMap         <- renderPlotly({ fig_trust    %>% layout(height = 800, geo = list(scope = 'asia')) })
               output$PlotlyCoronaConcernMap <- renderPlotly({fig_concern   %>% layout(height = 800, geo = list(scope = 'asia')) })
             },
             "4" = { #4 Europe
               output$PlotlyStressMap        <- renderPlotly({fig_stress    %>% layout(height = 800, geo = list(scope = 'europe')) })
               output$PlotlyIsolationMap     <- renderPlotly({fig_isolation %>% layout(height = 800, geo = list(scope = 'europe')) })
               output$PlotlyTrustMap         <- renderPlotly({fig_trust     %>% layout(height = 800, geo = list(scope = 'europe')) })
               output$PlotlyCoronaConcernMap <- renderPlotly({fig_concern   %>% layout(height = 800, geo = list(scope = 'europe')) })
             },
             "5" = { #5 North America
               output$PlotlyStressMap        <- renderPlotly({fig_stress    %>% layout(height = 800, geo = list(scope = 'north america')) })
               output$PlotlyIsolationMap     <- renderPlotly({fig_isolation %>% layout(height = 800, geo = list(scope = 'north america'))  })
               output$PlotlyTrustMap         <- renderPlotly({fig_trust     %>% layout(height = 800, geo = list(scope = 'north america')) })                 
               output$PlotlyCoronaConcernMap <- renderPlotly({fig_concern   %>% layout(height = 800, geo = list(scope = 'north america')) })                     
             },
             "6" = { #6 South America
               output$PlotlyStressMap        <- renderPlotly({fig_stress    %>% layout(height = 800, geo = list(scope = 'south america')) })
               output$PlotlyIsolationMap     <- renderPlotly({fig_isolation %>% layout(height = 800, geo = list(scope = 'south america')) })
               output$PlotlyTrustMap         <- renderPlotly({fig_trust     %>% layout(height = 800, geo = list(scope = 'south america')) })                 
               output$PlotlyCoronaConcernMap <- renderPlotly({fig_concern   %>% layout(height = 800, geo = list(scope = 'north america')) })                     
             },
             "7" = { #7 Oceania
               output$PlotlyStressMap        <- renderPlotly({fig_stress    %>% layout(height = 800, geo = list(projection = list(rotation = list(lon=-180,lat=-20),scale=1.7))) })
               output$PlotlyIsolationMap     <- renderPlotly({fig_isolation %>% layout(height = 800, geo = list(projection = list(rotation = list(lon=-180,lat=-20),scale=1.7))) })
               output$PlotlyTrustMap         <- renderPlotly({fig_trust     %>% layout(height = 800, geo = list(projection = list(rotation = list(lon=-180,lat=-20),scale=1.7))) })
               output$PlotlyCoronaConcernMap <- renderPlotly({fig_concern   %>% layout(height = 800, geo = list(projection = list(rotation = list(lon=-180,lat=-20),scale=1.7))) })        
             }
      )
    }
    })
  })
}
