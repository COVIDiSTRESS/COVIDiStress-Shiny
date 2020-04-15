server <- function(input, output, session) {
  
  ## Charger les donnÃ©es ----
  
  #create the URL where the dataset is stored with automatic updates every day
  url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.Date()-1, "%Y-%m-%d"), ".xlsx", sep = "")
  #url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = "")
  #download the dataset from the website to a local temporary file
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
  #read the Dataset sheet into R
  data <- read_excel(tf)
  
  ## Additionnal variables
  options('stringsAsFactors'=FALSE)
  Unique_GeoId_full <- unique(data$geoId)
  Unique_CountryName_full <- unique(toTitleCase(tolower(data$countriesAndTerritories)))
  Unique_CountryName_space <- gsub("_", " ", Unique_CountryName_full)
  data$casesCumSum <- unlist(lapply(Unique_GeoId_full, 
                                    function(n_GeoId) rev(cumsum(rev(data$cases[as.logical(data$geoId==n_GeoId)])))))
  data$deathsCumSum <- unlist(lapply(Unique_GeoId_full, 
                                     function(n_GeoId) rev(cumsum(rev(data$deaths[as.logical(data$geoId==n_GeoId)])))))
  
  updateSelectizeInput(session, 'CountryChoice', choices = Unique_CountryName_space, server = TRUE,
                       selected=c("France"))
                      #selected=c("France", "Italy", "Spain"))  
  
  ## Additional function
  gg_color_hue <- function(n) { #fonction couleur
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  pays_apply <- function(data_source, data_Pays, FUN = max){ #source = ce sur quoi on applique, pays = les pays
    unlist(lapply(unique(data_Pays), function(N_Pays) FUN(data_source[as.logical(data_Pays == N_Pays)])))
  }
  
  f_DateRelThreshold <- function(data_to_cut, t_threshold){#we use "rev" because data starts with earliest
    if(is.na(t_threshold))
    {
      rep(-1, length(data_to_cut))
    }else{
      if(max(data_to_cut)<t_threshold)
      {
        rep(-1, length(data_to_cut)) 
      }else{
        below_threshold <- rev(which(rev(data_to_cut)<t_threshold))[1]
        rev(c(rep(0, ifelse(is.na(below_threshold),0,below_threshold)), 
              c(1:length(which(rev(data_to_cut) >= t_threshold))))-1)  
      }
    }
  }
  
  
  f_RollingMean <- function(data_to_smooth, smooth_value){
    if(smooth_value==1){ data_to_smooth
    }else{  rollmean(data_to_smooth, k = input$slider_lissage, fill= NA)
    }
  }
  
  #CB_percent <- input$CheckBox_percent; pred_forward <- input$prediction_forward; pred_data <- input$prediction_data; df <- df2
  
  f_prediction <- function(df, pred_forward, pred_data, CB_percent)
  {
    if(!CB_percent){
      df$DifCasesCumMod <- df$DifCasesCumMod/df$CasesCumMod
    }
    dfPred <- df[1:input$prediction_data,]
    model <- lm(DifCasesCumMod~ DateRelCases, data =  dfPred)
    ndata <- data.frame(DateRelCases =
                          seq(max(dfPred$DateRelCases), by="1 day", 
                              length.out=input$prediction_forward+1)[2:(input$prediction_forward+1)])
    frcst <- forecast(model, newdata=ndata)
    vec0 <- rep(0, length(ndata))
    frcst$mean <- pmax(vec0, frcst$mean)
    frcst$lower[,1] <- pmax(vec0, frcst$lower[,1])
    frcst$upper[,1] <- pmax(vec0, frcst$upper[,1])
    
    #results
    NewCasespercent <- data.frame(
      mean = c(dfPred$DifCasesCumMod[1], frcst$mean),
      lower =  c(dfPred$DifCasesCumMod[1], frcst$lower[,1]),
      upper =  c(dfPred$DifCasesCumMod[1], frcst$upper[,1])
    )
    CasesCumSum <- data.frame(
      mean =  c(dfPred$CasesCumMod[1], dfPred$CasesCumMod[1]*cumprod(1+frcst$mean)),
      lower =  c(dfPred$CasesCumMod[1],dfPred$CasesCumMod[1]*cumprod(1+frcst$lower[,1])),
      upper =  c(dfPred$CasesCumMod[1],dfPred$CasesCumMod[1]*cumprod(1+frcst$upper[,1]))
    )
    NewCasesAbs <- data.frame(
      mean =  c(dfPred$Cases[1],diff(CasesCumSum$mean)),
      lower =  c(dfPred$Cases[1],diff( CasesCumSum$lower)),
      upper =  c(dfPred$Cases[1],diff( CasesCumSum$upper))
    )
    pred <- data.frame(
      DateRelCases = c(max(dfPred$DateRelCases), ndata$DateRelCases),
      NewCasespercentMean = NewCasespercent$mean,
      NewCasespercentLower = NewCasespercent$lower,
      NewCasespercentUpper = NewCasespercent$upper,
      CasesCumSumMean = CasesCumSum$mean,
      CasesCumSumLower = CasesCumSum$lower,
      CasesCumSumUpper = CasesCumSum$upper,
      NewCasesAbsMean = NewCasesAbs$mean,
      NewCasesAbsLower = NewCasesAbs$lower,
      NewCasesAbsUpper = NewCasesAbs$upper
    )
    pred$CasesCumMod <- pred$CasesCumSumMean
    pred$DifCasesCumMod <- pred$NewCasesAbsMean
    
    return(pred)
  }
  
  test_NAandNeg <- function(v) {
    if(is.null(v)){
      FALSE
      }else{
        if(is.na(v)){FALSE
        }else{if(v<=0){FALSE
        }else{TRUE}
        }
      }
  }
  
  
  ## Filtrer les donnÃ©es en fonction des pays ----
  observeEvent({
    input$CB_ratio
    input$CB_log
    input$slider_lissage
    input$CheckBox_percent
    input$num_Relative_case
    input$num_Relative_death
    input$Start_Date
    input$CountryChoice
    input$prediction_data
    input$prediction_forward
  },{
    
    #input <- c(); input$slider_lissage = 1; input$num_Relative_case = 0; input$CB_log = 0; input$CB_ratio = 0; 
    #input$CheckBox_percent=1; input$Start_Date = "2020-03-01"; input$num_Relative_death = 0;
    #input$prediction_data <- 10; input$prediction_forward <- 5
    #input$slider_lissage = 1; input$num_Relative_case = 0; input$CB_log = 0; input$CB_ratio = 0; input$num_Relative_death = 0;
    
    vec_code <- Unique_GeoId_full[which(Unique_CountryName_space %in% input$CountryChoice)]
    if(length(vec_code)>0){list_pays <-  vec_code
    }else{
      list_pays <- "FR"
    }
    
    filterPays <- as.logical((data$geoId %in% list_pays)*(data$dateRep>=ymd(input$Start_Date)))
    #list_pays <- c("BE") #list_pays <- c("ES", "IT")
    #fun_date_cases
    #filtre1Pays <- function(N_Pays) as.logical((data$geoId == N_Pays))#*(data$Month>2)*(data$Month<12))
    
    
    
    
    df2 <- data.frame(Country = data$geoId[filterPays],
                      Date = data$dateRep[filterPays],
                      DateRelCases = pays_apply(data$casesCumSum[filterPays], data$geoId[filterPays], 
                                                FUN = function(x) f_DateRelThreshold(x, input$num_Relative_case)),
                      DateRelDeaths = pays_apply(data$deathsCumSum[filterPays], data$geoId[filterPays], 
                                                 FUN = function(x) f_DateRelThreshold(x, input$num_Relative_death)),
                      # DateRelLockdown =  # Ã  faire
                      Cases = data$cases[filterPays],
                      CasesCumMod = pays_apply(data$casesCumSum[filterPays], data$geoId[filterPays],
                                               FUN = function(x) f_RollingMean(x, input$slider_lissage))/(
                                                 1+(data$popData2018[filterPays]/100000-1)*input$CB_ratio
                                               ),
                      DifCasesCumMod = pays_apply(
                        data$cases[filterPays]/(1+(data$casesCumSum[filterPays]-1)*input$CheckBox_percent),
                        data$geoId[filterPays],
                        FUN = function(x) f_RollingMean(x, input$slider_lissage)),
                      DifCasesCumMod_2 = pays_apply(
                        data$cases[filterPays]/(1+(data$casesCumSum[filterPays]-1)*input$CheckBox_percent),
                        data$geoId[filterPays],
                        FUN = function(x) c(f_RollingMean(-diff(x),input$slider_lissage), 0)),
                      Deaths = data$deaths[filterPays],
                      DeathsCumMod = pays_apply(data$deathsCumSum[filterPays], data$geoId[filterPays],
                                                FUN = function(x) f_RollingMean(x, input$slider_lissage))/(
                                                  1+(data$popData2018[filterPays]/100000-1)*input$CB_ratio
                                                ),
                      DifDeathsCumMod = pays_apply(
                        data$deaths[filterPays]/(1+(data$deathsCumSum[filterPays]-1)*input$CheckBox_percent),
                        data$geoId[filterPays],
                        FUN = function(x) f_RollingMean(x, input$slider_lissage)),
                      DifDeathsCumMod_2 = pays_apply(
                        data$deaths[filterPays]/(1+(data$deathsCumSum[filterPays]-1)*input$CheckBox_percent),
                        data$geoId[filterPays],
                        FUN = function(x) c(f_RollingMean(-diff(x),input$slider_lissage), 0))
    )
    
    
    
    
    Unique_CountryName_filter <- gsub("_", " ", unique(toTitleCase(tolower(data$countriesAndTerritories[filterPays]))))
    
    
    
    SCM <-  scale_color_manual(labels = Unique_CountryName_filter[order(list_pays)],
                               values = gg_color_hue(length(Unique_GeoId_full))[
                                 Unique_GeoId_full %in% sort(list_pays)][order(list_pays)])
    #values = gg_color_hue(length(Unique_GeoId_full))[Unique_GeoId_full %in% list_pays])
    theme_attribute <- theme(axis.title.x = element_text(size=20, face="bold"),
                             axis.title.y = element_text(size=20, face="bold"),
                             axis.text.x = element_text(size=15),
                             axis.text.y = element_text(size=10))
    
    
    
    test_cases <- test_NAandNeg(input$num_Relative_case)
    test_deaths <- test_NAandNeg(input$num_Relative_death)
    if(!test_cases){df2$DateRelCases = df2$Date}
    if(!test_deaths){df2$DateRelDeaths = df2$Date}
    
    
    pCasesCum <- ggplot(data=df2, aes(x=DateRelCases, y = CasesCumMod)) + dark_theme_dark()
    for (cntry in unique(df2$Country)) {
      pCasesCum <- pCasesCum +
        geom_line(aes(x = DateRelCases, y =CasesCumMod, group = Country, colour = Country),
                  size = 2, na.rm = TRUE, data = df2[df2$Country == cntry,]) +
        geom_point(aes(x = DateRelCases, y =CasesCumMod, group = Country, colour = Country),
                   size = 5, shape = 16, na.rm = TRUE, data = df2[df2$Country == cntry,])
    }
    pCasesCum <- pCasesCum +  SCM + {if(input$CB_log)scale_y_log10()} +
      xlab("Days") + ylab("Total number of cases") +
      theme_attribute + {if(test_cases)xlim(c(0, max(df2$DateRelCases)))} +
      guides(colour = guide_legend(override.aes = list(shape = NA)))
    
    pDeathsCum <- ggplot(data=df2, aes(x = DateRelDeaths, y=DeathsCumMod)) + dark_theme_dark()
    for (cntry in unique(df2$Country)) {
      pDeathsCum <- pDeathsCum +
        geom_line(aes(x= DateRelDeaths, y =  DeathsCumMod, group = Country, colour = Country),
                  size = 2, na.rm = TRUE, data = df2[df2$Country == cntry,]) +
        geom_point(aes(x= DateRelDeaths, y =  DeathsCumMod, group = Country, colour = Country),
                   size = 5, shape = 16, na.rm = TRUE, data = df2[df2$Country == cntry,])
    }
    pDeathsCum <- pDeathsCum +
      SCM + {if(input$CB_log)scale_y_log10()} + xlab("Days") + ylab("Total number of deaths") +
      theme_attribute + {if(test_deaths)xlim(c(0, max(df2$DateRelDeaths)))} +
      guides(colour = guide_legend(override.aes = list(shape = NA)))
    
    pCasesRel <- ggplot(data=df2, aes(x = DateRelCases, y = DifCasesCumMod)) + dark_theme_dark() 
    for (cntry in unique(df2$Country)) {
      pCasesRel <- pCasesRel +
        geom_line(aes(x = DateRelCases, y =DifCasesCumMod, group = Country, colour = Country),
                  size = 2, na.rm = TRUE, data = df2[df2$Country == cntry,]) +
        geom_point(aes(x = DateRelCases, y =DifCasesCumMod, group = Country, colour = Country),
                   size = 5, shape = 16, na.rm = TRUE, data = df2[df2$Country == cntry,])
    }
    pCasesRel <- pCasesRel +
      SCM + {if(input$CB_log && !input$CheckBox_percent)scale_y_log10()} + 
      xlab("Days") + ylab("Increase in cases") +
      theme_attribute + {if(test_cases)xlim(c(0, max(df2$DateRelCases)))} +
      guides(colour = guide_legend(override.aes = list(shape = NA)))
    
    
    
    pDeathsRel <- ggplot(data=df2, aes(x =  DateRelDeaths, y = DifDeathsCumMody)) + dark_theme_dark()
    for (cntry in unique(df2$Country)) {
      pDeathsRel <- pDeathsRel +
        geom_line(aes(x = DateRelDeaths, y = DifDeathsCumMod, group = Country, colour = Country),
                  size = 2, na.rm = TRUE, data = df2[df2$Country == cntry,]) +
        geom_point(aes(x = DateRelDeaths, y = DifDeathsCumMod, group = Country, colour = Country),
                   size = 5, shape = 16, na.rm = TRUE, data = df2[df2$Country == cntry,])
    }
    pDeathsRel <- pDeathsRel +
      SCM + {if(input$CB_log && !input$CheckBox_percent)scale_y_log10()} +
      xlab("Days") + ylab("Increase in deaths") +
      theme_attribute + {if(test_deaths)xlim(c(0, max(df2$DateRelDeaths)))} +
      guides(colour = guide_legend(override.aes = list(shape = NA)))
    
    test_data <- test_NAandNeg(input$prediction_data)
    test_forward <- test_NAandNeg(input$prediction_forward)
    
    if(input$prediction_data>0 && input$prediction_forward>0 && length(list_pays)==1 && test_data && test_forward){
      df2_pred <- f_prediction(df2, pred_forward, pred_data, input$CheckBox_percent)
      
      pCasesCum <- pCasesCum + geom_point(data = df2_pred, aes(x=DateRelCases,y=CasesCumSumMean)) +
        geom_ribbon(data = df2_pred, aes(ymin=CasesCumSumLower, ymax=CasesCumSumUpper),alpha=0.2) + 
        ylim(0, 1.5*max(df2_pred$CasesCumSumMean, df2$CasesCumMod))
      
      if(input$CheckBox_percent)
      {
        pCasesRel <- pCasesRel + geom_point(data = df2_pred, aes(x=DateRelCases,y = NewCasespercentMean)) +
            geom_ribbon(data = df2_pred, aes(ymin=df2_pred$NewCasespercentLower, ymax=df2_pred$NewCasespercentUpper),alpha=0.2)
      }else{
        pCasesRel <- pCasesRel + geom_point(data = df2_pred, aes(x=DateRelCases,y = NewCasesAbsMean)) +
                geom_ribbon(data = df2_pred, aes(ymin= NewCasesAbsLower, ymax= NewCasesAbsUpper),alpha=0.2) +
          ylim(0, 1.5*max(df2_pred$NewCasesAbsMean, df2$DifCasesCumMod))
      }
    
    
      #print(df2_pred$NewCasespercentMean)
      #k <-ggplot(df2_pred, aes(x=DateRelCases,y=CasesCumSumMean)) + dark_theme_dark()
      #k <- k+ geom_point(aes(x=Dates,y=CasesCumSumMean))
      #k+geom_ribbon(aes(ymin=CasesCumSumLower,
      #                 ymax=CasesCumSumUpper),alpha=0.2)  
    }
    
       
    
    output$tableDf <- renderTable(df2)
    output$PlotCasesCum<-renderPlot({ pCasesCum })#, height = 400,width = 600)
    output$PlotDeathsCum<-renderPlot({ pDeathsCum })#, height = 400,width = 600)
    output$PlotCasesRel<-renderPlot({ pCasesRel })#, height = 400,width = 600)
    output$PlotDeathsRel<-renderPlot({ pDeathsRel })#, height = 400,width = 600)
    output$PlotCasesRel2<-renderPlot({ pCasesRel_2 })#, height = 400,width = 600)
    output$PlotDeathsRel2<-renderPlot({ pDeathsRel_2 })#, height = 400,width = 600)
  })
  
}