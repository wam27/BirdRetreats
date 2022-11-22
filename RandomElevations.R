ramelev<-function(spp_list, occurrences, low_lim, up_lim, y){
  
  require(dplyr)
  Output_df <- data.frame('species' = character(),
                          'Year_range'=character(),
                          'Lower_elevation_limit'=double(),
                          'Upper_elevation_limit'=double(),
                          'Total_observations' = double(),
                          'Percent_of_total'= double(),
                          ### early: Before 2005 ####
                          'Observations_early_period'=double(),
                          ### late: After 2005 ####
                          'Observations_late_period'=double(),
                          
                          #OBSERVED
                          #### Low limit ######
                          'Observations_below_the_lower_limit_for_the_early_period'= double(),
                          'Observations_below_the_lower_limit_for_the_late_period'= double(),
                          #### Up limit ####
                          'Observations_above_the_upper_limit_for_the_early_period'= double(),
                          'Observations_above_the_upper_limit_for_the_late_period'= double(),
                          #EXPECTED
                          #### Low limit ######
                          'Average_expected_observations_below_the_lower_limit_for_the_early_period'=double(),
                          'Average_expected_observations_below_the_lower_limit_for_the_late_period'=double(),
                          #### Up limit ######
                          'Average_expected_observations_above_the_upper_limit_for_the_late_period'=double(),
                          'Average_expected_observations_above_the_upper_limit_for_the_early_period'=double(),
                          #### Differences ########
                          'Observed_minus_expected_below_the_lower_limit'=double(),
                          'Observed_minus_expected_avobe_the_upper_limit'=double(),
                          'Observed_minus_expected_below_the_lower_limit_late'=double(),
                          'Observed_minus_expected_avobe_the_upper_limit_early'=double(),
                          stringsAsFactors = FALSE)
  
  for (s in 1:length(spp_list[,1])){
    species<-spp_list[s,1]
    Output_df[s,'species'] <- species
    if (file.exists(paste0(occurrences, species, '_pres.csv'))){
      pres<-read.csv(paste0(occurrences, species, '_pres.csv'))
      if ('ELEVATIO_1' %in% colnames(pres)){
        pres['ELEVATION']<-pres$ELEVATIO_1
      }
      #THRESHOLDS
      Lower_elevation_limit<-pres%>%summarize(quants = quantile(ELEVATION, probs = c(low_lim)))
      Output_df[s,'Lower_elevation_limit'] <- Lower_elevation_limit
      Upper_elevation_limit<-pres%>%summarize(quants = quantile(ELEVATION, probs = c(up_lim)))
      Output_df[s,'Upper_elevation_limit'] <- Upper_elevation_limit
      
      if (file.exists(paste0(occurrences, species, '_gbif_P.csv'))){
        pres2<-read.csv(paste0(occurrences, species, '_gbif_P.csv'))
        if ('ELEVATIO_1' %in% colnames(pres2)){
          pres2['ELEVATION']<-pres2$ELEVATIO_1
        }
        pres<-pres2
      if ('YEAR_1' %in% colnames(pres)){
        pres['YEAR']<-pres$YEAR_1
      }
        
      pres <- pres %>% mutate(YEAR=as.numeric(YEAR)) %>% mutate(ELEVATION=as.numeric(ELEVATION))
      Year_range<-paste(min(pres$ELEVATION),"-",max(pres$ELEVATION))
      Output_df[s,'Year_range'] <- Year_range
      #Species_e<-pres$ScientificName[s]
      print(paste(s,species))
      
      #Total observations
      Total_observations<-length(pres[,1])
      Output_df[s,'Total_observations'] <- Total_observations
      Percent_of_total=Total_observations*low_lim
      Output_df[s,'Percent_of_total'] <- Percent_of_total
      
      
      ### early: Before threshold ####
      Early_pres<-pres%>%filter(pres$YEAR<y)
      oep=length(Early_pres[,1])
      Output_df[s,'Observations_early_period'] <- oep
      #### 5% early######
      obltep<-length(which(Early_pres$ELEVATION<Lower_elevation_limit[1,]))
      Output_df[s,'Observations_below_the_lower_limit_for_the_early_period'] <- obltep
      
      #### 95% early######
      oautep<-length(which(Early_pres$ELEVATION>Upper_elevation_limit[1,]))
      Output_df[s,'Observations_above_the_upper_limit_for_the_early_period'] <- oautep
      
      
      
      ### late: After threshold ####
      Late_pres<-pres%>%filter(pres$YEAR>=y)
      olp=length(Late_pres[,1])
      Output_df[s,'Observations_late_period'] <- olp
      
      
      #### 5% late######
      obltlp<-length(which(Late_pres$ELEVATION<Lower_elevation_limit[1,]))
      Output_df[s,'Observations_below_the_lower_limit_for_the_late_period'] <- obltlp
      
      #### 95% late######
      oautlp<-length(which(Late_pres$ELEVATION>Upper_elevation_limit[1,]))
      Output_df[s,'Observations_above_the_upper_limit_for_the_late_period'] <- oautlp
      
      #Lists
      eeu5_list<-list()
      ela95_list<-list()
      elu5_list<-list()
      eea95_list<-list()
      
      for (i in 1:1000){
        S<-sample(pres$ELEVATION,Total_observations,replace = FALSE)  #Simulation
        
        S_early<-S[1:oep]
        S_late<-S[oep+1:olp]
        
        #### 5% ######
        ex_eu5T_e<-length(which(S_early<Lower_elevation_limit[1,]))
        eeu5_list[[i]]<-ex_eu5T_e
        ex_lu5T_e<-length(which(S_late<Lower_elevation_limit[1,]))
        elu5_list[[i]]<-ex_lu5T_e
        #### 95% ######
        ex_la95T_e<-length(which(S_late>Upper_elevation_limit[1,]))
        ela95_list[[i]]<-ex_la95T_e
        ex_ea95T_e<-length(which(S_early>Upper_elevation_limit[1,]))
        eea95_list[[i]]<-ex_ea95T_e
      }
      
      #Getting means
      eobltep<-mean(unlist(eeu5_list))
      Output_df[s,'Average_expected_observations_below_the_lower_limit_for_the_early_period'] <- eobltep
      eoautlp<-mean(unlist(ela95_list))
      Output_df[s,'Average_expected_observations_above_the_upper_limit_for_the_late_period'] <- eoautlp
      
      eobltlp<-mean(unlist(elu5_list))
      Output_df[s,'Average_expected_observations_below_the_lower_limit_for_the_late_period'] <- eobltlp
      eoautep<-mean(unlist(eea95_list))
      Output_df[s,'Average_expected_observations_above_the_upper_limit_for_the_early_period'] <- eoautep
      
      
      diff_e<-obltep-eobltep
      Output_df[s,'Observed_minus_expected_below_the_lower_limit'] <- diff_e
      diff_l<-oautlp-eoautlp
      Output_df[s,'Observed_minus_expected_avobe_the_upper_limit'] <- diff_l
      
      diff_llt<-obltlp-eobltlp
      Output_df[s,'Observed_minus_expected_below_the_lower_limit_late'] <- diff_llt
      diff_eut<-oautep-eoautep
      Output_df[s,'Observed_minus_expected_avobe_the_upper_limit_early'] <- diff_eut
      }
    }
    }
  return (Output_df)
}




