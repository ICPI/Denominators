#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####============ AGE-SEX function ==============================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# creating function for to parallel process for each age-sex category

 function(k) {
  
  df2 <- df2y %>% 
    filter(agesex %in% k) 
  #~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ============= Running  the function for each quarter ~~~~~~~===================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Loop through all the quaters data and 
  # creates choice matrix for each period, and append into single dataset
  datalist = list()   #Create empty object to store each quarter choice matrix
  
  datalist <- map( .x = q_vec , .f = ~qchoice(.x))
  
  
  
  # Stack them all together to create choice matrix for 4 Qs
  cmat <- dplyr::bind_rows(datalist) %>% 
    mutate(pt_id = row_number()) %>%   # Give each agent an different ID each quarter
    mutate(agesex = k)
  
  tx <- df2 %>% 
    filter(sitetype %ni% c("Community")) %>%  
    select(uid, timept, lat, lng, TX_NEW)
  
  
  # Swap uid with siteid
  treat_nosx <- left_join(tx, sitekey) %>% 
    select(-uid) %>% 
    rename(fl_id_ray_match_tested = siteid) %>% 
    filter(!is.na(fl_id_ray_match_tested)) %>% 
    filter(!is.na(TX_NEW)) %>% 
    filter(!TX_NEW==0) %>% 
    # going wide for each quarter
    select(fl_id_ray_match_tested, TX_NEW, timept) %>%
    # spread(qt, TX_NEW) %>% 
    mutate_if(is.numeric, list(~replace(., is.na(.), 0 )))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ============= For Q1 ~~~~~~~===================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  z <- 1    # first period
  # Create the corresponding treatment dataset
  
  # subsetting for the referenced period ----------------------------------------------------------
  qty <- cmat %>% filter(q_tested %in% q_vec[z]) %>% 
    select(pt_id, siteid, c000:agesex, sitetype) 
  
  # For Qurter 1
  # ----------------------------------------------------------------------------
  # Refresh treatment dataset
  treat_nos <- treat_nosx %>% filter(timept %in% q_vec[z]) %>%  
    select(fl_id_ray_match_tested, TX_NEW) %>% 
    group_by(fl_id_ray_match_tested) %>% 
    summarise_all(list(~sum), na.rm=T)
  
  # ----------------------------------------------------------------------------
  # if the choice matrix for that quarter does not exist exit loop
  qt_df <- qty 
  
  abm_q(qt_df, treat_nos)
  
  # Get assigned and unassinged datasets  
  assigned <- qt_next %>% filter(assigned==1)  
  unassigned <- subset(qt_next, qt_df$assigned==0) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ============= For Q2 ~~~~~~~===================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  z <- 2   # 2nd period
  
  
  qty <- cmat %>% filter(q_tested %in% q_vec[z]) %>% 
    select(pt_id, siteid, c000:agesex, sitetype)
  
  qt_df <- rbind(qty, unassigned)
  
  
  # Adding the remaining Tx capacity
  treat_nos <- bind_rows(treat_nosx %>% filter(timept %in% q_vec[z]) , 
                         treat_next) %>% 
    select(fl_id_ray_match_tested, TX_NEW) %>% 
    group_by(fl_id_ray_match_tested) %>% 
    summarise_all(list(~sum), na.rm=T)
  
  # ###ASSIGN HTS_POS to TX SITES for Q2----------------------------------------------
  # ----------------------------------------------------------------------------
  abm_q(qt_df, treat_nos)
  
  assigned<- bind_rows(qt_next %>% filter(assigned==1), assigned) 
  dropout <- qt_next %>% filter(assigned==0 & q_tested %in% q_vec[z-1])
  unassigned <- qt_next %>% filter(assigned==0 & q_tested %in% q_vec[z]) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ============= For Q3 ~~~~~~~===================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  z <- 3   # 3rd period
  
  qty <- cmat %>% filter(q_tested %in% q_vec[z]) %>% 
    select(pt_id, siteid, c000:agesex, sitetype)
  
  qt_df <- rbind(qty, unassigned)
  
  
  # Adding the remaining Tx capacity
  treat_nos <- bind_rows(treat_nosx %>% filter(timept %in% q_vec[z]) , 
                         treat_next) %>% 
    select(fl_id_ray_match_tested, TX_NEW) %>% 
    group_by(fl_id_ray_match_tested) %>% 
    summarise_all(list(~sum), na.rm=T)
  
  # ----------------------------------------------------------------------------
  # ###ASSIGN HTS_POS to TX SITES for Q3----------------------------------------------
  # ----------------------------------------------------------------------------
  abm_q(qt_df, treat_nos)
  
  assigned<- bind_rows(qt_next %>% filter(assigned==1), assigned) 
  dropout <- bind_rows(qt_next %>% filter(assigned==0 & q_tested %in% q_vec[z-1]),
                       dropout)
  unassigned <- qt_next %>% filter(assigned==0 & q_tested %in% q_vec[z]) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ============= For Q4 ~~~~~~~===================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  z <- 4   # 4th period
  
  qty <- cmat %>% filter(q_tested %in% q_vec[z]) %>% 
    select(pt_id, siteid, c000:agesex, sitetype)
  
  qt_df <- rbind(qty, unassigned)
  
  
  # Adding the remaining Tx capacity
  treat_nos <- bind_rows(treat_nosx %>% filter(timept %in% q_vec[z]) , 
                         treat_nos) %>% 
    select(fl_id_ray_match_tested, TX_NEW) %>% 
    group_by(fl_id_ray_match_tested) %>% 
    summarise_all(list(~sum), na.rm=T)
  
  # ###ASSIGN HTS_POS to TX SITES for Q4----------------------------------------------
  # ----------------------------------------------------------------------------
  abm_q(qt_df, treat_nos)   
  
  assigned<- bind_rows(qt_next %>% filter(assigned==1), assigned) 
  dropout <- bind_rows(qt_next %>% filter(assigned==0 & q_tested %in% q_vec[z-1]),
                       dropout)
  unassigned <- qt_next %>% filter(assigned==0 & q_tested %in% q_vec[z]) 
  
  
  total_dropout <- dropout
  remaining_unassinged <- unassigned
  
  total_unassigned<- bind_rows(total_dropout,remaining_unassinged) %>% 
    mutate(dftype = "unassigned")
  
  total_assigned<- assigned %>% 
    mutate(dftype = "assigned")
  
  
  finaldf <- bind_rows(total_assigned, total_unassigned) %>% 
    select(pt_id, siteid, 
           lat,         
           lng,          
           assigned,    
           not_assigned, 
           q_treated,   
           fl_treated,   
           jump,        
           q_tested,     
           agesex,      
           sitetype,     
           dftype)
  
  return(finaldf)
}