#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions that creates a choice matrix for both, Community and Facility ====
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 function(qx) {
  
  if(!exists('df2')){matx <- NULL} else {
    
    fl <- df2 %>% 
      filter(sitetype %in% c("Facility")) %>%  
      filter(timept %in% qx) %>% 
      select(sitetype, uid, lat, lng, HTS_TST_POS)
    #filter(qt %in% qtz[i])
    
    
    if(!exists('fl') ){fl_matx <- NULL} else {
      
      # Swap uid with siteid
      flx <- left_join(fl, sitekey) %>% select(-uid)
      
      name1 <- names(flx)
      name2 <- paste(name1, "_2", sep="")
      
      site_1 <- flx
      site_2 <- flx
      names(site_2) <- name2
      
      site_1x <- site_1 
      site_2x <- site_2 %>%  
        select(-HTS_TST_POS_2)
      
      
      # Creating agents at each facility and community with splay
      fl_splay <- site_1x %>% 
        mutate(splaydist = 20) %>% # Setting a placeholder splay distance
        filter(HTS_TST_POS>0) %>% 
        uncount(HTS_TST_POS)
      
      #generate the random numbers to be applied to the coordinates
      fl_splay$u <- runif(nrow(fl_splay), min=-1, max=1)
      fl_splay$v <- runif(nrow(fl_splay), min=-1, max=1)
      
      
      #splay distribution assigns random distance of each HIV positive patient from the site
      fl_splayx <- fl_splay %>% 
        mutate(dlat = (splaydist/111.111)*u,  # distance in km
               dlon = ((splaydist/111.111)*v)*cos(deg2rad(lat))) %>% 
        mutate(lat = lat + dlat,
               lng = lng + dlon) %>% 
        select(sitetype, 
               # latn, lngn,
               lat, lng, siteid) %>% 
        mutate(pt_id = row_number())
      
      
      # testx <- fl_splayx %>% filter(siteid == 158)
      # 
      # # Testing the splay effect
      # leaflet(civ_district) %>%
      #   addPolygons(fillColor=~pal(level5name),
      #               color='white', weight=1, opacity=.7, fillOpacity = 0.3,
      #               popup = state_popup) %>%
      #   addCircleMarkers(data=testx, lng=~lngn, lat=~latn, radius=.2, opacity=.4,
      #                    color="blue") %>%
      # addCircleMarkers(data=testx, lng=~lng, lat=~lat, radius=.6, opacity=.3,
      #                    color="red") %>%
      #   addScaleBar()
      
      
      # Doing the cross-merge to get a cartesian product
      cross_df <- merge(x = fl_splayx, y = site_2x, by = NULL)
      
      if(!exists('cross_df')){fl_matx <- NULL} else {
        
        # Creating distance values
        fl_maty <- cross_df %>% 
          # Remove same site dyads
          filter(siteid != siteid_2) %>% 
          # Create the patient data points with variable splay radii 
          mutate(distance = distHaversine(cbind(lng, lat), cbind(lng_2, lat_2))) %>% 
          mutate(dist_km = distance*0.001) %>%
          arrange(pt_id, dist_km) %>% 
          #subset for only dydads within top 100 of rank distance
          group_by(pt_id) %>% 
          mutate(rank_d = row_number()) %>% 
          ungroup() %>% 
          filter(rank_d<=100 | dist_km<=50) %>%
          # For community sites, first choice for Tx can't be itself
          # mutate(rank_d = if_else(sitetype %in% c("Community"), rank_d-1, rank_d)) %>% 
          mutate(crank = paste("c", formatC(rank_d, width=3, flag="0"), sep="")) %>% 
          select(sitetype, siteid, pt_id, lat, lng, siteid_2, crank) %>% 
          spread(crank, siteid_2) %>% 
          mutate(c000 = siteid)
        
        
        fl_matx <- bind_rows(fl_maty, dummyfl) %>% 
          select(pt_id, sitetype, siteid, c000, c001:c100, lat, lng) %>% 
          mutate(
            assigned = 0,	
            not_assigned = 1, 
            q_treated	= "",
            fl_treated = NA,	
            jump	= "",
            q_tested = qx)
        
      }}
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= For Community sites ~~~~~~~===================
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Setting up loop where each community site is assigned a number of agents 
    # based on it's HTS_TST_POS numbers and then distributing the agents
    #  randomly over the pSNU it is located in.
    cm <- df2 %>% 
      filter(sitetype %in% c("Community") ) %>% 
      filter(timept %in% qx) %>% 
      select(uid, psnuuid, HTS_TST_POS) %>% 
      filter(HTS_TST_POS>0) %>% 
      filter(!is.na(HTS_TST_POS))
    
    if(!exists('cm')){ cm_matx <- NULL} else {
      
      cmx <- left_join(cm, sitekey) %>% select(-uid) 
      
      comvec <- unique(cmx$siteid)
      
      datalist2 = list()  
      
      w <- c(1:length(comvec))
      
      comsplay_possibly <- possibly(comsplay, otherwise=NULL)
      datalist2 <- map( .x = w, .f = ~comsplay_possibly(.x))
      
      if(length(datalist2)==0 | missing(datalist2)){ cm_matx <- NULL} else {
        
        allcom <- dplyr::bind_rows(datalist2) %>% 
          mutate(pt_id = row_number()) %>% 
          mutate(sitetype = "Community")
        
        
        # Doing the cross-merge to get a cartesian product
        cross_df2 <- merge(x = allcom, y = site_2x, by = NULL)
        
        if(nrow(cross_df2)==0 | missing(cross_df2)){ cm_matx <- NULL } else {
          cm_maty <- cross_df2 %>% 
            # Create the patient data points with variable splay radii 
            mutate(distance = if_else(is.na(lng)|is.na(lat), NA_real_,
                                      distHaversine(cbind(lng, lat), cbind(lng_2, lat_2)))) %>% 
            mutate(dist_km = distance*0.001) %>%
            arrange(pt_id, dist_km) %>% 
            #subset for only dydads within top 100 of rank distance
            group_by(pt_id) %>% 
            mutate(rank_d = row_number()) %>% 
            ungroup() %>% 
            filter(rank_d<=101 | dist_km<=50) %>%
            # For community sites, first choice for Tx can't be itself
            # mutate(rank_d = if_else(sitetype %in% c("Community"), rank_d-1, rank_d)) %>% 
            # dropping down one rank to make C001 to C00
            mutate(crank = paste("c", formatC(rank_d-1, width=3, flag="0"), sep="")) %>% 
            select(sitetype, siteid, pt_id, lat, lng, siteid_2, crank) %>% 
            spread(crank, siteid_2) 
          
          
          cm_matx <- bind_rows(cm_maty, dummyfl) %>% 
            select(pt_id, sitetype, siteid, c000, c001:c100, lat, lng) %>% 
            mutate(
              assigned = 0,	
              not_assigned = 1, 
              q_treated	= "",
              fl_treated = NA,	
              jump	= "",
              q_tested = qx
            ) }}}
    
    # Stack the facility and community choice matrices 
    matx <- bind_rows(fl_matx, cm_matx) }
  
  if(exists('matx')){matx} else {NULL} # add it to your list
  # }, error=function(e){cat("Quelle Erreure:",conditionMessage(e), "\n")})
}

