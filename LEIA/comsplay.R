#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Community splay function ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Splays agents uniformally within a community site's containing polygon
function(j){
  # subset psnu
  pgon <- psnu_shp[psnu_shp$uid 
                   %in% cmx[j,1],] 
  
  hts <- as.numeric(cmx[j,2])
  
  comid <- as.numeric(cmx[j,3])
  
  # # Skip if area of polygon is zero
  # Filling up PSNU with number of agents based on HTS value
  res <- try(spsample(pgon,n=hts,"random", iter=20),silent = TRUE)
  if (class(res) == "try-error"){NULL} else {
    
    fillpoly <- as.data.frame(spsample(pgon,n=hts,"random", iter=20))
    
    
    comm <- fillpoly %>% 
      mutate(siteid = comid)
    
    dummy <- fillpoly[F,F] %>% 
      mutate(x = NA, 
             y = NA)
    
    commx <- bind_rows(dummy, comm) %>% 
      select(siteid, x, y) %>% 
      rename(lng = x ,
             lat = y )
    
    commx }# add it to your list  
  
  
}
