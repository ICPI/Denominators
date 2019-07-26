# ===========================================================================
#   ABM movement algorithm implementation (Needs to be a sequential loop)
# ===========================================================================
 function(qt_df, treat_nos){
  
  n<-nrow(qt_df)
  # ncol(qt_df)
  set.seed(13)
  qt_df <- qt_df[sample(n),]
  
  for (i in 1:n)
  {
    if(qt_df$assigned[i]==1) {next}
    if(as.numeric(qt_df[i,3])==0) {next}
    if(qt_df[i,3] %in% treat_nos$fl_id_ray_match_tested=="FALSE"){next}
    if(treat_nos$fl_id_ray_match_tested[which(treat_nos$fl_id_ray_match_tested==
                                              as.numeric(qt_df[i,3]))]==0) {next}
    if(treat_nos$TX_NEW[treat_nos$fl_id_ray_match_tested==
                        as.numeric(qt_df[i,3])]>0) {
      qt_df$assigned[i]=1 ; 
      qt_df$q_treated[i]=q_vec[z];
      qt_df$not_assigned[i]=0;
      (treat_nos$TX_NEW[which(treat_nos$fl_id_ray_match_tested == as.numeric(qt_df[i,3]))]= 
         treat_nos$TX_NEW[which(treat_nos$fl_id_ray_match_tested ==as.numeric(qt_df[i,3]))]-1);
      qt_df$fl_treated[i]<-as.numeric(qt_df[i,3]); 
      qt_df$jump[i]<-1  
      print(paste("picking =", qt_df[i,2], "assigned? ", qt_df[i,106],
                  sep=" "))
    }
  }
  
  
  for (i in 1:n)
  {
    if(qt_df$assigned[i]==1) {next}
    print(paste("picking =", qt_df[i,2], "assigned? ", qt_df[i,106],
                sep=" "))
    
    for (j in 4:103)
    {
      print(paste("picking =", qt_df[i,2]," | rank=", j-2, "assigned? ", qt_df[i,106],
                  sep=" "))
      if(qt_df$assigned[i]==1) {break}
      if(is.na(qt_df[i,j])){break}
      if(as.numeric(qt_df[i,j])==0) {break}
      if(qt_df[i,j]%in%treat_nos$fl_id_ray_match_tested=="FALSE"){next}
      if((which(treat_nos$fl_id_ray_match_tested==as.numeric(qt_df[i,j]))==0)) {next}
      
      if(treat_nos$TX_NEW[which(treat_nos$fl_id_ray_match_tested==as.numeric(qt_df[i,j]))]>0) {
        qt_df$assigned[i]=1 ; 
        qt_df$q_treated[i]=q_vec[z]; 
        qt_df$not_assigned[i]=0;
        (treat_nos$TX_NEW[which(treat_nos$fl_id_ray_match_tested==as.numeric(qt_df[i,j]))]= 
           treat_nos$TX_NEW[which(treat_nos$fl_id_ray_match_tested ==as.numeric(qt_df[i,j]))]-1);
        qt_df$fl_treated[i]<-qt_df[i,j];
        qt_df$jump[i]<-j  
        
      }
    }
  }
  qt_next <<- qt_df
  treat_next <<- treat_nos
  # return(qt_next)
  # return(treat_next) 
}
