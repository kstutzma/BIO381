#################################
# FUNCTION: group_data
# packages: dplyr, tidyverse
# purpose: to group data by site, create a number of plots for each site, and create a average of counts for each species found at each site
# input: pitch pine treatment data csv
# output: sorted dataframe with n unique plots and spec_mean calculated
# -------------------------------
group_data <- function(d=NULL) {
  if(is.null(d)){
    x_var <- runif(3)
    y_var<- sample(LETTERS, 3)
    d <- data.frame(x_var,y_var)
  }
  
  d <- d %>% 
    group_by(Site) %>% 
    mutate(nplots=n_distinct(Plot_Num)) %>% 
    group_by(Species_Code) %>% 
    mutate(spc_mean = (sum(Total_Num)/nplots))
  
  d <- d %>% 
    distinct(Site, .keep_all = TRUE)
  
  return(d)
  
}
