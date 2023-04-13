#################################
# FUNCTION: analyze_data
# packages: none
# purpose: to calculate an ANOVA on species means and return summary statistics 
# input: sorted df using group_data
# output: p_val & f_stat
# -------------------------------
analyze_data <- function(d=NULL) {
  if(is.null(d)){
    x_var <- runif(3)
    y_var<- sample(LETTERS, 3)
    . <- data.frame(x_var,y_var)
  }
  
  d_aov <- aov(d$spc_mean~d$Latin_Name, data=d)
  . <- summary(d_aov)[[1]][1,4:5]
  
  return(.)
}