#  2023-03-29
#  Kathleen Stutzman
#  Functions Code for Homework 8

#################################
# FUNCTION: get_data
# packages: none
# purpose: create data for homework
# input: .csv or nothing for random generation
# output: data frame
# -------------------------------
get_data <- function(file_name=NULL) {
  if(is.null(file_name)) {
    d_frame <- data.frame(Burn0=rnorm(n=2, mean = 580, sd = 100),
                          Burn1=rnorm(n=2, mean = 800, sd = 100),
                          Burn2=rnorm(n=2, mean = 1000, sd = 100),
                          Burn3=rnorm(n=2, mean = 1925, sd = 200),
                          Burn4=rnorm(n=2, mean = 1925, sd = 200),
                          Burn5=rnorm(n=2, mean = 2650, sd = 400),
                          Burn6=rnorm(n=2, mean = 2650, sd = 400))
  } else {
    d_frame <- read.table(file=file_name,
                          header=TRUE,
                          sep=",")
  }
  
  return(d_frame)
}


#################################
# FUNCTION: rotate_data
# packages: none
# purpose: 
# input:
# output:
# -------------------------------
rotate_data <- function(z) {
    z <- z %>%
      pivot_longer(cols = 1:7,
                      names_to = "treatment",
                      values_to = "seedlingcounts")
return(z)
}

#################################
# FUNCTION: calculate_stuff
# packages: none
# purpose: create an anova 
# input:
# output:
# -------------------------------
calculate_stuff <- function(x) {
  
  y <- aov(rdata$seedlingcounts~rdata$treatment)
  summary(y)
  
  return(summary(y))
}

#################################
# FUNCTION: graph_it
# packages: none
# purpose:
# input:
# output:
# -------------------------------
graph_it <- function(x) {
  
graph_anova <- ggplot(rdata, aes(treatment, seedlingcounts)) +
  geom_violin()

return(graph_anova)
}



#################################
# FUNCTION: analyze_this
# packages: none
# purpose: create a t.test
# input: seedling data for burns 1 & 6
# output: t.test result
# -------------------------------
analyze_this <- function(z) {
   
x<-t.test(test_data$Burn1, test_data$Burn6)
  
 return(x) 
} 

#################################
# FUNCTION: subset_data
# packages: none
# purpose:
# input:
# output:
# -------------------------------
subset_data <- function(x) {
  
  filter_data <- filter(rdata, treatment %in% c("Burn1", "Burn6"))
  
  return(filter_data)
}


#################################
# FUNCTION: another_graph
# packages: none
# purpose:
# input:
# output:
# -------------------------------
another_graph <- function() {
  
  graph_ttest <- ggplot(newdata, aes(x=treatment, y=seedlingcounts)) +
    geom_boxplot()
  
  return(graph_ttest)
}
