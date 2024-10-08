
## Generate Some Common Label Structures; this isn't necessary and is customizable ##

labels5 <- c("`5` = Strongly Agree", 
              `4` = "Agree", 
              `3` = "Neutral", 
              `2` = "Disagree", 
              `1` = "Strongly Disagree")
labels4 <- c("`4` = Strongly Agree", 
             `3` = "Agree", 
             `1` = "Disagree", 
             `1` = "Strongly Disagree")

labels3 <- c("`3` = Agree", 
              `2` = "Neutral", 
              `1` = "Disagree")

zero.one<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}              
recodeAVP <- function(x, labels = labels, reverse = TRUE) {
  if (length(labels) == 5 & reverse == FALSE){
  dat = case_when(
    as.numeric(x) == 1 ~ 1,
    as.numeric(x) == 2 ~ 2,
    as.numeric(x) == 3 ~ 3,
    as.numeric(x) == 4 ~ 4,
    as.numeric(x) == 5 ~ 5,
    TRUE ~ NA_real_ 
  )
  }
  else if (length(labels) == 4 & reverse == FALSE){
    dat = case_when(
      as.numeric(x) == 1 ~ 1,
      as.numeric(x) == 2 ~ 2,
      as.numeric(x) == 3 ~ 3,
      as.numeric(x) == 4 ~ 4,
      TRUE ~ NA_real_ 
    )
  }
  else if (length(labels) == 4 & reverse == TRUE){
    dat = case_when(
      as.numeric(x) == 4 ~ 1,
      as.numeric(x) == 3 ~ 2,
      as.numeric(x) == 2 ~ 3,
      as.numeric(x) == 1 ~ 4,
      TRUE ~ NA_real_ 
    )
  }
  else if (length(labels) == 3 & reverse == FALSE){
    dat = case_when(
      as.numeric(x) == 1 ~ 1,
      as.numeric(x) == 2 ~ 2,
      as.numeric(x) == 3 ~ 3,
      TRUE ~ NA_real_ 
    )
  }
  else if (length(labels) == 5 & reverse == TRUE){
    dat = case_when(
      as.numeric(x) == 5 ~ 1,
      as.numeric(x) == 4 ~ 2,
      as.numeric(x) == 3 ~ 3,
      as.numeric(x) == 2 ~ 4,
      as.numeric(x) == 1 ~ 5,
      TRUE ~ NA_real_ 
    )
  }
  else if (length(labels) == 3 & reverse == TRUE){
    dat = case_when(
      as.numeric(x) == 3 ~ 1,
      as.numeric(x) == 2 ~ 2,
      as.numeric(x) == 1 ~ 3,
      TRUE ~ NA_real_ 
    )
  }

  return(list(data = dat, labels = labels))
  
}


## Recode Policy Variables, in 2022, 2024
recode_policy_variables <- function(x, reverse = TRUE) {
  if(reverse == TRUE){
    out <- case_when(
      as.numeric(x) == 1 ~ 5,
      as.numeric(x) == 2 ~ 4,
      as.numeric(x) == 3 ~ 2,
      as.numeric(x) == 4 ~ 1,
      as.numeric(x) == 5 ~ 3,
      TRUE ~ NA_real_
    )
  }
  else if(reverse == FALSE){
    out <- case_when(
      as.numeric(x) == 5 ~ 5,
      as.numeric(x) == 4 ~ 4,
      as.numeric(x) == 3 ~ 2,
      as.numeric(x) == 2 ~ 1,
      as.numeric(x) == 1 ~ 3,
      TRUE ~ NA_real_
    )
  }
    
    return(out)
  }

