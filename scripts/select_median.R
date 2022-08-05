setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/Thembisa/THEMBISAv18_original")

output <- read.delim("TotalHIVtests.txt", header=FALSE, row.names = 1)
names(output)[2:47] <- seq(1985, 2030)
output <- output %>% 
  select(-V2, -V49, -V50, -V51, -V52, -V53, -V54, -V55, -V56, -V57, -V58)
t_output <- t(output)
output_new <- as.data.frame(as_tibble(t_output))
output_new$median <- rep(NA, 46)
output_new$year <- seq(1985, 2030)

for (i in 1:46){
  output_new$median[i] <- median(t_output[i,])
}


output_new$median[46]

output_new %>% ggplot(aes(year, median)) + geom_line()

select_median <- function(output_name){
  output_txt <- paste(output_name, "txt", sep = ".")
  output_txt <- paste("THEMBISAv18", output_txt, sep = "/")
  output <- read.delim(output_txt, header=FALSE, row.names = 1)
  names(output)[2:87] <- seq(1985, 2070)
  output <- output %>% select(-V2)
  t_output <- t(output)
  output_new <- as.data.frame(as_tibble(t_output))
  output_new$median <- rep(NA, 86)
  for (i in 1:46){
    output_new$median[i] <- median(t_output[i,])
  }
  return(output_new$median)
}


