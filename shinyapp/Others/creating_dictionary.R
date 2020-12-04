#Creating dictionary file

files = dir()
temp_final <- NULL
for(i in 1:length(files)){
  temp <- read.csv2(files[i], h=T)
  if(ncol(temp)==2){
    temp$`Admin Lvl` <- 1
  }
  colnames(temp) <- c("Incorrect", "Correct", "Admin Lvl")
  temp_final <- rbind(temp,temp_final)
}
save(temp,file = "dictionary.Rdata")
