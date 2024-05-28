
  filteredWS <- df %>% 
    filter(`Last Observation Year` == 2023) %>% 
    ungroup() %>% 
    select(5, 8, 12, 9, 10, 11, 13, 14, 18, 19) %>% 
    arrange(Age, Stock) 
  
  # Convert `Brood Year` column to character type
  filteredWS <- filteredWS %>% mutate(`Brood Year` = as.character(`Brood Year`))
  

  

# asd <- as.data.frame(filteredWS)
# asd$Total <- apply(asd[,7:8],1,sum)

i <- which(asd$`Unique Tags Detected` >= 5)
# sum(asd$`Lower CI`[i],na.rm = TRUE)

tmp <- rep(NA,ncol(asd))
tmp[7:8] <- apply(asd[,7:8],2,sum)
tmp[9:10] <- apply(asd[i,8:9],2,sum)
rbind(asd,tmp)


1:10 |> mean() |> as.character()



head(asd)
  return(resultWS)
