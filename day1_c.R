get_variable_names <- function(links){
  
  # driver 실행
  
  remDr <- remoteDriver(remoteServerAddr = "localhost" ,
                        port = 4445L, 
                        browserName = "chrome")  
  remDr$open(silent = T)
  
  # xpath 
  
  xpath <- '//*[@id="cphContents_cphContents_cphContents_udpContent"]/div[3]/table/thead'
  
  # make list plate 
  
  variable_names <- list()
  
  
  for(i in 1:length(links)){
    
    remDr$navigate(links[[i]])
    
    variable_names[[i]] <- remDr$findElement(value = xpath)$getElementText() %>% 
      str_replace_all('GW RBI', 'GW/RBI') %>% 
      str_split(' ') %>% 
      unlist()
    
  }
  
  names(variable_names) <- names(links)
  
  remDr$close()
  
  return(variable_names)
  
}
