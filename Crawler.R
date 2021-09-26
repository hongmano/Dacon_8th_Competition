get_variable_names <- function(links){
  
  # driver
  
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


KBO_crawl <- function(start, end, links){
  
  # Get Variable Names & XPATH code
  
  v_names <- get_variable_names(links)
  data_xpath <- '//*[@id="cphContents_cphContents_cphContents_udpContent"]/div[3]/table/tbody'
  year_xpath <- '//*[@id="cphContents_cphContents_cphContents_ddlSeason_ddlSeason"]/option['
  position_xpath <- '//*[@id="cphContents_cphContents_cphContents_ddlPos_ddlPos"]/option['
  page_xpath <- '//*[@id="cphContents_cphContents_cphContents_ucPager_btnNo'
  
  # Crawl
  
  data_fin <- list(basic1 = c(),
                   basic2 = c(), 
                   detail = c())
  
  remDr <- remoteDriver(remoteServerAddr = "localhost" ,
                        port = 4445L, 
                        browserName = "chrome")  
  remDr$open(silent = T)
  
  
  # Basic1, Basic2, Detail
  
  for(link in 1:length(links)){
    
    remDr$navigate(links[[link]])
    
    # Year
    
    for(year in start:end){
      
      remDr$findElement(value = paste0(year_xpath, year - 1981, ']'))$clickElement()
      
      # Position
      
      for(position in 2:4){
        
        remDr$findElement(value = paste0(position_xpath, position, ']'))$clickElement()
        
        # Table List
        
        for(page in 1:50){
          
          try({
            
            remDr$findElement(value = paste0(page_xpath, page, '"]'))$clickElement()
            
            data_table <- remDr$findElement(value = data_xpath)$getElementText() %>%
              str_split('\n') %>%
              unlist() %>%
              str_split_fixed(' ', length(v_names[[link]])) %>%
              as.data.frame() %>%
              `colnames<-`(v_names[[link]]) %>%
              mutate(year = year,
                     position = position) %>% 
              mutate(position = ifelse(position == 2, 'catcher',
                                       ifelse(position == 3, 'infielder', 'outfielder')))
            
            data_fin[[link]] <- rbind(data_fin[[link]], data_table)
            
          })
        }
      }
    }
  }
  
  data_fin$basic2 <- data_fin$basic2 %>% select(-c(순위, 선수명, 팀명, year, position))
  data_fin$detail <- data_fin$detail %>% select(-c(순위, 선수명, 팀명, year, position))
  
  fin <- cbind(data_fin$basic1, 
               data_fin$basic2, 
               data_fin$detail)
  
  remDr$close()
  
  return(fin)
  
}
