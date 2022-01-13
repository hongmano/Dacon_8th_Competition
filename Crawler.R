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


KBO_crawl <- function(start, end){
  
  links <- list(
    
    basic1 = 'https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx',
    basic2 = 'https://www.koreabaseball.com/Record/Player/HitterBasic/Basic2.aspx',
    detail = 'https://www.koreabaseball.com/Record/Player/HitterBasic/Detail1.aspx'
    
  )
  
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
    Sys.sleep(1)
    
    # Year
    
    for(year in start:end){

      remDr$findElement(value = paste0(year_xpath, year - 1981, ']'))$clickElement()
      Sys.sleep(1)
      
      # Position
      
      for(position in 2:4){
        
        remDr$findElement(value = paste0(position_xpath, position, ']'))$clickElement()
        Sys.sleep(1)
        
        # Table List
        
        for(page in 1:50){
          
          try({
            
            
            remDr$findElement(value = paste0(page_xpath, page, '"]'))$clickElement()
            Sys.sleep(3)
            
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
            
            Sys.sleep(1)
            
            data_fin[[link]] <- rbind(data_fin[[link]], data_table)
            
          })
        }
      }
    }
  }
  
  fin <- data_fin$basic1 %>% 
    inner_join(data_fin$basic2) %>% 
    inner_join(data_fin$detail) %>% 
    unique %>% 
    rename(player = `선수명`,
           team = `팀명`)
  
  remDr$close()
  
  return(fin)
  
}

