
# 1. Get Variable Names From HTML Tables ----------------------------------

get_variables <- function(){
  
  variable_selector <- c('#cphContents_cphContents_cphContents_udpContent > div.record_result > table > thead > tr')
  
  # Detail1
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Detail1.aspx')
  
  detail1 <- remDr$findElement(using = 'css selector',
                               value = variable_selector)$getElementText() %>% 
    str_split(' ') %>% 
    unlist()
  
  detail1[1:3] <- c('INDEX', 'PLAYER', 'TEAM')
  detail1[9] <- 'GW/RBI'
  detail1 <- detail1[-10]
  
  # Basic1
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx')
  
  basic1 <- remDr$findElement(using = 'css selector',
                              value = variable_selector)$getElementText() %>% 
    str_split(' ') %>% 
    unlist()
  
  basic1[1:3] <- c('INDEX', 'PLAYER', 'TEAM')
  
  
  # Basic2
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Basic2.aspx')
  
  basic2 <- remDr$findElement(using = 'css selector',
                              value = variable_selector)$getElementText() %>% 
    str_split(' ') %>% 
    unlist()
  
  basic2[1:3] <- c('INDEX', 'PLAYER', 'TEAM')
  
  fin <- list(detail = c(detail1, 'POSITION', 'YEAR'),
              basic1 = c(basic1, 'POSITION', 'YEAR'),
              basic2 = c(basic2, 'POSITION', 'YEAR'))
  
  return(fin)
  
}

# 2. Crawl Data from HTML Tables ------------------------------------------

crawl_data <- function(min_year, max_year){
  
  # -1981 on HTML
  
  low <- min_year - 1981
  max <- max_year - 1981
  
  # Plate
  
  dat <- list()
  
  # Click Year Option
  
  for(year in low:max){
    
    remDr$findElement(using = "xpath",
                      value = paste0("//*[@id='cphContents_cphContents_cphContents_ddlSeason_ddlSeason']/option[", year, "]"))$clickElement()
    Sys.sleep(1)
    
    # Click Team Option (Maximum 10 Teams)
    
    for(team in 2:11){
      
      try(remDr$findElement(using = 'xpath',
                            value = paste0("//*[@id='cphContents_cphContents_cphContents_ddlTeam_ddlTeam']/option[", team, "]"))$clickElement(), break)
      Sys.sleep(1)
      
      # Click Position Option (Maximum 3 Positions)
      
      for(position in 2:4){
        
        try(remDr$findElement(using = 'xpath',
                              value = paste0("//*[@id='cphContents_cphContents_cphContents_ddlPos_ddlPos']/option[", position, "]"))$clickElement(), break)
        Sys.sleep(1)
        
        # Click Player Option (Maximum 30 Players)
        
        for(player in 1:30){
          
          try(player_data <- remDr$findElement(using = "xpath", 
                                               value = paste0("//*[@id='cphContents_cphContents_cphContents_udpContent']/div[3]/table/tbody/tr[", player, "]"))$getElementText(), break)
          
          position_value <- ifelse(position == 2, 'Catcher',
                                   ifelse(position == 3, 'Infielder', 'Outfielder'))
          
          player_data[[1]] <- paste(player_data[[1]], position_value, year + 1981) %>% 
            str_split_fixed(' ', 18) %>% 
            as.data.frame()
          
          dat <- append(dat, player_data)
          
        }
      }
    }
  }
  
  return(dat)
  
}


# 3. Let's Crawl ----------------------------------------------------------

get_data <- function(min_year, max_year){
  
  variable_names <- get_variables()
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx')
  Sys.sleep(1)
  
  basic1 <- crawl_data(min_year, max_year) %>% 
    bind_rows() %>% 
    `colnames<-`(variable_names$basic1)
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Basic2.aspx')
  Sys.sleep(1)
  
  basic2 <- crawl_data(min_year, max_year) %>% 
    bind_rows() 
  basic2 <- basic2[, 1:17] %>% 
    `colnames<-`(variable_names$basic2)
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Detail1.aspx')
  Sys.sleep(1)
  
  detail <- crawl_data(min_year, max_year) %>% 
    bind_rows()
  detail <- detail[, 1:16] %>% 
    `colnames<-`(variable_names$detail)
  
  data_fin <- inner_join(basic1, basic2) %>% 
    inner_join(detail)
  
  return(data_fin)
  
}
