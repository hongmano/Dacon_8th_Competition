  # 3-2-3. 세부정보
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx')
  detail <- c()
  
  for(year in low:max){
    
    remDr$findElement(using = "xpath",
                      value = paste0("//*[@id='cphContents_cphContents_cphContents_ddlSeason_ddlSeason']/option[",year,"]"))$clickElement()
    Sys.sleep(1)
    for (team in 2:11){
      
      try(remDr$findElement(using = 'xpath',
                        value = '//*[@id="cphContents_cphContents_cphContents_udpContent"]/div[2]/div[1]/ul/li[2]')$clickElement())
      try(remDr$findElement(using = 'xpath',
                            value = paste0("//*[@id='cphContents_cphContents_cphContents_ddlTeam_ddlTeam']/option[",team,"]"))$clickElement(), break)      
      
      Sys.sleep(1)
      
      for (page in 1:2){
        try(remDr$findElement(using = 'xpath',
                              value = paste0("//*[@id='cphContents_cphContents_cphContents_ucPager_btnNo",page,"']"))$clickElement(), break)
        Sys.sleep(1)
        
        for (player in 1:30){
          try(player_data <- remDr$findElement(using = "xpath", 
                                               value = paste0("//*[@id='cphContents_cphContents_cphContents_udpContent']/div[3]/table/tbody/tr[",player,"]"))$getElementText() %>% 
                str_split(' '), break)
          player_data[[1]] <- c(player_data[[1]], year)
          detail <- c(detail, player_data)
        }
      }
    }
  }
  
  detail <- as.data.frame(matrix(unlist(detail), 
                                 ncol = length(detail_colnames),
                                 byrow = T)) %>% `colnames<-`(detail_colnames) %>% 
    select(-c('순위'))
