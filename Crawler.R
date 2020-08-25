
# 1. Prepare --------------------------------------------------------------

# cmd 창에서
# cd C:\crawling
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445


# 2. Options / Packages ---------------------------------------------------

if (!require(RSelenium)) install.packages('RSelenium')
if (!require(stringr)) install.packages('stringr')
if (!require(dplyr)) install.packages('dplyr')

# 3. Crawling -------------------------------------------------------------

KBO_crawl <- function(min_year, max_year){
  
  low <- min_year - 1981
  max <- max_year - 1981
  
  ### 3-1. 변수명 저장
  
  # driver 실행
  
  remDr <- remoteDriver(remoteServerAddr = "localhost" ,port = 4445L, browserName = "chrome")  
  remDr$open(silent = T)
  
  # KBO 홈페이지 접속
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx')
  
  # 기본기록 page1 변수명
  
  page1_colnames <- remDr$findElement(using = "xpath",
                                      value = '//*[@id="cphContents_cphContents_cphContents_udpContent"]/div[3]/table/thead')$getElementText() %>% str_split(' ') %>% unlist()
  page1_colnames <- c(page1_colnames, 'year')
  
  # 기본기록 page2 변수명
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Basic2.aspx')
  page2_colnames <- remDr$findElement(using = "xpath",
                                      value = '//*[@id="cphContents_cphContents_cphContents_udpContent"]/div[3]/table/thead')$getElementText() %>% str_split(' ') %>% unlist()
  page2_colnames <- c(page2_colnames, 'year')
  
  # 세부기록 변수명
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Detail1.aspx')
  detail_colnames <- remDr$findElement(using = "xpath",
                                       value = '//*[@id="cphContents_cphContents_cphContents_udpContent"]/div[3]/table/thead')$getElementText() %>% str_split(' ') %>% unlist()
  detail_colnames[9] <- 'GW/RBI'
  detail_colnames <- detail_colnames[-10]
  detail_colnames <- c(detail_colnames, 'year')
  
  # 3-2-1. 기본정보 1
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx')
  page1 <- c()
  
  for(year in low:max){
    
    # 년도(29:39는 2010년 부터 2019년의 위치)
    
    remDr$findElement(using = "xpath",
                      value = paste0("//*[@id='cphContents_cphContents_cphContents_ddlSeason_ddlSeason']/option[",year,"]"))$clickElement()
    Sys.sleep(1)
    
    # 팀(최대 10팀이 존재했기에 2:11의 범위 설정)
    
    for (team in 2:11){
      try(remDr$findElement(using = 'xpath',
                            value = paste0("//*[@id='cphContents_cphContents_cphContents_ddlTeam_ddlTeam']/option[",team,"]"))$clickElement(), break)
      Sys.sleep(1)
      
      # 페이지 (팀 당 30명 이상의 선수가 있는 경우 2개의 페이지 활용함)
      page <- 2
      for (page in 1:2){
        try(remDr$findElement(using = 'xpath',
                              value = paste0("//*[@id='cphContents_cphContents_cphContents_ucPager_btnNo",page,"']"))$clickElement(), break)
        Sys.sleep(1)
        
        # 선수 (페이지 당 최대 30명의 데이터가 존재)
        
        for (player in 1:30){
          try(player_data <- remDr$findElement(using = "xpath", 
                                               value = paste0("//*[@id='cphContents_cphContents_cphContents_udpContent']/div[3]/table/tbody/tr[",player,"]"))$getElementText() %>% 
                str_split(' '), break)
          player_data[[1]] <- c(player_data[[1]], year)
          page1 <- c(page1, player_data)
        }
      }
    }
  }
  
  page1 <- as.data.frame(matrix(unlist(page1), 
                                ncol = length(page1_colnames),
                                byrow = T)) %>% `colnames<-`(page1_colnames) %>% 
    select(-c('순위'))
  
  # 3-2-2. 기본정보 2
  
  remDr$navigate('https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx')
  page2 <- c()
  
  for(year in low:max){
    remDr$findElement(using = "xpath",
                      value = paste0("//*[@id='cphContents_cphContents_cphContents_ddlSeason_ddlSeason']/option[",year,"]"))$clickElement()
    Sys.sleep(1)
    
    for (team in 2:11){
      remDr$findElement(using = 'xpath',
                        value = '//*[@id="cphContents_cphContents_cphContents_udpContent"]/div[2]/div[2]/a[2]')$clickElement()
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
          page2 <- c(page2, player_data)
        }
      }
    }
  }
  
  page2 <- as.data.frame(matrix(unlist(page2), 
                                ncol = length(page2_colnames),
                                byrow = T)) %>% `colnames<-`(page2_colnames) %>% 
    select(-c('순위'))
  
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
  
  fin <- list(page1 = page1,
              page2 = page2,
              detail = detail)
  
  return(fin)
}

# 4. Data Formatting ------------------------------------------------------

kbo2002_2019 <- KBO_crawl(2002, 2019)
data_fin <- kbo2002_2019$page1 %>% 
  inner_join(kbo2002_2019$page2) %>% 
  inner_join(kbo2002_2019$detail) %>% 
  unique
data_fin$year <- as.numeric(data_fin$year) + 1981
write.csv(data_fin, 'kbo_crawl.csv', row.names = F)

# 선수별 PK가 없기 때문에 동명이인은 제거해야 함
# 제거기준은 같은 시즌, 같은 팀의 동명이인은 모두 제거
# ex) LG의 작은 이병규, 큰 이병규

duplicate <- data_fin %>% 
  group_by(year, `팀명`, `선수명`) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  select(-n) %>% 
  inner_join(data_fin)

data_fin <- setdiff(data_fin, duplicate)
rm(duplicate)
