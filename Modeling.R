
# 1. Options / Package ----------------------------------------------------

options(scipen = 100)
if (!require(dplyr)) install.packages('dplyr')
if (!require(lubridate)) install.packages('lubridate')
if (!require(tm)) install.packages('tm')
if (!require(xgboost)) install.packages('xgboost')

# 2. Data Loading ---------------------------------------------------------

regular_season <- read.csv('Regular_Season_Batter.csv',
                           fileEncoding = 'UTF-8',
                           stringsAsFactor = F,
                           na.strings = '-')

day_by_day <- read.csv('Regular_Season_Batter_Day_By_Day_b4.csv',
                       fileEncoding = 'UTF-8',
                       stringsAsFactor = F,
                       na.strings = '-')

submission <- read.csv('submission.csv',
                       fileEncoding = 'UTF-8')

data_fin <- read.csv('kbo_crawl.csv',
                     stringsAsFactors = F)

# 3. Data Wrangling -------------------------------------------------------

# 3-1. regular_season

# height.weight 변수를  height(키), weight(몸무게)로 나눠주기

regular_season$height <- substr(regular_season$height.weight, 1, 5) %>% 
  str_remove('cm') %>%
  as.numeric()

regular_season$weight <- substr(regular_season$height.weight, 7, 20) %>% 
  str_remove('kg') %>%
  as.numeric()

regular_season$height.weight <- NULL

# year_born 변수를 ymd 형식으로 바꿔준 후 age(해당 시즌 당시의 나이) 변수 만들기

regular_season$born <- regular_season$year_born %>% 
  str_remove('년 ') %>% 
  str_remove('월 ') %>% 
  str_remove('일') %>% 
  ymd()

regular_season$age <- regular_season$year - year(regular_season$born) + 1

regular_season$year_born <- NULL

# position 변수를 position(포지션), hand(?타) 변수로 나눠주기

regular_season$hand <- ifelse(substr(regular_season$position, 1, 2) == '포수',
                              substr(regular_season$position, 6, 7),
                              substr(regular_season$position, 7, 8))

regular_season$position <- ifelse(substr(regular_season$position, 1, 1) == '포',
                                  '포수',
                                  substr(regular_season$position, 1, 3))

# starting_salary를 10,000원 단위로 바꿔주기

regular_season$starting_salary <- regular_season$starting_salary %>%
  str_remove('만원') %>% 
  str_remove('0달러') %>% 
  as.numeric()

# career 변수 삭제

regular_season$career <- NULL

# 1루타 변수 만들기

regular_season <- regular_season %>% 
  mutate(X1B = H - X2B - X3B - HR)


# year 변수를 string으로 변환

regular_season$year <- as.character(regular_season$year)

# 3-2. regular_season_day_by_day

# 경기 날짜 ymd 형식으로 만들기

day_by_day$date <- paste0(day_by_day$year, day_by_day$date) %>% 
  removePunctuation() %>% 
  ymd()

# 1루타 변수 만들기

day_by_day <- day_by_day %>% 
  mutate(X1B = H - X2B - X3B - HR)

# 누적 변수 만들기(avg2 처럼 AB, H, HR, etc.)

day_by_day <- day_by_day %>% arrange(batter_id, date)
test <- day_by_day %>% arrange(batter_id, date)

test <- test %>%
  group_by(batter_id, year) %>% 
  select_if(is.integer) %>% 
  mutate_all(cumsum) %>% 
  rename_at(.funs = function(x) paste0(x, '2'),
            .vars = c('AB', 'R', 'H', 'X1B', 'X2B', 'X3B', 'HR', 'RBI', 'SB', 
                      'CS', 'BB', 'HBP', 'SO', 'GDP'))

day_by_day <- bind_cols(day_by_day, test)  
rm(test)

# 3-3. data_fin

# 팀명, 선수명을 제외한 변수 numeric으로 변환

data_fin_chr <- data_fin %>% 
  select(`팀명`, `선수명`) %>% 
  `colnames<-`(c('team', 'batter_name'))

data_fin_num <- data_fin %>% select(-c(`팀명`, `선수명`)) %>% 
  apply(2, function(x){ifelse(x == '-', 0, x) %>% as.numeric(x)}) %>% 
  as.data.frame()

# 1루타 변수 생성

data_fin <- cbind(data_fin_chr, data_fin_num) %>% 
  mutate(X1B = H - X2B - X3B - HR)
rm(data_fin_chr, data_fin_num)

# 선수별 PK가 없기 때문에 동명이인은 제거해야 함
# 제거기준은 같은 시즌, 같은 팀의 동명이인은 모두 제거
# ex) LG의 작은 이병규, 큰 이병규

duplicate <- data_fin %>% 
  group_by(year, team, batter_name) %>% 
  tally() %>% 
  filter(n > 1) %>% 
  select(-n) %>% 
  inner_join(data_fin)

data_fin <- setdiff(data_fin, duplicate)

# 데이콘 데이터 상의 batter_id 매치

data_fin <- data_fin %>% 
  left_join(regular_season %>% select(batter_name, batter_id) %>% unique)

# AB >= 100인 데이터 걸러내기

data_fin_jujeon <- data_fin %>% filter(AB >= 100)
data_fin_hubo <- data_fin %>% filter(AB < 100)

# 4. Modeling -------------------------------------------------------------


# 4-1. Model 1 (AB >= 100) ------------------------------------------------

# 1년 전의 성적과 1년 후의 OPS 연결시키기
# ex) 2018년의 타격 성적 + 2019년의 OPS

data_modeling_jujeon <- data_fin_jujeon %>%
  select(-OPS) %>% 
  inner_join(data_fin %>% 
               select(batter_name, year, OPS) %>% 
               mutate(year = year-1))

# Mdel1(AB >= 100) Hold-Out

# Test (submission 데이터)

test_jujeon <- data_modeling_jujeon %>% 
  filter(batter_id %in% submission$batter_id & year == 2018) %>% 
  select(-c(team, batter_name, year))

dtest_jujeon <- xgb.DMatrix(as.matrix(test_jujeon %>% select(-c(OPS, batter_id))),
                            label = test_jujeon$OPS)

# Validation (Submission 타자들의 이전 데이터)

validation_jujeon <- data_modeling_jujeon %>%
  filter(batter_id %in% submission$batter_id & year != 2018) %>% 
  select(-c(team, batter_name, year))

dvalidation_jujeon <- xgb.DMatrix(as.matrix(validation_jujeon %>% select(-c(OPS, batter_id))),
                                  label = validation_jujeon$OPS)

# Train

train_jujeon <- data_modeling_jujeon %>% 
  filter(!batter_id %in% submission$batter_id) %>% 
  select(-c(team, batter_name, year))

dtrain_jujeon <- xgb.DMatrix(as.matrix(train_jujeon %>% select(-c(OPS, batter_id))),
                             label = train_jujeon$OPS)

# Random Search

best_param <- list()
best_seednumber <- 1234
best_rmse <- Inf
best_rmse_index <- 0

for (iter in 1:2) {
  param <- list(obj = 'reg:linear',
                eval_metric = "rmse",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround <-  2000
  cv.nfold <-  5 
  seed.number  <-  sample.int(10000, 1) 
  set.seed(seed.number)
  mdcv <- xgb.cv(data = dtrain_jujeon,
                 params = param,
                 nfold = cv.nfold, 
                 nrounds = cv.nround,
                 verbose = T,
                 early_stopping_rounds = 50,
                 print_every_n = 100,
                 maximize = FALSE)
  
  min_rmse_index  <-  mdcv$best_iteration
  min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_rmse_mean
  
  if (min_rmse < best_rmse) {
    best_rmse <- min_rmse
    best_rmse_index <- min_rmse_index
    best_seednumber <- seed.number
    best_param <- param
  }
}

# Fit

set.seed(best_seednumber)
model_jujeon <- xgb.train(data = dtrain_jujeon,
                        params = best_param,
                        nround = best_rmse_index,
                        verbose = T,
                        watchlist = list(train = dtrain_jujeon,
                                         validation = dvalidation_jujeon))

xgb.plot.importance(xgb.importance(model = model_jujeon))
mean((predict(model_jujeon, dtest_jujeon) - test_jujeon$OPS) ** 2)

# 4-2. Model 2 (AB < 100) -------------------------------------------------

# 1년 전의 성적과 1년 후의 OPS 연결시키기
# ex) 2018년의 타격 성적 + 2019년의 OPS

data_modeling_hubo <- data_fin_hubo %>%
  select(-OPS) %>% 
  inner_join(data_fin %>% 
               select(batter_name, year, OPS) %>% 
               mutate(year = year-1))

# Mdel1(AB >= 100) Hold-Out

# Test (submission 데이터)

test_hubo <- data_modeling_hubo %>% 
  filter(batter_id %in% submission$batter_id & year == 2018) %>% 
  select(-c(team, batter_name, year))

dtest_hubo <- xgb.DMatrix(as.matrix(test_hubo %>% select(-c(OPS, batter_id))),
                            label = test_hubo$OPS)

# Validation (Submission 타자들의 이전 데이터)

validation_hubo <- data_modeling_hubo %>%
  filter(batter_id %in% submission$batter_id & year != 2018) %>% 
  select(-c(team, batter_name, year))

dvalidation_hubo <- xgb.DMatrix(as.matrix(validation_hubo %>% select(-c(OPS, batter_id))),
                                  label = validation_hubo$OPS)

# Train

train_hubo <- data_modeling_hubo %>% 
  filter(!batter_id %in% submission$batter_id) %>% 
  select(-c(team, batter_name, year))

dtrain_hubo <- xgb.DMatrix(as.matrix(train_hubo %>% select(-c(OPS, batter_id))),
                             label = train_hubo$OPS)

# Random Search

best_param <- list()
best_seednumber <- 1234
best_rmse <- Inf
best_rmse_index <- 0

for (iter in 1:2) {
  param <- list(obj = 'reg:linear',
                eval_metric = "rmse",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround <-  2000
  cv.nfold <-  5 
  seed.number  <-  sample.int(10000, 1) 
  set.seed(seed.number)
  mdcv <- xgb.cv(data = dtrain_hubo,
                 params = param,
                 nfold = cv.nfold, 
                 nrounds = cv.nround,
                 verbose = T,
                 early_stopping_rounds = 50,
                 print_every_n = 100,
                 maximize = FALSE)
  
  min_rmse_index  <-  mdcv$best_iteration
  min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_rmse_mean
  
  if (min_rmse < best_rmse) {
    best_rmse <- min_rmse
    best_rmse_index <- min_rmse_index
    best_seednumber <- seed.number
    best_param <- param
  }
}

# Fit

set.seed(best_seednumber)
model_hubo <- xgb.train(data = dtrain_hubo,
                          params = best_param,
                          nround = best_rmse_index,
                          verbose = T,
                          watchlist = list(train = dtrain_hubo,
                                           validation = dvalidation_hubo))

xgb.plot.importance(xgb.importance(model = model_hubo))
mean((predict(model_hubo, dtest_hubo) - test_hubo$OPS) ** 2)

# 5. Submission -----------------------------------------------------------

test_jujeon$OPS_pred <- predict(model_jujeon, dtest_jujeon)
test_hubo$OPS_pred <- predict(model_hubo, dtest_hubo)

test_fin <- rbind(test_jujeon, test_hubo) %>% 
  select(batter_id, OPS, OPS_pred)

mean((test_fin$OPS - test_fin$OPS_pred) ** 2)

submission <- submission %>% inner_join(test_fin %>% select(-OPS))
write.csv(submission, 'submission.csv', row.names = F)
