
# 1. Prepare --------------------------------------------------------------

# cmd 창에서
# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.11.0.jar -port 4445

# 2. Options / Packages ---------------------------------------------------

options(scipen = 100)

if (!require(RSelenium)) install.packages('RSelenium'); require(RSelenium) # Selenium in R

if (!require(xgboost)) install.packages('xgboost'); require(xgboost) # XGBOOST

if (!require(dplyr)) install.packages('dplyr'); require(dplyr) # Wrangling
if (!require(stringr)) install.packages('stringr'); require(stringr) # Wrangling
if (!require(stringr)) install.packages('reshape2'); require(reshape2) # Wrangling
if (!require(scales)) install.packages('scales'); require(scales) # Wrangling

if (!require(ggplot2)) install.packages('ggplot2'); require(ggplot2) # Visualize
if (!require(ggpubr)) install.packages('ggpubr'); require(ggpubr) # Visualize
if (!require(plotly)) install.packages('plotly'); require(plotly) # Visualize
if (!require(ggcorrplot)) install.packages('ggcorrplot'); require(ggcorrplot) # Visualize

source('utils.R')

# 3. Crawling -------------------------------------------------------------

# Execute Chrome Driver

remDr <- remoteDriver(remoteServerAddr = "localhost" ,
                      port = 4445L, 
                      browserName = "chrome")  
remDr$open(silent = T)

# Get Data

data <- get_data(2010, 2020)

setwd('C:\\Users\\Mano\\Desktop\\프로젝트\\2019. 04[Dacon 6th Competition - KBO 타자들의 OPS 예측]\\데이터')
write.csv(data, 'kbo_crawl.csv', row.names = F)

# 4. Data Wrangling --------------------------------------------------------

data2 <- read.csv('kbo_crawl.csv')

# 4-1. To Numeric

str(data2)
data2 <- sapply(data2, function(x) str_replace_all(x, '-', '')) %>% as.data.frame()

data2_chr <- data2 %>% select(PLAYER, TEAM, POSITION, YEAR)
data2_num <- data2 %>% select(-c(PLAYER, TEAM, POSITION, YEAR))
data2_num <- sapply(data2_num, as.numeric) %>% as.data.frame()

data2 <- cbind(data2_chr, data2_num)


data2$INDEX <- NULL
data2[is.na(data2)] <- 0


str(data2)

# 4-2. Primary Key

# 동명이인 선수 존재

same_name <- data2 %>% group_by(YEAR, TEAM, PLAYER) %>% tally %>% filter(n != 1)

# 확인 결과 2010 ~ 2016 시즌 LG에 이병규 선수가 2명

same_name <- data2 %>% 
  filter(YEAR %in% 2010:2016 & TEAM == 'LG' & PLAYER == '이병규')

# 사전 지식 없이 구분 불가 -> 삭제

data2 <- setdiff(data2, same_name)

# 동명이인 제거했기에, YEAR + TEAM + 선수이름을 Primary Key로 사용가능

data2 <- data2 %>% mutate(PK = paste(YEAR, TEAM, PLAYER, sep = '_'))
data2$PK

# 4-3. 1루타 변수 생성
# 1루타 = 안타 - 2루타 - 3루타 - 홈런

data2 <- data2 %>% mutate(X1B = H - X2B - X3B - HR)

# 4-4. Train / Test Split
# 2020년의 성적을 예측하는 것이 목표이므로 2020년 데이터는 Test Data로
# *** 난이도 조절 위해 100타수 이상 선수만 Filtering

test_x <- data2 %>% filter(YEAR == 2019 & AB >= 100)
test_y <- data2 %>% 
  filter(YEAR == 2020) %>% 
  mutate(PK = paste0(as.numeric(substr(PK, 1, 4)) - 1, substr(PK, 5, 20))) %>% 
  filter(PK %in% test_x$PK) %>% 
  select(PK, OPS, AB)
  
train <- data2 %>% filter(YEAR != 2020 & YEAR != 2019)

### Test Data는 무슨 일이 있어도 열어보지 않음이 중요!!(Data Leakage)

# 5. EDA ------------------------------------------------------

### DOMAIN

train$RBI <- NULL
train$XBH <- NULL
train$R <- NULL
train$PA <- NULL
train$G <- NULL

### GENERAL
# 5-1. 성적 수렴 AB

ggplotly(train %>% 
           ggplot() +
           geom_point(aes(x = AB,
                          y = OPS,
                          col = ifelse(AB >= 50, 'Team', '4실'))) +
           geom_vline(xintercept = 50,
                      col= 'red',
                      linetype = 'dotted') +
           theme_bw() +
           theme(legend.position = 'none') +
           geom_smooth(aes(x = AB,
                           y = OPS),
                       method = 'auto'))

train <- train %>% filter(AB >= 50)

# 5-2. 연도 별 AB

ggplotly(train %>% 
           group_by(YEAR) %>% 
           summarise(AB = sum(AB)) %>% 
           ggplot(aes(x = YEAR,
                      y = AB,
                      fill = AB)) +
           geom_bar(stat = 'identity') +
           scale_fill_gradient(low = 'black',
                               high = 'red') +
           labs(x = '연도',
                y = '타수',
                title = '연도별 타수') +
           theme_bw())



# etc. 스포츠 데이터 특성

ggcorrplot(train %>% select_if(is.numeric) %>% cor,
           lab = T,
           hc.order = T)

train %>% select(OPS, PH.BA, GO.AO) %>% pairs()

train$PH.BA <- NULL
train$GO.AO <- NULL

### 타격 변수
# 5-3. 3루타가 관계 없음?

ggcorrplot(train %>%
             select(X1B, X2B, X3B, HR, OPS) %>% 
             cor,
           lab = T)

plot(density(train$X3B))

# 5-4. 타고투저

mano <- train %>% 
  select(YEAR, AB, H, X1B, X2B, X3B, HR) %>% 
  group_by(YEAR) %>% 
  summarise_all(.funs = sum)

ggplotly(melt(mano, id.vars = c('YEAR', 'AB', 'H')) %>%
           ggplot(aes(x = YEAR,
                      y = value,
                      col = variable)) +
           geom_line(aes(group = variable)) + 
           geom_point(aes(size = AB)) +
           labs(title = '연도 별 안타 빈도',
                y = 'Freq') +
           theme_bw())

# 5-5. 타격 유형

mano <- mano %>% 
  mutate(X1B_ratio = X1B / H * 100,
         X2B_ratio = X2B / H * 100,
         HR_ratio = HR / H * 100) %>% 
  select(YEAR, ends_with('ratio')) 
melt(mano, id.vars = 'YEAR') %>%
  ggplot() +
  geom_bar(aes(y = value, 
               x = YEAR, 
               fill = variable), 
           stat = "identity") +
  geom_text(aes(x = YEAR, 
                y = round(value) - 4, 
                label = paste0(round(value), "%")),
            colour = "black", 
            size = 3) +
  scale_y_continuous(labels = dollar_format(suffix = "%", 
                                            prefix = "")) +
  labs(y = "Percentage") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())


### 출루 변수
# 5-6 출루 유형

mano <- train %>% 
  select(YEAR, AB, BB, IBB, HBP) %>% 
  group_by(YEAR) %>% 
  summarise_all(.funs = sum)
ggplotly(melt(mano, id.vars = c('YEAR', 'AB')) %>%
           ggplot(aes(x = YEAR,
                      y = value,
                      col = variable)) +
           geom_point(aes(size = AB)) +
           geom_line(aes(group = variable)) +
           labs(title = '연도 별 4구/고의4구/사구 빈도',
                y = 'Freq') +
           theme_bw())

# 5-7 출루 유형

mano <- train %>% 
  select(YEAR, BB, IBB, HBP) %>% 
  group_by(YEAR) %>% 
  summarise_all(.funs = sum) %>% 
  mutate(BB_ratio = BB / (BB + IBB + HBP) * 100,
         IBB_ratio = IBB / (BB + IBB + HBP) * 100,
         HBP_ratio = HBP / (BB + IBB + HBP) * 100) %>% 
  select(YEAR, ends_with('_ratio'))
melt(mano, id.vars = 'YEAR') %>%
  ggplot() +
  geom_bar(aes(y = value, 
               x = YEAR, 
               fill = variable), 
           stat = "identity") +
  geom_text(aes(x = YEAR, 
                y = round(value), 
                label = paste0(round(value), "%")),
            colour = "black", 
            size = 3) +
  scale_y_continuous(labels = dollar_format(suffix = "%", 
                                            prefix = "")) +
  labs(y = "Percentage") +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank())

### POSITION

my_comparisons <- list( c("Catcher", "Infielder"), 
                        c("Catcher", "Outfielder"), 
                        c("Infielder", "Outfielder"))

ggboxplot(data = train, 
          x = 'POSITION', 
          y = 'OPS',
          color = 'POSITION', 
          palette = "jco", 
          bxp.errorbar = TRUE) +
  stat_boxplot(geom = 'errorbar', 
               data = train, 
               aes(x = POSITION, 
                   y = OPS,
                   color = POSITION)) +
  stat_compare_means(comparisons = my_comparisons) + 
  stat_compare_means(label.y = 1.5) +
  labs(x = '타격 유형') +
  theme_bw()

# 6. Feature Engineering --------------------------------------------------

# 비율

train <- train %>% 
  mutate(X1B = X1B / H,
         X2B = X2B / H,
         X3B = X3B / H,
         HR = HR / H)

test_x <- test_x %>% 
  mutate(X1B = X1B / H,
         X2B = X2B / H,
         X3B = X3B / H,
         HR = HR / H)

# 주성분분석

pca <- train %>% select(SLG, GPA, AVG, OBP)
pca_matrix <- prcomp(pca, scale. = T)

summary(pca_matrix)

pca_matrix$rotation <- pca_matrix$rotation[, 1:2]
pca <- pca_matrix$x %*% pca_matrix$rotation %>% 
  as.data.frame() %>% 
  `colnames<-`(c('PC1', 'PC2'))

train <- train %>% 
  mutate(PC1 = pca$PC1,
         PC2 = pca$PC2)

ggcorrplot(cor(train %>% select_if(is.numeric)),
           lab = T)

### TEST

pca <- test_x %>% select(SLG, GPA, AVG, OBP)
pca_matrix <- prcomp(pca, scale. = T)

summary(pca_matrix)

pca_matrix$rotation <- pca_matrix$rotation[, 1:2]
pca <- pca_matrix$x %*% pca_matrix$rotation %>% 
  as.data.frame() %>% 
  `colnames<-`(c('PC1', 'PC2'))

test_x <- test_x %>% 
  mutate(PC1 = pca$PC1,
         PC2 = pca$PC2)

# 7. Modeling -----------------------------------------------------------

train_x <- train %>% select(-OPS)
train_y <- train %>% select(PK, OPS)
train_y <- train_y %>% mutate(PK = paste0(as.numeric(substr(PK, 1, 4)) - 1, substr(PK, 5, 100)))

train_fin <- inner_join(train_x, train_y)


vars <- c('AB', 'H', 'X1B', 'X2B', 'X3B', 'HR', 'TB', 'SAC', 'SF', 'BB', 'IBB', 'HBP', 'SO', 'GDP', 'MH', 'RISP', 'GO', 'AO',
          'GW.RBI', 'BB.K', 'P.PA', 'ISOP', 'XR', 'PC1', 'PC2')
dtrain <- xgb.DMatrix(data = data.matrix(train_fin[, vars]),
                      label = train_fin$OPS)

# Random search for parameters

best_param <- list()
best_seednumber <- 1234
best_rmse <- Inf
best_rmse_index <- 0

for(iter in 1:10){
  
  param <- list(objective = "reg:squarederror",
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
  mdcv <- xgboost::xgb.cv(data = dtrain, 
                          params = param,  
                          nfold = cv.nfold, 
                          nrounds = cv.nround,
                          verbose = F, 
                          early_stopping_rounds = 30, 
                          maximize = FALSE)
  
  min_rmse_index  <-  mdcv$best_iteration
  min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_rmse_mean
  
  if (min_rmse < best_rmse) {
    best_rmse <- min_rmse
    best_rmse_index <- min_rmse_index
    best_seednumber <- seed.number
    best_param <- param
  }
  
  print(paste0(iter, 'th Trial has done....'))
}

nround <- best_rmse_index
set.seed(best_seednumber)

xgboost_model <- xgboost(data = dtrain,
                         params = best_param,
                         nround = nround, 
                         verbose = T,
                         print_every_n = 1)

# 8. Submitting -----------------------------------------------------------

dtest <- xgb.DMatrix(data = data.matrix(test_x[, vars]),
                      label = test_x$OPS)

test_x$pred <- predict(xgboost_model, dtest)

result <- inner_join(test_y, test_x %>% select(PK, pred)) %>% 
  mutate(error = OPS - pred)

### MSE

mean(result$error ** 2)

