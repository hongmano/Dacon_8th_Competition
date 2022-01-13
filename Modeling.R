
# 1. Packages / Options ---------------------------------------------------

if (!require(RSelenium)) install.packages('RSelenium'); require(RSelenium)
if (!require(stringr)) install.packages('stringr'); require(stringr)
if (!require(dplyr)) install.packages('dplyr'); require(dplyr)
if (!require(reshape2)) install.packages('reshape2'); require(reshape2)

if (!require(ggplot2)) install.packages('ggplot2'); require(ggplot2)
if (!require(ggpubr)) install.packages('ggpubr'); require(ggpubr)
if (!require(plotly)) install.packages('plotly'); require(plotly)
if (!require(ggcorrplot)) install.packages('ggcorrplot'); require(ggcorrplot)

if (!require(xgboost)) install.packages('xgboost'); require(xgboost)

# cd C:\selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.11.0.jar -port 4445

# 2. Data Crawling --------------------------------------------------------

data <- KBO_crawl(2010, 2020)
names(data) <- tolower(names(data))

# Duplicated Names?

duplicate <- data %>% group_by(year, player) %>% tally %>% filter(n > 1)

# 1년 전 성적과 1년 후 OPS 연결

data <- data %>% filter(!pk %in% paste(duplicate$year, duplicate$player, sep = '_'))

tst1 <- data %>% select(pk, ops)
tst2 <- data %>% mutate(pk = paste(year + 1, player, sep = '_')) %>% select(-c(ops, pk2))

train <- tst1 %>% inner_join(tst2) %>% filter(year != 2019)
test <- data %>% filter(year == 2019) %>% mutate(pk = pk2) %>% select(-pk2)

data$pk2 <- NULL

rm(tst1, tst2, data, duplicate)

# 3. Data Wrangling -------------------------------------------------------

# Character to Numeric

str(train)

train_chr <- train %>% select(pk, year, team, player, position)
train_num <- train %>% select(-c(pk, year, team, player, position))

train_chr <- apply(train_chr, 2, as.character) %>% as.data.frame()
train_num <- apply(train_num, 2, as.numeric) %>% as.data.frame()


test_chr <- test %>% select(pk, year, team, player, position)
test_num <- test %>% select(-c(pk, year, team, player, position))

test_chr <- apply(test_chr, 2, as.character) %>% as.data.frame()
test_num <- apply(test_num, 2, as.numeric) %>% as.data.frame()


train <- cbind(train_chr, train_num)
test <- cbind(test_chr, test_num)

str(train)

# Make X1B Variable

train <- train %>% 
  mutate(x1b = h - hr - x2b - x3b)

test <- test %>% 
  mutate(x1b = h - hr - x2b - x3b)

# Erase NAs

train <- na.omit(train)
test <- na.omit(test)

# 4. Domain Knowledge ---------------------------------------------------

# 4-1-1. Erase RBI / R (It is not about HITTING ABILITY)

train <- train %>% select(-c(rbi, r))
test <- test %>% select(-c(rbi, r))

# 4-1-2. Erase XBH (Linear Combination of X2B, X3B, HR)

train$xbh <- NULL
test$xbh <- NULL

# 4-1-3. Erase G, ab (Same Information with pa)

train %>%
  select(pa, g, ab) %>% 
  pairs()

train <- train %>% 
  mutate(pa2 = pa / g) %>% 
  select(-c(g, ab))

test <- test %>% 
  mutate(pa2 = pa / g) %>% 
  select(-c(g, ab))

# 4-2. Our Y Variable OPS 

train %>% 
  ggplot(aes(x = ops)) +
  geom_density() +
  theme_bw()

# 4-2-1. OPS ~ PA

train %>% 
  ggplot(aes(x = pa,
             y = ops)) +
  geom_point(size = 3,
             alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 600, 50)) +
  geom_smooth(formula = y ~ x,
              method = 'gam',
              size = 3) +
  geom_vline(aes(xintercept = 50),
             col = 'red') +
  theme_bw()

# 4-2-2. Over 50 PA

train %>% 
  filter(pa > 49) %>% 
  ggplot(aes(x = pa,
             y = ops)) +
  geom_point(size = 3,
             alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 600, 50)) +
  geom_smooth(formula = y ~ x,
              method = 'gam',
              size = 3) +
  geom_vline(aes(xintercept = 50),
             col = 'red') +
  theme_bw()

### IMPORTANT

train <- train %>% filter(pa > 49)

# 5. EDA ------------------------------------------------------------------

ggplotly(
  
  train %>% 
    group_by(year) %>% 
    summarise(pa = sum(pa)) %>% 
    ggplot(aes(x = year,
               y = pa,
               fill = pa)) +
    geom_bar(stat = 'identity') +
    scale_fill_gradient(low = 'black',
                        high = 'red') +
    labs(x = '연도',
         y = '타석 수',
         title = '연도별 타석 수') +
    theme_bw()
  
)

# 5-1. 안타 관련 변수 -----------------------------------------------------------------

# X3B????

train %>%
  select(x1b, x2b, x3b, hr, ops) %>% 
  pairs()

# Density of X3B

quantile(train$x3b, probs = seq(0, 1, 0.1))


# H by year

ggplotly(
  
  train %>% 
    select(year, pa, h, paste0('x', 1:3, 'b')) %>% 
    group_by(year) %>% 
    summarise_all(.funs = sum) %>% 
    melt(id.vars = c('year', 'pa', 'h')) %>% 
    ggplot(aes(x = year,
               y = value,
               col = variable)) +
    geom_point(aes(size = pa)) +
    geom_line(aes(group = variable)) +
    geom_line() +
    labs(title = '연도 별 안타 빈도',
         y = 'Freq') +
    theme_bw()
  
)

# Ratio??

ggplotly(
  
  train %>% 
    select(year, x1b, x2b, x3b, hr, h) %>% 
    group_by(year) %>% 
    summarise_all(.funs = sum) %>% 
    mutate(x1b_ratio = x1b / h * 100,
           x2b_ratio = x2b / h * 100,
           x3b_ratio = x3b / h * 100,
           hr_ratio = hr / h * 100) %>% 
    select(year, ends_with('ratio')) %>% 
    melt(id.vars = 'year') %>%
    ggplot(aes(x = value,
               y = year,
               fill = variable)) +
    geom_bar(stat = 'identity') +
    labs(y = "Percentage") +
    geom_text(aes(label = paste0(round(value, 0), '%'))) +
    theme_bw() 
)


# 5-2. 출루 관련 변수 -----------------------------------------------------------

train %>%
  select(bb, ibb, hbp, ops) %>% 
  pairs()

ggplotly(
  
  train %>% 
    select(year, pa, bb, ibb, hbp) %>% 
    group_by(year) %>% 
    summarise_all(.funs = sum) %>% 
    melt(id.vars = c('year', 'pa')) %>% 
    ggplot(aes(x = year,
               y = value,
               col = variable)) +
    geom_point(aes(size = pa)) +
    geom_line(aes(group = variable)) +
    geom_line() +
    labs(title = '연도 별 출루 빈도',
         y = 'Freq') +
    theme_bw()
  
)

ggplotly(
  
  train %>% 
    select(year, bb, ibb, hbp) %>% 
    group_by(year) %>% 
    summarise_all(.funs = sum) %>% 
    mutate(all = bb + ibb + hbp) %>% 
    mutate(bb_ratio = bb / all * 100,
           ibb_ratio = ibb / all * 100,
           hbp_ratio = hbp / all * 100) %>% 
    select(year, ends_with('ratio')) %>% 
    melt(id.vars = 'year') %>%
    ggplot(aes(x = value,
               y = year,
               fill = variable)) +
    geom_bar(stat = 'identity') +
    labs(y = "Percentage") +
    theme_bw() 
)


# 5-3. 아웃 관련 변수 -----------------------------------------------------------

train %>%
  select(so, go, ao, go.ao, gdp, sac, sf, ops) %>% 
  pairs()

train %>%
  select(so, go, ao, go.ao, gdp, sac, sf, ops) %>% 
  cor() %>% 
  ggcorrplot(lab = T,
             colors = c('blue', 'white', 'red'),
             hc.order = T)


# 5-4. Position -----------------------------------------------------------

my_comparisons <- list( c("Infielder", "Outfielder"), 
                        c("Infielder", "Catcher"), 
                        c("Outfielder", "Catcher"))

ggboxplot(data = train,
          x = 'position', 
          y = 'ops',
          color = 'position', 
          palette = "jco", 
          bxp.errorbar = TRUE) +
  
  stat_boxplot(geom = 'errorbar', 
               data = train, 
               aes(x = position, 
                   y = ops,
                   color = position)) +
  
  stat_compare_means(comparisons = my_comparisons) + 
  stat_compare_means(label.y = 1.7) +
  
  theme_bw()

table(train$position)

train <- train %>% mutate(position = ifelse(position != 'Catcher', 'Fielder', position))
test <- test %>% mutate(position = ifelse(position != 'Catcher', 'Fielder', position))

table(train$position)

# 5.5. Team ----------------------------------------------------------------

ggplot(train,
       aes(x = team,
           y = ops,
           col = team)) +
  geom_boxplot() +
  geom_jitter() +
  theme_bw()

# 6. Modeling -------------------------------------------------------------

train <- train %>% 
  select(-c(pk, team, player, year)) %>%
  na.omit()

test <- test %>% 
  select(-c(pk, team, player, year)) %>% 
  na.omit()

# 6-1. Regression ---------------------------------------------------------


### First Regression

model1 <- lm(train, formula =  ops ~ .,) %>% step()
summary(model1)

### WHY???

train %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  ggcorrplot(hc.order = T,
             lab = T)

### Mutate Ratio Variables

ratio_variables <- c('x1b x2b x3b hr tb sac sf bb ibb hbp so go ao') %>% 
  str_split(' ') %>% 
  unlist

train <- train %>% 
  mutate_at(ratio_variables, function(x) x / train$pa) %>% 
  na.omit()

test <- test %>% 
  mutate_at(ratio_variables, function(x) x / test$pa) %>% 
  na.omit()

### Second Regression

model2 <- lm(train, formula =  ops ~ .,) %>% step()
summary(model2)

# 6-2. XGB ----------------------------------------------------------------

train <- train %>% mutate(position = ifelse(position == 'Catcher', 0, 1))
test <- test %>% mutate(position = ifelse(position == 'Catcher', 0, 1))

dtrain <- xgb.DMatrix(as.matrix(train %>% select(-ops)),
                      label = train$ops)
dtest <- xgb.DMatrix(as.matrix(test %>% select(-ops)))


# Random Search

best_param <- list()
best_seednumber <- 1234
best_rmse <- Inf
best_rmse_index <- 0

for (iter in 1:100) {
  
  param <- list(obj = 'reg:linear',
                eval_metric = "rmse",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .1), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  
  cv.nround <-  2000
  cv.nfold <-  5 
  seed.number  <-  sample.int(10000, 1) 
  
  
  set.seed(seed.number)
  
  
  mdcv <- xgb.cv(data = dtrain,
                 params = param,
                 nfold = cv.nfold, 
                 nrounds = cv.nround,
                 verbose = T,
                 early_stopping_rounds = 100,
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
model3 <- xgb.train(data = dtrain,
                    params = best_param,
                    nround = best_rmse_index,
                    verbose = T)

xgb.ggplot.importance(xgb.importance(model = model3))

# 6-3. Torch --------------------------------------------------------------

# 7. Compare --------------------------------------------------------------

pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, dtest)

error1 <- test$ops - pred1
error2 <- test$ops - pred2
error3 <- test$ops - pred3

mean(error1^2 * test$pa / sum(test$pa), na.rm = T) %>% sqrt
mean(error2^2 * test$pa / sum(test$pa), na.rm = T) %>% sqrt
mean(error3^2 * test$pa / sum(test$pa), na.rm = T) %>% sqrt

