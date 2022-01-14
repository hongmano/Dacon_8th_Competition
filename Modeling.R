
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

data <- KBO_crawl(2010, 2020) %>% 
  mutate(pk = paste(year, player, sep = '_'))
names(data) <- tolower(names(data))

write.csv(data, 'crawl.csv', row.names = F)

# Duplicated Names?

duplicate <- data %>% group_by(year, player) %>% tally %>% filter(n > 1)

# 1년 전 성적과 1년 후 OPS 연결

data <- data %>% filter(!pk %in% paste(duplicate$year, duplicate$player, sep = '_'))

data <- data %>% 
  select(- ops) %>% 
  inner_join(data %>% 
               select(year, player, ops) %>% 
               mutate(pk = paste(year + 1, player ,sep = '_')) %>% 
               select(-c(year, player))) %>% 
  select(-`순위`) %>% 
  rename(x2b = `2b`,
         x3b = `3b`)

# 3. Data Wrangling -------------------------------------------------------

# Character to Numeric

str(data)

data_chr <- data %>% select(pk, year, team, player, position)
data_num <- data %>% select(-c(pk, year, team, player, position))

data_num <- apply(data_num, 2, as.numeric) %>% as.data.frame()

data <- cbind(data_chr, data_num)

str(data)

# Make X1B Variable

data <- data %>% 
  mutate(x1b = h - hr - x2b - x3b)

# Erase NAs

data <- na.omit(data)

# 4. Domain Knowledge ---------------------------------------------------

# 4-1-1. Erase RBI / R (It is not about HITTING ABILITY)

data <- data %>% select(-c(rbi, r))
data$rbi <- NULL
data$r <- NULL

# 4-1-2. Erase XBH (Linear Combination of X2B, X3B, HR)

data$xbh <- NULL

# 4-1-3. Erase G, ab (Same Information with pa)

data %>%
  select(pa, g, ab) %>% 
  pairs()

data <- data %>% 
  mutate(pa2 = pa / g) %>% 
  select(-c(g, ab))

# 4-2. Our Y Variable OPS 

data %>% 
  ggplot(aes(x = ops)) +
  geom_density() +
  theme_bw()

# 4-2-1. OPS ~ PA

data %>% 
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

data %>% 
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

data_50 <- data %>% filter(pa > 49)

# 5-1. 안타 관련 변수 -----------------------------------------------------------------

ggplotly(
  
  data %>% 
    group_by(year) %>% 
    summarise(pa = sum(pa)) %>% 
    ggplot(aes(x = factor(year),
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

# X3B????

data_50 %>%
  select(x1b, x2b, x3b, hr, ops) %>% 
  pairs()


# Density of X3B

quantile(data_50$x3b, probs = seq(0, 1, 0.1))


# H by year

ggplotly(
  
  data_50 %>% 
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
