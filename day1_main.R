
# 1. Packages / Options ---------------------------------------------------

if (!require(RSelenium)) install.packages('RSelenium'); require(RSelenium)
if (!require(stringr)) install.packages('stringr'); require(stringr)
if (!require(dplyr)) install.packages('dplyr'); require(dplyr)
if (!require(reshape2)) install.packages('reshape2'); require(reshape2)

if (!require(ggplot2)) install.packages('ggplot2'); require(ggplot2)
if (!require(plotly)) install.packages('plotly'); require(plotly)
if (!require(ggcorrplot)) install.packages('ggcorrplot'); require(ggcorrplot)

if (!require(xgboost)) install.packages('xgboost'); require(xgboost)


# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.11.0.jar -port 4445

# 2. Data Crawling --------------------------------------------------------

links <- list(
    
    basic1 = 'https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx',
    basic2 = 'https://www.koreabaseball.com/Record/Player/HitterBasic/Basic2.aspx',
    detail = 'https://www.koreabaseball.com/Record/Player/HitterBasic/Detail1.aspx'
    
)

data <- KBO_crawl(2010, 2020, links)
write.csv(data, 'C:\\Users\\Mano\\Desktop\\Soomgo\\DataAnalysis\\1005LMH\\data.csv', row.names = F)
data <- read.csv('C:\\Users\\Mano\\Desktop\\Soomgo\\DataAnalysis\\1005LMH\\data.csv')

# 3. Data Wrangling -------------------------------------------------------

# Change Korean Variable Names to English

names(data)[1:3] <- c('index', 'player', 'team')
names(data)[10:11] <- c('X2B', 'X3B')

# Make Primary Key to erase duplicate names

data <- data %>% 
    mutate(PK = paste(year, team, player, sep = '_'))

# Duplicated Names?

data$PK %>% table %>% View

# I think It is not distinguishable So,,,, Erase it!

data %>% filter(PK %in% c(paste(2010:2016, 'LG', '이병규', sep = '_'))) %>% View
data <- data %>% 
    filter(!PK %in% c(paste(2010:2016, 'LG', '이병규', sep = '_')))

# Character to Numeric

str(data)
data_chr <- data %>% select(PK, year, team, player, position)
data_num <- data %>% select(-c(PK, year, team, player, position))

data_chr <- apply(data_chr, 2, as.character) %>% as.data.frame()
data_num <- apply(data_num, 2, as.numeric) %>% as.data.frame()

data <- cbind(data_chr, data_num)
str(data)

# Make X1B Variable

data <- data %>% mutate(X1B = H - HR - X2B - X3B)

# Erase Index

data$index <- NULL

# Erase NAs

data <- na.omit(data)

# 4. Domain Knowledge ---------------------------------------------------

# 4-1-1. Erase RBI / R (It is not about HITTING ABILITY)

data$RBI <- NULL
data <- data %>% select(-R)

# 4-1-2. Erase XBH (Linear Combination of X2B, X3B, HR)

data$XBH <- NULL

# 4-1-3. Erase G, PA (Same Information with AB)

data %>%
    select(PA, G, AB) %>% 
    pairs()

data$G <- NULL
data$AB <- NULL


# 4-2. Our Y Variable OPS 

data %>% 
    ggplot(aes(x = OPS)) +
    geom_density() +
    theme_bw()

# 4-2-1. OPS ~ PA

data %>% 
    ggplot(aes(x = PA,
               y = OPS)) +
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
    filter(PA > 49) %>% 
    ggplot(aes(x = PA,
               y = OPS)) +
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

data <- data %>% filter(PA > 49)
