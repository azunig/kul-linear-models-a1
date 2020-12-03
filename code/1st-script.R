#install.packages('gridExtra')
library(tidyverse)
library(gridExtra)
library(ggplot2)

data <- read.table('./src/work.sat(Project 11-12).txt', header = TRUE, sep = ';')
head(data)
str(data)

### Descriptive statistics
options(repr.plot.width = 20, repr.plot.height = 6)

# Distribution of response variable - work satisfaction
hist1 <- ggplot(data, aes(y)) + 
  geom_histogram(colour = 'green', fill = NA) + 
  ggtitle('Distribution of Response variable') + 
  xlab('Response variable - Work Satisfaction') + 
  theme_linedraw()

# Distribution of explanatory variable - salary
bar1 <- ggplot(data, aes(salary)) + 
  geom_bar(colour = 'blue', fill = NA) + 
  ggtitle('Distribution of Salary') + 
  xlab('Explanatory variable - Salary') + 
  theme_linedraw()

# Distribution of explanatory variable - work load
bar2 <- ggplot(data, aes(wl)) + 
  geom_bar(colour = 'red', fill = NA) + 
  ggtitle('Distribution of Work load') + 
  xlab('Explanatory variable - Work load') + 
  theme_linedraw()

grid.arrange(hist1, bar1, bar2, ncol = 3)

# Distribubtion of work satisfaction by groups - Same scale
ggplot(data, aes(y, salary)) + 
  geom_boxplot(colour = rep(c('red', 'blue'), 3), ) + 
  facet_wrap(~ wl, nrow = 3) + 
  xlab('Work Satisfaction (y)') + 
  ylab('Salary') + 
  ggtitle('Distribution of Work satisfaction') + 
  theme_linedraw()

# Distribubtion of work satisfaction by groups - Different scales
ggplot(data, aes(y, salary)) + 
  geom_boxplot(colour = rep(c('red', 'blue'), 3), ) + 
  facet_wrap(~ wl, nrow = 3, scale = 'free') + 
  xlab('Work Satisfaction (y)') + 
  ylab('Salary') + 
  ggtitle('Distribution of Work satisfaction') + 
  theme_linedraw()


# Mean of response variable by workload and salary
table_unbal_mean <- with(data, tapply(y, list(wl, salary), mean))
table_unbal_mean %>% round(2) %>% as.table()

# Frequency table of response variable by workload and salary
table_unbal_freq <- with(data, table(wl, salary))
table_unbal_freq


# Data in long format
data_longer <- table_unbal_mean %>% 
  as.data.frame()

data_longer$`wl` <- row.names(data_longer)

data_longer <- data_longer %>% 
  pivot_longer(cols = c(Low, High), names_to = 'salary', values_to = 'mean_y')


# Interaction plot
ggplot(data_longer, aes(salary, mean_y)) + 
  geom_line(aes(group = wl, color = wl)) + 
  xlab('Salary') + 
  ylab('Mean value of Work Satisfaction') + 
  ggtitle('Interaction Plot')

ggplot(data_longer, aes(wl, mean_y)) + 
  geom_line(aes(group = salary, color = salary)) + 
  xlab('Workload') + 
  ylab('Mean value of Work Satisfaction') + 
  ggtitle('Interaction Plot')




