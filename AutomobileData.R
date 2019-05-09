library(ggplot2)
auto <- read.csv("auto.csv")

auto %>% 
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


#Miles Per Gallon
qplot(auto$mpg, xlab = 'Miles Per Gallon', ylab = 'Count', binwidth = 2, 
      main='Frequency Histogram: Miles per Gallon')

table(auto$cylinders)
auto <- auto[!auto$cylinders %in% c(3, 5),]
qplot(auto$cylinders, ylab = 'Count', xlab = 'Cylinders')

table(auto$origin)

cor(auto[ , c('weight', 'displacement', 'horsepower', 'acceleration')], 
    use='complete')

ggplot(data = auto, aes(x = weight, y = mpg)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab('MPG') +
  ylab('Weight') +
  ggtitle('MPG vs. Weight')

fit = lm(mpg ~ weight, data=auto)
summary(fit)

ggplot(data = auto, aes(x = acceleration, y = mpg)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab('MPG') +
  ylab('Acceleration') +
  ggtitle('MPG vs. Acceleration')
fit = lm(mpg ~ acceleration, data=auto)
summary(fit)

ggplot(data = auto, aes(x = as.factor(auto$year), y = auto$mpg)) +
  geom_boxplot() +
  xlab('Origin Year') +
  ylab('MPG') +
  ggtitle('MPG Comparison by origin Year')

ggplot(data = auto, aes(x = as.factor(origin), y = mpg)) +
  geom_boxplot() +
  xlab('Country of Origin') +
  ylab('MPG') +
  ggtitle('MPG Comparison by Country of Origin')

ggplot(data = auto, aes(x = as.factor(origin), y = weight)) +
  geom_boxplot() +
  xlab('Country of Origin') +
  ylab('Weight') +
  ggtitle('Weight Comparison by Country of Origin') 

ggplot(data = auto, aes(x = as.factor(cylinders), y = mpg)) +
  geom_boxplot() +
  xlab('Number of Cylinders') +
  ylab('MPG') +
  ggtitle('MPG Comparison by Number of Cylinders') 

ggplot(data = auto, aes(x = as.factor(cylinders), y = weight)) +
  geom_boxplot() +
  xlab('Number of Cylinders') +
  ylab('Weight') +
  ggtitle('Weight Comparison by Number of Cylinders') 

ggplot(data = auto, aes(x = weight, y = mpg)) +
  geom_point() +
  xlab('Weight') +
  ylab('MPG') +
  ggtitle('MPG vs. Weight')

library(ggplot2)
#Multivariate Plots
ggplot(data = auto, aes(x = as.factor(year), fill = as.factor(cylinders))) +
  geom_bar() +
  facet_wrap(~ origin, ncol = 1) +
  xlab('Model Year') +
  ylab('Count') +
  ggtitle('Each Country count of automobiles Over Time')
  
  
ggplot(data = auto, aes(x = as.factor(year), y = weight)) +
  geom_boxplot() +
  facet_wrap(~ origin, ncol = 1) +
  xlab('Model Year') +
  ylab('Weight') +
  ggtitle('Weight Distributions Over Time by Country of Origin')

ggplot(data = auto, aes(x = as.factor(year), y = mpg)) +
  geom_boxplot() +
  facet_wrap(~ origin) +
  xlab('Model Year') +
  ylab('MPG') +
  ggtitle('MPG Over Time by Country of Origin')

model <- lm(mpg ~., data=auto) 
names(summary(model))
summary(model)$coef[,3:4]

summary(model)$fstatistic
head(auto)
names(auto)[4] <- "hsp"
