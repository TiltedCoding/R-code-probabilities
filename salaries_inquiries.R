# Exercise 1.
#install.packages("TeachingDemos")
library(TeachingDemos)
#install.packages("readxl")

salaries <- read.delim('salaries.txt')
head(salaries)
summary(salaries)

# Make lists from columns.
men_list <- c(salaries$MALES)
women_list <- c(salaries$FEMALES)

# Men
N_men <- length(men_list)
mean_men <- mean(men_list)
sd_men <- sd(men_list)

#Women

mean_women <- mean(women_list)
N_women <- length(women_list)
sd_women <- sd(women_list)

#(a)
#95% Confidence interval for males.

# The variance of the population is unknown, therefore the t-student wil be used to find the confidence interval.
Tlowerlimit_men <- mean_men - qt(0.975, N_men - 1)*sd_men/sqrt(N_men)
Tupperlimit_men <- mean_men + qt(0.975, N_men - 1)*sd_men/sqrt(N_men)
Tlowerlimit_men
Tupperlimit_men 

#(b)
# 95% Confidence interval for females.

Tlowerlimit_women <- mean_women - qt(0.975, N_women - 1)*sd_women/sqrt(N_women)
Tupperlimit_women <- mean_women + qt(0.975, N_women - 1)*sd_women/sqrt(N_women)
Tlowerlimit_women
Tupperlimit_women

#(c)
#90% Confidence interval for mean difference of the average salaries between males and femals.

list_diff= men_list - women_list
mean_d = mean(list_diff)
sd_d = sd(list_diff)
N_d = length(list_diff)

Tlowerlimit_meandiff <- mean_d - qt(0.95, N_d - 1)*sd_d/sqrt(N_d)
Tupperlimit_meandiff <- mean_d + qt(0.95, N_d - 1)*sd_d/sqrt(N_d)
Tlowerlimit_meandiff
Tupperlimit_meandiff

#(d)

t.test(men_list, women_list, paired=TRUE, conf.level = 0.95, var.equal=TRUE, alternative='greater')

# p-value > ??, so the Hypothesis 0 is not rejected, meaning H0 is true.

#(e)

var.test(men_list, women_list, conf.level=0.99)

# p-value is again greater than ??, so the Hypothesis 0, which states that the variances are equal between males and females, is again not rejected

#(f)

t.test(men_list, women_list, paired=TRUE, conf.level = 0.90, var.equal=TRUE)

# ?? is equal to 0.1 and p-value is 0.2127, meaning that the statistical analysis shows that gender does not make a difference on the salary


# Exercise 2.

library(readxl)

# Import the xls file.
inquiries <- read_excel('inquiries.xls')

# Add a column of the total inquiries of all the sections of the newspaper.
inquiries$Total_inquiries <- inquiries$News + inquiries$Business + inquiries$Sports
inquiries

# (a)

library(ggplot2)
ggplot(data = inquiries,
       aes(x=Day, y=rowSums(inquiries[,c('News', 'Business', 'Sports')]))) +
        xlab('Day') +
        ylab('Number of Inquiries') +
        labs(title = 'Inquiries Per Day') +
        geom_boxplot()

ggplot(data = inquiries,
       aes(x=Day, y=Total_inquiries)) +
  xlab('Day') +
  ylab('Number of Inquiries') +
  labs(title = 'Inquiries Per Day') +
  geom_boxplot()


ggplot(data = inquiries,
       aes(x=Day, y=News)) +
  xlab('Day') +
  ylab('News') +
  labs(title = 'News inquiries Per Day') +
  geom_boxplot()

ggplot(data = inquiries,
       aes(x=Day, y=Business)) +
  xlab('Day') +
  ylab('Business') +
  labs(title = 'Business inquiries Per Day') +
  geom_boxplot()

ggplot(data = inquiries,
       aes(x=Day, y=Sports)) +
  xlab('Day') +
  ylab('Sports') +
  labs(title = 'Sports inquiries Per Day') +
  geom_boxplot()

#attach(inquiries)

# (b)

anova_news <- aov(News~factor(Day), data = inquiries)
anova_business <- aov(Business~factor(Day), data = inquiries)
anova_sports <- aov(Sports~factor(Day), data = inquiries)
anova_inquiries <- aov(Total_inquiries~factor(Day), data = inquiries)


anova_news
anova_business
anova_sports

anova_inquiries



summary(anova_news)
summary(anova_business)
summary(anova_sports)

summary(anova_inquiries)


# The outputs show that for a significance level of ??=0.05 (or 95% confidence) all sections are statistically significant and the null hypothesis is rejected for every section (news, sports, business).
# This means that not all means are equal. The business section has a lower significance code, meaning that for a confidence of 99% it is not statistically significant and the null hypothesis can not be rejected.
# The degrees of freedom of days or DFG is 5-1=4 while the DFE is N-k or 20-5=15. Finally, The DF total is the sum of DFE and DGE and is equal to 19.
# Reviewing the F-value presented in the outputs and comparing it to the Critical F in the appropriate table, One can see that for both 0.99 and 0.95, the news and sports section's Null hypotheses are rejected.
# On the other hand, for the business section, for 0.95 the H0 can be rejected but for 0.99 we fail to reject the null hypothesis.
# The above conclusions can also be derived by reviewing the p-value given for each section.


# As far as the review of the total inquiries goes, The original hypothesis that the means are equal (or that it is not statistically significant) is rejected.

# (c)


TukeyHSD(anova_news)
TukeyHSD(anova_business)
TukeyHSD(anova_sports)

TukeyHSD(anova_inquiries)

# The H0 -Null Hypothesis- is that the difference is not significant.
# Reviewing the Tukey Honestly Significant Differences, it is clear that for all sections, the expected difference between Tuesdays and Wednesdays is NOT significant.
# This can be derived by the fact that between the lower and upper bound of the interval that the TukeyHSD gives for the pair Wednesday-Tuesday zero is contained.
# Seeing the p adjusted one can also come to the conclusion that the expected difference is not significant.
# The difference between the means of Wednesday and Tuesday is 0.25 for Sports, -1.00 for News and -0.25 for Business.

# For the total inquiries, the review is the same. The difference is not significant since there is a zero within the given interval.



# (d)

anova_newsd <- aov(Total_inquiries~News, data = inquiries)
anova_businessd <- aov(Total_inquiries~Business, data = inquiries)
anova_sportsd <- aov(Total_inquiries~Sports, data = inquiries)


summary(anova_newsd)
summary(anova_businessd)
summary(anova_sportsd)


# Reviewing the p-values of each section, it is shown that for the News and Sports sections, the null hypothesis that they are not significant is rejected.
# On the other hand, the original hypothesis for the Business section is failed to be rejected, meaning that it is not significant.


# (e)


# Add a column of only the statistically significant sections.
inquiries$Significant_inquiries <- inquiries$News + inquiries$Sports
inquiries

anova_newse <- aov(Significant_inquiries~News, data = inquiries)
anova_sportse <- aov(Significant_inquiries~Sports, data = inquiries)

summary(anova_newse)
summary(anova_sportse)

# Again, the null hypothesis that these sections are not significant are rejected based on the p-value of the One-Way ANOVA.


# (f)


inquiries
Days <- factor(rep(rep(1:5, each=4), 2))
Days

Section <- factor(rep(1:2, each=20))
Section

number_of_inquiries <- c(inquiries$News, inquiries$Sports)

new_inquiries <- cbind('Day'=Days, 'Section'=Section, 'Number_of_Inquiries'=number_of_inquiries)
new_inquiries

two_way_fit <- aov(number_of_inquiries~Days*Section)

summary(two_way_fit)

# The two way fit summary, shows the significance for each of the Day and Section.
# Concerning the Days, the null hypothesis is that it is statistically insignificant, meaning the mean of inquiries for each day is equal. This is safely rejected as shown by its p-value.
# As far as sections go, again taking into account that the null hypothesis is that each section is statistically insignificant for the inquiries, this hypothesis is again rejected.
# For Days AND sections, the null hypothesis is rejected safely with confidence up to 99%.


# (g)

library(MASS)


model <- lm(Significant_inquiries~., data = inquiries)
stepAIC(model, direction = 'forward')

Mondays <- c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
Tuesdays <- c(0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)
Wednesdays <- c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0)
Thursdays <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0)
Fridays <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1)
Dummy <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

inquiries$Monday <- Mondays
inquiries$Tuesdays <- Tuesdays
inquiries$Wednesdays <- Wednesdays
inquiries$Thursdays <- Thursdays
inquiries$Fridays <- Fridays
inquiries$Dummy <- Dummy

inquiries_new <- inquiries[,c(2,4,6,7,8,9,10,11,12)]

model <- lm(Significant_inquiries~., data = inquiries)
stepAIC(model, direction = 'both')



