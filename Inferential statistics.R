# ============================
# 1️⃣ Read Data
# ============================
df = read_xlsx("C:/Users/mohad/OneDrive/Desktop/inferential_statistics_data.xlsx")
# Reads the Excel data set into R

# ============================
# 2️⃣ Basic Descriptive Statistics
# ============================
library(dplyr)
library(summarytools)

glimpse(df)    # Quick overview of the data set structure
descr(df)      # Detailed descriptive stats: mean, median, SD, min, max, etc.

# ============================
# 3️⃣ 95% Confidence Interval for Mean Income
# ============================
t.test(df$Income, conf.level = 0.95)
# Provides the 95% CI for mean Income

# ============================
# 4️⃣ One-sample t-tests
# ============================
t.test(df$Income, mu = 230 )
# H0: mean income = 230
# H1: mean income != 230 (two-sided test)

t.test(df$Income, mu = 230, alternative = "greater")
# H0: mean income <= 230
# H1: mean income > 230 (one-sided)

t.test(df$Income, mu = 230, alternative = "less")
# H0: mean income >= 230
# H1: mean income < 230 (one-sided)

# ============================
# 5️⃣ Independent t-test (Male vs Female Income)
# ============================
t.test(Income ~ Gender, data = df, var.equal = TRUE)
# H0: mean income of males = mean income of females
# H1: mean income differs
# Assumes equal variance between groups

# ============================
# 6️⃣ Chi-square Test (Categorical Association)
# ============================
tb <- table(df$Education_Level, df$Employment_Status)
chisq.test(tb)
# H0: Education Level and Employment Status are independent
# H1: They are associated
# Non-parametric test for categorical data

# ============================
# 7️⃣ Non-parametric Tests
# ============================

# Test for normality of Income
shapiro.test(df$Income)
# H0: Income is normally distributed
# H1: Income is NOT normally distributed
# If p < 0.05 → reject H0 → data is non-normal → use non-parametric tests

# Mann–Whitney / Wilcoxon rank-sum test for two independent groups
wilcox.test(Income ~ Gender, data = df)
# Non-parametric equivalent of independent t-test
# H0: Distributions of Income for Male and Female are the same

# Kruskal–Wallis test for more than two groups
kruskal.test(df$Income ~ df$Education_Level)
# Non-parametric equivalent of one-way ANOVA
# H0: All Education_Level groups have same distribution

# ============================
# 8️⃣ ANOVA (Parametric)
# ============================
anova_model = aov(Income ~ Education_Level, data = df)
summary(anova_model)
# H0: Mean Income is same across all Education_Level groups
# H1: At least one group mean differs
# Use only if normality & equal variance assumptions hold

# ============================
# 9️⃣ Simple Linear Regression
# ============================
simple_reg = lm(df$Income ~ df$Gender)
summary(simple_reg)
# Predict Income using Gender
# Checks relationship, coefficients, p-values, R-squared

# ============================
# 10️⃣ Multiple Linear Regression
# ============================
mult_reg = lm(df$Income ~ df$Gender + df$Employment_Status + df$Education_Years)
summary(mult_reg)
# Predict Income using multiple variables
# Checks significance of each predictor, overall model fit (R-squared), F-statistic
