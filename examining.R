# Examining Data:

load("/Users/matthewsimonson2/Desktop/Data_Science_Exercises/zmPDSwR-master/Custdata/exampleData.rData")
summary(custdata)

# Typical problems revealed by data summaries

# MISSING VALUES

summary(custdata)

# state.of.res     custid        sex     is.employed         income                   marital.stat health.ins
# California  :114   Min.   :   2068   F:440   Mode :logical   Min.   : -8700   Divorced/Separated:155   Mode :logical
# New York    : 94   1st Qu.: 345667   M:560   FALSE:73        1st Qu.: 14600   Married           :516   FALSE:159
# Pennsylvania: 63   Median : 693403           TRUE :599       Median : 35000   Never Married     :233   TRUE :841
# Ohio        : 59   Mean   : 698500           NA's :328       Mean   : 53505   Widowed           : 96   NA's :0
# Illinois    : 52   3rd Qu.:1044606                           3rd Qu.: 67000
# Texas       : 51   Max.   :1414286                           Max.   :615000
# (Other)     :567

# housing.type recent.move      num.vehicles        age        is.employed.fix1   age.normalized
# Homeowner free and clear    :157   Mode :logical   Min.   :0.000   Min.   :  0.0   Length:1000        Min.   :-2.74074
# Homeowner with mortgage/loan:412   FALSE:820       1st Qu.:1.000   1st Qu.: 38.0   Class :character   1st Qu.:-0.72626
# Occupied with no rent       : 11   TRUE :124       Median :2.000   Median : 50.0   Mode  :character   Median :-0.09011
# Rented                      :364   NA's :56        Mean   :1.916   Mean   : 51.7                      Mean   : 0.00000
#  NA's                        : 56                   3rd Qu.:2.000   3rd Qu.: 64.0                      3rd Qu.: 0.65207
# Max.   :6.000   Max.   :146.7                      Max.   : 5.03516
# NA's   :56

#  Median.Income    income.norm            gp            income.lt.30K      age.range       Income
#  Min.   :37427   Min.   :-0.1956   Min.   :0.0002281   Mode :logical   [0,25]  : 56   Min.   :     0
#  1st Qu.:44819   1st Qu.: 0.2812   1st Qu.:0.2618117   FALSE:562       (25,65] :732   1st Qu.: 25000
#  Median :50118   Median : 0.6712   Median :0.5127602   TRUE :438       (65,Inf]:212   Median : 45000
#  Mean   :50919   Mean   : 1.0781   Mean   :0.5016471   NA's :0                        Mean   : 66199
# 3rd Qu.:55534   3rd Qu.: 1.3508   3rd Qu.:0.7405944                                  3rd Qu.: 82000
# Max.   :68187   Max.   :11.7870   Max.   :0.9988350                                  Max.   :615000
# NA's   :328

summary(hhdata)

# household_id  cust_id      income        gp
# hh1:2        cust1:5   0      :1   Min.   :0.2382
# hh2:1        cust2:4   100000 :1   1st Qu.:0.5289
# hh3:3        cust3:3   110000 :1   Median :0.7027
# hh4:3                  35020  :1   Mean   :0.5906
# hh5:3                  36000  :1   3rd Qu.:0.7108
# 47950  :1   Max.   :0.8803
# (Other):6

summary(medianincome)

# State    Median.Income
# : 1   Min.   :37427
# Alabama   : 1   1st Qu.:47483
# Alaska    : 1   Median :52274
# Arizona   : 1   Mean   :52655
# Arkansas  : 1   3rd Qu.:57195
# California: 1   Max.   :68187
# (Other)   :46

# You will likely encounter missing values when model scoring, so you should deal with them during model training
# dropping subjects with missing values may be ok if there are not very many, other methods should be used if removing missing values significantly reduces the sample size

# INVALID VALUES AND OUTLIERS
# Even when a column or variable isn’t missing any values,
# you still want to check that the values that you do have make sense.

 summary(custdata$income)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -8700   14600   35000   53500   67000  615000

 summary(custdata$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0    38.0    50.0    51.7    64.0   146.7

#DATA RANGE

# make sure there is enough variation in the age and income of your cus- tomers for you to see the relationships
summary(custdata$income)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
-8700   14600   35000   53500   67000  615000

sd(custdata$income)
# 65478.07

mean(custdata$income)
# 53504.77

sd(custdata$income)/mean(custdata$income)
# 1.22378
# rough rule of thumb is the ratio of the standard deviation to the mean. If that ratio is very small, then the data isn’t varying much.

# UNITS

summary(Income)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -8.7    14.6    35.0    53.5    67.0   615.0

# the units for income are annual salary / 1000, this must be known or problems will arise
# catch by checking data definitions in data dictionaries or documentation


#####################################################
# Spotting problems using graphics and visualization:
#####################################################

# Visually checking distributions for a single variable

library(ggplot2)

# histogram of age
ggplot(custdata) +
  geom_histogram(aes(x=age), binwidth=5, fill="gray")

# DENSITY PLOTS:
# A point on a density plot corresponds to the % of the data with a particular value, effectively a continuous histogram

library(scales)

ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_continuous(labels=dollar)

# for highly skewed data that is non-negative, one way to bring out more detail is to plot the distribution on a logarithmic scale

ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) +
  annotation_logticks(sides="bt")

# A log scale should be used when what constitutes a “significant difference” depends on the order of magnitude of the variable

# BAR CHARTS
# A bar chart is a histogram for discrete data

ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")

# Bar charts are most useful when the number of categories is large
# Sorted bar chart:

statesums <- table(custdata$state.of.res)
statef <- as.data.frame(statesums)
colnames(statef)<-c("state.of.res", "count")

summary(statef)

statef <- transform(statef,state.of.res=reorder(state.of.res, count)) # re-order rows by the values in the columns
summary(statef)

ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
  stat="identity",
  fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))

########################################################
# Visually checking relationships between two variables:￼￼
########################################################￼
￼
# LINE PLOTS

x <- runif(100)
y <- x^2 + 0.2*x
ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_line()

# SCATTER PLOTS AND SMOOTHING CURVES

custdata2 <- subset(custdata,
                    (custdata$age > 0
                     & custdata$age < 100
                     & custdata$income > 0)) # subset to reasonable range of values for both variables

cor(custdata2$age, custdata2$income)

# SCATTER PLOT
ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + ylim(0, 200000)

# add a linear fit to make relationship clearer

ggplot(custdata2, aes(x=age, y=income)) + geom_point() +
  stat_smooth(method="lm") +
  ylim(0, 200000)

# add a loess curve to the plot to examine local effects more clearly

ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + geom_smooth() +
  ylim(0, 200000)

# local smoother curves can work well to make changes in probability between categories more clear:

ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) +
  geom_point(position=position_jitter(w=0.05, h=0.05)) +
  geom_smooth()

# Using hexbinning works well when there is high data volume

library(hexbin)
ggplot(custdata2, aes(x=age, y=income)) +
  geom_hex(binwidth=c(5, 10000)) +
  geom_smooth(color="white", se=FALSE) +
  ylim(0,200000)

# visualize the relationship between two categorical variables

# bar charts should have parameters set so the most relevant relationships are the most obvious
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins))

ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="dodge")

ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins),
                            position="fill")


# Plotting with a rug shows data density, in this case, In this case the bars convey the ratio, while the rug contains the volume

ggplot(custdata, aes(x=marital.stat)) +
  geom_bar(aes(fill=health.ins), position="fill") +
  geom_point(aes(y=-0.05), size=0.75, alpha=0.3,
             position=position_jitter(h=0.01))

# Faceted plots are useful when the figure is split between groups, or more than one aspect is best conveyed through multiple plots

# Side by side bar chart:
ggplot(custdata2) +
  geom_bar(aes(x=housing.type, fill=marital.stat ), position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# faceted bar chart:
ggplot(custdata2) +
  geom_bar(aes(x=marital.stat), position="dodge", fill="darkgray") +
  facet_wrap(~housing.type, scales="free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

