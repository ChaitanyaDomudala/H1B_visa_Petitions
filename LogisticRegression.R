library(dplyr)
library("tidyr")
library(readr)
library(ggplot2)
library(plotrix)
library("stringr")
library("stringdist")
library(sm)
library(tools)
library(treemap)
library(leaflet)
library(knitr)
library(kableExtra)
library(formattable)
library(RgoogleMaps)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(CatEncoders)
library(dummies)
library(mltools)
library(randomForest)
library("sqldf")
library(caret)
library(car)
library(ROCR)



visa = read.csv("C:/Rutgers/Courses/Fall_2020/Prob&Stat_Inference/Project/archive/h1b_kaggle.csv")
head(visa,10) %>%
kable( "html", escape=F, align="c") %>%
kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
summary(visa)

# Data Cleansing
dataset = visa[!names(visa)=="X"]
dataset = separate(dataset, WORKSITE, c("CITY", "STATE"), sep = ', ', remove = TRUE)
dataset = dataset[!is.na(dataset$YEAR),] # only 13 is missing
dataset = dataset[!is.na(dataset$FULL_TIME_POSITION),] # only 2 is missing
dataset$CASE_STATUS = as.factor(dataset$CASE_STATUS)
levels(dataset$CASE_STATUS)[5] = "PENDING REVIEW" # long factor level
dataset$CITY = toupper(dataset$CITY) %>% as.factor()
dataset$YEAR = as.factor(dataset$YEAR)
dataset$SOC_NAME = tolower(dataset$SOC_NAME) %>% str_trim() %>% as.factor()
dataset$JOB_TITLE = tolower(dataset$JOB_TITLE) %>% str_trim() %>% as.factor()
dataset$EMPLOYER_NAME = tolower(dataset$EMPLOYER_NAME) %>% str_trim() %>% as.factor()
dataset$PREVAILING_WAGE = as.numeric(dataset$PREVAILING_WAGE)


jobs_df = count(dataset, JOB_TITLE, sort = TRUE)
wordcloud(words = jobs_df$JOB_TITLE, 
          freq = jobs_df$n, 
          min.freq = 70,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.25, 
          colors=brewer.pal(8, "Dark2")) 



data_job_pattern <- "^DATA SCIENTIST*"
datasc_jobs <- subset(visa, grepl(data_job_pattern, toupper(visa$JOB_TITLE)) == T)


head(datasc_jobs,10) %>%
  kable( "html", escape=F, align="c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center")


ggplot(data = datasc_jobs %>% group_by(CASE_STATUS) %>% summarise(PERCENT = n()*100/nrow(datasc_jobs)),
       aes(x = reorder(CASE_STATUS, PERCENT), y = PERCENT, fill = CASE_STATUS)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette="Set2") +
  geom_text(aes(label = paste0(round(PERCENT,1),"%")), hjust = 1.2) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10)) +
  coord_flip() +
  labs(y = "Petitions made(in percentage)", x = "CASE_STATUS",
       title = "PETITION STATUS of DATA SCIENTIST JOBS")


ggplot(data = subset(datasc_jobs, datasc_jobs$PREVAILING_WAGE < 
                       quantile(datasc_jobs$PREVAILING_WAGE,0.999)),
       aes(PREVAILING_WAGE/1000)) + 
  geom_histogram(color = "darkblue", fill = 'lightblue', binwidth = 2.5) + 
  scale_x_continuous(breaks = seq(0,150,10)) +
  labs(x = "Salary (in thousand USD)", y = "Number of  jobs",
       title = "Data Scientist Salary Distribution")


summary(datasc_jobs$PREVAILING_WAGE)



ds_wage = datasc_jobs %>% group_by(YEAR) %>% 
  summarise(median_salary = median(PREVAILING_WAGE), count = n())
ggplot(data = ds_wage, aes(x = as.numeric(as.character(YEAR)), y = median_salary)) +
  geom_line(size=2) +
  geom_point() +
  labs(x = "YEAR", y = "Median Salary(in USD)", title = "Data Scientist salary trend")



ggplot(data = ds_wage, aes(x = as.numeric(as.character(YEAR)), y = count)) +
  geom_line() +
  geom_point() 

top_employer_count <- datasc_jobs %>% group_by(EMPLOYER_NAME) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(15, wt = count)


ggplot(data = top_employer_count, aes(x = reorder(EMPLOYER_NAME, count),
                                      y = count, fill = EMPLOYER_NAME)) +
  geom_bar(stat = "identity",fill="lightblue", colour="black") +
  labs(x = "EMPLOYER", y = "Number of Data Scientist",
       title = "Top Data Science Employers (in terms of petitions made)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,150,15)) +
  coord_flip()


top_employer_salary <- datasc_jobs %>% group_by(EMPLOYER_NAME) %>% 
  summarise(median_wage = median(PREVAILING_WAGE)) %>%
  arrange(desc(median_wage)) %>%
  top_n(15, wt = median_wage)
ggplot(data = top_employer_salary, aes(x = reorder(EMPLOYER_NAME, median_wage),
                                       y = median_wage/1000, fill = EMPLOYER_NAME)) +
  geom_bar(stat = "identity",fill="lightgreen", colour="black") +
  labs(x = "EMPLOYER", y = "Median Wage (in USD)",
       title = "Top Data Science Employers (in terms of salary offered)") +
  geom_text(aes(label = paste0("$",median_wage)), hjust = 1.2) +
  theme(legend.position = "none", axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  coord_flip()


datasc_jobs$WORKSITE <- factor(datasc_jobs$WORKSITE)
top_worksite_count <- datasc_jobs %>% group_by(WORKSITE) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(15, wt = count)
ggplot(data = top_worksite_count, aes(x = reorder(WORKSITE, count),
                                      y = count, fill = WORKSITE)) +
  geom_bar(stat = "identity",fill='grey',colour='black') + 
  labs(x = "CITY", y = "Number of Data Scientists",
       title = "TOP Work Locations (in terms of petitions made)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,120000,15000)) +
  coord_flip()


top_worksite_salary <- datasc_jobs %>% group_by(WORKSITE) %>%
  summarise(median_wage = median(PREVAILING_WAGE)) %>%
  arrange(desc(median_wage)) %>%
  top_n(15, wt = median_wage)
ggplot(data = top_worksite_salary, aes(x = reorder(WORKSITE, median_wage),
                                       y = median_wage, fill = WORKSITE)) +
  geom_bar(stat = "identity",fill='pink',colour='black') + 
  labs(x = "CITY", y = "MEDIAN SALARY",
       title = "TOP Work Locations (in terms of salary offered)") +
  geom_text(aes(label = paste0("$",median_wage)), hjust = 1.2) +
  theme(legend.position = "none", axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_flip()


visa %>% filter(!is.na(PREVAILING_WAGE)) %>% filter(PREVAILING_WAGE>0) %>% 
  filter(!is.na(JOB_TITLE)) %>% filter(!is.na(YEAR)) %>% filter(CASE_STATUS == "CERTIFIED") %>%
  filter(JOB_TITLE %in% c("DATA SCIENTIST")) %>% ungroup() -> dws
x = dws$PREVAILING_WAGE
y = dws$YEAR
y.f <- factor(y, levels= c(2011, 2012,2013, 2014, 2015, 2016),
              labels = c("2011", "2012", "2013", "2014", "2015", "2016")) 
sm.density.compare(x,y.f, xlab="Prevailing wage", ylab="Density")
title(main="Density plot for DATA SCIENTIST by years")
grid()
colfill<-c(2:(2+length(levels(y.f)))) 
legend(x = "topright", levels(y.f), fill=colfill)

##


visa_state = separate(visa, WORKSITE, c("CITY", "STATE"), sep = ', ', remove = TRUE)
head(visa_state)


visa_state %>% filter(!is.na(STATE)) %>%  
  group_by(STATE) %>% summarise(nr = length(STATE)) %>% ungroup() -> dstate
#dstate$STATE <- tolower(dstate$STATE)
dstate$nr <- dstate$nr/1000
colnames(dstate) <- c("region","value")
treemap(dstate, 
        index=c("region"), 
        type="value",
        vSize = "value",  
        vColor = "value",
        palette = "BrBG",  
        title=sprintf("Applications per state"), 
        title.legend = "Applications (thousands)",
        fontsize.title = 14 
)


## Worksite and year


#group applications by state and year
visa_state %>% filter(!is.na(STATE)) %>%  filter(!is.na(YEAR)) %>%
  group_by(YEAR, STATE) %>% summarise(nr = length(CASE_STATUS)) %>% ungroup() -> dys
#dys$state <- tolower(dys$state)
dys$nr <- dys$nr/1000
colnames(dys) <- c("YEAR", "region","value")
drawApplicationsTreeMap <- function(YEAR){
  dys2 <- subset(dys[,2:3], dys$YEAR == YEAR)
  treemap(dys2, 
          index=c("region"), 
          type="value",
          vSize = "value",  
          vColor="value",
          palette = "Set3",  
          title=sprintf("Applications during year %d",YEAR), 
          title.legend="Applications (thousands)",
          fontsize.title = 14 
  )
}


for (year in c(2011,2012,2013,2014,2015,2016)){
  drawApplicationsTreeMap(year)
}


### Full time positions


visa %>% filter(!is.na(FULL_TIME_POSITION)) %>% group_by(FULL_TIME_POSITION) %>% summarise(nr = length(lat)) %>% ungroup() -> dp

lbls = c("Part time","Full time")

pcts = round(dp$nr / sum(dp$nr) * 100,0)

lbls = paste(lbls, pcts)

lbls = paste(lbls,"%", sep="")

cols = c("brown", "light blue")

pie3D(x=dp$nr, labels=lbls, col = cols, explode=0, main = "Positions type")

### Employer name

## Selecting by nr
visa %>% group_by(EMPLOYER_NAME) %>% summarise(nr = length(lat)) %>% top_n(n=30) %>% ungroup() -> emp_name
ggplot(data = emp_name, aes(x = reorder(EMPLOYER_NAME,nr), y = nr/1000)) +  
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="", x ="Employer name (TOP 30)", y = "Number of applications (thousands)")

#Filtering data by JOB_TITLE
visa %>% group_by(JOB_TITLE) %>% summarise(nr = length(lat)) %>% 
  top_n(n=50) %>% arrange(-nr) %>% ungroup() -> job


visa %>% filter(!is.na(PREVAILING_WAGE)) %>% filter(PREVAILING_WAGE>0) %>% filter(!is.na(YEAR)) %>%
  filter(CASE_STATUS == "CERTIFIED") %>% filter(JOB_TITLE %in% job$JOB_TITLE[1:10]) %>%
  group_by(JOB_TITLE,YEAR) %>% summarise(avg = mean(PREVAILING_WAGE)) %>% ungroup() -> job_filter

ggplot(data = job_filter, aes(x = YEAR, y = avg/1000, colour = JOB_TITLE)) +       
  geom_line() + geom_point() + theme_bw() + theme(legend.position="right") +
  labs(x="Year", y="Average salary (thousands USD)", colour="Job title", 
       title="Prevailing salaries (per year and job title)",
       subtitle="Only CERTIFIED applications included")


visa %>% filter(!is.na(PREVAILING_WAGE)) %>% filter(PREVAILING_WAGE>0) %>% filter(!is.na(YEAR)) %>%
  filter(CASE_STATUS == "CERTIFIED") %>% 
  filter(JOB_TITLE %in% c("DATA SCIENTIST", "CHIEF DATA SCIENTIST", "DATA ANALYTICS ENGINEER")) %>%
  group_by(JOB_TITLE,YEAR) %>% summarise(avg = mean(PREVAILING_WAGE)) %>% ungroup() -> spec_JT

ggplot(data = spec_JT, aes(x = YEAR, y = avg/1000, colour = JOB_TITLE)) +       
  geom_line() + geom_point() + theme_bw() + theme(legend.position="right") +
  labs(x="Year", y="Average salary (thousands USD)", colour="Job title", 
       title="Prevailing salaries (per year and job title)",
       subtitle="Only CERTIFIED applications included for 3 Data Science related roles")



# Logistic regression

# Data pre-processing . COnsidering only certified and denied cases 

logdataset<-filter(visa,CASE_STATUS %in% c('CERTIFIED','DENIED')  & YEAR == 2016 )
logdataset<-logdataset[complete.cases(logdataset),]

logdata<-logdataset

logdata[,c(1,3,5,8,10,11)]<-NULL

logdata<-separate(data = logdata, col = WORKSITE, into = c("CITY", "STATE"), sep = ",")


## Data Cleaning-Selecting only major occupations
logdata$occ<-NA
logdata$occ[grep("engineer",logdata$SOC_NAME, ignore.case = T)]<-"ENGINEER"
logdata$occ[grep("manager",logdata$SOC_NAME, ignore.case = T)]<-"MANAGER"
logdata$occ[grep("technician",logdata$SOC_NAME, ignore.case = T)]<-"TECHNICIAN"
logdata$occ[grep("teacher",logdata$SOC_NAME, ignore.case = T)]<-"TEACHER"
logdata$occ[grep("executive",logdata$SOC_NAME, ignore.case = T)]<-"EXECUTIVE"
logdata$occ[grep("accountant",logdata$SOC_NAME, ignore.case = T)]<-"ACCOUNTANT"


logdata$SOC_NAME<-NULL
logdata$CITY<- NULL

## removing states with low count
state<-sqldf("select count(*) cc, STATE from 'logdata' group by STATE")
moddata<-sqldf("select * from state where cc>2000 AND STATE <> ' NA'")
logdata$STATE<-ifelse(logdata$STATE %in% moddata$STATE,logdata$STATE,NA)

##converting the dependent variable to binary
logdata$CASE_STATUS<-ifelse(logdata$CASE_STATUS %in% c("CERTIFIED"),"1","0" )

##selecting only complete cases
logdata<-logdata[complete.cases(logdata),]

##converting categorical variables into factors
logdata[,c(-3)]<- lapply(logdata[,c(-3)], as.factor)


finaldata<-logdata

##converting categorical variables to dummy variables for logistic regression
dummy<-dummyVars("~.",data=finaldata)

trsf<-data.frame(predict(dummy,newdata=finaldata))
data_with_dum<-cbind(finaldata[1],trsf[,c(-1,-2)])

colnames(data_with_dum)
#n-1 dummy variables for n categories
data_final<-data_with_dum[,c(-3,-37,-43)]

##fitting the model on the complete dataset
finalmodel.fit <-glm(CASE_STATUS~., family=binomial(link = logit), data = data_final)
summary(finalmodel.fit)


##finding and removing variables with high VIF
viff<-vif(finalmodel.fit)
which(as.numeric(viff)>8,arr.ind=T)
data_final<-data_final[,c(-5,-23,-31)]



##splitting the dataset into training and testing set
set.seed(32388)

inTrain <- createDataPartition(y = data_final$CASE_STATUS,p = .79, list = FALSE)
training <- data_final[inTrain,]
testing <- data_final[-inTrain,]


##fitting the model on the training dataset
finalmodel.train.fit <-glm(CASE_STATUS~., family=binomial(link = logit), data = training)
summary(finalmodel.train.fit)

coef(finalmodel.fit)


###Finding Prdicitons on Testing set
prediction<-predict(finalmodel.train.fit,newdata=testing,type="response")


##cutoff for prediction
prediction[prediction<0.97]<-0
prediction[prediction>=0.97]<-1
p=as.factor(prediction)

##confusion matrix
confusionMatrix(p,testing$CASE_STATUS)

####ROC Curve
pred <- prediction( prediction, testing$CASE_STATUS)
perf <- performance(pred,"tpr","fpr")
plot(perf)

###Area Under the Curve
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
auc

### Boosting

#Fitting the model on the complete dataset

boostfinal<-data_final
modelBoost.fit <- train(CASE_STATUS ~. , method="gbm", data = boostfinal, verbose=F)


##splitting the dataset into training and testing set
set.seed(32388)

inTrain <- createDataPartition(y = boostfinal$CASE_STATUS,p = .79, list = FALSE)
boosttraining <- boostfinal[inTrain,]
boosttesting <- boostfinal[-inTrain,]

#Train the model on the training data
set.seed(123)
modelBoost <- train(CASE_STATUS ~. , method="gbm", data=boosttraining, verbose=F)
summary(modelBoost)

predBoost <- predict(modelBoost, boosttesting, type="raw")

##cutoff for prediction
predBoost[predBoost < 0.89]<-0
predBoost[predBoost>=0.89]<-1
q=as.factor(predBoost)

##confusion matrix
confusionMatrix(q,testing$CASE_STATUS)

predBoostFinal <- prediction(as.numeric(q), as.numeric(testing$CASE_STATUS))
rocBoost <- performance(predBoostFinal, "tpr", "fpr")
plot(rocBoost)

aucBoost.tmp <- performance(predBoostFinal,"auc");
aucBoost <- as.numeric(aucBoost.tmp@y.values)
aucBoost

###### Random Forest

dataset_bak = dataset

# Further preprocessing

# we are gonna classify whether a candidate is certified or denied, hence dropping records with other 2 values
dataset = filter(dataset, CASE_STATUS != "CERTIFIED-WITHDRAWN" & CASE_STATUS != "WITHDRAWN" & CASE_STATUS != "INVALIDATED" & CASE_STATUS != "PENDING REVIEW")


randomRows = sample(1:length(dataset[,1]), 100000, replace=T)

df = dataset %>% slice(randomRows)

dim(df)


# we will use state as the location index so dropping other location related features
df = subset(df, select=-c(CITY, lon, lat))


# target feature encoding
df$CASE_STATUS = factor(df$CASE_STATUS, levels = c('CERTIFIED', 'DENIED' ), labels = c(1,0))


dim(df)
head(df)

# 1 hot encode the feature FULL_TIME_POSITION
df <- cbind(df, dummy(df$FULL_TIME_POSITION, sep = "_FTP_"))
df = subset(df, select=-c(FULL_TIME_POSITION))


# 1 hot encode the feature YEAR
df <- cbind(df, dummy(df$YEAR, sep = "_Year_"))
df = subset(df, select=-c(YEAR))


#length(unique(dataset[,"STATE"]))

#head(dataset)


dim(df)
head(df)

# Label Encode rest of the categorical variables
# Saving names of categorical variables
factors <- c("EMPLOYER_NAME", "SOC_NAME", "JOB_TITLE", "STATE")

# Label Encoder

for (i in factors){
  encode <- LabelEncoder.fit(df[, i])
  df[, i] <- transform(encode, df[, i])
}



# trying again by dropping these features
df = subset(df, select=-c(EMPLOYER_NAME, SOC_NAME, JOB_TITLE))


# to prevent conflict between "transform"" function from external library and inbuilt
detach("package:CatEncoders", unload=TRUE)


#We can use the transform method to change the in built type of each feature.
df <- transform(
  df,
  CASE_STATUS=as.factor(CASE_STATUS),
  #EMPLOYER_NAME=as.integer(EMPLOYER_NAME),
  #SOC_NAME=as.integer(SOC_NAME),
  #JOB_TITLE=as.integer(JOB_TITLE),
  PREVAILING_WAGE=as.integer(PREVAILING_WAGE),
  STATE=as.integer(STATE),
  df_FTP_N=as.factor(df_FTP_N),
  df_FTP_Y=as.factor(df_FTP_Y),
  df_Year_2011=as.factor(df_Year_2011),
  df_Year_2012=as.factor(df_Year_2012),
  df_Year_2013=as.factor(df_Year_2013),
  df_Year_2014=as.factor(df_Year_2014),
  df_Year_2015=as.factor(df_Year_2015),
  df_Year_2016=as.factor(df_Year_2016)
)

head(df)


sample <- sample.int(n = nrow(df), size = floor(0.8*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

print(dim(train))
print(dim(test))


rf <- randomForest(CASE_STATUS ~ .,  data=train,  na.action = na.omit)



# First column is the target/label column
pred = predict(rf, newdata=test[, 2:ncol(test)])

cm = table(test[, 1], pred)
cm

tp = cm[1,1]
fp = cm[1,2]
fn = cm[2,1]
tn = cm[2,2]

confusionMatrix(t(pred),test$CASE_STATUS)

cat(paste("\nAccuracy =",(tp+tn)/(tp+fp+fn+tn)))
cat(paste("\nMisclassification =",(fp+fn)/(tp+fp+fn+tn)))
cat(paste("\nPrecision =",(tp)/(tp+fp)))
cat(paste("\nSensitivity/Recall =",(tp)/(tp+fn)))
cat(paste("\nSpecificity =",(tn)/(fp+tn)))

rfNUM= as.numeric(pred)
rfNUM[is.na(rfNUM)] <- 0
rfpred <- prediction( rfNUM, test$CASE_STATUS)
rfperf <- performance(rfpred,"tpr","fpr")
plot(rfperf)

###Area Under the Curve
auc.tmp <- performance(rfpred,"auc");
auc2 <- as.numeric(auc.tmp@y.values)
auc2


plot(tp)
