q1

library(readxl)
library(tidyverse)
EU = read_excel("CA1-2023 .xlsx", sheet= 'EU')
US = read_excel("CA1-2023 .xlsx", sheet = 'US')
View(EU)
View(US)
View(combined_EU_US_F)
#unify format for two datasets,Ensure both dataframes/tibbles have the same formatting and are compatible with each
#other.
US$Batch.No<-as.numeric(US$Batch.No)
US$Op.Salary<-as.numeric(US$Op.Salary)
US$Batch.vol<-as.numeric(US$Batch.vol)
US$Titre<-as.numeric(US$Titre)
US$Pressure<-as.numeric(US$Pressure)
US$Temp<-as.numeric(US$Temp)


q2

#convert 'l' to 1,'o' to 0 
US$Batch.No<- gsub("l", "1", US$Batch.No)
US$Batch.No<- gsub("O", "0", US$Batch.No)
US$Op.Salary<-gsub('O','0',US$Op.Salary)
US$Op.Salary<-gsub('l','1',US$Op.Salary)
US$Batch.vol<-gsub('l','1',US$Batch.vol)
US$Batch.vol<-gsub('O','0',US$Batch.vol)
US$Pressure<-gsub('l','1',US$Pressure)
US$Pressure<-gsub('O','0',US$Pressure)
US$Batch_age<-gsub('O','0',US$Batch_age)
US$Batch_age<-gsub('l','1',US$Batch_age)
US$Titre<-gsub('O','0',US$Titre)
US$Titre<-gsub('l','1',US$Titre)
US$Temp<-gsub('O','0',US$Temp)
US$Temp<-gsub('l','1',US$Temp)
US$Op_f.name<-gsub('O','o',US$Op_f.name)
US$Op_f.name<-gsub('1','l',US$Op_f.name)

#convert column format
colnames(US)<-c('Batch.No','Site','Op_s.name','Op_f.name','Op.Salary','Batch.prod','Batch.vol','Pressure','Batch_age','Titre','Temp')
colnames(EU)<-c('Batch.No','Op_s.name','Op_f.name','Op.Salary','Site','Batch.vol','Batch.prod','Titre','Batch_age','Pressure','Temp')

#remove space before surnames in Op_s.name column of EU
EU$Op_s.name<-str_trim(EU$Op_s.name)
#remove space before firstnames in Op_f.name column of EU
EU$Op_f.name<-trimws(EU$Op_f.name)

q3

install.packages("lubridate")
install.packages("stringr")
library(lubridate)
library(stringr)

#creat a new column in EU

Completion_date <- as.Date("2023-08-01")
EU$Completion_date<- Completion_date + EU$Batch_age
EU$Completion_date<- format(EU$Completion_date, "%a %dth %B %Y")

#creat a new column in US

Completion_date <- as.Date("2023-09-27")

#convert character format to numeric
US$Batch_age<-as.numeric(US$Batch_age)

US$Completion_date <- Completion_date + US$Batch_age
US$Completion_date<- format(US$Completion_date, "%a %dth %B %Y")

str(EU)
str(US)


q4

combined_EU_US <- rbind(US, EU)
View(combined_EU_US)
str(combined_EU_US)


q5

combined_EU_US1$Op_s.name <- sub("^(Mc|Mac|O')([A-Za-z]+)$", "\\1\\2", combined_EU_US1$Op_s.name)
combined_EU_US1$Initials <- paste0(substr(combined_EU_US1$Op_f.name, 1, 1),substr(combined_EU_US1$Op_s.name, 1, 4) )

#doubts?? this one is not working 
combined_EU_US$Op_s.name <- ifelse(
  +     grepl("^(Mc|Mac|O')", combined_EU_US$Op_s.name),
  +     substr(combined_EU_US$Op_s.name, 1, 4),   # 截取前四个字符
  +     substr(combined_EU_US$Op_s.name, 1, 1)    # 截取第一个字符
  + )
> 
  > # 使用paste0函数将Op_s.name和Op_f.name串联
  > combined_EU_US$Initials <- paste0(combined_EU_US$Op_s.name, substr(combined_EU_US$Op_f.name, 1, 1))

#Using sum(is.na(combined_EU_US_salary)) to check missing values of numeric variables
q6

batch<-substr(combined_EU_US$Batch.No,
nchar(combined_EU_US$Batch.No)-1,nchar(combined_EU_US$Batch.No))
site<-substr(combined_EU_US$Site,1,1)
combined_EU_US$ReferenceCode<-paste0(batch,'-',site,'_',
combined_EU_US$Initials,'(',combined_EU_US$Batch.prod,')')

q7

combined_EU_US$Operator_Title[combined_EU_US$Op.Salary
>=45000&combined_EU_US$Op.Salary<60000]<-'Operations Technician'
combined_EU_US$Operator_Title[combined_EU_US$Op.Salary
>=60000&combined_EU_US$Op.Salary<70000]<-'Process Engineer'
combined_EU_US$Operator_Title[combined_EU_US$Op.Salary
>=70000&combined_EU_US$Op.Salary<80000]<-'Senior Process Engineer'
combined_EU_US$Operator_Title[combined_EU_US$Op.Salary
>=80000&combined_EU_US$Op.Salary<100000]<-'Managing Operator'

#q8
library(psych)
#description of Batch.vol in different salary scale
describe(combined_EU_US_salary$Batch.vol[combined_EU_US_salary$Op.Salary>=45000&combined_EU_US_salary$Op.Salary<60000])
describe(combined_EU_US_salary$Batch.vol[combined_EU_US_salary$Op.Salary>=60000&combined_EU_US_salary$Op.Salary<70000])
describe(combined_EU_US_salary$Batch.vol[combined_EU_US_salary$Op.Salary>=70000&combined_EU_US_salary$Op.Salary<80000])
describe(combined_EU_US_salary$Batch.vol[combined_EU_US_salary$Op.Salary>=80000&combined_EU_US_salary$Op.Salary<100000])
#description of Titre in different salary scale
describe(combined_EU_US_salary$Titre[combined_EU_US_salary$Op.Salary>=45000&combined_EU_US_salary$Op.Salary<60000])
describe(combined_EU_US_salary$Titre[combined_EU_US_salary$Op.Salary>=60000&combined_EU_US_salary$Op.Salary<70000])
describe(combined_EU_US_salary$Titre[combined_EU_US_salary$Op.Salary>=70000&combined_EU_US_salary$Op.Salary<80000])
describe(combined_EU_US_salary$Titre[combined_EU_US_salary$Op.Salary>=80000&combined_EU_US_salary$Op.Salary<100000])
library(ggplot2)
#generate bar plot for salary scale counts for each site
ggplot(combined_EU_US_salary,aes(x=Site,fill=Operator_Title))+geom_bar(position ='dodge')
+labs(title='Salary by Site and Operator Title')

#generate boxplot for titre and salary scale
ggplot(combined_EU_US_salary,aes(x=Operator_Title,y=Titre,fill=Operator_Title))
+geom_boxplot()+labs(title='Boxplot of Titre and Salary scale')

#generate boxplot for pressure and salary scale
ggplot(combined_EU_US_salary,aes(x=Operator_Title,y=Pressure,fill=Operator_Title))
+geom_boxplot()+labs(title='Boxplot of Pressure and Salary scale')

#generate boxplot for Temperature  and salary scale
ggplot(combined_EU_US_salary,aes(x=Operator_Title,y=Temp,fill=Operator_Title))
+geom_boxplot()+labs(title='Boxplot of Temperature and Salary scale')

#generate boxplot for Batch.vol  and salary scale
ggplot(combined_EU_US_salary,aes(x=Operator_Title,y=Batch.vol,fill=Operator_Title))
+geom_boxplot()+labs(title='Boxplot of batch.vol and Salary scale')

#q9

ggplot(combined_EU_US_salary,aes(x=Batch.vol,y=Op.Salary,size=Titre,shape=Site,color=Operator_Title))+geom_point()+labs(title='Scatterplot of Volume and Salary')
#summary of lm
summary(lm(Batch.vol ~ Pressure + Titre + Temp, data = combined_EU_US_salary))
summary(lm(Batch.vol ~ Pressure , data = combined_EU_US_salary))

#q10

library(GGally)

#subset data to make ggpairs plot to see if there are any variables has notable relationship with volume
subset_data<-combined_EU_US_salary[,c('Op.Salary','Batch.vol','Pressure','Batch_age','Titre','Temp','Operator_Title','Site')]
ggpairs(subset_data)

#we abserved a clear linear relationship between pressure and volumn


#scatterplot of pressure and volumn
ggplot(combined_EU_US_salary,aes(x=Pressure,y=Batch.vol))+geom_point()

#we also observed LA with less Age and higher temp and salary
#use boxplot with different color to compare different significants between sites

#we are comparing Site ~ Op.Salary/Temp/Titre/Batch_age/Pressure/Batch.vol
#boxplot these 6 pairs of variables 
install.packages(ggpubr)
library(ggpubr)
ggboxplot(combined_EU_US_salary,x='Site',y='Op.Salary',color='Site',add='jitter',platte='npg')+labs(title ='Boxplot of Site by Salary')
ggboxplot(combined_EU_US_salary,x='Site',y='Temp',color='Site',add='jitter',platte='npg')+labs(title ='Boxplot of Site by Tempareture')
ggboxplot(combined_EU_US_salary,x='Site',y='Titre',color='Site',add='jitter',platte='npg')+labs(title ='Boxplot of Site by Titre')
ggboxplot(combined_EU_US_salary,x='Site',y='Batch_age',color='Site',add='jitter',platte='npg')+labs(title ='Boxplot of Site by Age')
ggboxplot(combined_EU_US_salary,x='Site',y='Pressure',color='Site',add='jitter',platte='npg')+labs(title ='Boxplot of Site by Pressure')
ggboxplot(combined_EU_US_salary,x='Site',y='Batch.vol',color='Site',add='jitter',platte='npg')+labs(title ='Boxplot of Site by Batch.vol')
or
ggplot(combined_EU_US_salary,aes(x=Site,y=Batch.vol,color=Site))+geom_jitter()+geom_boxplot()+labs(title='boxplot of site by volumn')

#difference between color=Site/fill=Site is if to fill the color inside box
ggplot(combined_EU_US_salary,aes(x=Site,y=Batch.vol,fill=Site))+geom_jitter()+geom_boxplot()+labs(title='boxplot of site by volumn')

#respectiveily analysis significant level among class of site and 6 variables using add p_value into graph

#add p-value into boxplot which compares the significance of Temp among LA/San Francisco/Dublin,but p>0.05
my_comparisons <- list(c("LA", "Dublin"),c("LA", "San Francisco"),c("Hamburg", "San Francisco"))
ggboxplot(combined_EU_US_salary, x = "Site", y = "Temp",
          color = "Site", palette = "npg")+
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons, label.y = c(35,36,37))+
  stat_compare_means(label.y = 38)

#so only analysis boxplot instead of significance among site and different other numeric variables





#ggpaired lines ?? what kind of situation to use it 
ggpaired(combined_EU_US_salary,x='Operator_Title',y='Batch.vol',color='Operator_Title',line.color = 'gray',line.size = 0.4)+labs(title='boxplot of site by volumn')+stat_compare_means(paired = TRUE)

#tapply函数和do.call函数，对数据进行了按照Operator_Title分类的批次（Batch.vol）的汇总统计
site_summary <- tapply(combined_EU_US_salary$Batch.vol, combined_EU_US_salary$Operator_Title, summary)
all_sites_summary <- do.call(rbind, site_summary) all_sites_summary

#test of Operator_Title(operation technician/managing operator ) and batch.vol
t<-t.test(combined_EU_US_salary$Batch.vol[combined_EU_US_salary$Operator_Title=='Managing Operator'],combined_EU_US_salary$Batch.vol[combined_EU_US_salary$Operator_Title=='Operations Technician'])
t


#add line into linear model
library(ggplot2)

# 创建散点图
ggplot(combined_EU_US_salary, aes(x = Pressure, y = Batch.vol)) +
geom_point() +
     
# 添加线条
 geom_abline(intercept = coef(l)[1], slope = coef(l)[2], color = "red")
