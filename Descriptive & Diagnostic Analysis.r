#Ahmed Ali Saleh Ali Alyafai -TP074799
options(max.print = 11072)

data_set=read.csv("C:\\Users\\Ahmed\\Desktop\\R\\asthma_dataset.csv",header=TRUE)
View(data_set)

sum(is.na(data_set$DietQuality_P))#TO check if there is any null value 
str(data_set)

summary(data_set)
colnames(data_set)
head(data_set$DietQuality_P)

install.packages("Hmisc")
library(Hmisc)
describe(data_set)

#to get a full understanding of the data set

#--------------------------------------
#phase 1
#Descriptive analytics
# In this section we will delve into a direct relationship between the independent variable which is Diet Quality and the variable of interest which is asthma risk

library(ggplot2)

ggplot(data_set, aes(x = factor(Diagnosis_P), y = DietQuality_P)) +
  geom_violin(fill = "lightblue", alpha = 0.5) +
  labs(title = "Violin Plot of Diet Quality by Diagnosis", x = "Diagnosis", y = "Diet Quality")


ggplot(data_set, aes(x=as.factor(Diagnosis_P),y=DietQuality_P))+geom_boxplot(color="darkgreen", fill="lightgreen")+
  labs(title = "Distribution of Diet quality among asthma diet", x="Asthma Occurence", y="Diet Qaulity")
#------------------------

#notes 
# first determine the people diagnosed with asthma and save it in a new subset called diagnosed and the rest store them in a column named healthy

Diagnosed <- subset(data_set,data_set$Diagnosis_P==1)

#now we need to get the rate of occurrence over the original data_set and the result is .8099
Diagnosed_rate <- nrow(Diagnosed)/nrow(data_set) 

#creating a subset for people for people who are having no asthma a
Healthy=subset(data_set,data_set$Diagnosis_P==0)
nrow(Healthy)

# make comparison with the Diet Quality

#now lets get to find the correlation between diet quality and asthma risk 
# getting the asthma risk mean on diet quality

mean(Diagnosed$DietQuality_P)
# we got 5.05 now lets get the higher and lower level of diet quality

# one approach is to set the median as a central measurement for the diet

occurrence <- median(data_set$DietQuality_P)
occurrence

# now we got the median of the Diet Quality which is 5.05

# now lets calculate the measurement for the chances of asthma occurrence with low and high diet quality

higher_Diet_Diagnosed=nrow(Diagnosed[Diagnosed$DietQuality_P >= occurrence, ])# 5057 as result for people with better diet quality
lower_Diet_Diagnosed=nrow(Diagnosed[Diagnosed$DietQuality_P < occurrence, ])# 3911 as result for people with lower diet quality



# now lets calculate the measurement for the association of diet quality with having no asthma

higher_Diet_Healthy=nrow(Healthy[Healthy$DietQuality_P >= occurrence, ])# 1156 as result for people with better diet quality

lower_Diet_Healthy=nrow(Healthy[Healthy$DietQuality_P < occurrence, ])#  948 as result for people with lower diet quality


#now we will make a test to determine if its statistically significant
t.test(Healthy$DietQuality_P, Diagnosed$DietQuality_P, alternative = "two.sided" ,conf.level = .99 )
# p-value = .9864 > .05 this indicates the occurrence of the values is likely randomly generated so we don't reject null values
# the p value is very huge, therefore no strong evidence to prove the hypothesis

#lets visualize the findings

#we first create a data frame to represent all the findings 
Sum_data <- data.frame(
  Asthma_Status = rep(c("Healthy", "Diagnosed"),each=2),
  Diet_Quality=rep(c("Higher Diet Quality","Lower Diet Quality"),times=2),
  counts=c(higher_Diet_Healthy, lower_Diet_Healthy,
               higher_Diet_Diagnosed,lower_Diet_Diagnosed)
)


# now lets use a barplot to represent the findings
#intsall liberary ggplot for aesthitacl representation of the data
install.packages("ggplot2")
library(ggplot2)

options(repr.plot.width=8,repr.plot.height=6)
ggplot(Sum_data, aes(x= Asthma_Status, y=counts, fill=Asthma_Status))+ 
         geom_bar(stat="identity", position = "dodge")+facet_wrap(~Diet_Quality)+
         labs(title = "counts of Asthma vs Healthy Individuals ordered by Diet Quality",
              x="Health Status",
              y="counts of occurrence")+         scale_fill_manual(values = c("red","darkgreen"))+ geom_text(aes(label=counts),position = position_stack(vjust=.5), color="white")+
         theme_minimal()+theme(
           plot.title = element_text(face="bold",size=10),
           axis.title.x = element_text(face="bold",size=8),
           axis.title.y = element_text(face="bold",size=8),
           axis.text.x = element_text(face="bold",size=8),
           axis.text.y = element_text(face="bold",size=8)
         )

ggsave("C:\\Users\\Ahmed\\Desktop\\image.pdf", plot=p, width = 8, height = 6, dpi=300)

Sum_data_Per <- data.frame(
  Asthma_Status = rep(c("Healthy", "Diagnosed"),each=2),
  Diet_Quality=rep(c("Higher Diet Quality","Lower Diet Quality"),times=2),
  precentage=c(higher_Diet_Healthy / nrow(data_set[data_set$DietQuality_P >= occurrence,]), 
           lower_Diet_Healthy / nrow(data_set[data_set$DietQuality_P < occurrence,]),
           higher_Diet_Diagnosed / nrow(data_set[data_set$DietQuality_P >= occurrence,]),
           lower_Diet_Diagnosed / nrow(data_set[data_set$DietQuality_P < occurrence,])
           )
)

ggplot(Sum_data_Per, aes(x=Asthma_Status, y=precentage, fill= Asthma_Status))+ geom_bar(stat = "identity", position="dodge", color="Black")+
  labs(title = "proportion of Asthma vs Healthy Individuals ordered by Diet Quality",
       x="Health Status",
       y="counts of occurrence")+scale_fill_manual(values = c("gray","pink"))+facet_wrap(~Diet_Quality)+geom_text(aes(label =scales ::percent(precentage, accuracy = .1)),
                                                                                                                   position = position_stack(vjust = .4), color="black")+
  theme_minimal()
  
  

  
#by calculating the proportion for higher and lower quality for for no asthma occurrence we get the following

higher_Diet_Healthy / nrow(Healthy) #0.5494 stands for the result of the proportion of high diet quailty
lower_Diet_Healthy / nrow(Healthy) #0.4506 stands for the result of the proportion of low diet quality


#now lets get the full number of diet quality 

nrow(data_set[data_set$DietQuality_P < occurrence, ]) # the number of people with lower Diet quality is 4859
nrow(data_set[data_set$DietQuality_P >= occurrence, ]) # the number of people with better Diet quality is 6213


#now lets calculate the proportion for both

higher_Diet_Diagnosed / nrow(Diagnosed) #.5639 stands for the proportion of having asthma among higher diet quality
lower_Diet_Diagnosed / nrow(Diagnosed) #0.4361 stands for the proportion among lower diet quality proportion






#---------------------------------------------------------------
# now lets conclude the findings 
#people who are having better diet quality in which they are 6213 person 4930 of them are having asthma with a proportion of .5639
# also 1156 of those people are having no asthma with a proportion of .5494

#-----------------------

#people who are having lower diet quality in which they are 4859 person 3911 of them are having asthma with a proportion of .4361
# also 948 of those people are having no asthma with a proportion of.4506

#Since its statistically insignificant we conclude that the hypothesis is rejected
# the above explanation illustrated direct relationship between the Diet Quality and the asthma 


#Phase 1 finished



#________________________________________________
#phase 2
# Diagnostic Analysis

#now in this part we will delve into the correlation between the diet quality and education level
occurrence <- median(data_set$DietQuality_P)
# for getting the median value to determine high and low level
describe(data_set$DietQuality_P)
# to make sure the median is 5.05 

#creating a subset for better and low diet quality
#people who are higher than median assumed to have better diet quality and vice versa

#but first we must apply a linear regression plot to make sure that there is a correlation between education and level and diet 
  
  ggplot(data_set ,aes(x=EducationLevel_P ,y=DietQuality_P))+
    geom_jitter(alpha = 0.5, color="skyblue")+ geom_smooth(method="lm", color="darkblue", se=TRUE)+
    labs(title = "counts of Diet Quality vs Education level ordered by Diet adherence",
         x="Education Level",
         y="Diet Status")

  
Higher_Diet= subset(data_set, data_set$DietQuality_P >= occurrence)
Lower_Diet= subset(data_set, data_set$DietQuality_P < occurrence)

# as we can see there is a linear regression correlation between Diet and Education now lets prove that statistically
# we will check if its statically significant by applying t.test

t.test(Higher_Diet$EducationLevel_P, Lower_Diet$EducationLevel_P, alternative = "two.sided", conf.level = .99)
  
#p- value < .05 which is 1.127e-14 therefore we reject the null values and all the values there are following dedicated hypothesis
# in this case degree of Diet Quality can vary depending on the Level of education
      

#Creating a data frame to gather the findings
Diet_frame <- data.frame(
  Diet_Status = rep(c("Higher Diet adherence","Lower diet adherence"), each=4),
  Eduaction_Status=rep(c("None","High_Scool","Bachlor's","Higher_Degrees"),2),
  frequencies=c(nrow(Higher_Diet[Higher_Diet$EducationLevel_P==0,]), nrow(Higher_Diet[Higher_Diet$EducationLevel_P==1,]),
                nrow(Higher_Diet[Higher_Diet$EducationLevel_P==2, ]),nrow(Higher_Diet[Higher_Diet$EducationLevel_P==3,]),
                nrow(Lower_Diet[Lower_Diet$EducationLevel_P==0,]), nrow(Lower_Diet[Lower_Diet$EducationLevel_P==1,]),
                nrow(Lower_Diet[Lower_Diet$EducationLevel_P==2, ]),nrow(Lower_Diet[Lower_Diet$EducationLevel_P==3,]))
                
)
Diet_frame
#visualizing those findings with a barplot

options(repr.plot.height = 8,repr.plot.width = 6)
ggplot(Diet_frame, aes(x=Eduaction_Status, y=frequencies, fill=Diet_Status))+
         geom_bar(stat="identity",position = "dodge")+ geom_text(aes(label = frequencies), position = position_stack(vjust = .4), color="white")+
  labs(title = "counts of Diet Quality vs Education level ordered by Diet adherence",
       x="Education Level",
       y="Frequencies")+
  scale_fill_manual(values=c("darkblue","chocolate"))+
  theme_minimal()+theme(plot.title = element_text(face="bold",size=14),
                        axis.title.x = element_text(face="bold",size=12),
                        axis.title.y = element_text(face="bold",size=12),
                        axis.text.x = element_text(face="bold",size=10),
                        axis.text.y = element_text(face="bold",size=10),
                        legend.title = element_text(face="bold",size=12),
                        legend.text = element_text(face="bold",size=10),
    
  )+facet_wrap(~Diet_Status, ncol = 2)
  

#Since the number of high school educated people is high we will make calculation depending on the rate or the proportion

Diet_frame_rate <- data.frame(
    Diet_Status = rep(c("Higher Diet adherence","Lower diet adherence"), each=4),
    Eduaction_Status=rep(c("None","High_Scool","Bachlor's","Higher_Degrees"),2),
    frequencies=c(nrow(Higher_Diet[Higher_Diet$EducationLevel_P==0,]) / nrow(data_set[data_set$EducationLevel_P==0,]), 
                  nrow(Higher_Diet[Higher_Diet$EducationLevel_P==1,]) / nrow(data_set[data_set$EducationLevel_P==1,]),
                  nrow(Higher_Diet[Higher_Diet$EducationLevel_P==2, ])/ nrow(data_set[data_set$EducationLevel_P==2,]),
                  nrow(Higher_Diet[Higher_Diet$EducationLevel_P==3,])/ nrow(data_set[data_set$EducationLevel_P==3,]),
                  nrow(Lower_Diet[Lower_Diet$EducationLevel_P==0,]) / nrow(data_set[data_set$EducationLevel_P==0,]), 
                  nrow(Lower_Diet[Lower_Diet$EducationLevel_P==1,])/ nrow(data_set[data_set$EducationLevel_P==1,]),
                  nrow(Lower_Diet[Lower_Diet$EducationLevel_P==2, ])/ nrow(data_set[data_set$EducationLevel_P==2,]),
                  nrow(Lower_Diet[Lower_Diet$EducationLevel_P==3,])/ nrow(data_set[data_set$EducationLevel_P==3,]))
    
  )
  Diet_frame_rate
# now lets visualize this data again to see the rate of occurrence of each
ggplot(Diet_frame_rate, aes(x=Eduaction_Status, y=frequencies, fill=Diet_Status))+
    geom_bar(stat="identity",position = "dodge")+
    labs(title = "Proportion of Diet Quality vs Education level ordered by Diet adherence",
         x="Education Level",
         y="Diet Status")+
    scale_fill_manual(values=c("pink","skyblue"))+geom_text(aes(label = scales :: percent(frequencies, accuracy = .1)),
                                                            position = position_stack(vjust = .4), color="black", size = 3)+
  theme_minimal()+theme(plot.title = element_text(face="bold",size=14),
                        axis.title.x = element_text(face="bold",size=12),
                        axis.title.y = element_text(face="bold",size=12),
                        axis.text.x = element_text(face="bold",size=10),
                        axis.text.y = element_text(face="bold",size=10),
                        legend.title = element_text(face="bold",size=12),
                        legend.text = element_text(face="bold",size=10),
                        
)+facet_wrap(~Diet_Status, ncol = 2)
  
  
# as a result people with higher education have better diet occurrence
# result shown higher educational degrees as the higher rate of having better diet
# then the Bachelor's degree people as the second
# figure shows the worst with dieting habits are the one's not educated

  
# now lets see the affect of these results on asthma risk
  
# lets first create two subsets representing Higher and lower educated people
  
# in this case people above high school will be considered as higher educated people and vice versa
# result will only take place for those who have better diet quality
  
Higher_Educated_People = subset(Higher_Diet[Higher_Diet$EducationLevel_P > 1,])
Lower_Educated_People = subset(Higher_Diet[Higher_Diet$EducationLevel_P <= 1,])
  
#creating a data frame 
  
asthma_Diet_frame <- data.frame(
    Asthma_Status=rep(c("Healthy", "Diagnosed"), each=2),
    Education_status=rep(c("High", "Low"),2),
    count=c(nrow(Higher_Educated_People[Higher_Educated_People$Diagnosis_P ==0, ]),
            nrow(Lower_Educated_People[Lower_Educated_People$Diagnosis_P==0,]),
            nrow(Higher_Educated_People[Higher_Educated_People$Diagnosis_P ==1, ]),
            nrow(Lower_Educated_People[Lower_Educated_People$Diagnosis_P ==1,]))
  )
  asthma_Diet_frame
  
  #this data frame calculates counts or frequencies
  # visualizing the above data 
  
  ggplot(asthma_Diet_frame, aes(x=Education_status, y=count, fill=Asthma_Status))+ 
    geom_bar(stat="identity", position="dodge")+
    scale_fill_manual(values = c("darkred","darkblue"))+geom_text(aes(label = count),
                                                              position = position_stack(vjust = .4), color="white", size = 3)+
    labs(title=" Asthma counts comparison with respect to higher diet adherence and education level",
         x= "Education Level",
         y= "Frequencies")+
    theme_minimal()+theme(plot.title = element_text(face="bold",size=14),
                          axis.title.x = element_text(face="bold",size=12),
                          axis.title.y = element_text(face="bold",size=12),
                          axis.text.x = element_text(face="bold",size=10),
                          axis.text.y = element_text(face="bold",size=10),
                          legend.title = element_text(face="bold",size=12),
                          legend.text = element_text(face="bold",size=10))+
    facet_wrap(~Asthma_Status)
  
#now lets get the proportion of both values to for more precision
  
asthma_Diet_frame_rate <- data.frame(
    Asthma_Status = rep(c("Healthy", "Diagnosed"), each = 2),
    Education_status = rep(c("High", "Low"), 2),
    proportion = c(
      nrow(Higher_Educated_People[Higher_Educated_People$Diagnosis_P == 0, ]) / nrow(Higher_Educated_People),
      nrow(Lower_Educated_People[Lower_Educated_People$Diagnosis_P == 0, ]) / nrow(Lower_Educated_People),
      nrow(Higher_Educated_People[Higher_Educated_People$Diagnosis_P == 1, ]) / nrow(Higher_Educated_People),
      nrow(Lower_Educated_People[Lower_Educated_People$Diagnosis_P == 1, ]) / nrow(Lower_Educated_People)
    )
  )
asthma_Diet_frame_rate  

ggplot(asthma_Diet_frame_rate, aes(x=Education_status, y=proportion, fill=Asthma_Status))+ 
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values = c("salmon","turquoise"))+geom_text(aes(label = scales :: percent(proportion, accuracy = .1)),
                                                            position = position_stack(vjust = .4), color="white", size = 3)+
  labs(title=" Asthma rate comparison with respect to higher diet adherence and education level",
       x= "Education Level",
       y= "Proportion")+
  theme_minimal()+theme(plot.title = element_text(face="bold",size=14),
                        axis.title.x = element_text(face="bold",size=12),
                        axis.title.y = element_text(face="bold",size=12),
                        axis.text.x = element_text(face="bold",size=10),
                        axis.text.y = element_text(face="bold",size=10),
                        legend.title = element_text(face="bold",size=12),
                        legend.text = element_text(face="bold",size=10))+
  facet_wrap(~Asthma_Status, ncol=2)
  
# as we can see here the proportion of high educated people with high diet quality for asthma as less than non educated people with bad diet 

p_no_asthma_H <- nrow(Higher_Educated_People[Higher_Educated_People$Diagnosis_P == 0, ]) / nrow(Higher_Educated_People)*100
p_no_asthma_L <- nrow(Lower_Educated_People[Lower_Educated_People$Diagnosis_P == 0, ]) / nrow(Lower_Educated_People)*100

addition <- p_no_asthma_H + p_no_asthma_L

No_asthma_high_educated_People= (p_no_asthma_H / addition) *100

No_asthma_Low_Educated_People= (p_no_asthma_L / addition) *100

#creating a pie chart to represent the results
slices <- data.frame(
  Education_Status = c("Higher Educated", "Lower Educated"),
  Proportion = c(No_asthma_high_educated_People, No_asthma_Low_Educated_People)
)

# Create the pie chart with ggplot2
ggplot(slices, aes(x = "", y = Proportion, fill = Education_Status)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of No Asthma by Education Level and higher Diet Qaulity") +
  theme_void() +
  scale_fill_manual(values = c("lightblue", "lightgreen"))

#Hypothesis not proved as the proportion of occurrence must be higher than 70 per and 
# in this case its only 25% were free of asthma from the Higher diet and education people
# now lets see if its statistically significant by applying t.test

t.test(Higher_Educated_People$Diagnosis_P, Lower_Educated_People$Diagnosis_P, alternative = "two.sided", conf.level = .99)

# p-value 6.481e-15 < .05 therefor we reject null values and all the values are following the hypothesis stated
#Therefore it's statistically significant


#----------------------------------------------------
# now lets see the impact of education level on smoking habits
#we apply t test to make sure there is a statistical difference
t.test(Higher_Educated_People$Smoking_P, Lower_Educated_People$Smoking_P ,alternative = "two.sided", conf.level = .95)
#p-value is .00053 <.05 therefore we reject the null hypothesis and indeed! there is an impact on smoking


# justification
# lets calculate the proportion for each
Education_level <- c(rep("High", nrow(Higher_Educated_People)), rep("Low", nrow(Lower_Educated_People)))
Smoking_status <- c(Higher_Educated_People$Smoking_P, Lower_Educated_People$Smoking_P)

# Combine the data into a data frame
combined_data <- data.frame(Education_level, Smoking_status)

combined_data
# Create a contingency table
table_education_smoking <- table(combined_data$Education_level, combined_data$Smoking_status)

# Apply prop.table to get the proportions
proportion_table <- prop.table(table_education_smoking ,margin = 1)
proportion_df <- as.data.frame(proportion_table)
colnames(proportion_df) <- c("Education_status", "Smoking_Status", "Proportion")

# Adjust Smoking_Status values from 0/1 to "No"/"Yes"
proportion_df$Smoking_Status <- ifelse(proportion_df$Smoking_Status == 0, "No", "Yes")

# Load required libraries
library(ggplot2)
library(scales)

# Visualization using ggplot2
ggplot(proportion_df, aes(x = Education_status, y = Proportion, fill = Smoking_Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)),
            position = position_stack(vjust = 0.4), color = "white", size = 3) +
  labs(title = "Comparing Smoking Habits by Educational Level",
       x = "Education Level", y = "Proportion") +
  theme_minimal() +
  facet_wrap(~ Smoking_Status, ncol = 2) +
  theme_minimal()

# now lets see the impact on asthma risk
# for that we will pick the non smokers of any level of education with Higher Diet Quality to see the impact on asthma

Educated_Non_Smokers=subset(Higher_Educated_People[Higher_Educated_People$Smoking_P==0,])
Non_Educated_Non_Smokers=subset(Lower_Educated_People[Lower_Educated_People$Smoking_P==0,])

#creating a data frame to organize results
asthma_Smoking_frame_rate <- data.frame(
  Asthma_Status = rep(c("No", "Yes"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  proportion = c(
    nrow(Educated_Non_Smokers[Educated_Non_Smokers$Diagnosis_P==0,]) / nrow(Educated_Non_Smokers),
    nrow(Non_Educated_Non_Smokers[Non_Educated_Non_Smokers$Diagnosis_P==0,])/ nrow(Non_Educated_Non_Smokers),
    nrow(Educated_Non_Smokers[Educated_Non_Smokers$Diagnosis_P==1,]) / nrow(Educated_Non_Smokers),
    nrow(Non_Educated_Non_Smokers[Non_Educated_Non_Smokers$Diagnosis_P==1,]) / nrow(Non_Educated_Non_Smokers)
  )
)
ggplot(asthma_Smoking_frame_rate, aes(x=Education_status, y=proportion, fill=Asthma_Status))+
  geom_bar(stat="identity", position="dodge")+scale_fill_manual(values=c("gold","skyblue"))+geom_text(aes(label = scales :: percent(proportion, accuracy = .1)),
                                                                                                  position = position_stack(vjust = .4), color="white", size = 3)+
  labs(title = "Comparing high diet quality and no smoking habits with asthma rate depending on the Educational Level",
       x="Education Level", y="Proportion")+facet_wrap(~Asthma_Status, ncol = 2)+
  theme_minimal()

#as we can see here higher education level with better diet and non smoking habits lowers the impact of asthma risk
# Explanation
# Independent variable <- Diet Quality <- its value remains High since the study measures its impact with other variables
# Moderator <- Education level <- helps in strengthening the proportion of High Diet adherence lowering other risks
# Second moderator <- Smoking <- its value support the claim of our hypothesis towards asthma risk
# variable of Interest <- Diagnosis which is the asthma risk


#----------------------------------
# now lets see the impact of education level on Pet exposure

# lets apply t.test again to see if education level and high diet quality impact pet exposure
t.test(Higher_Educated_People$PetAllergy_P, Lower_Educated_People$PetAllergy_P, alternative = "two.sided", conf.level = .99)
# p-value is .00156 <.05 therefore we reject null hypothesis and we prove that there is an impact on Pet Allergy

# creating a data frame to see the proportion of level of education with pet allergy
Pet_frame_rate <- data.frame(
  Pet_Allergy_Status = rep(c("No", "Yes"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  proportion = c(
    nrow(Higher_Educated_People[Higher_Educated_People$PetAllergy_P == 0, ]) / nrow(Higher_Educated_People),
    nrow(Lower_Educated_People[Lower_Educated_People$PetAllergy_P == 0, ]) / nrow(Lower_Educated_People),
    nrow(Higher_Educated_People[Higher_Educated_People$PetAllergy_P == 1, ]) / nrow(Higher_Educated_People),
    nrow(Lower_Educated_People[Lower_Educated_People$PetAllergy_P == 1, ]) / nrow(Lower_Educated_People)
  )
)

ggplot(Pet_frame_rate, aes(x=Education_status, y= proportion, fill=Pet_Allergy_Status))+
  geom_bar(stat="identity", position = "dodge")+geom_text(aes(label = scales :: percent(proportion, accuracy = .1)),
                                                          position = position_stack(vjust = .4), color="black", size = 3)+
  scale_fill_manual(values = c("yellow","purple"))+
  labs(title = "Comparing higher Diet Quality with pet allergy depending on the Educational Level",
       x="Education Level", y="Proportion") +facet_wrap(~Pet_Allergy_Status, ncol = 2)
  
# now we need to see its impact on asthma risk 
# for that we will pick the non pet allergic of any level education with Higher Diet Quality to see the impact on asthma
Non_Pet_Educated=subset(Higher_Educated_People[Higher_Educated_People$PetAllergy_P==0,])
Non_Pet_Non_Educated=subset(Lower_Educated_People[Lower_Educated_People$PetAllergy_P==0,])

#creating a data frame to represent all the values

asthma_Pet_frame_rate <- data.frame(
  Asthma_Status = rep(c("No", "Yes"), each = 2),
  Education_status = rep(c("High", "Low"), 2),
  proportion = c(
    nrow(Non_Pet_Educated[Non_Pet_Educated$Diagnosis_P==0,]) / nrow(Non_Pet_Educated),
    nrow(Non_Pet_Non_Educated[Non_Pet_Educated$Diagnosis_P==0,]) / nrow(Non_Pet_Non_Educated),
    nrow(Non_Pet_Educated[Non_Pet_Educated$Diagnosis_P==1,]) / nrow(Non_Pet_Educated),
    nrow(Non_Pet_Non_Educated[Non_Pet_Educated$Diagnosis_P==1,]) / nrow(Non_Pet_Non_Educated)
    )
)


ggplot(asthma_Pet_frame_rate, aes(x=Education_status, y=proportion, fill=Asthma_Status))+
  geom_bar(stat="identity", position="dodge")+scale_fill_manual(values=c("seagreen","indianred"))+geom_text(aes(label = scales :: percent(proportion, accuracy = .1)),
                                                                                                        position = position_stack(vjust = .4), color="white", size = 3)+
  labs(title = "Comparing High diet quality and no Pet Allergic people with asthma rate depending on the Educational Level",
       x="Education Level", y="Proportion")+facet_wrap(~Asthma_Status,ncol = 2)



#as we can see here higher education level with better diet and non pet allergic lowers the impact of asthma risk
# Pet allergy in this case stands as second moderator variable
# Diet Quality in all cases remains consistent. why ?
# Due the use of it as an independent in which its value affects the flow of the data


#----------------------
# now lets see the impact of education level on Pullotion Exposure

# lets apply t.test again to see if education level and high diet quality impact Pollution Exposure
t.test(Higher_Educated_People$PollutionExposure_P, Lower_Educated_People$PollutionExposure_P, alternative = "two.sided", conf.level = .99)
# p-value is .5464 >.05 therefore we accept null hypothesis and there is high chance that the values came by random
# Higher Diet quality with education level have no impact on Pollution Exposure

# Phase 2 finished

