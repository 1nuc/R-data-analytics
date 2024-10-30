#Ahmed Ali Saleh Ali Alyafai -TP074799
data_set=read.csv("C:\\Users\\Ahmed\\Desktop\\R\\asthma_dataset.csv",header=TRUE)
library(Hmisc)
describe(data_set)
library(dplyr)

#In this section we will go through the data set and see which variable has great impact on determining the  value of the Diet Quality
#applying a t test 
#intended p value is .001 as it represents highly significance
#now  we need to find the value that probably determine the frequency of Diet Quality
# getting all the column names 
Integer_N <- c("Gender_P", "Ethnicity_P", "FamilyHistoryAsthma_P",
               "HayFever_P", "GastroesophagealReflux_P",
               "Eczema_P", "Wheezing_P", 
               "ShortnessOfBreath_P", "ChestTightness_P", "Coughing_P", 
               "NighttimeSymptoms_P", "ExerciseInduced_P", "HistoryOfAllergies_P")
 
Numeric_N <- c("BMI_P","Age_P", "PhysicalActivity_P", "SleepQuality_P",
               "PollutionExposure_P", "DustExposure_P", "LungFunctionFVC_P",
               "LungFunctionFEV1_P")

for (r in Numeric_N){
  test <- cor.test(data_set$DietQuality_P, data_set[[r]], method = "pearson")
  if(test$p.value <=.001){
    print(paste("There is a relationship between Diet Quality and ",r))    
  }  
}
for (i in Integer_N){
  
  a_test <- t.test(data_set$DietQuality_P , data_set[[i]], alternative = "two.sided", conf.level = .95)
  if(a_test$p.value <= .001){
    print(paste("There is a relationship between Diet Quality and ",i))    
    
  }
}

# now lets discover the relationship between BMI and Diet Quality and see its impact

#applying a regersion model for the relationship
library(ggplot2)
ggplot(data_set, aes(x=DietQuality_P, y= PhysicalActivity_P))+geom_jitter(alpha=.2, color="blue")+
  geom_smooth(method = "lm", color="yellow", se=TRUE)+
  labs(title = "counts of Diet Quality vs Physical activity by Diet adherence",
                                                           x="Diet Quality",
                                                           y="Physical activity")
Occurrance <- function(variable){
  Occurrance_V <- median(data_set[[variable]])
  return(Occurrance_V)
}


High_Diet <- subset(data_set, data_set$DietQuality_P >= Occurrance("DietQuality_P"))
Low_Diet <- subset(data_set, data_set$DietQuality_P < Occurrance("DietQuality_P"))


High_Diet_Act= nrow(subset(High_Diet, High_Diet$PhysicalActivity_P >= Occurrance("PhysicalActivity_P")))
High_Diet_L_Act= nrow(subset(High_Diet, High_Diet$PhysicalActivity_P <= Occurrance("PhysicalActivity_P")))
Low_Diet_H_Act= nrow(subset(Low_Diet, Low_Diet$PhysicalActivity_P >= Occurrance("PhysicalActivity_P")))
Low_Diet_Act= nrow(subset(Low_Diet, Low_Diet$PhysicalActivity_P <= Occurrance("PhysicalActivity_P")))


data_frame <- data.frame(
  Diet_status=rep(c("High","low"),times=2),
  Physical_Activity_status= rep(c("High","Low"), each=2),
  Rate=c(High_Diet_Act / nrow(High_Diet)
              , Low_Diet_H_Act / nrow(Low_Diet), 
              High_Diet_L_Act / nrow(High_Diet), 
              Low_Diet_Act / nrow(Low_Diet))
)
data_frame


options(repr.plot.width=8,repr.plot.height=6)
ggplot(data_frame, aes(x= Diet_status, y=frequency, fill=Physical_Activity_status))+ 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "counts of Physical activity ordered by Diet Quality",
       x="Health Status",
       y="counts of occurrence")+
  scale_fill_manual(values = c("blue","orange"))+
theme_minimal()+theme(
  plot.title = element_text(face="bold",size=14),
  axis.title.x = element_text(face="bold",size=12),
  axis.title.y = element_text(face="bold",size=12),
  axis.text.x = element_text(face="bold",size=10),
  axis.text.y = element_text(face="bold",size=10),
  legend.axis.title = element_text(face="bold",size=12),
  legend.axis.text = element_text(face="bold",size=10),
)

#Now we will see how those figures effects the distribution of asthma


High_Diet_Act= (subset(High_Diet, High_Diet$PhysicalActivity_P >= Occurrance("PhysicalActivity_P")))
High_Diet_L_Act=(subset(High_Diet, High_Diet$PhysicalActivity_P <= Occurrance("PhysicalActivity_P")))
Low_Diet_H_Act= (subset(Low_Diet, Low_Diet$PhysicalActivity_P >= Occurrance("PhysicalActivity_P")))
Low_Diet_Act= (subset(Low_Diet, Low_Diet$PhysicalActivity_P <= Occurrance("PhysicalActivity_P")))


data_frame_asthma <- data.frame(
  Diet_Physical_status = rep(c("High_Diet & Activity","High_Diet_Low_Activity","Low_Diet_High_Activity","Low_Diet & Activity"),times=2),
  asthma_status  = rep(c("diagnosed","Healthy"), each=4),
  Rate=c(nrow(subset(High_Diet_Act, Diagnosis_P==1)) / nrow(High_Diet_Act),
         nrow(subset(High_Diet_L_Act, Diagnosis_P==1))/ nrow(High_Diet_L_Act),
         nrow(subset(Low_Diet_H_Act, Diagnosis_P==1)) /nrow(Low_Diet_H_Act),
         nrow(subset(Low_Diet_Act, Diagnosis_P==1)) /nrow(Low_Diet_Act),
         nrow(subset(High_Diet_Act, Diagnosis_P==0)) / nrow(High_Diet_Act),
         nrow(subset(High_Diet_L_Act ,Diagnosis_P==0))/ nrow(High_Diet_L_Act),
         nrow(subset(Low_Diet_H_Act,Diagnosis_P==0)) /nrow(Low_Diet_H_Act),
         nrow(subset(Low_Diet_Act ,Diagnosis_P==0)) /nrow(Low_Diet_Act))
)
data_frame_asthma


ggplot(data_frame_asthma, aes(x=Diet_Physical_status, y= Rate, fill=Diet_Physical_status))+geom_bar(stat="identity", width=.8, color="black")+
  geom_text(aes(label = scales::percent(Rate, accuracy = .1)), position = position_stack(vjust = .5),size = 3, color="white")+
  scale_fill_manual(values = c("red","green", "skyblue","orange"))+ facet_wrap(~asthma_status)
theme_minimal()






#---------------------------------------------------------------------------------------------------------------------------

# now lets see how many males and females in that study 0 male 1 female 

High_Diet_M= nrow(subset(High_Diet,High_Diet$Gender_P==0))
High_Diet_W=nrow(subset(High_Diet,High_Diet$Gender_P==1))
Low_Diet_M=nrow(subset(Low_Diet,Low_Diet$Gender_P==0))
Low_Diet_W= nrow(subset(Low_Diet, Low_Diet$Gender_P==1))

data_frame_G <- data.frame(
  Diet_status=rep(c("High","low"),times=2),
  Gender_Kind= rep(c("Male","Female"), each=2),
  Rate=c(High_Diet_M/ nrow(data_set[data_set$Gender_P==0,]),
         Low_Diet_M / nrow(data_set[data_set$Gender_P==0,]), 
         High_Diet_W / nrow(data_set[data_set$Gender_P==1,]), 
         Low_Diet_W / nrow(data_set[data_set$Gender_P==1,]))
)
data_frame_G

ggplot(data_frame_G, aes(x = "", y = Rate, fill = Gender_Kind)) + 
  geom_col(color = "black") + 
  coord_polar(theta = "y") + 
  facet_wrap(~ Diet_status) +
  labs(title = "Diet Status Distribution by Gender") +
  theme_void() +
  theme(legend.position = "right")+geom_text(aes(label = scales::percent(Rate, accuracy = 0.1)), 
                                               position = position_stack(vjust = 0.5), size = 3)



#now we will see the impact on asthma data set 
# Gender-Based Asthma Analysis
High_Diet_M = subset(High_Diet, High_Diet$Gender_P == 0)
High_Diet_W = subset(High_Diet, High_Diet$Gender_P == 1)
Low_Diet_M = subset(Low_Diet, Low_Diet$Gender_P == 0)
Low_Diet_W = subset(Low_Diet, Low_Diet$Gender_P == 1)

data_frame_gender_asthma <- data.frame(
  Gender_Diet_status = rep(c("High_Diet_Male", "High_Diet_Female", "Low_Diet_Male", "Low_Diet_Female"), times = 2),
  asthma_status = rep(c("diagnosed", "Healthy"), each = 4),
  Rate = c(
    nrow(subset(High_Diet_M, Diagnosis_P == 1)) / nrow(High_Diet_M),   # High Diet Male - Diagnosed
    nrow(subset(High_Diet_W, Diagnosis_P == 1)) / nrow(High_Diet_W),   # High Diet Female - Diagnosed
    nrow(subset(Low_Diet_M, Diagnosis_P == 1)) / nrow(Low_Diet_M),     # Low Diet Male - Diagnosed
    nrow(subset(Low_Diet_W, Diagnosis_P == 1)) / nrow(Low_Diet_W),     # Low Diet Female - Diagnosed
    nrow(subset(High_Diet_M, Diagnosis_P == 0)) / nrow(High_Diet_M),   # High Diet Male - Healthy
    nrow(subset(High_Diet_W, Diagnosis_P == 0)) / nrow(High_Diet_W),   # High Diet Female - Healthy
    nrow(subset(Low_Diet_M, Diagnosis_P == 0)) / nrow(Low_Diet_M),     # Low Diet Male - Healthy
    nrow(subset(Low_Diet_W, Diagnosis_P == 0)) / nrow(Low_Diet_W)      # Low Diet Female - Healthy
  )
)

# Gender-Based Asthma Visualization
ggplot(data_frame_gender_asthma, aes(x = asthma_status, y = Rate, fill = Gender_Diet_status)) +
  geom_bar(stat = "identity", width = .8, color = "black") +
  geom_text(aes(label = scales::percent(Rate, accuracy = .1)), position = position_stack(vjust = .5), size = 3, color = "white") +
  scale_fill_manual(values = c("pink", "darkblue", "pink", "darkblue")) +
  facet_wrap(~ Gender_Diet_status) +
  theme_minimal()















#---------------------------------------------------------------------------------------------------------------------------
data_Frame_E <- data.frame(
  Diet_status = rep(c("High", "Low"), each = 4),
  Ethnicity = rep(c("Caucasian", "African_American", "Asian", "Other"), times = 2),
  Precentage = c(
    (nrow(High_Diet[High_Diet$Ethnicity_P == 0,]) / nrow(data_set[data_set$Ethnicity_P == 0,])),  # Caucasian
    (nrow(High_Diet[High_Diet$Ethnicity_P == 1,]) / nrow(data_set[data_set$Ethnicity_P == 1,])),  # African_American
    (nrow(High_Diet[High_Diet$Ethnicity_P == 2,]) / nrow(data_set[data_set$Ethnicity_P == 2,])),  # Asian
    (nrow(High_Diet[High_Diet$Ethnicity_P == 3,]) / nrow(data_set[data_set$Ethnicity_P == 3,])),  # Other
    (nrow(Low_Diet[Low_Diet$Ethnicity_P == 0,]) / nrow(data_set[data_set$Ethnicity_P == 0,])),    # Caucasian (Low Diet)
    (nrow(Low_Diet[Low_Diet$Ethnicity_P == 1,]) / nrow(data_set[data_set$Ethnicity_P == 1,])),    # African_American (Low Diet)
    (nrow(Low_Diet[Low_Diet$Ethnicity_P == 2,]) / nrow(data_set[data_set$Ethnicity_P == 2,])),    # Asian (Low Diet)
    (nrow(Low_Diet[Low_Diet$Ethnicity_P == 3,]) / nrow(data_set[data_set$Ethnicity_P == 3,]))     # Other (Low Diet)
  )
)
data_Frame_E

ggplot(data_Frame_E, aes(x=Diet_status, y= Precentage, fill=Ethnicity))+geom_bar(stat="identity", width=.8, color="black")+
  geom_text(aes(label = scales::percent(Precentage, accuracy = .1)), position = position_stack(vjust = .5),size = 3, color="white")+
  scale_fill_manual(values = c("red","green", "skyblue","orange"))+ facet_wrap(~Ethnicity)
  theme_minimal()

  
  
  
  
  # Ethnicity-Based Asthma Analysis
  High_Diet_Caucasian = subset(High_Diet, High_Diet$Ethnicity_P == 0)
  High_Diet_African_American = subset(High_Diet, High_Diet$Ethnicity_P == 1)
  High_Diet_Asian = subset(High_Diet, High_Diet$Ethnicity_P == 2)
  High_Diet_Other = subset(High_Diet, High_Diet$Ethnicity_P == 3)
  
  Low_Diet_Caucasian = subset(Low_Diet, Low_Diet$Ethnicity_P == 0)
  Low_Diet_African_American = subset(Low_Diet, Low_Diet$Ethnicity_P == 1)
  Low_Diet_Asian = subset(Low_Diet, Low_Diet$Ethnicity_P == 2)
  Low_Diet_Other = subset(Low_Diet, Low_Diet$Ethnicity_P == 3)
  
  data_frame_ethnicity_asthma <- data.frame(
    Ethnicity_Diet_status = rep(c("High_Diet_Caucasian", "High_Diet_African_American", "High_Diet_Asian", "High_Diet_Other", 
                                  "Low_Diet_Caucasian", "Low_Diet_African_American", "Low_Diet_Asian", "Low_Diet_Other"), times = 2),
    asthma_status = rep(c("diagnosed", "Healthy"), each = 8),
    Rate = c(
      nrow(subset(High_Diet_Caucasian, Diagnosis_P == 1)) / nrow(High_Diet_Caucasian),   # High Diet Caucasian - Diagnosed
      nrow(subset(High_Diet_African_American, Diagnosis_P == 1)) / nrow(High_Diet_African_American),   # High Diet African American - Diagnosed
      nrow(subset(High_Diet_Asian, Diagnosis_P == 1)) / nrow(High_Diet_Asian),           # High Diet Asian - Diagnosed
      nrow(subset(High_Diet_Other, Diagnosis_P == 1)) / nrow(High_Diet_Other),           # High Diet Other - Diagnosed
      nrow(subset(Low_Diet_Caucasian, Diagnosis_P == 1)) / nrow(Low_Diet_Caucasian),     # Low Diet Caucasian - Diagnosed
      nrow(subset(Low_Diet_African_American, Diagnosis_P == 1)) / nrow(Low_Diet_African_American),   # Low Diet African American - Diagnosed
      nrow(subset(Low_Diet_Asian, Diagnosis_P == 1)) / nrow(Low_Diet_Asian),             # Low Diet Asian - Diagnosed
      nrow(subset(Low_Diet_Other, Diagnosis_P == 1)) / nrow(Low_Diet_Other),             # Low Diet Other - Diagnosed
      nrow(subset(High_Diet_Caucasian, Diagnosis_P == 0)) / nrow(High_Diet_Caucasian),   # High Diet Caucasian - Healthy
      nrow(subset(High_Diet_African_American, Diagnosis_P == 0)) / nrow(High_Diet_African_American),   # High Diet African American - Healthy
      nrow(subset(High_Diet_Asian, Diagnosis_P == 0)) / nrow(High_Diet_Asian),           # High Diet Asian - Healthy
      nrow(subset(High_Diet_Other, Diagnosis_P == 0)) / nrow(High_Diet_Other),           # High Diet Other - Healthy
      nrow(subset(Low_Diet_Caucasian, Diagnosis_P == 0)) / nrow(Low_Diet_Caucasian),     # Low Diet Caucasian - Healthy
      nrow(subset(Low_Diet_African_American, Diagnosis_P == 0)) / nrow(Low_Diet_African_American),   # Low Diet African American - Healthy
      nrow(subset(Low_Diet_Asian, Diagnosis_P == 0)) / nrow(Low_Diet_Asian),             # Low Diet Asian - Healthy
      nrow(subset(Low_Diet_Other, Diagnosis_P == 0)) / nrow(Low_Diet_Other)              # Low Diet Other - Healthy
    )
  )
  
  # Ethnicity-Based Asthma Visualization
  ggplot(data_frame_ethnicity_asthma, aes(x = asthma_status, y = Rate, fill = Ethnicity_Diet_status)) +
    geom_bar(stat = "identity", width = .8, color = "black") +
    geom_text(aes(label = scales::percent(Rate, accuracy = .1)), position = position_stack(vjust = .5), size = 3, color = "white") +
    scale_fill_manual(values = c("darkred", "darkgreen", "darkblue", "darkorange", "darkgray", "purple", "darkcyan", "black")) +
    facet_wrap(~ Ethnicity_Diet_status) +
    theme_minimal()
  