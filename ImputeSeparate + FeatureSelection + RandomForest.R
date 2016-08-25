poll=read.csv("train2016.csv",na.strings=c("NA"))

#Removing observations with invalid Year of Birth values
poll=poll[-c(which((poll$YOB>2005 & !is.na(poll$YOB)) | (poll$YOB<1920 & !is.na(poll$YOB)))),]

#Setting blanks as NA in the non-question columns
poll$Gender[poll$Gender==""]=NA
poll$Income[poll$Income==""]=NA
poll$HouseholdStatus[poll$HouseholdStatus==""]=NA
poll$EducationLevel[poll$EducationLevel==""]=NA

#Factorizing the columns
poll$Gender=factor(poll$Gender,levels=c("Male","Female"),ordered=FALSE)
poll$EducationLevel=factor(poll$EducationLevel,levels=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree"),ordered=TRUE)
poll$Income=factor(poll$Income,levels=c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000"),ordered=TRUE)
poll$HouseholdStatus=factor(poll$HouseholdStatus,levels=c("Domestic Partners (no kids)","Domestic Partners (w/kids)","Married (no kids)","Married (w/kids)","Single (no kids)","Single (w/kids)"),ordered=FALSE)
poll$Age=2016-poll$YOB
#poll$NACounts=rowSums(poll[,8:107]=="")
poll$YOB=NULL

#Imputation
pollImputed=data.frame(poll$Gender,poll$Income,poll$HouseholdStatus,poll$EducationLevel,poll$Age)
pollImputed=complete(mice(pollImputed))

poll$Gender=pollImputed$poll.Gender
poll$Income=pollImputed$poll.Income
poll$HouseholdStatus=pollImputed$poll.HouseholdStatus
poll$EducationLevel=pollImputed$poll.EducationLevel
poll$Age=pollImputed$poll.Age

#Feature Selection
RegressionFields="Party ~ "
temp="Gender+Income+HouseholdStatus+EducationLevel+Age"
for (i in 7:107){
	if (chisq.test(table(poll$Party,poll[[i]]))$p.value<0.005 & i!=6){
		temp=paste(temp,colnames(poll)[i],sep="+")
		cat(colnames(poll)[i],"\n")
	}
}
RegressionFields=paste(RegressionFields,temp,sep="")


rfmodel=randomForest(formula(RegressionFields),data=poll)
prediction=predict(rfmodel)
(table(poll$Party,prediction)[1]+table(poll$Party,prediction)[4])/nrow(poll)



polltest=read.csv("test2016.csv",na.strings=c("NA"))

#Setting blanks as NA in the non-question columns
polltest$Gender[polltest$Gender==""]=NA
polltest$Income[polltest$Income==""]=NA
polltest$HouseholdStatus[polltest$HouseholdStatus==""]=NA
polltest$EducationLevel[polltest$EducationLevel==""]=NA

#Factorizing the columns
polltest$Gender=factor(polltest$Gender,levels=c("Male","Female"),ordered=FALSE)
polltest$EducationLevel=factor(polltest$EducationLevel,levels=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree"),ordered=TRUE)
polltest$Income=factor(polltest$Income,levels=c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000"),ordered=TRUE)
polltest$HouseholdStatus=factor(polltest$HouseholdStatus,levels=c("Domestic Partners (no kids)","Domestic Partners (w/kids)","Married (no kids)","Married (w/kids)","Single (no kids)","Single (w/kids)"),ordered=FALSE)
polltest$Age=2016-polltest$YOB
#polltest$NACounts=rowSums(polltest[,8:107]=="")
polltest$YOB=NULL

#Imputation
polltestImputed=data.frame(polltest$Gender,polltest$Income,polltest$HouseholdStatus,polltest$EducationLevel,polltest$Age)
polltestImputed=complete(mice(polltestImputed))

polltest$Gender=polltestImputed$polltest.Gender
polltest$Income=polltestImputed$polltest.Income
polltest$HouseholdStatus=polltestImputed$polltest.HouseholdStatus
polltest$EducationLevel=polltestImputed$polltest.EducationLevel
polltest$Age=polltestImputed$polltest.Age

testprediction=predict(rfmodel,newdata=polltest)
submission=data.frame(USER_ID=polltest$USER_ID,Predictions=testprediction)


write.csv(submission,file="Submission06102209.csv",row.names=FALSE)