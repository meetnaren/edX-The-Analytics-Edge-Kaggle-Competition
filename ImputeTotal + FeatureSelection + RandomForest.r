poll=read.csv("train2016.csv",na.strings=c("NA"))
poll=poll[-c(which((poll$YOB>2005 & !is.na(poll$YOB)) | (poll$YOB<1920 & !is.na(poll$YOB)))),]
polltest=read.csv("test2016.csv",na.strings=c("NA"))
polltest=data.frame(polltest[,1:6],Party="",polltest[,7:107])
polltotal=rbind(poll,polltest)

#Setting blanks as NA in the non-question columns
polltotal$Gender[polltotal$Gender==""]=NA
polltotal$Income[polltotal$Income==""]=NA
polltotal$HouseholdStatus[polltotal$HouseholdStatus==""]=NA
polltotal$EducationLevel[polltotal$EducationLevel==""]=NA

#Factorizing the columns
polltotal$Gender=factor(polltotal$Gender,levels=c("Male","Female"),ordered=FALSE)
polltotal$EducationLevel=factor(polltotal$EducationLevel,levels=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree"),ordered=TRUE)
polltotal$Income=factor(polltotal$Income,levels=c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000"),ordered=TRUE)
polltotal$HouseholdStatus=factor(polltotal$HouseholdStatus,levels=c("Domestic Partners (no kids)","Domestic Partners (w/kids)","Married (no kids)","Married (w/kids)","Single (no kids)","Single (w/kids)"),ordered=FALSE)
polltotal$Age=2016-polltotal$YOB
polltotal$NACounts=rowSums(polltotal[,8:107]=="")
polltotal$YOB=NULL

#Imputation
polltotalImputed=data.frame(polltotal$Gender,polltotal$Income,polltotal$HouseholdStatus,polltotal$EducationLevel,polltotal$Age)
polltotalImputed=complete(mice(polltotalImputed))

polltotal$Gender=polltotalImputed$polltotal.Gender
polltotal$Income=polltotalImputed$polltotal.Income
polltotal$HouseholdStatus=polltotalImputed$polltotal.HouseholdStatus
polltotal$EducationLevel=polltotalImputed$polltotal.EducationLevel
polltotal$Age=polltotalImputed$polltotal.Age
polltotal$Age=as.factor(cut(polltotal$Age,breaks=c(0,16,25,36,49,64,81,100),labels=FALSE))
poll=polltotal[1:5556,]

poll$Party=factor(poll$Party,levels=c("Democrat","Republican"),ordered=FALSE)

polltest=polltotal[5557:6948,]
polltest$Party=NULL

#Feature Selection
RegressionFields="Party ~ "
temp="Gender+HouseholdStatus+EducationLevel+Age+NACounts"
for (i in 7:107){
	if (chisq.test(table(poll$Party,poll[[i]]))$p.value<0.05 & i!=6){
		temp=paste(temp,colnames(poll)[i],sep="+")
		cat(colnames(poll)[i],"\n")
	}
}
RegressionFields=paste(RegressionFields,temp,sep="")


rfmodel=randomForest(formula(RegressionFields),data=poll)
prediction=predict(rfmodel)
(table(poll$Party,prediction)[1]+table(poll$Party,prediction)[4])/nrow(poll)

testprediction=predict(rfmodel,newdata=polltest)
submission=data.frame(USER_ID=polltest$USER_ID,Predictions=testprediction)


write.csv(submission,file="Submission06121635.csv",row.names=FALSE)