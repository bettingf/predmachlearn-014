library(caret)
train<-read.csv("pml-training.csv")
test<-read.csv("pml-testing.csv")
training<-train[,!is.na(train[1,])]
testing<-test[,!is.na(train[1,])]
cols<-names(which(lapply(training, length)>1))
training<-training[,cols]
testing<-testing[,intersect(names(testing),cols)]
training$X<-NULL
training<-as.data.frame(training)

training<-training[sample(1:dim(training)[1]),]
rfModFit1<-train(classe ~ ., data=head(training,n=200), method="rf", ntree=10, nPerm=5, do.trace=TRUE, ntry=10)
varImp(rfModFit1)

qplot(roll_belt, magnet_dumbbell_y, color=classe, data=training)


rfModFit<-train(classe ~ roll_belt+magnet_dumbbell_y + magnet_arm_x+raw_timestamp_part_1+num_window, data=training, method="rf", ntree=10, nPerm=5, do.trace=TRUE, ntry=10)
rfModFit
rfModFit$finalModel


answers<-predict(rfModFit, testing)
answers
