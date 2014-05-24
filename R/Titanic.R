# setwd("Kaggle/Titanic/R")

library("caret")
library("dplyr")
library("ggplot2")

FileDir <- "../data"

TrainFile <- "/train.csv"
TestFile <- "/test.csv"

titanic <- function(FileDir="../data", DataSet="train"){
    
  create_features("train")
  create_features("test")
  
}

create_features <- function(DataSet="train"){
  
  if(DataSet=="train"){
    dat <- read.csv(paste0(FileDir, TrainFile))
  }else if(DataSet=="test"){
    dat <- read.csv(paste0(FileDir, TestFile))
  }else{
    stop()
  }
  
  #
  # Create features
  #
  
  str(dat)
  
  dat$Pclass <- as.factor(dat$Pclass)
  dat$Sex <- as.factor(dat$Sex)
  dat$Embarked <- as.factor(dat$Embarked)
  
  dat[dat==""] <- NA
  
  #
  # Exploration
  #
  if(DataSet == "train"){
    summary(dat[dat$Survived == 1, ])
    summary(dat)
    
    plotContinuousDists <- function(variable){
      dd <- dat[, c("Survived", variable)]
      names(dd) <- c("SurvNum", "vari")
      dd$Survived <- "Survived"
      dd$Survived[dd$SurvNum==0] <- "Died"
      
      output <- ggplot(dd, aes(x=vari, y=..density.., fill=Survived)) +
        geom_histogram(color="black") + 
        facet_wrap(~Survived, ncol=1) +
        labs(x=variable, y=NULL, title=paste0("Survival Distribution: ", variable))
      
      return(output)
    }  
    plotDiscreteDists <- function(variable){
      dd <- dat[, c("Survived", variable)]
      names(dd) <- c("SurvNum", "vari")
      dd$Survived <- "Survived"
      dd$Survived[dd$SurvNum==0] <- "Died"
      
      output <- ggplot(dd, aes(x=vari, fill=Survived)) +
        geom_bar(color="black") + 
        facet_wrap(~Survived, ncol=1) +
        labs(x=variable, y=NULL, title=paste0("Survival Distribution: ", variable))
      
      return(output)
    }
    
    agePlot <- plotContinuousDists("Age")
    farePlot <- plotContinuousDists("Fare")
    sexPlot <- plotDiscreteDists("Sex")
    classPlot <- plotDiscreteDists("Pclass")
    sibSpPlot <- plotContinuousDists("SibSp")
    parchPlot <- plotContinuousDists("Parch")
    embPlot <- plotDiscreteDists("Embarked")
    
    
    numNAs <- apply(dat, 2, function(x) sum(is.na(x)))
    
    table(dat$Embarked)  
  }
  
  #
  #Extract title
  #
  
  dat$Title <- sapply(as.character(dat$Name), function(nm){
    title <- strsplit(strsplit(nm, ", ")[[1]][2], "\\.")[[1]][1]
    
    return(title)
  })
  
  dat$Title[dat$Title %in% c("Ms")] <- "Miss"
  dat$Title[dat$Title %in% c("Lady", "Mme", "Dona", "Mlle", "the Countess")] <- "NobleWoman"
  dat$Title[dat$Title %in% c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Sir")] <- "OtherMale"
  
  #
  #Cabin Level
  #
  dat$CabinLevel <- sapply(as.character(dat$Cabin), function(cb){
    ifelse(is.na(cb), NA, strsplit(cb, "")[[1]][1])
  })
  
  if(DataSet=="train"){
    titlePlot <- plotDiscreteDists("Title")
    cabinPlot <- plotDiscreteDists("CabinLevel")
    
    #Age exploration
    
    plotAgeBoxplots <- function(variable){
      dd <- dat[, c("Age", variable)]
      names(dd) <- c("Age", "vari")
      dd$vari <- as.factor(dd$vari)
      
      output <- ggplot(dd, aes(x=vari, y=Age)) +
        geom_boxplot(fill="orange") + 
        labs(x=variable, y=NULL, title=paste0("Survival Distribution: ", variable))
      
      return(output)
    }  
  
    sexAge <- plotAgeBoxplots("Sex")
    classAge <- plotAgeBoxplots("Pclass")
    embAge <- plotAgeBoxplots("Embarked")
    titleAge <- plotAgeBoxplots("Title")
    cabinAge <- plotAgeBoxplots("CabinLevel")
    sibSpAge <- plotAgeBoxplots("SibSp")
    parchAge <- plotAgeBoxplots("Parch")
  }
  
  #
  # Parch and SibSp vars
  #
  
  dat$ParchEq2 <- 0
  dat$ParchEq2[dat$Parch == 2] <- 1
  dat$SibsBuck <- "Few"
  dat$SibsBuck[dat$SibSp == 2] <- "Two"
  dat$SibsBuck[dat$SibSp > 2] <- "Many"
  
  #
  # Miss age
  #
  
  dat$MissingAge <- ifelse(is.na(dat$Age), 1, 0)
  dat$MissingCabin <- ifelse(is.na(dat$Cabin), 1, 0)
  
  #
  # Imputation
  #
  
  dat$Embarked[is.na(dat$Embarked)] <- "S"
  
  noAgeNdx <- which(is.na(dat$Age))
  
  if(DataSet=="train"){
    
    ageImpVars <- c("Pclass", "SibsBuck", "ParchEq2", "Title")
    classGroups <- group_by(dat, Pclass) %.% summarise(count=length(Age),  
      median=median(Age, na.rm=TRUE), mean=mean(Age, na.rm=TRUE), sd=sd(Age, na.rm=TRUE))
    sibsGroups <- group_by(dat, SibsBuck) %.% summarise(count=length(Age), 
      median=median(Age, na.rm=TRUE), mean=mean(Age, na.rm=TRUE), sd=sd(Age, na.rm=TRUE))
    parchGroups <- group_by(dat, ParchEq2) %.% summarise(count=length(Age), 
      median=median(Age, na.rm=TRUE), mean=mean(Age, na.rm=TRUE), sd=sd(Age, na.rm=TRUE))
    titleGroups <- group_by(dat, Title) %.% summarise(count=length(Age), 
      median=median(Age, na.rm=TRUE), mean=mean(Age, na.rm=TRUE), sd=sd(Age, na.rm=TRUE))
    
    examples <- nrow(dat)
    
    classGroups <- classGroups[classGroups$count / examples > 0.02, ]
    sibsGroups <- sibsGroups[sibsGroups$count / examples > 0.02, ]
    parchGroups <- parchGroups[parchGroups$count / examples > 0.02, ]
    titleGroups <- titleGroups[titleGroups$count / examples > 0.02, ]
    
    ageImpute <- data.frame(age=dat$Age,
                            class=classGroups$median[match(dat$Pclass, classGroups$Pclass)],
                            sibs=sibsGroups$median[match(dat$SibsBuck, sibsGroups$SibsBuck)],
                            parch=parchGroups$median[match(dat$ParchEq2, parchGroups$ParchEq2)],
                            title=titleGroups$median[match(dat$Title, titleGroups$Title)])
    
    ageImputeNonNA <- ageImpute[!is.na(ageImpute$age), ]
    
    
    age_imputaion <- list(classGroups=classGroups, sibsGroups=sibsGroups, 
                          parchGroups=parchGroups, titleGroups=titleGroups)
    
    saveRDS(age_imputaion, paste0(FileDir, "/age_imputaion.rds"))
    
  }else if(DataSet=="test"){
    age_imputaion <- readRDS(paste0(FileDir, "/age_imputaion.rds"))
  }
  
  classGroups <- age_imputaion$classGroups
  sibsGroups <- age_imputaion$sibsGroups
  parchGroups <- age_imputaion$parchGroups
  titleGroups <- age_imputaion$titleGroups
  
  imputedAge <- 0.32 * age_imputaion$classGroups$median[match(dat$Pclass, classGroups$Pclass)] +
    0.68 * titleGroups$median[match(dat$Title, titleGroups$Title)]
  imputedAge[is.na(imputedAge)] <- parchGroups$median[match(dat$ParchEq2, parchGroups$ParchEq2)][is.na(imputedAge)]
  
  dat$Age[noAgeNdx] <- imputedAge[noAgeNdx] 
  
  #
  # More features
  #
  
  dat$FareFamily <- dat$Fare / (dat$SibSp + dat$Parch + 1)
  
  #Impute 0 fares (TODO)
  fareIs0Ndx <- which(dat$FareFamily == 0)
  
  #
  # More features
  #  
  
  dat$CabinSide <- sapply(as.character(dat$Cabin), function(cb){
    
    if(is.na(cb)) return(NA)
    
    splitstr <- strsplit(cb, "")[[1]]
    lastNum <- as.numeric(splitstr[length(splitstr)])
    
    if(!is.numeric(lastNum)) return("neither")
    
    return(ifelse(lastNum %% 2 == 1, "starboard", "port"))
  })
  
  #
  # Reduce to feature set
  #
  
  if(DataSet=="train"){  
    datFeatures <- dat[, c("Survived", "Pclass", "Sex", "Age", "Title",
                       "SibSp", "Parch", "FareFamily", "CabinLevel", "CabinSide", "Embarked", "MissingAge")]
  }else if(DataSet=="test"){
    datFeatures <- dat[, c("Pclass", "Sex", "Age", "Title",
                       "SibSp", "Parch", "FareFamily", "CabinLevel", "CabinSide", "Embarked", "MissingAge")]
  }
  
  datFeatures$CabinLevel[is.na(datFeatures$CabinLevel)] <- "none"
  datFeatures$CabinSide[is.na(datFeatures$CabinSide)] <- "none"
  
  write.csv(datFeatures, paste0(FileDir, "/features_", DataSet, ".csv"), row.names=FALSE)
  
  if(DataSet=="train"){
    
    #
    # Model
    #
    
    features_train <- read.csv(paste0(FileDir, "/features_train.csv"), stringsAsFactors=FALSE)
    
    features_train$Survived <- as.factor(features_train$Survived)
    features_train$Pclass <- as.factor(features_train$Pclass)
    features_train$Sex <- as.factor(features_train$Sex)
    features_train$CabinLevel <- as.factor(features_train$CabinLevel)
    features_train$CabinSide <- as.factor(features_train$CabinSide)
    features_train$Embarked <- as.factor(features_train$Embarked)
    features_train$Title <- as.factor(features_train$Title)
    
    set.seed(1234)
    
    trControl <- trainControl(method="repeatedcv", number=10, repeats=2)
    tuneGrid <- data.frame(.mtry=c(4, 8, 12))
    
    model_rf <- train(form=Survived ~ ., data=features_train,
                       metric = "Accuracy",
                       method="rf", 
                       trControl=trControl,
                       tuneGrid=tuneGrid)
    
    model_rf
    
    saveRDS(model_rf, paste0(FileDir, "/model1.rds"))
  }
}


make_predictions <- function(FileDir="../data"){
  
  model <- readRDS(paste0(FileDir, "/model1.rds"))
  
  #Get the test data
  features_test <- read.csv(paste0(FileDir, "/features_test.csv"), stringsAsFactors=TRUE)   
  
  features_test$Pclass <- as.factor(features_test$Pclass)
  features_test$FareFamily <- median(features_test$FareFamily)
  
  test_preds <- predict(model, features_test)
  
  test_dat <- read.csv(paste0(FileDir, TestFile))
  
  output <- data.frame(PassengerId=test_dat$PassengerId, pred=test_preds)
}






