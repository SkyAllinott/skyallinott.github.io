#Load raw data
train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

#Add a survived variable to the test set to allow for combining data sets
test.Survived <- data.frame(Survived= rep("none", nrow(test)), test[,])
#Rearrange columns 1 and 2
test.Survived <- test.Survived[c(2,1,3,4,5,6,7,8,9,10,11,12)]

#Combine data
data.combined <-rbind(train,test.Survived)

#A bit about R data types (e.g. factors)
  #str compactly displays the structure of an arbitrary R object (definition)
    #Factors are vectors with category- like dropdowns
    #Sex is a factor w/ 2 levels- Two choices in the dropdown
    #N/A is NULL. Did not have the age for one of the passengers
str(data.combined)

#Converting survived and pclass from integer/character to factor
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
  #Verifying data switch
  str(data.combined)

#Take a look at gross survival rates
table(data.combined$Survived)
  #More people did not survive than did survive

#Distribution across classes (NOT the death distribution)
table(data.combined$Pclass)

#Loading ggplot 2 package to visualize
library(ggplot2)


#Hypothesis - First class survived at a higher rate
  #This makes the bar graph and gives it some design as well
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill=factor(Survived))) +
  geom_bar(width=0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill="Survived")
#Hypothesis seems to be true- More survived than perished in first class, oppposite for first

#Examine the first few names in the training data set (Convert name from factor to character)
head(as.character(train$Name))

#How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))

#Taking a closer look at duplicate names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Pulling the duplicate names 
data.combined[which(data.combined$Name %in% dup.names),]
  #Concluding that the duplicates are not repeated, rather, coincidence that two people had same names


#What's up with the "Miss" and "Mr" thing?
library(stringr)

#Any correlation between titles and other variables? (e.g. sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss")), ]
misses[1:5,]
  #4/5 misses survived, AND 4/5 were in third class, where mortality was high
  #High variance in age 
  #Sibsp is number of siblings/spouses on board (4 year old likely has a sibling)
  #Parch is number of parents/children on board (4 year old likely has a parent)
  #Since Sibsp is 0 mostly, we can conclude "Miss" is for unmarried women, may also tend to be younger

#Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]
  #Mrs. seem to be older, many are travelling with spouse or sibling, perhaps spouse more likely

#Check out males to see if pattern continues
males <- data.combined[which(train$Sex =="male"),]
males[1:5,]

#Expand upon the relationship between "Survived" and "Pclass" by adding the new "Title" variable to the data set and then explore
#pontential 3 dimensional relationship

#Create a utility function to help with title extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)

#Since we only have survived labels for the train set, only use the first 891 rows
ggplot(data.combined[1:891,], aes(x=title, fill=Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill="Survived")
                            #Mr in third class did NOT survive very much, "Women and children first"?
                            #In first class, this is especially pronounced

#Whats the distribution oof female to male?
table(data.combined$Sex)
  #Predominantly male

#Visualize the 3-way relationship of sex, pclass and survival compare to analysis
ggplot(data.combined[1:891,], aes(x=Sex,fill=Survived)) +
  geom_bar(width=0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill="Survived")

    #Males in first class were proportionally more likely to survive. Second class not much better than third?
    #First class females mostly survived, as well as second, but third class females are 50/50

#Age and sex seem pretty important as derived from analysis of title, lets look at age
summary(data.combined$Age)
summary(data.combined[1:891, "Age"])
          #263 of 1309 ages are MISSING. That is quite a lot.
          #Lets examine correlation between age and title to try and infer missing ages

#To be thorough, taking a look at survival rates broken out by sex, class, and title
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count") +
  ggtitle("Effects of age, class and sex on survival")

#Validating that "Master" is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)
  #Median is 4 years old. Seems Master is a good proxy for male children (Max is 14.5)

#Because males are either "Mr." or "Master.", we can reasonably assume "Mr." corresponds to older males
#Let's try "Miss.", which is more complicated

misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)
  #Min is <1 years old, max is 63 years old, MUCH more varied. Also, there are 50 NA's
  #Seems to be skewed so that more "Miss." are older than younger

#Lets plot this
ggplot(misses[misses$Survived != "none",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")

#ok, apppears female children have a different survival rate
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))
    #Picked 14.5 to be less than as that was the max in the master data set

#Move onto SibSp variable, summarizing the variable
summary(data.combined$SibSp)

#Can we treat SibSp as a factor?
length(unique(data.combined$SibSp))
data.combined$SibSp <- as.factor(data.combined$SibSp)

#We believe title is predictive, visualize survival rates by sibsp, class and title
ggplot(data.combined[1:891,], aes(x= SibSp, fill= Survived)) +
  geom_bar(width=1) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill= "Survived")

#Lets do the same thing with the parch variable
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x=Parch, fill= Survived)) +
  geom_bar(width=1) +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill= "Survived")

#Lets try some feature engineering. What about creating family size feature?
temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
#Creates a new column in data.combined, counting your sibling/spouse, parent/child, and yourself
data.combined$family.size <- as.factor(temp.SibSp + temp.Parch + 1)

#Visualize to see if its predictive
ggplot(data.combined[1:891,], aes(x=family.size, fill= Survived)) +
         geom_bar(width=1) +
         facet_wrap(~Pclass + title) +
         ggtitle("Pclass, Title") +
         xlab("Family size") +
         ylab("Total Count") +
         ylim(0,300) +
         labs(fill="Survived")

#Lets take a look at the ticket variable now
str(data.combined$Ticket)
  #Based on the huge amount of values these should be characters NOT factors
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#No apparent structure. Can we find some? 
#Look at just the first character for each
ticket.first.char <- ifelse(data.combined$Ticket == ""," ", substr(data.combined$Ticket,1,1))
unique(ticket.first.char)

#Convert these to factors for analysis purpose and visualization
data.combined$Ticket.first.char <- as.factor(ticket.first.char)

#First, plot the data
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

#Ticket seems like it may be predictive. Lets investigate. Maybe 1st character being 1 means more likely to be 1?
ggplot(data.combined[1:891,], aes(x= Ticket.first.char, fill=Survived)) +
  geom_bar()+
  facet_wrap(~Pclass) +
  ggtitle("Pclass and Ticket.first.char") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill="Survived")
                                #This may be true, as seen from graph (uncertain)
#Lastly, see if we get a pattern using combination of pclass and title
ggplot(data.combined[1:891,], aes(x= Ticket.first.char, fill= Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Ticket.first.char") +
  ylab("Total count") +
  ylim(0,300) +
  labs(fill="Survived")
                            #Doesnt seem to be much predictive power in the ticket.

#Next up- the fares passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))
#282 unique ticket fares- not a good idea to make it a factor, treat as numeric and visualize

ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

#Check to see if fare is predictive
ggplot(data.combined[1:891,], aes(x= Fare, fill=Survived)) +
  geom_histogram(binwidth=5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill="Survived")

#Take a look at cabins
str(data.combined$Cabin)

#187 factors? Convert to character string
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin =="EMPTY"), "Cabin"] <- "U"
data.combined$Cabin[1:100]

#Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

#Add to combined data set and plot
data.combined$cabin.first.char <- cabin.first.char

#Plot this
ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill=Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill= "Survived")
#It seems those without cabins are more likely to perish. Perhaps labelled cabins are those in 1st or 2nd class (Doesn't tell us
#much as we already know that Pclass affects the survival rate)

ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill="Survived")
#We can see most of the empty cabins come from third class as previously guessed. 
#Cabins DONT have much predictive power, CLASS does

#Does this feature improve upon Pclass and Title?
ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill="Survived")
#Not seeing much

#What about people with multiple cabins?
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin," "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x=Cabin.multiple, fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill="Survived")
#Not seeing much

#Does survivability depend on where you got aboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)
#C=Cherbourg, Q=Queenstown, S=Southampton

#Plot for analysis
ggplot(data.combined[1:891,], aes(x=Embarked, fill=Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill="Survived")
#Doesn't seem to be much correlation between location and classes or survivability

#-------------------------------------------------------------------------------------------------------

#Starting Exploratory modelling

#-------------------------------------------------------------------------------------------------------

library(randomForest)

#Train a random forest with the default parameters, Pclass and Title
rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x= rf.train.1, y=rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

#Achieving 80% accuracy with only TWO variables; Pclass and Title. Agrees with intuition that these are most powerful
#Seems good at identifying people who die, less accurate predicting survivors

#Train a Random Forest using pclass, title & SibSp
rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x= rf.train.2, y=rf.label, importance = TRUE, ntree= 1000)
rf.2
varImpPlot(rf.2)
#More than 1% decrease in error rate. 1% increase in accuracy.

#Lets try another random forest with Parch, instead of SibSp
rf.train.3 <- data.combined[1:891, c("Pclass", "title", "Parch")]
set.seed(1234)
rf.3 <- randomForest(x=rf.train.3, y=rf.label, importance = TRUE, ntree= 1000)
rf.3
varImpPlot(rf.3)
#We improve from ORIGINAL random forest, but NOT from SibSp Random Forest

#What about them together? Are they better together?
rf.train.4 <- data.combined[1:891, c("Pclass", "title", "SibSp", "Parch")]
set.seed(1234)
rf.4 <- randomForest(x=rf.train.4, y=rf.label, importance= TRUE, ntree= 1000)
rf.4
varImpPlot(rf.4)
#They ARE better together (down approximately 2% from first Random forest)

#What about family size?
rf.train.5 <- data.combined[1:891, c("Pclass", "title", "family.size")]
set.seed(1234)
rf.5 <- randomForest(x=rf.train.5, y=rf.label, importance= TRUE, ntree=1000)
rf.5
varImpPlot(rf.5)
#Even better than Parch and SibSp together. Creates a 18.18% error rate.

#Lets try with Pclass, title, sibsp AND family size
rf.train.6 <- data.combined[1:891, c("Pclass", "title", "SibSp", "family.size")]
set.seed(1234)
rf.6 <-randomForest(x=rf.train.5, y=rf.label, importance= TRUE, ntree= 1000)
rf.6
#SAME error rate as with just family size
