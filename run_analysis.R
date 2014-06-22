X_test <- read.table("./test/X_test.txt")
X_train <- read.table("./train/X_train.txt")
y_test <- read.table("./test/y_test.txt")
y_train <- read.table("./train/y_train.txt")
ytrainvec <- y_train$V1
fy_train <- factor(ytrainvec, labels = c("Walking", "Walking_Upstairs", 
	"Walking_Downstairs", "Sitting", "Standing", "Laying"))
ytestvec <- y_test$V1
fy_train1 <- data.frame(fy_train)
fy_test <- factor(ytestvec, labels = c("Walking", "Walking_Upstairs", 
	"Walking_Downstairs", "Sitting", "Standing", "Laying"))
fy_test1 <- data.frame(fy_test)
variable <- c(1:6, 41:46, 81:86, 121:126, 161:166, 201, 202, 
	214, 215, 227, 228, 240, 241, 253, 254, 267:271, 345:350, 
	424:429, 503, 504, 516, 517, 529, 530, 542, 543)

##now read features.txt and select V2, create a vector of names from variable. 
s <- cbind(fy_test1, X_test[variable])
r <- cbind(fy_train1, X_train[variable])
features <- read.table("features.txt")
features1 <- as.character(features$V2[variable])
n <- c("Activity")
varNames <- c(n, features1)
names(s) <- varNames
names(r) <- varNames
big_tidy1 <- rbind(s, r)
write.table(big_tidy1, file = "tidy.txt")

##Doing another thing now. 
test_with_subjects <- cbind(read.table("./test/y_test.txt"), 
	X_test)
test_with_subjects_and_acts <- cbind(read.table("./test/subject_test.txt"), 
	test_with_subjects)
train_with_subjects <- cbind(read.table("./train/y_train.txt"), 
	X_train)
train_with_subjects_and_acts <- cbind(read.table("./train/subject_train.txt"), 
	train_with_subjects)
big_messy <- rbind(test_with_subjects_and_acts, train_with_subjects_and_acts)
features2 <- as.character(features$V2)
names(big_messy) <- c("subjects", "activity", features2)
sp <- split(big_messy, list(big_messy$subjects, big_messy$activity))
columnsmean <- function(y, removeNA = TRUE) {
	nc <- ncol(y)
	means <- numeric(nc)
	for (i in 1:nc) {
		means[i] <- mean(y[, i], na.rm = removeNA)

	}
	means
}
ls <- lapply(sp, columnsmean)
recover <- t(as.data.frame(ls))
colnames(recover) <- c("subjects", "activity", features2)
recovery <- as.data.frame(recover)
fac <- factor(recovery$activity, labels = c("Walking", "Walking_Upstairs", 
	"Walking_Downstairs", "Sitting", "Standing", "Laying"))
fac1 <- data.frame(fac)
recovery$activity <- fac1
write.table(recovery, file = "means_by_subject_and_activity.txt")
