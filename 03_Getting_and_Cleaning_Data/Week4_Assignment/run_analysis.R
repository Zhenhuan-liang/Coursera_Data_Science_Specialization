
# 1. Load X train and test data, and merges to one data set.
X_train <- fread("./UCI HAR Dataset/train/X_train.txt")
X_test <- fread("./UCI HAR Dataset/test/X_test.txt")
X <- rbind(X_train, X_test)

# 2. Load eeach features names, reset it to X
df_features <- fread('./UCI HAR Dataset/features.txt')
names_features <- df_features$V2
names(X) <- names_features

# 3. Load y train and test data, and merges to one data set.
y_train <- fread('./UCI HAR Dataset/train/y_train.txt')
y_test <- fread('./UCI HAR Dataset/test/y_test.txt')
y <- rbind(y_train, y_test)
names(y) <- "y"

# 4. Load subject train and test data, and merges to one data set.
subject_train <- fread('./UCI HAR Dataset/train/subject_train.txt')
subject_test <- fread('./UCI HAR Dataset/test/subject_test.txt')
subject <- rbind(subject_train, subject_test)
names(subject) <- "subject"

# 5. Merge X, y and subject to one data set.
data <- cbind(subject, X, y)

# 6. Using `grep()` to select variables which including character "mean" and "std".
select_cols <- c(grep("mean|std|subject", names(data)), 563)
sub_data <- data[,..select_cols]
drop_cols <- grep("meanFreq", names(sub_data))
data_mean_std <- sub_data[, -..drop_cols]

# 7. Load activity labels name, and convert the number of y to character
activity_labels <- fread('UCI HAR Dataset/activity_labels.txt')
names(activity_labels) <- c("y", "activity")
data_mean_std <- merge(data_mean_std, activity_labels, by = "y")
data_mean_std <- data_mean_std[, !"y"]

# 8. Average of each variable for each activity and each subject
avg_var_activity_subject <- data_mean_std %>% 
        group_by(activity, subject) %>% 
        summarise_all(mean)
