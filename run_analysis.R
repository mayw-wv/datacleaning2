# The code will follow the steps outlined in the project writeup.



# the data will be prefaced by "proj" except for any temporary data files or save files defined along the way

proj_header <- read.table("features.txt", sep="", header=FALSE)
proj_data <- read.table("x_train.txt", sep="", header=FALSE)
proj_data2 <- read.table("x_test.txt", sep="", header=FALSE)
# now append the two data sets: train and test and add proj_header as the names(proj_data)
proj_data <- rbind(proj_data, proj_data2)
names(proj_data) <- proj_header[,2]


# now read in the y-train/test and subject train/test data sets
y_train_data <- read.table("y_train.txt", sep="", header=FALSE)
y_test_data <- read.table("y_test.txt", sep="", header=FALSE)
subject_train_data <- read.table("subject_train.txt", sep="", header=FALSE)
subject_test_data <- read.table("subject_test.txt", sep="", header=FALSE)

# now append train and test, then add in the other columns to the proj_data set
activity_type <- rbind(y_train_data, y_test_data)
subject <- rbind(subject_train_data, subject_test_data)

proj_data <- cbind(activity_type, subject, proj_data)

names(proj_data)[1] <- "activity"
names(proj_data)[2] <- "subject"

# Step 1 - at this point the data have been merged  


#  select the attributes that have mean or std in them - keep the first two columns, must use grepl
mean_std <- c(TRUE, TRUE)
for (i in c(3:length(names(proj_data) ) ) ) mean_std[i]<-grepl("mean", names(proj_data)[i] ) |grepl("std", names(proj_data)[i])   
proj_data_mean_std <- proj_data[,mean_std]


# take out variable with "meanFreq" - that is NOT the same as mean since parallel variables exist like 
# "fBodyGyro-mean()-Z" and "fBodyGyro-meanFreq()-Z"
pos_meanFreq <- grep("meanFreq", names(proj_data_mean_std))
proj_data_mean_std <- proj_data_mean_std[,-pos_meanFreq]
proj_data_mean_std_save <- proj_data_mean_std      # save a copy to double check calculations later

# Step 2 - at this point only mean and std columns are left


# the next step is to use descriptive labels to name the activities: the mapping will be activity: 1 -> Walking  2 -> Walking_upstairs
# 3 ->  Walking_downstairs  4 -> Sitting  5 -> Standing  6 - Laying

f_name <- function(iType) { activity_names <- c("Walking", "Walking_upstairs", "Walking_downstairs", "Sitting", "Standing", "Laying")
                            activity_names[iType]}

proj_data_mean_std$activity <- f_name(proj_data_mean_std$activity)  # overwrite activity column with the new descriptive labels

# Step 3 - at this point the descriptive activity names have been added to the data set

# the next step 4 will take more code.  The naming method is described in the code book but for completeness
# it is repeated here.  A variable like "tBodyAcc-mean()-y" becomes "mean(y, tBodyAcc)" more like an "R" function.
# the pattern is: function(coordinate, measurement) for example: function = mean, coordinate = y and measurement is "tBodyAcc"
# Another form is function(measurement_type) if no coordinates X, Y, or Z are involved.  Similar functions for "std" are also defined.
# These functions are used on the names(proj_data_mean_std) to change the names then these names are used going forward.
# Four functions are defined that change the variable names to the forms.
# this function is used for mean(x, measurement) names, f_std is same except for std names, the suffix 2 means the
# measurement only form (e.g. mean(tBodyAcc))
f_mean <- function(char_exp)  {
  pos_mean <- regexpr("mean", char_exp)      # position of substring "mean"  the X Y or Z will be 7 places further on 
  front = substr(char_exp, 1, pos_mean-2)        # the front part of the name will end 2 positions before
  coor = substr(char_exp, pos_mean+7,pos_mean+7) # the X, Y or Z coordinate
  
  paste("mean(",coor,",",front,")")
}

f_std <- function(char_exp)  {
  pos_mean <- regexpr("std", char_exp)       # position of substring "std"  the X Y or Z will be 6 places further on 
  front = substr(char_exp, 1, pos_mean-2)        # the front part of the name will end 2 positions before
  coor = substr(char_exp, pos_mean+6,pos_mean+6) # the X, Y or Z coordinate
  
  paste("std(",coor,",",front,")")
}

f_mean2 <- function(char_exp)  {
  pos_mean <- regexpr("mean", char_exp)      # position of substring "mean"  
  front = substr(char_exp, 1, pos_mean-2)        # the front part of the name will end 2 positions before
  
  paste("mean(",front,")")
}

f_std2 <- function(char_exp)  {
  pos_mean <- regexpr("std", char_exp)      # position of substring "mean"  
  front = substr(char_exp, 1, pos_mean-2)        # the front part of the name will end 2 positions before
  
  paste("std(",front,")")
}


f_var_name <- function(char_exp) {
            if (grepl("mean", char_exp) &  ((regexpr("mean", char_exp) + 6 ) < nchar(char_exp) ) ) {result <- f_mean(char_exp)}
            else
            if (grepl("mean", char_exp) &  ((regexpr("mean", char_exp) + 6 ) > nchar(char_exp) ) ) {result <- f_mean2(char_exp)}
            else
            if (grepl("std", char_exp) &  ((regexpr("std", char_exp) + 5 ) < nchar(char_exp) ) )  {result <- f_std(char_exp)}
            else
            if (grepl("std", char_exp) &   ((regexpr("std", char_exp) + 5 ) > nchar(char_exp) ) ) {result <- f_std2(char_exp)}
            #   print(paste(char_exp, result) )
            result
                                 }
# the following loop goes through the names of the main data set and changes all variable names according to the above
# naming convention
temp_names <- c("activity", "subject")   # initialize

for (i in c(3:length(names(proj_data_mean_std)))) {char_exp <- temp[i]
                   temp_names <- c(temp_names,f_var_name(char_exp) ) }


names(proj_data_mean_std) <- temp_names    
proj_data <- proj_data_mean_std   #  drop the "_mean_std" suffix to shorten the data file name.  "proj_data" is the Step 4 output data file.

# Step 4 is completed at this point


# now need to aggregate over subject and activity type - for each column


tidy_data <- aggregate(proj_data, list(activity=proj_data[,1], subject=proj_data[,2]), mean)
tidy_data <- tidy_data[,-c(3,4)]  # remove excess

# output data
write.table(tidy_data, file= "tidy.txt", row.name=FALSE)

# Step 5 is complete at this point "tidy" is the 2nd independant tidy data set mentioned in the directions.
