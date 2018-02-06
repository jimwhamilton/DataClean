# CodeBook for Coursera Programming Assignment 

## Getting and Cleaning Data: Week 4

### The features selected for this database come from the following:-

* They were recorded for a number of subjects who performed the activity for each window sample. Its range is from 1 to 30 and are labelled in this dataset as SubjectID.

* The activity being performed at the time of is called Activity and can have one of six values "WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS
", "SITTING", "STANDING
" and "LAYING
".

* Accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

### These signals were used to estimate variables of the feature vector for each pattern:  
### '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag

### The set of variables that were estimated from these signals are: 

* mean: Mean value
* std: Standard deviation

### These variables were then averaged by SubjectID and Activity.



### Notes: 
Features are normalized and bounded within [-1,1].


### References:

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012





