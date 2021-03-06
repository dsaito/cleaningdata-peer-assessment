General Information
===================

The variables in the original data were obtained from the Human Activity Recognition Using
Smartphones Dataset, as described in the "Original Features" section of this document. 
In all, there is a total of 30 participants, divided in 2 groups: trainning and test. 
The variables and values are analyzed according to Coursera's Getting and Cleaning Data Peer Assessment, as decribed step by step in the README.txt file. In brief, after the run_analysis.R 
program is run, the results of both study groups (test and trainning) are joined into a single
set, and only the variables containing the MEAN or STANDARD DEVIATION are kept. The final result is achieved by further taking the MEAN of every variable selected in the last step, according to subject and activity (walking, walking downstairs, walking upstairs, sitting, standing and laying).

To achieve these results, the dplyr R package was used to to create groups of data, 
calculate the mean, and summarize the results. These steps are fully documented inside the 
run_analysis.R script itself in the form of comment lines, so you can follow along the analysis process, if you desire.

Output Features - MEAN and STANDARD DEVIATION variables
=======================================================

After the run_analysis.R program is run, the features in the output file (Peer Assessment.txt) 
are only those containing the MEAN and STANDARD DEVIATION, as described below. XYZ denote three different axis variables (X, Y and Z), so for each of these variables, there will be a total of 3 in the dataset. However, it's important to note that not all of these variables may be present in the output. The units (and meaning) of each variable are described down below, in the "Original Features" section. In brief, variables preceeded by 'f' correspond to FREQUENCY measurements, whereas those preceeded by 't' are TIME measurements:

	tBodyAcc-XYZ-mean
	tGravityAcc-XYZ-mean
	tBodyAccJerk-XYZ-mean
	tBodyGyro-XYZ-mean
	tBodyGyroJerk-XYZ-mean
	tBodyAccMag-mean
	tGravityAccMag-mean
	tBodyAccJerkMag-mean
	tBodyGyroMag-mean
	tBodyGyroJerkMag-mean
	fBodyAcc-XYZ-mean
	fBodyAccJerk-XYZ-mean
	fBodyGyro-XYZ-mean
	fBodyAccMag-mean
	fBodyAccJerkMag-mean
	fBodyGyroMag-mean
	fBodyGyroJerkMag-mean

	gravityMean
	tBodyAccMean
	tBodyAccJerkMean
	tBodyGyroMean
	tBodyGyroJerkMean

	tBodyAcc-XYZ-std
	tGravityAcc-XYZ-std
	tBodyAccJerk-XYZ-std
	tBodyGyro-XYZ-std
	tBodyGyroJerk-XYZ-std
	tBodyAccMag-std
	tGravityAccMag-std
	tBodyAccJerkMag-std
	tBodyGyroMag-std
	tBodyGyroJerkMag-std
	fBodyAcc-XYZ-std
	fBodyAccJerk-XYZ-std
	fBodyGyro-XYZ-std
	fBodyAccMag-std
	fBodyAccJerkMag-std
	fBodyGyroMag-std
	fBodyGyroJerkMag-std

Original Features  
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw
signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured
at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass
Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration
signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-
XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk
signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional
signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag,
tBodyGyroMag, tBodyGyroJerkMag).

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ,
fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to
indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for each pattern:   '-XYZ' is
used to denote 3-axial signals in the X, Y and Z directions.

	tBodyAcc-XYZ
	tGravityAcc-XYZ
	tBodyAccJerk-XYZ
	tBodyGyro-XYZ
	tBodyGyroJerk-XYZ
	tBodyAccMag
	tGravityAccMag
	tBodyAccJerkMag
	tBodyGyroMag
	tBodyGyroJerkMag
	fBodyAcc-XYZ
	fBodyAccJerk-XYZ
	fBodyGyro-XYZ
	fBodyAccMag
	fBodyAccJerkMag
	fBodyGyroMag
	fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

	mean(): Mean value
	std(): Standard deviation
	mad(): Median absolute deviation 
	max(): Largest value in array
	min(): Smallest value in array
	sma(): Signal magnitude area
	energy(): Energy measure. Sum of the squares divided by the number of values. 
	iqr(): Interquartile range 
	entropy(): Signal entropy
	arCoeff(): Autorregresion coefficients with Burg order equal to 4
	correlation(): correlation coefficient between two signals
	maxInds(): index of the frequency component with largest magnitude
	meanFreq(): Weighted average of the frequency components to obtain a mean frequency
	skewness(): skewness of the frequency domain signal 
	kurtosis(): kurtosis of the frequency domain signal 
	bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
	angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

	gravityMean
	tBodyAccMean
	tBodyAccJerkMean
	tBodyGyroMean
	tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'
