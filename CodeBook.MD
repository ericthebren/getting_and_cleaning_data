Introduction

	The script, run_analysis.R has a number of steps:
	- obtain data.
	- consolidate data together.
	- limit measured features to subset of columns which contains either mean or standard devication in their description.
	- add descriptive names for the activities.
	- take the mean for the feature data, grouped by and sorted by subject and activity.
	- output a clean data file with summary statistics called tidy.csv.  This file is loaded in the repository.

	
Variables

	Downloaded Data Variables
	The following variables are use as references for each downloaded file
	- activitiesLabels
	- features
	- subject_text
	- subject_train
	- data_test
	- data_train
	- activity_test
	- activity_train
	
	Merged Reference Variables
	The following variables are intermediate steps, merging the downloaded data variables together:
	- data_trainandtest: merging data_test and data_train
	- activity_trainandtest: merging activity_test and activity_train
	- subject_trainandtest: merging subject_test and subject_train
	- subject_activity: merging activity_trainandtest with activity_trainandtest
	- AllData: merging subject_activity with data_trainandtest
	
	Filtered Variables
	The following variables were used to filter AllData, to arrive at the final output
	- mergemeansd: filtering AllData to show only mean and standard deviation columns
	- meanmeasure: creating mean values for subject and activity based on mergemeansd variables