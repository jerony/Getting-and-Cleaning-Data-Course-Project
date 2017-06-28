The script begins by importing the train data set into R. The information from the "features info" file is also imported into R. Each column is assigned the appropriate label from the "features info" file. The activity labels are also imported, and a function modifies these labels to be self-descriptive, (for example, the function converts the input 1 to "Walking", the activity associated with this label). These self-descriptive activity labels are stored in a column called ActivityLabels. Finally, the subject labels are imported and appended to the test data set in a column called SubjectLabel. Once imported, the same operations, (excluding identifying the columns with the features labels), are performed on the test data set; the tidy test data set is then attached to the train data set, and the rows of the merged table are rearranged by subject label. The mean and sd feature columns are then extracted from this merged data set, and grouped by subject label and activity label. The mean of each extracted feature, (grouped by subject and activity), is taken and organized in the final product of the script: the GroupedData table.
