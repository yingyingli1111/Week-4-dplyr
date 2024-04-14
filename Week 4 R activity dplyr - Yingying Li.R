#Week 4: dplyr package

#Task: Write the function to get a dataset from Base R: Titanic
#and give the dataframe a new name of your choice
#(hint: you will want your data to be a dataframe. Use the function: as.data.frame(Titanic))

Titanic <- as.data.frame(Titanic)
#See the top rows of the data
#TASK: Write the function to see the top rows of the data
head(Titanic)
#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr
install.packages('dplyr')
library(dplyr)
#Let's just see the Survived and Sex columns
#Task: Write the function to 'select' the Survived and Sex columns 
#(hint: use the 'select' function)
select(Titanic,Survived, Sex)
#Let's name the dataset with just the two columns, Survived and Sex
#TASK: Write the function to save the two columns as one new dataset
#and give it a name
subset1 <- select(Titanic,Survived, Sex)

#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a -column)
subset1 <- select(subset1,Survived, -Sex)
#Let's rename a column name
#TASK: Write the function that renames 'Sex' to 'Gender'
rename <- function(data){
  data <- data %>% 
    rename(Gender = Sex)
  return(data)
}
#Let's make a new dataframe with the new column name
#TASK: Write the function that names a new dataset that includes the 'gender' column
Titanic_rename <- rename(Titanic)

#Let's 'filter' just the males from our dataset
#TASK: Write the function that includes only rows that are 'male'
filter_male <- function(data, column_name = "Gender") {
  filtered_data <- data %>%
    dplyr::filter(.data[[column_name]] == "Male")
  return(filtered_data)
}
male_only <- filter_male(Titanic_rename, "Gender")


#Let's 'arrange' our data by gender (not the data you just filtered)
#TASK: Write the function to group the data by gender (hint: arrange())
group_by_gender <- function(data, column_name = "Gender") {
  grouped_data <- data %>%
    group_by(.data[[column_name]])
  return(grouped_data)
}
titanic_arrange <- group_by_gender(Titanic_rename,"Gender")
#Let's see how many people were examined in the dataset (total the frequency in the original dataframe)
#TASK: Sum the Freq column
people_count <- sum(titanic$Freq)
people_count
#TASK: After you run it, write the total here:2201

#Since we have a males dataset, let's make a females dataset
#TASK: Write the function that includes only rows that are 'female'
filter_female <- function(data, column_name = "Gender") {
  filtered_data <- data %>%
    dplyr::filter(.data[[column_name]] == "Female")
  return(filtered_data)
}
female_only <- filter_female(Titanic_rename, "Gender")

#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')
union_set <- function(subset1, subset2) {
  combined_df <- bind_rows(subset1, subset2)
  return(combined_df)
}
combined_set <- union_set(male_only, female_only)
#Optional Task: add any of the other functions 
#you learned about from the dplyr package

# let's sort by freq
sort_by_freq <- function(df){
  sorted_df <- df %>%
    arrange(Freq)
  return(sorted_df)
}
sort_freq <- sort_by_freq(combined_set)
