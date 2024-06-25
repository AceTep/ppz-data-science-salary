install.packages('tidyverse')
install.packages('lubridate')
install.packages('dplyr')
install.packages('ggplot2')

library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Read the dataset
data <- read.csv("DataScience_salaries_2024.csv")
head(data)


# unite
data <- unite(data, "job", experience_level, job_title, sep = " - ")
head(data)

# razdioba frekvencija
frequency_distribution <- table(data$job)
frequency_distribution_job_sorted <- sort(frequency_distribution, decreasing = TRUE)
frequency_distribution_job_sorted
top_5_jobs <- head(frequency_distribution_job_sorted, 10)

# Create the bar plot for top 5 job titles
barplot(top_5_jobs, 
        main = "Top 5 Job Titles by Frequency",
        xlab = "Job Title",
        ylab = "Frequency",
        col = "skyblue",
        border = "black",
        las = 2,  # Make the x-axis labels perpendicular to the axis
        cex.names = 0.8)  # Adjust the size of the x-axis labels

# Improved bar plot with ggplot2
top_5_jobs_df <- as.data.frame(top_5_jobs)
colnames(top_5_jobs_df) <- c("JobTitle", "Frequency")

ggplot(top_5_jobs_df, aes(x = reorder(JobTitle, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Top 10 Job Titles by Frequency", x = "Job Title", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12))


# Convert work_year to date
data <- data %>%
  mutate(date = ymd(paste0(work_year, "-01-01"))) %>%
  select(-work_year)

# Extract year and month from the date
data <- data %>%
  mutate(year = year(date),
         month = month(date))
head(data)
# Group by year and calculate average salary in USD
data_summary <- data %>%
  group_by(year) %>%
  summarise(avg_salary_usd = mean(salary_in_usd, na.rm = TRUE))

data_summary

ggplot(data_summary, aes(x = factor(year), y = avg_salary_usd)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Salary in USD by Year",
       x = "Year",
       y = "Average Salary (USD)") +
  theme_minimal()



# mjere centralne tendencije

# Prosjek
mean_salary <- mean(data$salary_in_usd , na.rm = TRUE)
mean_salary

# Medijan
median_salary <- median(data$salary_in_usd , na.rm = TRUE)
median_salary


# Mod
tablica <- table(data$salary_in_usd)
modus=names(tablica)[tablica == max(tablica)]
cat("\nModus:", modus)

prvi_kvartil <- quantile(data$salary_in_usd, 0.25)
drugi_kvartil <- median(data$salary_in_usd)
treci_kvartil <- quantile(data$salary_in_usd, 0.75)
prvi_kvartil
drugi_kvartil
treci_kvartil




srednja_vrijednost_place <- aggregate(salary_in_usd ~ job + employee_residence + year, data = data, FUN = mean)
srednja_vrijednost_place <- arrange(srednja_vrijednost_place, desc(salary_in_usd))
head(srednja_vrijednost_place,15)

data <- separate(data, job, into = c("experience_level", "job_title"), sep = " - ")
head(data)

mean_salary <- aggregate(salary_in_usd ~ job_title, data = data, FUN = mean)
mean_salary

data$monthly_salary_usd <- data$salary_in_usd / 12
head(data)


hist(data$monthly_salary_usd, 
     main = "Histogram of Monthly Salary in USD", 
     xlab = "Monthly Salary in USD", 
     ylab = "Frequency",
     col = "skyblue",
     border = "black")






final_data <- data %>%
  filter(experience_level == "MI" & year == 2021) %>%
  select(year, job_title, salary_in_usd, monthly_salary_usd, employee_residence) %>%
  mutate(salary_ratio = salary_in_usd / (monthly_salary_usd * 12)) %>% arrange(desc(monthly_salary_usd)) 

final_data

final_data <- data %>%
  select(year, job_title, salary_in_usd, monthly_salary_usd, employee_residence) %>%
  mutate(bonus_10_percent_month = monthly_salary_usd * 0.10) %>% arrange(desc(monthly_salary_usd))

final_data

final_data <- data %>%
  filter(experience_level == "MI") %>%
  select(year, job_title, salary_in_usd, monthly_salary_usd, employee_residence) %>%
  mutate(bonus_10_percent_month = monthly_salary_usd * 0.10)

final_data = head(final_data, 15)

ggplot(final_data, aes(x = job_title, y = bonus_10_percent_month, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "10% Monthly Bonus by Job Title", x = "Job Title", y = "10% Monthly Bonus (USD)", fill = "Work Year")



average_salary_by_country <- data %>%
  group_by(year, employee_residence) %>%
  summarise(average_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
  ungroup()

# Identify the country with the highest average salary for each year
best_average_paycheck_country <- average_salary_by_country %>%
  group_by(year) %>%
  filter(average_salary == max(average_salary)) %>%
  ungroup()
options(scipen = 999)
# Print the result to check
print(best_average_paycheck_country)

# Plot the best average paycheck country for each year
ggplot(best_average_paycheck_country, aes(x = factor(year), y = average_salary, fill = employee_residence)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  theme_minimal(base_size = 15) +
  labs(
    title = "Country with the Best Average Paycheck for Each Year",
    x = "Work Year",
    y = "Average Salary (USD)",
    fill = "Country"
  ) +
  scale_fill_brewer(palette = "Set3")



