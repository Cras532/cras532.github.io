
library(readxl)
library(tidyverse)
library(scales)
library(kableExtra)
library(knitr)

raw_data <- read_excel("Degree vs Income Data.xlsx")

# Identify where each section begins
masters_start <- which(raw_data$`Bachelors Degree` == "Masters Degree")
doctorate_start <- which(raw_data$`Bachelors Degree` == "Doctorate Degree")

# Split data into 3 sections
bachelors_data <- raw_data[1:(masters_start - 1), ] %>%
  mutate(DegreeLevel = "Bachelors")

masters_data <- raw_data[(masters_start + 1):(doctorate_start - 1), ] %>%
  mutate(DegreeLevel = "Masters")

doctorate_data <- raw_data[(doctorate_start + 1):nrow(raw_data), ] %>%
  mutate(DegreeLevel = "Doctorate")


# Combine all into one clean dataframe
clean_data <- bind_rows(bachelors_data, masters_data, doctorate_data) %>%
  rename(Discipline = `Bachelors Degree`)


# Optional: reorder columns
clean_data <- clean_data %>%
  relocate(DegreeLevel, .before = Discipline)


# View result
head(clean_data)




#now convert to long format for plotting
# Make salary long
salary_long <- clean_data %>%
  pivot_longer(
    cols = starts_with("Average Salary"),
    names_to = "Year",
    values_to = "Salary"
  ) %>%
  mutate(Year = str_extract(Year, "\\d+"))



# Make tuition long
tuition_long <- clean_data %>%
  pivot_longer(
    cols = starts_with("Average Tuition"),
    names_to = "Year",
    values_to = "Tuition"
  ) %>%
  mutate(Year = str_extract(Year, "\\d+"))




tidy_data <- left_join(salary_long, tuition_long,
                       by = c("DegreeLevel", "Discipline", "Year"))



ggplot(tidy_data, aes(x = Year, group = interaction(DegreeLevel, Discipline))) +
  geom_line(aes(y = Salary, color = "Salary"), size = 1) +
  geom_line(aes(y = Tuition, color = "Tuition"), size = 1) +
  facet_grid(DegreeLevel ~ Discipline, scales = "free_y") +
  scale_color_manual(values = c("Salary" = "forestgreen", "Tuition" = "red")) +
  labs(title = "Salary vs Tuition Over Time",
       x = "Year", y = "Amount ($)", color = "Legend") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#split the degree levels
# Split data into three degree levels
bachelors_data <- tidy_data %>% filter(DegreeLevel == "Bachelors")
masters_data   <- tidy_data %>% filter(DegreeLevel == "Masters")
doctorate_data <- tidy_data %>% filter(DegreeLevel == "Doctorate")


# bachelors plot
ggplot(bachelors_data, aes(x = Year, group = Discipline)) +
  geom_line(aes(y = Salary, color = "Salary"), size = 1) +
  geom_line(aes(y = Tuition, color = "Tuition"), size = 1, linetype = "dashed") +
  facet_wrap(~Discipline, scales = "free_y") +
  scale_color_manual(values = c("Salary" = "forestgreen", "Tuition" = "firebrick")) +
  labs(title = "Bachelors Degrees: Salary vs Tuition Over Time",
       x = "Year", y = "Amount ($)", color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter to 2024 data only
bachelors_2024 <- bachelors_data %>%
  filter(Year == "2024") %>%
  mutate(
    ROI = Salary / Tuition
  )

# Filter for 2024 data
bachelors_2024 <- bachelors_data %>%
  filter(Year == "2024") %>%
  mutate(
    ROI = Salary / Tuition
  ) %>%
  arrange(desc(ROI)) %>%
  mutate(Rank = row_number())  # Add a ranking column
# View full ranked table
print(bachelors_2024 %>% select(Rank, Discipline, Salary, Tuition, ROI))

ggplot(bachelors_2024, aes(x = reorder(Discipline, ROI), y = ROI)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "ROI Ranking for Bachelor's Degrees (2024)",
       x = "Discipline",
       y = "Salary-to-Tuition Ratio (ROI)") +
  theme_minimal()

bachelors_real_world <- bachelors_data %>%
  filter(Year == "2024") %>%
  mutate(
    NetIncome = Salary * 0.75,
    AnnualLoanPayment = NetIncome * 0.10,
    RealWorldPaybackYears = Tuition / AnnualLoanPayment
  ) %>%
  arrange(RealWorldPaybackYears) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Discipline, Salary, Tuition, RealWorldPaybackYears)

ggplot(bachelors_real_world, aes(x = reorder(Discipline, RealWorldPaybackYears), y = RealWorldPaybackYears)) +
  geom_col(fill = "skyblue4") +
  coord_flip() +
  labs(title = "Estimated Real-World Payback Time (Bachelor's, 2024)",
       x = "Discipline",
       y = "Years to Pay Off Tuition\n(@ 10% of Net Income)") +
  theme_minimal()

knitr::kable(bachelors_real_world, digits = 1,
             caption = "Estimated Payback Time (Real World) – Bachelor's Degrees (2024)")







# Masters plot


ggplot(masters_data, aes(x = Year, group = Discipline)) +
  geom_line(aes(y = Salary, color = "Salary"), size = 1) +
  geom_line(aes(y = Tuition, color = "Tuition"), size = 1) +
  facet_wrap(~Discipline, scales = "free_y") +
  scale_color_manual(values = c("Salary" = "forestgreen", "Tuition" = "red")) +
  labs(title = "Masters Degrees: Salary vs Tuition Over Time",
       x = "Year", y = "Amount ($)", color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter Master's data for 2024 and calculate ROI
masters_2024 <- masters_data %>%
  filter(Year == "2024") %>%
  mutate(
    ROI = Salary / Tuition
  ) %>%
  arrange(desc(ROI)) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Discipline, Salary, Tuition, ROI)

masters_real_world <- masters_data %>%
  filter(Year == "2024") %>%
  mutate(
    NetIncome = Salary * 0.75,
    AnnualLoanPayment = NetIncome * 0.10,
    RealWorldPaybackYears = Tuition / AnnualLoanPayment
  ) %>%
  arrange(RealWorldPaybackYears) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Discipline, Salary, Tuition, RealWorldPaybackYears)

ggplot(masters_2024, aes(x = reorder(Discipline, ROI), y = ROI)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "ROI Ranking – Master's Degrees (2024)",
       x = "Discipline",
       y = "Salary-to-Tuition Ratio (ROI)") +
  theme_minimal()

ggplot(masters_real_world, aes(x = reorder(Discipline, RealWorldPaybackYears), y = RealWorldPaybackYears)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Estimated Real-World Payback Time (Master's, 2024)",
       x = "Discipline",
       y = "Years to Pay Off Tuition\n(@ 10% of Net Income)") +
  theme_minimal()

knitr::kable(masters_real_world, digits = 1,
             caption = "Estimated Payback Time (Real World) – Master's Degrees (2024)")












# Doctorate Plot
ggplot(doctorate_data, aes(x = Year, group = Discipline)) +
  geom_line(aes(y = Salary, color = "Salary"), size = 1) +
  geom_line(aes(y = Tuition, color = "Tuition"), size = 1) +
  facet_wrap(~Discipline, scales = "free_y") +
  scale_color_manual(values = c("Salary" = "forestgreen", "Tuition" = "red")) +
  labs(title = "Doctorate Degrees: Salary vs Tuition Over Time",
       x = "Year", y = "Amount ($)", color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ROI and Real-World Payback for Doctorate Degrees
doctorate_2024 <- doctorate_data %>%
  filter(Year == "2024") %>%
  mutate(
    ROI = Salary / Tuition,
    NetIncome = Salary * 0.75,
    AnnualLoanPayment = NetIncome * 0.10,
    RealWorldPaybackYears = Tuition / AnnualLoanPayment
  )


ggplot(doctorate_2024, aes(x = reorder(Discipline, ROI), y = ROI)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "ROI Ranking – Doctorate Degrees (2024)",
       x = "Discipline",
       y = "Salary-to-Tuition Ratio (ROI)") +
  theme_minimal()

# Create ranked summary table
doctorate_real_world <- doctorate_2024 %>%
  arrange(RealWorldPaybackYears) %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Discipline, Salary, Tuition, RealWorldPaybackYears)


knitr::kable(doctorate_real_world, digits = 1,
             caption = "Estimated Payback Time (Real World) – Doctorate Degrees (2024)")



# Top and bottom 3 of any discipline across all degrees

# Combine all degree levels and calculate real-world payback
combined_2024 <- bind_rows(bachelors_data, masters_data, doctorate_data) %>%
  filter(Year == "2024") %>%
  mutate(
    NetIncome = Salary * 0.75,
    AnnualLoanPayment = NetIncome * 0.10,
    RealWorldPaybackYears = Tuition / AnnualLoanPayment
  )

# Add a flag to mark top or bottom 3
top_and_bottom3 <- combined_2024 %>%
  arrange(RealWorldPaybackYears) %>%
  slice(c(1:3, (n() - 2):n())) %>%
  mutate(Position = case_when(
    row_number() <= 3 ~ "top",
    TRUE ~ "bottom"
  )) %>%
  select(Position, DegreeLevel, Discipline, Salary, Tuition, RealWorldPaybackYears)

# Build styled table
top_and_bottom3 %>%
  mutate(Discipline = ifelse(
    Position == "top",
    cell_spec(Discipline, underline = TRUE, color = "darkgreen"),
    cell_spec(Discipline, underline = TRUE, color = "darkred")
  )) %>%
  select(-Position) %>%
  kable(format = "html", escape = FALSE, digits = 1,
        caption = "Top & Bottom 3 Disciplines by Real-World Payback Time – All Degree Levels (2024)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))


kable((raw_data), caption = "Degree vs Income Data")


