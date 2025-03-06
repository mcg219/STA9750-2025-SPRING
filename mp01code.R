library(dplyr)
library(readr)

nyc_payroll <- read.csv("nyc_payroll_export.csv", stringsAsFactors = FALSE)


# Create unique employee_id (for later grouping)
nyc_payroll$employee_id <- paste(
  nyc_payroll$first_name,
  nyc_payroll$last_name,
  nyc_payroll$mid_init,
  sep = "_"
)

# Compute per-row total pay based on pay_basis
nyc_payroll <- nyc_payroll %>%
  mutate(
    row_total_pay = case_when(
      pay_basis == "per Annum" ~ base_salary,
      pay_basis == "per Hour"  ~ (base_salary * regular_hours) + (base_salary * ot_hours * 1.5),
      pay_basis == "per Day"   ~ {
        hr_rate <- base_salary / 7.5
        (hr_rate * regular_hours) + (hr_rate * ot_hours * 1.5)
      },
      TRUE ~ NA_real_
    )
  )

# Intro Questions
row_highest_base <- nyc_payroll[which.max(nyc_payroll$base_salary), ]
highest_base_title <- row_highest_base$title_description

employee_year_pay <- nyc_payroll %>%
  group_by(fiscal_year, employee_id) %>%
  summarize(
    total_pay = sum(row_total_pay, na.rm = TRUE),
    first_name = first(first_name),
    last_name = first(last_name),
    mid_init = first(mid_init),
    title_description = first(title_description),
    agency_name = first(agency_name),
    .groups = "drop"
  )

row_highest_total <- employee_year_pay[which.max(employee_year_pay$total_pay), ]
highest_total_individual <- row_highest_total[, c("first_name", "last_name", "mid_init", "fiscal_year")]

ot_by_employee <- nyc_payroll %>%
  group_by(employee_id) %>%
  summarize(
    total_ot_hours = sum(ot_hours, na.rm = TRUE),
    first_name = first(first_name),
    last_name = first(last_name),
    mid_init = first(mid_init),
    .groups = "drop"
  )
row_most_ot <- ot_by_employee[which.max(ot_by_employee$total_ot_hours), ]
individual_most_ot <- row_most_ot[, c("first_name", "last_name", "mid_init")]

agency_averages <- nyc_payroll %>%
  group_by(agency_name) %>%
  summarize(avg_total_pay = mean(row_total_pay, na.rm = TRUE), .groups = "drop")
highest_average_agency <- agency_averages[which.max(agency_averages$avg_total_pay), ]

count_by_agency_year <- nyc_payroll %>%
  group_by(fiscal_year, agency_name) %>%
  summarize(num_unique_employees = n_distinct(employee_id), .groups = "drop")
most_employees_by_year <- count_by_agency_year %>%
  group_by(fiscal_year) %>%
  slice_max(order_by = num_unique_employees, n = 1) %>%
  ungroup()

ot_ratio_by_agency <- nyc_payroll %>%
  group_by(agency_name) %>%
  summarize(
    total_ot_hours = sum(ot_hours, na.rm = TRUE),
    total_regular_hours = sum(regular_hours, na.rm = TRUE),
    ot_ratio = ifelse(total_regular_hours == 0, NA, total_ot_hours / total_regular_hours),
    .groups = "drop"
  )
highest_ot_ratio_agency <- ot_ratio_by_agency[which.max(ot_ratio_by_agency$ot_ratio), ]

five_boroughs <- c("BRONX", "QUEENS", "BROOKLYN", "MANHATTAN", "STATEN ISLAND")
outside_data <- nyc_payroll %>%
  filter(pay_basis == "per Annum", !(work_location_borough %in% five_boroughs))
avg_salary_outside <- mean(outside_data$base_salary, na.rm = TRUE)

annual_payroll <- nyc_payroll %>%
  group_by(fiscal_year) %>%
  summarize(total_annual_pay = sum(row_total_pay, na.rm = TRUE), .groups = "drop")
earliest_year <- min(annual_payroll$fiscal_year, na.rm = TRUE)
latest_year <- max(annual_payroll$fiscal_year, na.rm = TRUE)
pay_earliest <- annual_payroll$total_annual_pay[annual_payroll$fiscal_year == earliest_year]
pay_latest <- annual_payroll$total_annual_pay[annual_payroll$fiscal_year == latest_year]
payroll_growth <- pay_latest - pay_earliest

#Policy 1
mayor_year_pay <- employee_year_pay %>%
  filter(title_description == "MAYOR") %>%
  group_by(fiscal_year) %>%
  summarize(mayor_total_pay = sum(total_pay, na.rm = TRUE), .groups = "drop")
emp_vs_mayor <- employee_year_pay %>%
  left_join(mayor_year_pay, by = "fiscal_year") %>%
  mutate(
    above_mayor = total_pay > mayor_total_pay,
    amount_above = ifelse(above_mayor, total_pay - mayor_total_pay, 0)
  )
employees_above_mayor <- emp_vs_mayor %>% filter(above_mayor)
total_savings_mayor_cap <- sum(emp_vs_mayor$amount_above, na.rm = TRUE)
impact_by_agency_title <- emp_vs_mayor %>%
  filter(above_mayor) %>%
  group_by(agency_name, title_description) %>%
  summarize(
    num_employees_capped = n(),
    total_cut_amount = sum(amount_above, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_cut_amount))

#Policy 2
nyc_payroll <- nyc_payroll %>%
  mutate(
    base_hourly_rate = case_when(
      pay_basis == "per Hour" ~ base_salary,
      pay_basis == "per Day"  ~ base_salary / 7.5,
      TRUE                    ~ NA_real_
    ),
    ot_premium_paid = case_when(
      pay_basis %in% c("per Hour", "per Day") ~ 0.5 * base_hourly_rate * ot_hours,
      TRUE                                   ~ 0
    )
  )
full_time_hours <- 2080
ot_analysis <- nyc_payroll %>%
  group_by(agency_name, title_description) %>%
  summarize(
    total_ot_hours   = sum(ot_hours, na.rm = TRUE),
    total_ot_premium = sum(ot_premium_paid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    full_time_equivalents = total_ot_hours / full_time_hours,
    potential_savings     = total_ot_premium
  )
agency_savings <- ot_analysis %>%
  group_by(agency_name) %>%
  summarize(total_agency_savings = sum(potential_savings, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_agency_savings))
