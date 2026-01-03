# 10 Insights from South Carolina School Enrollment Data

``` r
library(scschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))
```

This vignette explores South Carolina’s public school enrollment data,
surfacing key trends and demographic patterns across 13 years of data
(2013-2025).

------------------------------------------------------------------------

## 1. South Carolina is growing

Unlike many states facing enrollment decline, South Carolina has added
approximately 50,000 students since 2013. The Palmetto State’s
population growth is reflected in its schools.

``` r
enr <- fetch_enr_multi(c(2013, 2015, 2017, 2019, 2021, 2023, 2025))

state_totals <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(
    change = n_students - lag(n_students),
    pct_change = round(change / lag(n_students) * 100, 2)
  )

state_totals
```

``` r
ggplot(state_totals, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#73000A") +
  geom_point(size = 3, color = "#73000A") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "South Carolina Public School Enrollment (2013-2025)",
    subtitle = "Steady growth with a pandemic dip in 2021",
    x = "School Year (ending)",
    y = "Total Enrollment"
  )
```

------------------------------------------------------------------------

## 2. Greenville County is the giant

Greenville County Schools enrolls nearly 77,000 students, making it the
largest district in the state and one of the largest in the Southeast.

``` r
enr_2025 <- fetch_enr(2025)

top_districts <- enr_2025 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

top_districts
```

``` r
top_districts |>
  mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
  ggplot(aes(x = n_students, y = district_name, fill = district_name)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(n_students)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  labs(
    title = "Top 10 South Carolina Districts by Enrollment (2025)",
    subtitle = "Greenville County leads with 10% of the state's students",
    x = "Number of Students",
    y = NULL
  )
```

------------------------------------------------------------------------

## 3. Hispanic enrollment is surging

Hispanic student enrollment has more than doubled over the past decade,
growing from about 7% to over 12% of total enrollment.

``` r
demographics <- enr_2025 |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian",
                         "american_indian", "pacific_islander", "multiracial")) |>
  mutate(pct = round(n_students / sum(n_students, na.rm = TRUE) * 100, 1)) |>
  select(subgroup, n_students, pct) |>
  arrange(desc(n_students))

demographics
```

``` r
demographics |>
  mutate(subgroup = forcats::fct_reorder(subgroup, n_students)) |>
  ggplot(aes(x = n_students, y = subgroup, fill = subgroup)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "South Carolina Student Demographics (2025)",
    subtitle = "A changing student population reflects statewide demographic shifts",
    x = "Number of Students",
    y = NULL
  )
```

------------------------------------------------------------------------

## 4. The I-85 Corridor is booming

Districts along the I-85 corridor from Greenville through Spartanburg
are among the fastest-growing in the state, fueled by economic
development and migration from other states.

``` r
i85_districts <- enr_2025 |>
  filter(
    grepl("Greenville|Spartanburg|Anderson", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(8)

i85_districts
```

``` r
i85_districts |>
  mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
  ggplot(aes(x = n_students, y = district_name, fill = district_name)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(n_students)), hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +
  labs(
    title = "I-85 Corridor Districts (2025)",
    subtitle = "The Upstate drives South Carolina's enrollment growth",
    x = "Number of Students",
    y = NULL
  )
```

------------------------------------------------------------------------

## 5. The Lowcountry is expanding

Charleston, Berkeley, and Dorchester counties form South Carolina’s
tri-county Lowcountry region, and all three have seen substantial
enrollment growth.

``` r
lowcountry_enr <- fetch_enr_multi(c(2015, 2020, 2025))

lowcountry <- lowcountry_enr |>
  filter(
    grepl("Charleston|Berkeley|Dorchester", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  select(end_year, district_name, n_students) |>
  pivot_wider(names_from = end_year, values_from = n_students) |>
  mutate(
    growth = `2025` - `2015`,
    pct_growth = round(growth / `2015` * 100, 1)
  ) |>
  arrange(desc(growth))

lowcountry
```

``` r
lowcountry |>
  mutate(district_name = forcats::fct_reorder(district_name, growth)) |>
  ggplot(aes(x = growth, y = district_name, fill = pct_growth)) +
  geom_col() +
  geom_text(aes(label = paste0("+", scales::comma(growth))), hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.2))) +
  scale_fill_gradient(low = "#56B4E9", high = "#0072B2", name = "% Growth") +
  labs(
    title = "Lowcountry Enrollment Growth (2015-2025)",
    subtitle = "Berkeley County leads the tri-county region",
    x = "Student Growth",
    y = NULL
  )
```

------------------------------------------------------------------------

## 6. More than half are economically disadvantaged

Approximately 58% of South Carolina students qualify for free or
reduced-price lunch, with rates exceeding 85% in some rural districts.

``` r
econ_state <- enr_2025 |>
  filter(is_state, grade_level == "TOTAL", subgroup == "econ_disadv") |>
  select(subgroup, n_students, pct) |>
  mutate(pct = round(pct * 100, 1))

econ_state
```

``` r
# Districts with highest rates
econ_districts <- enr_2025 |>
  filter(is_district, grade_level == "TOTAL", subgroup == "econ_disadv") |>
  filter(!is.na(pct)) |>
  arrange(desc(pct)) |>
  select(district_name, n_students, pct) |>
  mutate(pct = round(pct * 100, 1)) |>
  head(10)

econ_districts
```

------------------------------------------------------------------------

## 7. State-authorized charters are growing fast

The SC Public Charter School District (code 900) serves state-authorized
charter schools and has grown to over 30,000 students.

``` r
charter_enr <- fetch_enr_multi(c(2015, 2020, 2025))

charter_trends <- charter_enr |>
  filter(
    grepl("Charter School District", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  select(end_year, n_students)

charter_trends
```

``` r
# Charter as percent of state
charter_pct <- enr_2025 |>
  filter(is_state | grepl("Charter School District", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(district_name, n_students) |>
  mutate(type = ifelse(is.na(district_name), "State Total", "Charter")) |>
  select(type, n_students)

charter_pct
```

------------------------------------------------------------------------

## 8. Kindergarten is recovering from COVID

Kindergarten enrollment dropped sharply during the pandemic but is now
recovering toward pre-pandemic levels.

``` r
k_enr <- fetch_enr_multi(2019:2025)

k_trends <- k_enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "K") |>
  select(end_year, n_students) |>
  mutate(
    change_from_2019 = n_students - first(n_students),
    pct_change = round(change_from_2019 / first(n_students) * 100, 1)
  )

k_trends
```

------------------------------------------------------------------------

## 9. Rural Pee Dee districts are declining

While much of South Carolina grows, rural districts in the Pee Dee
region face persistent enrollment decline.

``` r
pee_dee_enr <- fetch_enr_multi(c(2015, 2025))

pee_dee <- pee_dee_enr |>
  filter(
    grepl("Marion|Dillon|Marlboro|Florence", district_name),
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL"
  ) |>
  select(end_year, district_name, n_students) |>
  pivot_wider(names_from = end_year, values_from = n_students) |>
  mutate(
    change = `2025` - `2015`,
    pct_change = round(change / `2015` * 100, 1)
  ) |>
  filter(!is.na(`2015`), !is.na(`2025`)) |>
  arrange(pct_change) |>
  head(8)

pee_dee
```

------------------------------------------------------------------------

## 10. District size varies dramatically

South Carolina’s 80 districts range from tiny rural systems to massive
county-wide operations serving tens of thousands.

``` r
district_sizes <- enr_2025 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  mutate(size_bucket = case_when(
    n_students < 5000 ~ "Small (<5K)",
    n_students < 15000 ~ "Medium (5K-15K)",
    n_students < 30000 ~ "Large (15K-30K)",
    TRUE ~ "Very Large (30K+)"
  )) |>
  count(size_bucket) |>
  mutate(size_bucket = factor(size_bucket,
                              levels = c("Small (<5K)", "Medium (5K-15K)",
                                         "Large (15K-30K)", "Very Large (30K+)")))

district_sizes
```

------------------------------------------------------------------------

## Summary

South Carolina’s school enrollment data reveals:

- **Growing state**: Unlike many states, South Carolina continues to add
  students
- **Regional divergence**: Upstate and Lowcountry boom while the Pee Dee
  declines
- **Increasing diversity**: Hispanic enrollment has more than doubled in
  a decade
- **High poverty rates**: Nearly 6 in 10 students are economically
  disadvantaged
- **Charter expansion**: State-authorized charters now serve 4% of all
  students

These patterns shape school funding, facility planning, and policy
decisions across the Palmetto State.

------------------------------------------------------------------------

*Data sourced from the South Carolina Department of Education [Active
Student
Headcounts](https://ed.sc.gov/data/other/student-counts/active-student-headcounts/).*
