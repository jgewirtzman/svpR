
# Load required libraries
library(dplyr)
library(readr)
library(rmarkdown)

# Load guest cost data
guests <- read_csv("individual_guest_costs.csv")

# Prepare party-level summaries
party_groups <- guests %>%
  group_by(party_name, party_email) %>%
  summarise(
    guests = list(name),
    arrival_days = list(
      unique(c(
        if (any(`Friday Accommodation Cost` > 0 | `Friday Meal Cost` > 0)) "Friday",
        if (any(`Saturday Accommodation Cost` > 0 | `Saturday Meal Cost` > 0)) "Saturday"
      ))
    ),
    guests_df = list(cur_data_all()),
    total_due = sum(`Friday Meal Cost` + `Friday Accommodation Cost` +
                    `Saturday Meal Cost` + `Saturday Accommodation Cost` +
                    `Sunday Meal Cost` + `Sunday Accommodation Cost`),
    .groups = "drop"
  )

# Loop and render PDF for each party
dir.create("invoices", showWarnings = FALSE)

for (i in 1:nrow(party_groups)) {
  this <- party_groups[i, ]
  
  rmarkdown::render(
    input = "invoice_template.Rmd",
    output_file = paste0("invoices/", make.names(this$party_name), "_invoice.pdf"),
    params = list(
      party_name = this$party_name,
      guests = this$guests[[1]],
      arrival_days = this$arrival_days[[1]],
      guests_df = this$guests_df[[1]],
      total_due = this$total_due
    ),
    envir = new.env()
  )
}
