#### Preamble ####
# Purpose: Clean the survey data downloaded from DHS Program
# Author: Isaac Ng
# Data: April 9 2022
# Contact: isaac.ng@mail.utoronto.ca


#### Workspace setup ####
library(pdftools)
library(tidyverse)
library(here)

# Loading the PDF data
Ghana1998 <- pdf_text(here::here("inputs/data/Ghana1998.pdf"))

# Converting it into a table using tibble
Ghana1998 <- tibble(raw_text = Ghana1998,
                        page_number = c(1:249))

# Finding the desired page number in the document
Ghanavacs <- Ghana1998[121,]
# Separate the lines with the newline symbol
Ghanavacs <- separate_rows(Ghanavacs, raw_text, sep = "\\n", convert = FALSE)
# Keep only the desired lines for the table
Ghanavacs <- Ghanavacs[13:40,]
# First separate it into the first column and the data (since there is only 1 
# space between some data values whereas the first column is separated by at 
# least 10)
Ghanavacs <- Ghanavacs |>
  separate(col = raw_text,
           into = c("Background Characteristic", "Data"),
           sep = "\\s{10,}",
           remove = FALSE,
           fill = "right"
  ) |>
  # then separate it into the remaining columns
  separate(col = Data,
           into = c("BCG", "DPT 1", "DPT 2", "DPT 3+", "Polio 0", "Polio 1", 
                    "Polio 2", "Polio 3+", "Measles", "Fully-immunized", 
                    "Yellow fever", "None", "Percentage with a vaccination card"
                    , "Percentage who received vitamin A in the last 6 months", 
                    "Number of children"),
           sep = "\\s{1,}",
           remove = FALSE,
           fill = "right"
           )

# write the first csv with the whole table
write.csv(Ghanavacs, "inputs/data/cleaned_full_vac_data.csv", row.names=FALSE)

# continue cleaning by limited the columns and rows that we desire
Ghanavacs <- Ghanavacs[13:22,]
Ghanavacs <- Ghanavacs |>
  select(`Background Characteristic`, `Fully-immunized`, None)

# convert each column into doubles so they are not type character. Also row 7 
# uses brackets to contain their values so, insert them manually
Ghanavacs$`Fully-immunized` <- as.double(Ghanavacs$`Fully-immunized`)
Ghanavacs$None <- as.double(Ghanavacs$None)
Ghanavacs["Semi-immunized"] <- tibble(rep(100.0, 10)) - Ghanavacs['Fully-immunized'] - Ghanavacs['None']
Ghanavacs[7,2] <- 66.6
Ghanavacs[7,3] <- 2.6
Ghanavacs[7,4] <- 100.0 - 66.6 - 2.6

# Do the same for mortality rates. First get the desired page, then separate by
# the newline symbol. Reduce the table further into only the rows that contain 
# data from the table. Finally, split it into Tab, the first column, and the 
# remainder data for exactly the same reasons as above. Then split the data
# column into their respective columns
Ghana_child_morts <- Ghana1998[103,]
Ghana_child_morts <- separate_rows(Ghana_child_morts, raw_text, sep = "\\n", convert = FALSE)
Ghana_child_morts <- Ghana_child_morts[11:30,]
Ghana_child_morts <- Ghana_child_morts |>
  separate(col = raw_text,
           into = c("Tab", "Socioeconomic characteristic", "Data"),
           sep = "\\s{10,}",
           remove = FALSE,
           fill = "right"
  ) |>
  separate(col = Data,
           into = c("Neonatal mortality", "Postneonatal mortality", 
                    "Infant mortality", "Child mortality", 
                    "Under-five mortality"),
           sep = "\\s{1,}",
           remove = FALSE,
           fill = "right"
  )

# write a csv for this entire table before continuing
write.csv(Ghana_child_morts, "inputs/data/cleaned_full_mort_data.csv", row.names=FALSE)

# limit the table to only the desired rows and columns
Ghana_child_morts <- Ghana_child_morts[5:14,] |>
  select("Socioeconomic characteristic", "Neonatal mortality", 
         "Postneonatal mortality", "Infant mortality", "Child mortality", 
         "Under-five mortality")

# convert their typing from character to doubles. Row 7 uses brackets so insert
# those values manually
Ghana_child_morts$`Neonatal mortality` <- as.double(Ghana_child_morts$`Neonatal mortality`)
Ghana_child_morts$`Postneonatal mortality` <- as.double(Ghana_child_morts$`Postneonatal mortality`)
Ghana_child_morts$`Infant mortality` <- as.double(Ghana_child_morts$`Infant mortality`)
Ghana_child_morts$`Child mortality` <- as.double(Ghana_child_morts$`Child mortality`)
Ghana_child_morts$`Under-five mortality` <- as.double(Ghana_child_morts$`Under-five mortality`)
Ghana_child_morts[7,2] <- 54.4
Ghana_child_morts[7,3] <- 22.9
Ghana_child_morts[7,4] <- 77.3
Ghana_child_morts[7,5] <- 55.7
Ghana_child_morts[7,6] <- 128.7

# finally, write csvs ffor our cleaned data
write.csv(Ghanavacs, "inputs/data/cleaned_vac_data.csv", row.names=FALSE)
write.csv(Ghana_child_morts, "inputs/data/cleaned_mort_data.csv", row.names=FALSE)