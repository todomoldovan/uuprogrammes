library(rvest)
library(dplyr)
library(dplyr)
library(purrr)
library(stringr)

###########################################################
#                                                         #
#  Script used to scrape information from www.uu.se about # 
#  science & technology programmes at Uppsala University  #
#  All URLs accessed 2024-02-29. Authors: Stina Brunzell, #
#  Theodora Moldovan, Ida Nilsson.                        #
#                                                         #
###########################################################

###########################################################
# Programme scraping
###########################################################

# URL of the page to scrape (all results loaded to not deal with pagination)
main_url <- "https://www.uu.se/en/study/search?type=Programme&faculty=Faculty+of+Science+and+Technology&start=90"

# Read the HTML content of the page
page <- read_html(main_url)

# Function to extract the program details from the search results
extract_program_details <- function(program_node) {
  program_string <- html_text(program_node, trim = TRUE)
  program_url <- html_attr(program_node, "href")
  name <- sub(",.*", "", program_string) 
  credits <- sub(".*?,\\s*(\\w+).*", "\\1", program_string)
  code <- sub(".*\\(([^)]+)\\).*", "\\1", program_string)
  level <- ifelse(grepl("^Bachelor's", name), "Bachelor",
                  ifelse(grepl("^Master's", name), "Master", "Preparatory"))
  full_url <- paste0("https://www.uu.se", program_url) 
  data.frame(name = name, credits = credits, code = code, level = level, url = full_url, stringsAsFactors = FALSE)
}

# Extract program details from search results
programs <- page %>%
  html_elements(".list-group-item-action .search-result-hit-title a") %>%
  lapply(extract_program_details) %>%
  bind_rows()

# Filter out preparatory programmes (different structure than bachelor/master programmes)
programs <- programs %>%
  filter(level != "Preparatory")

# Filter out programmes with preparatory courses (same programme but with an additional course) 
programs <- programs[!grepl("preparatory", programs$name, ignore.case = TRUE), ]

# Filter out Sino-Swedish CS programme (no outline)
programs <- programs[!grepl("Sino", programs$name, ignore.case = TRUE), ]

# Function to extract further program details from the program pages
extract_all_program_details <- function(program_page_url) {
  program_page <- read_html(program_page_url)
  # Helper function to clean up the extracted text
  clean_text <- function(html_node) {
    gsub("\\s+", " ", html_text(html_node, trim = TRUE))
  }
  # Extract the details using the dt + dd sibling combinator and clean the text
  details <- list(
    location = clean_text(html_element(program_page, xpath = "//dt[contains(text(), 'Location')]/following-sibling::dd")),
    teaching_form = clean_text(html_element(program_page, xpath = "//dt[contains(text(), 'Teaching form')]/following-sibling::dd")),
    pace_of_study = clean_text(html_element(program_page, xpath = "//dt[contains(text(), 'Pace of study')]/following-sibling::dd")),
    language_of_instruction = clean_text(html_element(program_page, xpath = "//dt[contains(text(), 'Language of instruction')]/following-sibling::dd")),
    outline_url = {
      # Use an XPath selector to find the 'a' tag with 'outline' in its href 
      outline_xpath <- "//a[contains(@href, 'outline') and (contains(text(), '2024') or contains(text(), '2023'))]"
      outline_url <- html_elements(program_page, xpath = outline_xpath) %>% html_attr("href")
      # Check if any URLs were found
      if (length(outline_url) == 0) {
        NA  # Return NA if no matching URL found
      } else {
        # Use the first (latest) URL if multiple were found
        outline_url <- outline_url[1]
        if (!grepl("^http", outline_url)) {
          paste0("https://www.uu.se", outline_url)
        } else {
          outline_url
        }
      }
    }
  )
  return(details)
}

# Iterate over each program URL and extract details
programs_details <- lapply(programs$url, function(url) {
  #Sys.sleep(1)  # be polite by sleeping 1 second between requests
  extract_all_program_details(url)
})

# Combine the details into a dataframe
details_df <- do.call(rbind, programs_details)
details_df <- as.data.frame(details_df, stringsAsFactors = FALSE)

# Make sure the rownames are NULL to avoid issues when binding the columns
rownames(details_df) <- NULL

# Bind the details to the original dataframe
programs <- bind_cols(programs, details_df)

# Function to convert list columns to character strings (for exporting)
convert_lists_to_character <- function(df) {
  # Iterate over each column
  for (colname in names(df)) {
    # Check if the column is a list by looking at the first element
    if (is.list(df[[colname]]) && length(df[[colname]]) > 0) {
      # Use sapply to collapse each list element into a single character string
      df[[colname]] <- sapply(df[[colname]], function(x) {
        if (is.null(x)) return(NA) # Handle NULLs
        paste(x, collapse=", ")
      })
    }
  }
  return(df)
}

# Apply the conversion function to the programs dataframe
programs <- convert_lists_to_character(programs)

# Biology programs have a joint outline (not worth scraping, would be faster to hard code)
#bio_outline_url <- "https://www.ibg.uu.se/education/master/biology/programme-content/"
#programs$outline_url <- ifelse(programs$outline_url == "NA" | is.na(programs$outline_url), bio_outline_url, programs$outline_url)

# Filter out biology programmes
programs <- programs %>% filter(outline_url != "NA") # no outline for each programme (specialization)

# Filter out duplicate leadership course 
programs <- programs[-10, ] # Same course listed twice in the search results 

# Differentiate between the two Master's Programme in Materials Engineering
programs$name[65] <- "Integrated Master's Programme in Materials Engineering" # 300 credits version # 58

# Differentiate between the two Master's Programme in Wind Power Project Management
programs$name[87] <- "Magister Programme in Wind Power Project Management" # 60 credits version # 80

# Save dataframe to csv
write.csv(programs, "programs.csv", row.names = FALSE)

###########################################################
# Course scraping
###########################################################

# Function to extract the course details from the outlines
extract_course_details <- function(outline_page_url, program_title) {
  page <- read_html(outline_page_url)
  course_nodes <- page %>% html_elements("li")
  courses <- course_nodes %>% 
    html_text() %>% 
    map_df(~{
      course_detail <- .x
      course_name_extracted <- str_extract(course_detail, "^[^\\d]+") %>% str_trim() %>% str_remove(",\\s*$")
      number_of_credits_extracted <- str_extract(course_detail, "\\d+(?=\\s?credits)") %>% str_trim()
      course_code_extracted <- str_extract(course_detail, "(?<=\\()[^)]+(?=\\))") %>% str_trim()
      main_field_of_study_extracted <- str_extract(course_detail, "(?<=Main field\\(s\\) of study and in-depth level: ).*") %>% str_trim()
      tibble(
        course_name = course_name_extracted,
        number_of_credits = number_of_credits_extracted,
        course_code = course_code_extracted,
        main_field_of_study = main_field_of_study_extracted,
        program_title = program_title # %>% str_trim() # Use the passed program title
      )
    })
  return(courses)
}

# Initialize courses table
courses <- tibble()

# Loop through each row in the programs dataframe
for(i in 1:nrow(programs)) {
  outline_url <- programs$outline_url[i]
  program_title <- programs$name[i] # Use the corresponding program name
  # Apply the extract_course_details function 
  course_details <- extract_course_details(outline_url, program_title)
  #Sys.sleep(1)
  #print(i)
  courses <- bind_rows(courses, course_details)
}

# Keep courses only (scraped too much information because outline pages are inconsistent)
courses <- courses %>% filter(number_of_credits != "") # all courses have a number of credits 

# Order courses alphabetically
courses <- courses[order(courses$course_name),]

# Remove duplicate rows (some outlines break up e.g. 10 credit courses into 5 credits in each period)
courses <- unique(courses)

# Save dataframe to csv
write.csv(courses, "courses.csv", row.names = FALSE)

###########################################################
# Post-scrape: add biology courses from commom "outline"
###########################################################

courses2 <- read.csv("courses.csv")
programs2 <- read.csv("programs2.csv")

# Master's Programme in Biology – Cell and Molecular Biology
new_courses_cell <- data.frame(
  course_name = c("Trends in Molecular Biology and Biotechnology", "RNA: structure, function and biology", "Structure and Function of Macromolecules", "Genes, brain and behaviour", "Microbiology", "Molecular Cell Biology", "Functional genomics", "Protein Engineering"),
  number_of_credits = c(15, 15, 15, 15, 15, 15, 15, 15),
  course_code = c("1BG369", "1BG388", "1BG349", "1BG344", "1BG307", "1BG320", "1BG322", "1BG301"),
  main_field_of_study = c(NA, NA, NA, NA, NA, NA, NA, NA),
  program_title = c("Master's Programme in Biology – Cell and Molecular Biology", "Master's Programme in Biology – Cell and Molecular Biology", "Master's Programme in Biology – Cell and Molecular Biology", "Master's Programme in Biology – Cell and Molecular Biology", "Master's Programme in Biology – Cell and Molecular Biology", "Master's Programme in Biology – Cell and Molecular Biology", "Master's Programme in Biology – Cell and Molecular Biology", "Master's Programme in Biology – Cell and Molecular Biology")
)
courses2 <- rbind(courses2, new_courses_cell)

# Master's Programme in Biology – Ecology and Conservation
new_courses_ecology <- data.frame(
  course_name = c("Ecology D", "Evolutionary Processes", "Limnology D", "Population and Community Ecology", "Conservation biology", "Behavioural ecology", "Ecological methods"),
  number_of_credits = c(15, 15, 15, 15, 15, 15, 15),
  course_code = c("1BG382", "1BG373", "1BG505", "1BG309", "1BG318", "1BG319", "1BG324"),
  main_field_of_study = c(NA, NA, NA, NA, NA, NA, NA),
  program_title = c("Master's Programme in Biology – Ecology and Conservation", "Master's Programme in Biology – Ecology and Conservation", "Master's Programme in Biology – Ecology and Conservation", "Master's Programme in Biology – Ecology and Conservation", "Master's Programme in Biology – Ecology and Conservation", "Master's Programme in Biology – Ecology and Conservation", "Master's Programme in Biology – Ecology and Conservation")
)
courses2 <- rbind(courses2, new_courses_ecology)

# Master's Programme in Biology – Environmental Toxicology
new_courses_toxicology <- data.frame(
  course_name = c("Toxicology D", "Ecology D", "Limnology D", "Ecotoxicology", "Developmental Biology Including the Development of the Nervous System", "Toxicology and risk assessment"),
  number_of_credits = c(15, 15, 15, 15, 15, 15),
  course_code = c("1BG381", "1BG382", "1BG505", "1BG308", "1BG510", "1BG509"),
  main_field_of_study = c(NA, NA, NA, NA, NA, NA),
  program_title = c("Master's Programme in Biology – Environmental Toxicology", "Master's Programme in Biology – Environmental Toxicology", "Master's Programme in Biology – Environmental Toxicology", "Master's Programme in Biology – Environmental Toxicology", "Master's Programme in Biology – Environmental Toxicology", "Master's Programme in Biology – Environmental Toxicology")
)
courses2 <- rbind(courses2, new_courses_toxicology)

# Master's Programme in Evolutionary Biology – MEME (separate outline?)
new_courses_meme <- data.frame(
  course_name = c("Evolutionary processes", "Evolutionary patterns"),
  number_of_credits = c(15, 15),
  course_code = c("1BG373", "1BG306"),
  main_field_of_study = c(NA, NA),
  program_title = c("Master's Programme in Evolutionary Biology – MEME", "Master's Programme in Evolutionary Biology – MEME")
)
courses2 <- rbind(courses2, new_courses_meme)

# Master's Programme in Biology – Evolutionary Biology
new_courses_evolutionary <- data.frame(
  course_name = c("Evolutionary processes", "Evolutionary patterns", "Behavioural ecology", "Population Genomics", "Evolution and Development"),
  number_of_credits = c(15, 15, 15, 15, 15),
  course_code = c("1BG373", "1BG306", "1BG319", "1BG508", "1BG397"),
  main_field_of_study = c(NA, NA, NA, NA, NA),
  program_title = c("Master's Programme in Biology – Evolutionary Biology", "Master's Programme in Biology – Evolutionary Biology", "Master's Programme in Biology – Evolutionary Biology", "Master's Programme in Biology – Evolutionary Biology", "Master's Programme in Biology – Evolutionary Biology")
)
courses2 <- rbind(courses2, new_courses_evolutionary)

# Master's Programme in Biology – Immunology and Microbiology
new_courses_immunology <- data.frame(
  course_name = c("Trends in Molecular Biology and Biotechnology", "Microbiology", "Immunology", "Molecular Infection Biology"),
  number_of_credits = c(15, 15, 15, 15),
  course_code = c("1BG396", "1BG307", "1BG313", "1BG326"),
  main_field_of_study = c(NA, NA, NA, NA),
  program_title = c("Master's Programme in Biology – Immunology and Microbiology", "Master's Programme in Biology – Immunology and Microbiology", "Master's Programme in Biology – Immunology and Microbiology", "Master's Programme in Biology – Immunology and Microbiology")
)
courses2 <- rbind(courses2, new_courses_immunology)

# Master's Programme in Biology – Ecosystems and Aquatic Ecology
new_courses_ecosystems <- data.frame(
  course_name = c("Limnology D", "Ecology D", "Evolutionary Processes", "Toxicology D", "Applied Ecosystem Ecology", "Biodiversity and Ecosystem Functioning", "Ecosystems in the Anthropocene"),
  number_of_credits = c(15, 15, 15, 15, 15, 15, 15),
  course_code = c("1BG505", "1BG382", "1BG373", "1BG381", "1BG305", "1BG514", "1BG513"),
  main_field_of_study = c(NA, NA, NA, NA, NA, NA, NA),
  program_title = c("Master's Programme in Biology – Ecosystems and Aquatic Ecology", "Master's Programme in Biology – Ecosystems and Aquatic Ecology", "Master's Programme in Biology – Ecosystems and Aquatic Ecology", "Master's Programme in Biology – Ecosystems and Aquatic Ecology", "Master's Programme in Biology – Ecosystems and Aquatic Ecology", "Master's Programme in Biology – Ecosystems and Aquatic Ecology", "Master's Programme in Biology – Ecosystems and Aquatic Ecology")
)
courses2 <- rbind(courses2, new_courses_ecosystems)

# Master's Programme in Biology – NABiS – Nordic Master in Biodiversity and Systematics
new_courses_nabis <- data.frame(
  course_name = c("Fundamental and molecular systematics", "Informatics toolbox for systematics", "Fungal diversity and evolution", "Diversity and Identification of Marine Invertebrates"),
  number_of_credits = c(10, 5, 10, 5),
  course_code = c("1BG393", "1BG395", "1BG376", "1BG394"),
  main_field_of_study = c(NA, NA, NA, NA),
  program_title = c("Master's Programme in Biology – NABiS – Nordic Master in Biodiversity and Systematics", "Master's Programme in Biology – NABiS – Nordic Master in Biodiversity and Systematics", "Master's Programme in Biology – NABiS – Nordic Master in Biodiversity and Systematics", "Master's Programme in Biology – NABiS – Nordic Master in Biodiversity and Systematics")
)
courses2 <- rbind(courses2, new_courses_nabis)

write.csv(courses2, "courses2.csv", row.names = FALSE)
