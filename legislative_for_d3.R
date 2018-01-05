library(tidyverse)
library(forcats)
library(rvest)



df <- read_csv("2016 legislative profiles.csv") %>%
	select(-`Male population`:-`Percent female`) %>%
	mutate(chamber = word(District, 2)) %>%
	mutate(`Total population_x` = `Total population`, `Total population_xx` = `Total population`) %>%
	select(district = District, chamber, 2:6, `Total population_x`, 7:14, `Total population_xx`, everything()) %>%
	gather(key = indicator, value = value, -district, -chamber) %>%
	mutate(topic = as.factor(indicator) %>% fct_collapse(
		age = c("Total population", "Population under age 18", "Percent under age 18", "Population ages 65+", "Percent ages 65+"), 
		race = c("Total population_x", "Latino population", "Percent Latino", "White population", "Percent white", "Black population", "Percent black", "Other race population", "Percent other race"), 
		households = c("Total households", "Owner-occupied households", "Homeownership rate", "Cost-burdened households", "Cost-burden rate", "Households with a vehicle", "Percent of households with a vehicle"),
		education = c("Population ages 25+", "Adults without high school degree", "Percent of adults without high school degree", "Adults with bachelor's or higher", "Percent of adults with bachelor's or higher"),
		income = c("Median household income", "Poverty status known", "Population in poverty", "Poverty rate", "Low-income population", "Low-income rate", "Households receiving public assistance/SNAP", "Percent of households receiving public assistance/SNAP"), 
		immigration = c("Total population_xx", "Foreign-born population", "Percent foreign-born"), 
		income_kids = c("Under 18, poverty status known", "Children in poverty", "Child poverty rate", "Low-income children", "Child low-income rate"), 
		income_seniors = c("Ages 65+, poverty status known", "Seniors in poverty", "Senior poverty rate", "Low-income seniors", "Senior low-income rate"))
		) %>%
	mutate(displayTopic = topic %>% fct_relabel(str_to_title) %>% fct_recode("Race and ethnicity" = "Race", "Income by age: children" = "Income_kids", "Income by age: seniors" = "Income_seniors")) %>%
	mutate(type = ifelse(str_detect(indicator, "(Percent|rate|Median)"), "map", "table")) %>%
	mutate(format = ifelse(str_detect(indicator, "Median"), "$,", ifelse(type == "map", ".0%", ","))) %>%
	mutate(indicator = str_replace(indicator, "_\\w+", "")) %>%
	group_by(district, topic) %>%
	mutate(order = row_number()) %>%
	group_by(district) %>% 
	mutate(topicOrder = row_number()) %>%
	ungroup() %>%
	mutate(number = str_extract(district, "\\d+$")) %>%
	mutate(alt = as.factor(chamber) %>% fct_recode("U" = "Senate", "L" = "House")) %>%
	mutate(lvl = alt %>% fct_recode("610" = "U", "620" = "L")) %>%
	mutate(pad = str_pad(number, width = 3, pad = "0", side = "left")) %>%
	mutate(id = ifelse(district == "Connecticut", "Connecticut", sprintf("%s%s500US09%s", lvl, alt, pad))) %>%
	select(-number:-lvl) %>%
	mutate(chamber = ifelse(district == "Connecticut", "State", chamber)) %>%
	arrange(topicOrder, order, chamber, pad)


df %>%
	select(-topic) %>%
	rename(topic = displayTopic) %>%
	write_csv("legislative_data.csv", na = "")


# get legislator names
html_sen <- read_html("https://www.cga.ct.gov/asp/menu/slist.asp")

sen_bills <- html_sen %>%
	html_node("table.footable") %>%
	html_nodes("td") %>%
	html_nodes("a") %>%
	html_attr("href") %>%
	str_subset("^/asp/")
senate <- html_sen %>%
	html_node("table.footable") %>%
	html_table()
senate$url <- paste0("https://www.cga.ct.gov", sen_bills)


html_house <- read_html("https://www.cga.ct.gov/asp/menu/hlist.asp")

house_bills <- html_house %>%
	html_node("table.footable") %>%
	html_nodes("td") %>%
	html_nodes("a") %>%
	html_attr("href") %>%
	str_subset("^/asp/")
house <- html_house %>%
	html_node("table.footable") %>%
	html_table()
house$url <- paste0("https://www.cga.ct.gov", house_bills)

reps <- bind_rows(
	senate %>% mutate(chamber = "Senate") %>% mutate(District = str_extract(District, "\\d+")),
	house %>% mutate(chamber = "House") %>% mutate(District = as.character(District))
	) %>%
	as_tibble() %>%
	select(-Bills) %>%
	mutate(title = ifelse(chamber == "Senate", "Sen.", "Rep.")) %>%
	separate(Name, into = c("last", "first"), sep = ", ") %>%
	mutate(string = sprintf("%s %s %s (%s)", title, first, last, Party)) %>%
	mutate(alt = as.factor(chamber) %>% fct_recode("U" = "Senate", "L" = "House")) %>%
	mutate(lvl = alt %>% fct_recode("610" = "U", "620" = "L")) %>%
	mutate(pad = str_pad(District, width = 3, pad = "0", side = "left")) %>%
	mutate(id = sprintf("%s%s500US09%s", lvl, alt, pad)) %>%
	select(id, string, url)

write_csv(reps, "legis_names.csv")

# get town names for districts
html_town <- read_html("https://www.cga.ct.gov/asp/content/townlist.asp")

towns <- html_town %>%
	html_node("table.footable") %>%
	html_table()

towns %>%
	as_tibble() %>%
	select(-4) %>%
	setNames(c("town", "House", "Senate")) %>%
	mutate(House = str_extract_all(House, "\\d+")) %>%
	mutate(Senate = str_extract_all(Senate, "\\d+")) %>%
	gather(key = chamber, value = district, -town) %>%
	unnest(district) %>%
	mutate(alt = as.factor(chamber) %>% fct_recode("U" = "Senate", "L" = "House")) %>%
	mutate(lvl = alt %>% fct_recode("610" = "U", "620" = "L")) %>%
	mutate(pad = str_pad(district, width = 3, pad = "0", side = "left")) %>%
	mutate(id = sprintf("%s%s500US09%s", lvl, alt, pad)) %>%
	select(id, chamber, town) %>%
	write_csv("town_x_district.csv")
