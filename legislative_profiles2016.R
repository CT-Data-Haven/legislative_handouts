library(acsprofiles)
library(tidyverse)

################## VARIABLES ############
year <- 2016
###############################################

# need to use this workaround for problems in API for 2016
# acs.fetch(endyear=2016, span=5, variable=acs.lookup(endyear=2015, span=5, table.number="B01001"), geography = geo.make(state = 09))

table_nums <- list(
	total_pop = "B01003",
	sex_by_age = "B01001",
	race = "B03002",
	foreign_born = "B05001",
	tenure = "B25003",
	housing_cost = "B25106",
	vehicles = "B08201", 
	education = "B06009",
	median_income = "B19013",
	poverty = "C17002",
	pov_age = "B17024",
	snap = "B19058"
)
geo_up <- geo.make(state = 9, state.legislative.district.upper = "*")
geo_low <- geo.make(state = 9, state.legislative.district.lower = "*")
ct_geo <- geo.make(state = 9)
geos <- c(geo_up, geo_low, ct_geo)
fetch <- table_nums %>% map(~acs.fetch(geography = geos, endyear = year, variable = acs.lookup(endyear = 2015, span = 5, table.number = .), col.names = "pretty"))
save(fetch, file = "fetch_legis16.RData")
# load("fetch_all_towns16.RData")
table <- vector("list", length = length(fetch)) %>% set_names(names(fetch))

beepr::beep()

# TOTAL POPULATION
table$total_pop <- total_pop <- fetch$total_pop[, 1]
acs.colnames(table$total_pop) <- "num_total_pop"


# SEX & AGE
# #, % age 0-17; #, % age 65+; #, % male; #, % female
age <- fetch$sex_by_age
sexes <- list(
	male = 2,
	female = 26
)
ages <- list(
	under18 = c(3:6, 27:30),
	age65plus = c(20:25, 44:49)
)
table$sex_by_age <- cbind(
	calc_acs_table(ages, total_pop, age),
	calc_acs_table(sexes, total_pop, age)
)
acs.colnames(table$sex_by_age) <- c("num_age_0_17", "per_age_0_17", "num_age_65plus", "per_age_65plus",
																		 "num_male", "per_male", "num_female", "per_female")
rm(age, sexes, ages)

# RACE / HISPANIC
# #, % hispanic, white non-hispanic, black non-hispanic, other non-hispanic
race <- fetch$race
races <- list(
	hispanic = 12,
	white = 3,
	black = 4,
	other = 5:9
)
table$race <- calc_acs_table(races, total_pop, race)
acs.colnames(table$race) <- c("num_hispanic", "per_hispanic", "num_white", "per_white", "num_black", "per_black", "num_other_race", "per_other_race")
rm(race, races)


# FOREIGN BORN
# #, % foreign born
table$foreign_born <- calc_acs_table(list(fb = 5:6), total_pop, fetch$foreign_born)
acs.colnames(table$foreign_born) <- c("num_foreign_born", "per_foreign_born")


# TENURE
# # households, #, % owner occupied
hh <- fetch$tenure[, 1] # save this for other calcs
table$tenure <- list(hh, calc_acs_table(list(owned = 2), hh, fetch$tenure)) %>% reduce(cbind)
acs.colnames(table$tenure) <- c("num_households", "num_owned_hh", "per_owned_hh")


# HOUSING COST BURDEN
# #, % cost burdened -- combine owned + rented
table$housing_cost <- calc_acs_table(list(burden = c(6, 10, 14, 18, 22, 28, 32, 36, 40, 44)), hh, fetch$housing_cost)
acs.colnames(table$housing_cost) <- c("num_cost_burden", "per_cost_burden")


# VEHICLES
# #, % households with car
table$vehicles <- calc_acs_table(list(vehicle = 3:6), hh, fetch$vehicles)
acs.colnames(table$vehicles) <- c("num_hh_w_car", "per_hh_w_car")


# EDUCATIONAL ATTAINMENT
# # age 25+; #, % age 25+ w/o hs degree; #, % age 25+ w/ bachelor's or higher
age25 <- fetch$education[, 1]
edus <- list(
	no_hs = 2,
	bach_plus = 5:6
)
table$education <- list(
	age25,
	calc_acs_table(edus, age25, fetch$education)
) %>% reduce(cbind)
acs.colnames(table$education) <- c("num_pop_age_25plus", "num_no_hs", "per_no_hs", "num_bach_plus", "per_bach_plus")
rm(age25, edus)


# MEDIAN HOUSEHOLD INCOME
# make NA for regions
income <- fetch$median_income
# is.na(income[2:5]@estimate) <- T
# is.na(income[2:5]@standard.error) <- T
table$median_income <- income
acs.colnames(table$median_income) <- "num_median_hh_income"
rm(income)


# POVERTY & LOW INCOME
# # pov status determined; #, % below 1.0 ratio, below 2.0 ratio
poverty <- fetch$poverty
deter <- poverty[, 1]
povs <- list(
	pov = 2:3,
	low_inc = 2:7
)
table$poverty <- list(
	deter,
	calc_acs_table(povs, deter, poverty)
) %>% reduce(cbind)
acs.colnames(table$poverty) <- c("num_pov_status_determined", "num_poverty", "per_poverty", "num_low_income", "per_low_income")
rm(poverty, deter, povs)


# POVERTY, LOW INCOME BY AGE
# # pov status determined, #, % poverty, low income for ages 0-17, 65+
pov_age <- fetch$pov_age
under18deter <- apply(X = pov_age[, c(2, 15, 28)], FUN = sum, MARGIN = 2, agg.term = "under18")
over65deter <- apply(X = pov_age[, c(106, 119)], FUN = sum, MARGIN = 2, agg.term = "65plus")
povs18 <- list(
	pov = c(3:5, 16:18, 29:31),
	low = c(3:10, 16:23, 29:36)
)
povs65 <- list(
	pov = c(107:109, 120:122),
	low = c(107:114, 120:127)
)
table$pov_age <- list(
	under18deter,
	calc_acs_table(povs18, under18deter, pov_age),
	over65deter,
	calc_acs_table(povs65, over65deter, pov_age)
) %>% reduce(cbind)
acs.colnames(table$pov_age) <- c("num_pov_status_determined_0_17", "num_poverty_0_17", "per_poverty_0_17", "num_low_income_0_17", "per_low_income_0_17", "num_pov_status_determined_65plus", "num_poverty_65plus", "per_poverty_65plus", "num_low_income_65plus", "per_low_income_65plus")
rm(pov_age, under18deter, over65deter, povs18, povs65)


# RECEIPT OF SNAP
# #, % of households receiving SNAP/public assistance
table$snap <- calc_acs_table(list(snap = 2), fetch$snap[, 1], fetch$snap)
acs.colnames(table$snap) <- c("num_snap", "per_snap")


# BIND ALL TOGETHER
all_tables <- table %>% reduce(cbind)

prof_df <- data.frame(name = all_tables@geography$NAME, all_tables@estimate) %>%
	as_tibble() %>%
	mutate_at(vars(starts_with("per")), funs(round(., digits = 3))) %>%
	mutate_at(vars(starts_with("num")), funs(round(., digits = 0))) %>%
	mutate(name = str_replace(name, " \\(2016\\).+", ""))

write_csv(prof_df, "2016_legislative_profiles_working.csv", na = "")

prof_df %>%
	setNames(c("District", "Total population", "Population under age 18", "Percent under age 18", "Population ages 65+", "Percent ages 65+", "Male population", "Percent male", "Female population", "Percent female", "Latino population", "Percent Latino", "White population", "Percent white", "Black population", "Percent black", "Other race population", "Percent other race", "Foreign-born population", "Percent foreign-born", "Total households", "Owner-occupied households", "Homeownership rate", "Cost-burdened households", "Cost-burden rate", "Households with a vehicle", "Percent of households with a vehicle", "Population ages 25+", "Adults without high school degree", "Percent of adults without high school degree", "Adults with bachelor's or higher", "Percent of adults with bachelor's or higher", "Median household income", "Poverty status known", "Population in poverty", "Poverty rate", "Low-income population", "Low-income rate", "Under 18, poverty status known", "Children in poverty", "Child poverty rate", "Low-income children", "Child low-income rate", "Ages 65+, poverty status known", "Seniors in poverty", "Senior poverty rate", "Low-income seniors", "Senior low-income rate", "Households receiving public assistance/SNAP", "Percent of households receiving public assistance/SNAP")) %>%
	write_csv(paste(year, "legislative profiles.csv"), na = "")

