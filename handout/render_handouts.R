library(tidyverse)

setwd("~/_R/legislators/handout")

# need to redo senate

ids <- read_csv("../town_x_district.csv") %>%
	select(-town) %>%
	unique() %>%
	mutate(num = stringr::str_sub(id, -3)) %>%
	mutate(name = paste(chamber, num, sep = "_")) %>%
	filter(chamber == "Senate")

# walk2(ids$id, ids$name,  ~rmarkdown::render(input = "./legislative_handout_v2.Rmd", output_file = sprintf("./output/2016_profile_%s.pdf", .y), params = list(this_id = .x)))

walk2(ids$id, ids$name, function(id, name) {
	rmarkdown::render(input = "./legislative_handout_v2.Rmd", output_file = sprintf("./output/2016_profile_%s.pdf", name), params = list(this_id = id))
	dev.off()
	print(id)
})

beepr::beep(sound = 1)

