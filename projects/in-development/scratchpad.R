library(dplyr)
packageVersion("dplyr")
# [1] ‘0.7.0’

x <- as.character(1:10)

# With recode function using backquotes as arguments
dplyr::recode(x, `2` = 20L, `4` = 40L)


map_from <- as.character(x)
map_to <- as.character(c(1, 1, 1, 1, 20, 40, 1, 1, 1, 1))

dplyr::recode(x, names = map_from, values = map_to)
do.call(dplyr::recode, args = list(map_from, map_to))
# Note: it is necessary to add "L" a numerical value.
dplyr::recode(x, `2` = 20, `4` = 40)
# [1] NA 20 NA 40 NA NA NA NA NA NA
# Warning message:
# Unreplaced values treated as NA as .x is not compatible. Please specify replacements exhaustively or supply .default

# With recode function using characters as arguments
as.numeric(dplyr::recode(as.character(x), "2" = "20", "4" = "40"))
# [1]  1 20  3 40  5  6  7  8  9 10


test <- grouped_classifications$how_many %>% head(n=100) %>% as.character

x <- grouped_classifications %>% mutate(how2 = dplyr::recode(as.character(how_many), "1" = 1L, "2" = 2L, "35" = 4L, "610" = 8L, "MANY" = 20L))
do.call(what = recode, args = list(as.character(grouped_classifications), lookup))


do.call("dplyr::recode", c(list(x), setNames(as.list(values), names)))


w <- read.csv("data/head_kenya.csv", stringsAsFactors = F)
x <- w %>% filter(workflow_version != 126.73) %>% head(n=2000)
write.csv(x, "data/kenya-sample.csv", row.names = F)
check_workflow(w)
