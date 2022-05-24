library("skimr")
library("janitor")
library("tidyverse")
library("lubridate")
library("dplyr")
library(ggplot2)
library(data.table)
library("glmnet")
library("vip")
library("viridis")
library("gt")
library("mice")
library("zoo")
library("DescTools")
library(readr)
sample_submission <- read_csv("sample_submission.csv", 
                              col_types = cols(id = col_number(), target = col_number()))
library(readr)
test <- read_csv("test.csv")
View(test)
library(readr)
train <- read_csv("train.csv")
##Let's see how looks like a sample of 20 predictors.
set.seed(123)
draw <- sample(length(train), 20) %>% sort()

glimpse(train[, draw])
skim <- train %>% skim()

skim_df <- skim %>% 
  as_tibble()
print(skim_df)

skim_cat <- skim_df %>% 
  filter(skim_type != "numeric") %>% 
  select(skim_variable:n_missing, character.empty:character.whitespace) %>% 
  rename(variable = "skim_variable",
         empty = "character.empty",
         n_unique = "character.n_unique",
         whitespace = "character.whitespace")

skim_cat %>% 
  mutate(rank = row_number()) %>% 
  sample_n(20) %>% 
  arrange(rank) %>% 
  select(- rank) %>% 
  gt() %>% 
  gt::tab_header(
    title = "Data structure overview of a sample of 20 categorical variables"
  ) %>% 
  gt::tab_options(data_row.padding = px(1))

skim_num <- skim_df %>% 
  filter(skim_type == "numeric", skim_variable != "id") %>% 
  select(skim_variable, numeric.mean:numeric.hist)

new_numNames <- skim_num %>% 
  names() %>% 
  str_remove_all("numeric.") %>% 
  str_remove("skim_")

names(skim_num) <- new_numNames

set.seed(123)

skim_num %>% 
  mutate(rank = row_number()) %>% 
  sample_n(20) %>% 
  arrange(rank) %>% 
  select(- rank) %>%
  gt() %>%
  gt::tab_header(
    title = "Summary of statistics of a sample of 20 numerical variables data structure overview"
  ) %>% 
  gt::tab_options(data_row.padding = px(1))
#
## join train and test into 1 dataset
#
netflix_tt <- bind_rows(train, test, .id = "set")
##
is_missing <- lapply(netflix_tt, is.na)
                         SDcols = names(netflix_tt)[- c(1,2,3)]
                         by = (set)[, id := I]

##find the mean values of of the feature coumns in test and train
 mean_na <- netflix_tt %>% 
  select(- c("id", target,)) %>% 
  group_by(set) %>% 
  mutate(across(everything(), is.na)) %>% 
  summarize(across(everything(), mean))
 
## change the "set" column name frrom '1' and '2' to 'train' &'test'.
##Then change the columns to rows and put them under a 'feature' and 'prop_na' col.
## then filter out the 0's and create a rank column to rank 
 tidy_meanNA <- mean_na %>% 
   mutate(set = case_when(set == "1" ~ "train",
                          set == "2" ~ "test")) %>% 
   pivot_longer(feature_0:feature_506, 
                names_to = "feature",
                values_to = "prop_NA") %>% 
   filter(prop_NA != 0) %>% 
   mutate(rank = str_extract(feature, "\\d+") %>% as.numeric())
 ##
 tidy_meanNA %>% 
   ggplot(aes(reorder(feature, rank), prop_NA, fill = feature)) +
   geom_col(show.legend = FALSE) + 
   scale_y_continuous(labels = scales::percent)+
   coord_flip() +
   facet_wrap(~ set) +
   theme_minimal() + 
   labs(x = "feature", y = "% NA",
        title = "Percentage of missing values by train and test ") +
   theme(axis.text.y = element_text(size = 6),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12))
 ##
 na_sum <- netflix_tt %>% 
   select(- c("id", "target")) %>% 
   is.na() %>% 
   as_tibble() %>% 
   mutate(na_sum = rowSums(across(everything()))) 
 
 na_sum %>% 
   mutate(na_sum = as.factor(na_sum)) %>% 
   ggplot(aes(na_sum, fill = na_sum))  +
   geom_bar() + 
   theme_minimal() +
   theme(legend.position = "none") +
   labs(title = "distributin of missing value per observation",
        x = "number of missing value per observation") +
   theme(axis.text.x = element_text(size = 9))
 ##Drop features with more than 25% missing values
 cleaned_mp <- tidy_meanNA %>% filter(prop_NA > 0.25)
col_remove <- c("feature_5", "feature_6", "feature_7", "feature_8", "feature_9", "feature_10", "feature_11", "feature_12", "feature_13",
                "feature_75", "feature_76", "feature_83","feature_84", "feature_85", "feature_86", "feature_153", "feature_172", "feature_193",
                 "feature_194", "feature_195", "feature_196", "feature_197", "feature_198", "feature_202", "feature_203", "feature_204", "feature_255",
                 "feature_5", "feature_6", "feature_7", "feature_8", "feature_9", "feature_10", "feature_11", "feature_12", "feature_13", 
                 "feature_75", "feature_76", "feature_83", "feature_84", "feature_85", "feature_86", "feature_153", "feature_172", "feature_193",
                 "feature_194", "feature_195", "feature_196", "feature_197", "feature_198", "feature_202", "feature_203", "feature_204", "feature_255")
clean_netflix_tt <- netflix_tt%>%
  select(- one_of(col_remove))

## replace the NA values with mean

num_col <- unlist(lapply(clean_netflix_tt, is.numeric))
data_num <- clean_netflix_tt[, num_col]
print(data_num)
mean_num  <- na.aggregate(data_num)
glimpse(mean_num)


##replace the NA values in the charcter column with mode
#get na character columns
cha_col <- unlist(lapply(clean_netflix_tt, is.character))
data_cha <- clean_netflix_tt[, cha_col]
#count the number of NA values in data_cha
colSums(is.na(data_cha))
#create the mode function
my_mode <- function(x) {
  unique_x <- na.omit(unique(x))
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]}
#find the mode of the na colms and replace the NA's with the mode of it's column
data_cha$feature_191[is.na(data_cha$feature_191)] <- my_mode(data_cha$feature_191)
data_cha$feature_192[is.na(data_cha$feature_192)] <- my_mode(data_cha$feature_192)
data_cha$feature_200[is.na(data_cha$feature_200)] <- my_mode(data_cha$feature_200)
data_cha$feature_201[is.na(data_cha$feature_201)] <- my_mode(data_cha$feature_201)

## convert date/time stamp to date/time...

data_cha$feature_191 <- lubridate::dmy(data_cha$feature_191)
data_cha$feature_192 <- lubridate::dmy(data_cha$feature_192)
data_cha$feature_200 <- lubridate::dmy(data_cha$feature_200)
data_cha$feature_199 <- lubridate::dmy(data_cha$feature_199)
data_cha$feature_201 <- lubridate::dmy(data_cha$feature_201)

data_cha_weekday <- data_cha %>% mutate(feature_191weekdays = weekdays(data_cha$feature_191),
                                        feature_192weekdays = weekdays(data_cha$feature_192),
                                        feature_199weekdays = weekdays(data_cha$feature_199),
                                        feature_200weekdays = weekdays(data_cha$feature_200),
                                        feature_201weekdays = weekdays(data_cha$feature_201))
data_cha_weekday <- data_cha_weekday %>% mutate(feature_191month = strftime(data_cha_weekday$feature_191, "%m"),
                                                feature_192month = strftime(data_cha_weekday$feature_192, "%m"),
                                                feature_199month = strftime(data_cha_weekday$feature_199, "%m"),
                                                feature_200month = strftime(data_cha_weekday$feature_200, "%m"),
                                                feature_201month = strftime(data_cha_weekday$feature_201, "%m"))
data_cha_weekday <- data_cha_weekday %>% mutate(feature_191year = strftime(data_cha_weekday$feature_191, "%Y"),
                                                feature_192year = strftime(data_cha_weekday$feature_192, "%Y"),
                                                feature_199year = strftime(data_cha_weekday$feature_199, "%Y"),
                                                feature_200year = strftime(data_cha_weekday$feature_200, "%Y"),
                                                feature_201year = strftime(data_cha_weekday$feature_201, "%Y"))
#convert from character column to numeric column
data_cha_weekday <- data_cha_weekday %>% mutate(feature_191month = as.numeric(data_cha_weekday$feature_191month),
                                                feature_192month = as.numeric(data_cha_weekday$feature_192month),
                                                feature_199month = as.numeric(data_cha_weekday$feature_199month),
                                                feature_200month = as.numeric(data_cha_weekday$feature_200month),
                                                feature_201month = as.numeric(data_cha_weekday$feature_201month))
#Change the months columns from numeric to months names
data_cha_weekday <- data_cha_weekday %>% mutate(feature_191month = month.name[data_cha_weekday$feature_191month],
                                                feature_192month = month.name[data_cha_weekday$feature_192month],
                                                feature_199month = month.name[data_cha_weekday$feature_199month],
                                                feature_200month = month.name[data_cha_weekday$feature_200month],
                                                feature_201month = month.name[data_cha_weekday$feature_201month])
##count unique values in cha
unii <- unidata_cha %>% 
  group_by(feature) %>%
  summarise(count = n_distinct(unidata_cha))

as.data.frame(table(data_cha_weekday$feature_18)) 

unidata_cha <- data_cha_weekday %>% 
  pivot_longer(feature_0:feature_371, 
               names_to = "feature")
view(unii)
