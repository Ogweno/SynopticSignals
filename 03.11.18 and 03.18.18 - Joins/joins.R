#load packages
library(dplyr)
library(purrr)

#create datasets
animal <- c("elephant", "duck", "cow", "penguin", "giraffe", "snake", "cat", "cow", "butterfly", "elmo")
class1 <- c("mammal", "bird", "mammal", "bird", "mammal", "reptile", "mammal", "mammal", "insect", "unknown")
class2 <- c("mammal", "bird", "reptile", "insect", "amphibian", "bird")
phylum <- c("chordata", "chordata", "chordata", "arthropoda", "chordata", "chordata")



animal_df <- data_frame(animal, class = class1)

class_df <- data_frame(class = class2, phylum)

#check to make sure 2 datasets created successfully
animal_df
class_df

# mutating joins
# inner join
animal_df %>% inner_join(class_df, "class")

# left join
animal_df %>% left_join(class_df, "class")

# right join
animal_df %>% right_join(class_df, "class")

# left join same as right join
class_df %>% left_join(animal_df, "class")

# full join
animal_df %>% full_join(class_df, "class")

# semi join
animal_df %>% semi_join(class_df, "class")

# anti join
animal_df %>% anti_join(class_df, "class")

class_df %>% anti_join(animal_df, "class")

# distinct left join
distinct(animal_df) %>% left_join(distinct(class_df), "class")


# create habitat data frame with animal and habitat
animal2 <- c("elephant", "duck", "cow", "penguin", "giraffe", "snake", "cat", "butterfly", "elmo")
habitat <- c("savanna", "farm", "farm", "artic", "savanna", "garden", "farm", "garden", "sesame street")

habitat_df <- data_frame(animal = animal2, habitat)

habitat_df

# (1) Using dplyr to join multiple datasets together
distinct(animal_df) %>% 
  left_join(distinct(class_df), by = "class") %>% 
  left_join(habitat_df, by = "animal")

# (2) Match with non-unique keys in each dataset
habitat_df2 <- data_frame(animal2, habitat)

# Match with non-unique keys in each dataset
distinct(animal_df) %>%
  left_join(distinct(class_df), by = "class") %>%
  left_join(habitat_df2, by = c("animal" = "animal2"))


class3 <- c("mammal", "bird", "reptile", "insect", "amphibian")
kingdom <- c("Animal", "Animal", "Animal", "Animal", "Animal")

kingdom_df <- data_frame(class = class3, kingdom)

kingdom_df

# (3) Using dplyr and then the purr package to efficiently combine datasets on same key 
library(purrr)

distinct(animal_df) %>% 
  left_join(distinct(class_df), by = "class") %>% 
  left_join(kingdom_df, by = "class")

animal_df2 <- distinct(animal_df)
class_df2 <- distinct(class_df)

list(animal_df2, class_df2, kingdom_df) %>%
  reduce(left_join, "class") 

# Adding back in the habitat dataset
list(animal_df2, class_df2, kingdom_df) %>%
  reduce(left_join, "class") %>%
  left_join(habitat_df, "animal")

# With semi_join will only bring back animals and classes where the class is in the other two datasets.
list(animal_df2, class_df2, kingdom_df) %>%
  reduce(semi_join, "class") %>%
  arrange(animal)




# (4) Joining on multiple criteria

# Elephant and Penguin dataset - does not have common name
genus <- c("Elephas", "Loxodonta", "Loxodonta", "Aptenodytes", "Pygoscelis", "Pygoscelis", "Aptenodytes")
species <- c("maximus", "africana", "cyclotis", "fosteri", "papua", "antartica", "patagonicus")

# Create table of elephant species with common name
elephant_genus <- c("Elephas", "Loxodonta", "Loxodonta")
elephant_species <- c("maximus", "africana", "cyclotis")
elephant_name <- c("asian elephant", "african bush elephant", "african forest elephant")

# Create table of penguin species with common name
penguin_genus <- c("Aptenodytes", "Pygoscelis", "Pygoscelis", "Aptenodytes")
penguin_species <- c("fosteri", "papua", "antartica", "patagonicus")
penguin_name <- c("emperor penguin", "gentoo penguin", "chinstrap penguin", "king penguin")

# Data frame with elephant and penguin species, want common name
elephants_penguins <- data_frame(genus, species)
elephants_penguins

# Tables with elephant and penguin species and common names 
elephant_species <- data_frame(genus = elephant_genus, species = elephant_species, name = elephant_name)
penguin_species <- data_frame(genus = penguin_genus, species = penguin_species, name = penguin_name)

elephant_species
penguin_species

# Trying to use the reduce function on all three datasets
list(elephants_penguins, elephant_species, penguin_species) %>%
  reduce(left_join, c("genus", "species"))

# Can use bind rows first and then a right join to pull in common names
elephant_species %>% 
  bind_rows(penguin_species) %>%
  right_join(elephants_penguins, by = c("genus", "species"))