
require(tidyverse)

acts = read_csv('data/all_final_acts.csv', col_names = F) %>% 
  mutate(letter = letters) %>% 
  select(letter, everything())


letter_distances = acts %>% 
  select(-letter) %>% 
  dist()

letter_distances = as.matrix(letter_distances)
rownames(letter_distances) = letters
colnames(letter_distances) = letters

letter_distances = as.data.frame(as.table(letter_distances)) %>% 
  rename(letter_1 = Var1, letter_2 = Var2, orthographic_distance = Freq) %>% 
  filter(letter_1 != letter_2) %>% 
  filter(as.numeric(factor(letter_1)) > as.numeric(factor(letter_2)))

MSD = letter_distances %>% 
  summarise(M = mean(orthographic_distance),
            SD = sd(orthographic_distance))

letter_distances = letter_distances %>% 
  mutate(M = MSD$M,
         SD = MSD$SD,
         orthographic_distance_z = (orthographic_distance-M)/SD)

rm(acts)
