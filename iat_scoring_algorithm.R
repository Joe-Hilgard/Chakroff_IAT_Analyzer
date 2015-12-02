library(dplyr)
library(tidyr)

dat = read.delim("flat_data.txt")

# Use data from blocks 3, 4, 6, and 7
dat = dat %>% 
  filter(block %in% c("block3", "block4", "block6", "block7"),
         rt < 10000)

# check for subjects w/ 10% of trials w/ RT < 300
dat = dat %>% 
  group_by(sub) %>% 
  summarise("pct_too_fast" = mean(rt < 300)) %>% 
  left_join(dat, ., by = "sub")

# Discard subjects w/ too many too-fast trials
dat = dat %>% 
  filter(pct_too_fast < .1)

# Make mean of correct latencies for each block
dat = dat %>% 
  group_by(sub, block) %>% 
  summarise("meanCRT_block" = mean(rt)) %>% 
  left_join(dat, ., by = c("sub", "block"))

# Make pooled SD for all trials in B3 & B6
dat = dat %>% 
  filter(block %in% c("block3", "block6")) %>% 
  group_by(sub) %>% 
  summarise("sdRT_b6b3" = sd(rt)) %>% 
  left_join(dat, ., by = "sub")

# And another pooled SD for all trials in B4 & B7
dat = dat %>% 
  filter(block %in% c("block4", "block7")) %>% 
  group_by(sub) %>% 
  summarise("sdRT_b7b4" = sd(rt)) %>% 
  left_join(dat, ., by = "sub")

# Replace each error latency with block mean + 600
dat$adjusted_rt = ifelse(dat$acc == 1, dat$rt, dat$meanCRT_block + 600)

# Average the resulting values for each of the four blocks
avg_dat = dat %>% 
  filter(block %in% c("block3", "block4", "block6", "block7")) %>% 
  group_by(sub, block) %>% 
  summarise("mean_crit_adj_RT" = mean(adjusted_rt)) %>% 
  spread(key = block, value = mean_crit_adj_RT)

# Compute two differences: B6 - B3 and B7 - B4
avg_dat = avg_dat %>% 
  mutate("diff_b6b3" = block6 - block3,
         "diff_b7b4" = block7 - block4)

# Divide each difference by its associated pooled-trials SD from Step 6
dat = dat %>% 
  left_join(., avg_dat, by = "sub") %>% 
  mutate("quotient_b6b3" = diff_b6b3 / sdRT_b6b3,
         "quotient_b7b4" = diff_b7b4 / sdRT_b7b4)

# Average the two quotients from Step 11
dat = dat %>% 
  mutate("dScore" = (quotient_b6b3 + quotient_b7b4)/2)

# OPTIONAL: prune intermediate columns
dat = dat %>% 
  select(-(pct_too_fast:quotient_b7b4))

# OPTIONAL: export only subject ID and IAT score
tinyDat = dat %>% 
  select(sub, dScore) %>% 
  distinct()

write.table(dat, "full_table.txt", sep="\t", row.names=F)
write.table(dat, "tiny_table.txt", sep="\t", row.names=F)
