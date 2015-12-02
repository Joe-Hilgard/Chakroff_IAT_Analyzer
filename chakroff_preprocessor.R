# Load (and install) dependencies
if(!require(dplyr)){install.packages('dplyr')}
library(dplyr)

# Import data file
iatdat = read.csv(file.choose(), stringsAsFactors=F)
# Discard redundant column names
iatdat=iatdat[-1,]

# Get columns representing IAT data.
blockCols = grep("Part", names(iatdat))
# Prepare holster data frame
outDat = NULL
for (i in 1:nrow(iatdat)) {
  for (j in blockCols) {
    # Split text cell into individual trials
    y = iatdat[i, j] %>% 
      strsplit(split=",") %>% 
      unlist
    # split trial text into individual codes
    stim = substr(y, 1, 2)
    acc = substr(y, 3, 3)
    rt = substr(y, 4, nchar(y)) # note use of nchar b/c rt can be any number of digits long
    # Put together as data frame
    export_ij = data.frame("sub" = i,
                           "block" = j - min(blockCols) + 1,
                           "stim" = stim,
                           "acc" = ifelse(acc == "C", 1, 0),
                           "rt" = as.numeric(rt))
    # Concatenate export_ij to outDat
    outDat = rbind(outDat, export_ij)
  }
}

# Discard "EN" trials
outDat = outDat %>%
  filter(stim != "EN")

# Give blocks non-numeric labels
outDat$block = paste("block", outDat$block, sep = "")

# Here it would be good to add codes for:
  # Congruency of block
  # Stimulus content per trial

# Data export
write.table(outDat, "flat_data.txt", sep = "\t", row.names=F)