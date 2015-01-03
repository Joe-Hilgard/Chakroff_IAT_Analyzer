iatdat = read.csv(file.choose(), stringsAsFactors=F)
iatdat=iatdat[-1,]

x = iatdat[,11]
y = strsplit(x, split=",")[[1]]
y = y[y!="END"]
stim = substr(y, 1, 2)
acc = substr(y, 3, 3)
RT = substr(y, 4, nchar(y))

outdat = data.frame("sub" = 1
                    , "block" = 1
                    , "stim" = stim
                    , "acc" = acc
                    , "RT" = RT)


for (i in 11:17) { #select block
  
  
}