
# BDSF
BDSF <- read.csv('data-raw/BDSF.txt', header = FALSE)[ , 1]
save(BDSF, file = 'data/BDSF.RData')

# CPBP2014
CPBP2014 <- read.csv('data-raw/CPBP2014.txt', header = FALSE)[ , 1]
save(CPBP2014, file = 'data/CPBP2014.RData')

# EDPM2014
EDPM2014 <- read.csv('data-raw/EDPM2014.txt', header = FALSE)[ , 1]
save(EDPM2014, file = 'data/EDPM2014.RData')

# EPWS2014
EPWS2014 <- read.csv('data-raw/EPWS2014.txt', header = FALSE)[ , 1]
save(EPWS2014, file = 'data/EPWS2014.RData')

