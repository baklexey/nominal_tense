# This is statistical evidence for significance of Siberia for the distribution of Nominal Tense worldwide. 
# For this analysis Chi-square test and Fisher's exact test are used. 
# The table for Chi-square test lacks Africa and Papunesia, because they received null values in the row NT.

library(vcd)

nominal_tense_chi <- matrix(c(9,12,19,5,11,8,1,4), byrow=TRUE, ncol=4) #table for Chi-square test (without Africa and Papunesia)
rownames(nominal_tense_chi) <- c("No NT", "NT")
colnames(nominal_tense_chi) <- c("South America", "North America", "Australia", "Siberia")
nominal_tense_chi
chisq.test(nominal_tense_chi)
assocstats(nominal_tense_chi)


nominal_tense_fisher <- matrix(c(9,12,19,5, 20, 20, 11,8,1,4,0,0), byrow=TRUE, ncol=6) #complete table of values for all macroareas
rownames(nominal_tense_fisher) <- c("No NT", "NT")
colnames(nominal_tense_fisher) <- c("South America", "North America", "Australia", "Northeastern Siberia", "Africa", "Papunesia")
nominal_tense_fisher

chisq.test(nominal_tense_fisher)$expected[2,1] #expected value for all macroareas besides Siberia
chisq.test(nominal_tense_fisher)$expected[2,4] #expected value for Siberia

south_america_fisher <- matrix(c(11,9,14,75), byrow=TRUE, ncol=2) #table with parameters ± in South America (rows) and ± Nominal Tense (columns)
rownames(south_america_fisher) <- c("+SA", "-SA")
colnames(south_america_fisher) <- c("+NT", "-NT")
south_america_fisher
fisher.test(south_america_fisher, alternative = "greater")
assocstats(south_america_fisher)

north_america_fisher <- matrix(c(8,12,17,72), byrow=TRUE, ncol=2) #table with parameters ± in North America (rows) and ± Nominal Tense (columns)
rownames(north_america_fisher) <- c("+NA", "-NA")
colnames(north_america_fisher) <- c("+NT", "-NT")
north_america_fisher
fisher.test(north_america_fisher, alternative = "greater")
assocstats(north_america_fisher)

australia_fisher <- matrix(c(1,19,24,65), byrow=TRUE, ncol=2) #table with parameters ± in Australia (rows) and ± Nominal Tense (columns)
rownames(australia_fisher) <- c("+AU", "-AU")
colnames(australia_fisher) <- c("+NT", "-NT")
australia_fisher
fisher.test(australia_fisher, alternative = "less")
assocstats(australia_fisher)

siberia_fisher <- matrix(c(5,4,20,80), byrow=TRUE, ncol=2) #table with parameters ± in Siberia (rows) and ± Nominal Tense (columns)
rownames(siberia_fisher) <- c("+SI", "-SI")
colnames(siberia_fisher) <- c("+NT", "-NT")
siberia_fisher
fisher.test(siberia_fisher, alternative = "greater")
assocstats(siberia_fisher)

africa_fisher <- matrix(c(0,20,25,64), byrow=TRUE, ncol=2) #table with parameters ± in Africa (rows) and ± Nominal Tense (columns)
rownames(africa_fisher) <- c("+AF", "-AF")
colnames(africa_fisher) <- c("+NT", "-NT")
africa_fisher
fisher.test(africa_fisher, alternative = "less")
assocstats(africa_fisher)

papunesia_fisher <- matrix(c(0,20,25,64), byrow=TRUE, ncol=2) #table with parameters ± in Papunesia (rows) and ± Nominal Tense (columns)
rownames(papunesia_fisher) <- c("+AF", "-AF")
colnames(papunesia_fisher) <- c("+NT", "-NT")
papunesia_fisher
fisher.test(papunesia_fisher, alternative = "less")
assocstats(papunesia_fisher)


