rm()
#6a
library(DoE.base)
design_6a <- fac.design(nlevels=c(2,2,2,2), factor.names=c("A","B","C","D"))
gwl_6a <- GWLP(design_6a)
summary(design_6a)
print(gwl_6a)

#6b
library(DoE.wrapper)
design_6b <- FrF2(nfactors=4, nruns=16, blocks=4, alias.block.2fis=TRUE, 
                  randomize=FALSE)
gwl_6b <- GWLP(design_6b)
summary(design_6b)
print(gwl_6b)

#6c
library(FrF2)
generatorsA_6c <- c("ABC","BCD")
generatorsB_6c <- c("ABC","ADE")
generatorsC_6c <- c("ABCD","ABDE")
DesignA_6c <- FrF2(nfactors = 7, nruns = 2^(7-2), resolution = 4, 
                   generators = generatorsA_6c)
DesignB_6c <- FrF2(nfactors = 7, nruns = 2^(7-2), resolution = 4, 
                   generators = generatorsB_6c)
DesignC_6c <- FrF2(nfactors = 7, nruns = 2^(7-2), resolution = 4, 
                   generators = generatorsC_6c)

print(DesignA_6c)
print(DesignB_6c)
print(DesignC_6c)

summary(DesignA_6c)
summary(DesignB_6c)
summary(DesignC_6c)

#6d
library(FrF2)
generatorsA <- c("ABCD","ABCE","ABDE","ACDE","BCDE") 
generatorsB <- c("ABC","BCD","ACD","ABD","ABCD","AB")

DesignA <- FrF2(nfactors = 10, nruns = 2^(10-5), generators = generatorsA)
DesignB <- FrF2(nfactors = 10, nruns = 2^(10-6), generators = generatorsB)

print(DesignA)
print(DesignB)

summary(DesignA)
summary(DesignB)

#6e
library(FrF2)
factors <- c("A", "B", "C", "D", "E", "F", "G", "H")

design_6e <- FrF2(64, nfactors = 8, factors = factors)
print(design_6e)
summary(design_6e)