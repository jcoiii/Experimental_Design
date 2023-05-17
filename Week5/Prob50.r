library(FrF2)

factors <- c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K")
design <- FrF2(nruns = 16, nfactors = length(factors),
factors = factors, gen.level = c(2, 2, 2, 2, 1, 1, 1, 1, 1, 1))

defining_relation <- attr(design, "defn.rel")
print(defining_relation)

alias_table <- alias(design)
print(alias_table)
