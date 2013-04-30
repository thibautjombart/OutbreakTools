## load the dataset
data(FluH1N1pdm2009)
## create obkData object
x <- new("obkData", individuals = FluH1N1pdm2009$individuals, samples = FluH1N1pdm2009$samples,
dna = FluH1N1pdm2009$dna, trees = FluH1N1pdm2009$trees)
## have a look at the summary
summary(x)
