## load the dataset
data(FluH1N1pdm2009)
## create obkData object
x <- new("obkData", individuals = FluH1N1pdm2009$individuals, samples = FluH1N1pdm2009$samples,
    dna = FluH1N1pdm2009$dna, trees = FluH1N1pdm2009$trees)

## first simple tree
p <- plotggphy(x, ladderize = TRUE)

## build tip attribute and use sample dates to scale the x-axis as date time
p <- plotggphy(x, ladderize = TRUE, build.tip.attribute = TRUE, branch.unit = "year",
    tip.dates = "date")

## alternatively extract sample dates from the tip labels
p <- plotggphy(x, ladderize = TRUE, branch.unit = "year", guess.tip.dates.from.labels = TRUE,
    set.guess = list(prefix = "_", order = 1, from = "last"))

## change x breaks and labels
p <- plotggphy(x, ladderize = TRUE, build.tip.attribute = TRUE, branch.unit = "year",
    tip.dates = "date", major.breaks = "month", axis.date.format = "%b%Y")

## color-code tip location
p <- plotggphy(x, ladderize = TRUE, build.tip.attribute = TRUE, branch.unit = "year",
    tip.dates = "date", tip.colour = "location")

## change tip size and transparency
p <- plotggphy(x, ladderize = TRUE, build.tip.attribute = TRUE, branch.unit = "year",
    tip.dates = "date", tip.colour = "location", tip.size = 3, tip.alpha = 0.75)