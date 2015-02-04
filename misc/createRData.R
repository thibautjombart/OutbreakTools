

fasta2DNAbin.github <- function(github.fasta) {

	require(RCurl)
	require(seqinr)
	require(ape)

	urlSeq <- getURL(github.fasta)
	tmp <- read.fasta(textConnection(urlSeq))
	tmp2 <- as.matrix(sapply(tmp, function(x) as.vector(x)))
	my.seq <- as.DNAbin(t(tmp2))

	return(my.seq)
}

BeastRData <- function(dir_RData) {
	#create a RData with Beast phylogenies
	#H1N1 2009 pandemic 
	#data are extracted from Trevor Bedford's BEAST tutorial: https://github.com/trvrb/influenza-dynamics-practical

	github.fasta <- "https://raw.github.com/trvrb/influenza-dynamics-practical/master/data/pandemic.fasta"
	dna <- fasta2DNAbin.github(github.fasta)

	require(RCurl)
	require(ape)
	require(OutbreakTools)


	urlPhylo <- getURL("https://raw.github.com/trvrb/influenza-dynamics-practical/master/output/pandemic_geo.mcc")
	my.phylo <- read.nexus(textConnection(urlPhylo))

	urlPhylo_posterior <- getURL("https://raw.github.com/trvrb/dynamics-practical/master/output/pandemic_geo.trees")

	seq.date <- as.Date(.extract.string(my.phylo$tip.label, "_", 1, from = "last"))
	seq.location <- .extract.string(my.phylo$tip.label, "_", 2, from = "last")

	individuals <- data.frame(individualID = seq_along(my.phylo$tip.label), location= seq.location)
	samples <- data.frame(individualID = individuals$individualID, sampleID = individuals$individualID, date = seq.date, sequenceID = my.phylo$tip.label)
	trees<-c(my.phylo)
	
	FluH1N1pdm2009<-list(individuals= individuals, samples= samples, dna=dna, trees= trees)
	
	save(FluH1N1pdm2009,file=paste(dir_RData,"FluH1N1pdm2009.RData",sep="/"))
	
}

localBeastRData <- function() {

	require(ape)
	require(seqinr)

	dna <- read.fasta(file.path(dir_data,"pandemic.fasta"))
	dna <- as.matrix(sapply(dna, function(x) as.vector(x)))
	dna <- as.DNAbin(t(dna))

	sampled_phylo <- read.nexus(file.path(dir_tree,"pandemic_geo.trees"))

	# read beast log and pick ML tree
	df_log <- read.table(file.path(dir_tree,"pandemic_geo.log"),h=T)
	tree_ll <- df_log$posterior
	ind <- which.max(tree_ll)

	my.phylo <- sampled_phylo[[ind]]

	seq.date <- as.Date(.extract.string(my.phylo$tip.label, "_", 1, from = "last"))
	seq.location <- .extract.string(my.phylo$tip.label, "_", 2, from = "last")

	individuals <- data.frame(individualID = seq_along(my.phylo$tip.label), location= seq.location)
	samples <- data.frame(individualID = individuals$individualID, sampleID = individuals$individualID, date = seq.date, sequenceID = my.phylo$tip.label)
	trees<-c(my.phylo)
	
	FluH1N1pdm2009 <- list(individuals= individuals, samples= samples, dna=dna, trees= trees)
	
	save(FluH1N1pdm2009,file=paste(dir_pkg_data,"FluH1N1pdm2009.RData",sep="/"))


}

main<-function(){
	
	dir_RData<-"/Users/tonton/Documents/GitProjects/OutbreakTools/code/pkg/data"
	BeastRData(dir_RData)
	load(paste(dir_RData,"FluH1N1pdm2009.RData",sep="/"))
	dir_data <- "/Users/Tonton/work/projects/OutbreakTools/dynamics-practical-master/data"
	dir_tree <- "/Users/Tonton/work/projects/OutbreakTools/dynamics-practical-master/output"
	dir_pkg_data <- "/Users/Tonton/work/projects/OutbreakTools/code/pkg/data"
	dir_tmp <- "/Users/Tonton/work/projects/OutbreakTools"

	# dir_RData<-"/Users/Tonton/work/projects/OutbreakTools/code/pkg/data"
	# BeastRData(dir_RData)
	# load(paste(dir_RData,"FluH1N1pdm2009.RData",sep="/"))

	load(paste(dir_tmp,"FluH1N1pdm2009.RData",sep="/"))
	attach(FluH1N1pdm2009)

	x <- new("obkData", individuals = individuals, dna = FluH1N1pdm2009$dna,
		dna.individualID = samples$individualID, dna.date = samples$date,
		trees = FluH1N1pdm2009$trees)

	detach(FluH1N1pdm2009)

## change x breaks and labels
	p <- plotggphy(x, ladderize = TRUE, branch.unit = "year", major.breaks = "2 month", axis.date.format = "%b%Y", tip.color = "location", tip.size=3, tip.alpha=0.75)
	# p

	png(file.path(dir_tmp,"H1N1_tree.png"),units="in",res=300,width=7,height=7)
	print(p)
	dev.off()


}

main()


