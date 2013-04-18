

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
	urlPhylo <- getURL("https://raw.github.com/trvrb/influenza-dynamics-practical/master/output/pandemic_geo.mcc")
	my.phylo <- read.nexus(textConnection(urlPhylo))


	seq.date <- as.Date(.extract.string(my.phylo$tip.label, "_", 1, from = "last"))
	seq.location <- .extract.string(my.phylo$tip.label, "_", 2, from = "last")

	individuals <- data.frame(individualID = seq_along(my.phylo$tip.label), location= seq.location)
	samples <- data.frame(individualID = individuals$individualID, sampleID = individuals$individualID, date = seq.date, seqID = my.phylo$tip.label)
	trees<-c(my.phylo)
	
	H1N1pdm2009<-list(individuals= individuals, samples= samples, dna=dna, trees= trees)
	
	save(H1N1pdm2009,file=paste(dir_RData,"H1N1pdm2009.RData",sep="/"))
	
}

main<-function(){
	
	dir_RData<-"/Users/tonton/Documents/GitProjects/epibase/code/pkg/data"
	BeastRData(dir_RData)
	load(paste(dir_RData,"H1N1pdm2009.RData",sep="/"))

}

main()


