\name{fakefludata}
\docType{data}
\alias{fakefludata}

\title{Fake flu dataset}
\description{
  This data set is a fake small flu data set. It contains three objects.
	
	\code{Patientsdata} is a dataframe containing patients names and gender. 
	
	\code{samplefludata} is a datafram containing 
	\itemize{
		\item{\code{patient}: the patient names,}
		\item{\code{SampleID}: the samples ID,}
		\item{\code{Sampletype}: the sample type (one of nasal swab, blood sample or oral swab),}
		\item{\code{sampledate}: the date the sample was taken,}
		\item{\code{Flu.viro}: the results (+ or -) of a flu virology when performed,}
		\item{\code{Serology.2009.cali}: the results of a serology test against Flu 2009 California (), when performed,}
		\item{\code{Flu.seq}: an identifier to a viral sequence when performed,}
		\item{\code{Drug.Resistance.gene}: yes or no when a resistance test was performed.}
	}
	
	\code{clinicalfludata} is a list of the following dataframes: 
	\itemize{
		\item{\code{RespiratorySymptoms} contains individuals names, the severity characteristics of symptoms and the date of the symptoms,}
		\item{\code{Temperature} contains individuals names, the date (and am or pm) their temperature was measured and the temperature measured,}
		\item{\code{HospitalAdmission} contains individuals names, whether they have been admitted to hospital or not and if admitted, the date of admission,}
		\item{\code{Quarantine} contains individuals names and the date they were quarantined.}
	}
	
}
\keyword{dataset}
