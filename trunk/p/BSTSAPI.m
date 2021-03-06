BSTSAPI	;GDIT/HS/BEE-Standard Terminology API Program ; 5 Nov 2012  9:53 AM
	;;1.0;IHS STANDARD TERMINOLOGY;;Sep 10, 2014;Build 101
	;
	Q
	;
SEARCH(OUT,IN)	;PEP - Perform Codeset Search
	;
	; See SEARCH^BSTSAPIA for a detailed description of the input parameters
	; and the output format.
	;
	Q $$SEARCH^BSTSAPIA(OUT,$G(IN))
	;
CODESETS(OUT,IN)	;PEP - Return list of available code sets
	;
	; See CODESETS^BSTSAPIA for a detailed description of the input parameters
	; and the output format.
	;
	Q $$CODESETS^BSTSAPIA(OUT,$G(IN))
	;
VERSIONS(OUT,IN)	;PEP - Return a list of available versions for a code set
	;
	; See VERSIONS^BSTSAPIA for a detailed description of the input parameters
	; and the output format.
	;
	Q $$VERSIONS^BSTSAPIA(OUT,$G(IN))
	;
CVRSN(OUT,IN)	;PEP - Return the Current Version For the Code Set
	;
	; See CVRSN^BSTSAPID for a detailed description of the input parameters
	; and the output format.
	;
	Q $$CVRSN^BSTSAPID(OUT,$G(IN))
	;
SUBSET(OUT,IN)	;PEP - Return the list of subsets available for a Code Set
	;
	; See SUBSET^BSTSAPIA for a detailed description of the input parameters
	; and the output format.
	;
	Q $$SUBSET^BSTSAPIA(OUT,$G(IN))
	;
VALTERM(OUT,IN)	;PEP - Returns whether a given term is a valid
	;
	; See VALTERM^BSTSAPIB for a detailed description of the input parameters
	; and the output format.
	;
	Q $$VALTERM^BSTSAPIB(OUT,$G(IN))
	;
DSCLKP(OUT,IN)	;PEP - Returns detail information for a specified Description Id
	;
	; See DSCLKP^BSTSAPIB for a detailed description of the input parameters
	; and the output format.
	;
	Q $$DSCLKP^BSTSAPIB(OUT,$G(IN))
	;
DTSLKP(OUT,IN)	;PEP - Returns detail information for a specified DTS Id
	;
	; See DTSLKP^BSTSAPIB for a detailed description of the input parameters
	; and the output format.
	;
	Q $$DTSLKP^BSTSAPIB(OUT,$G(IN))
	;
CNCLKP(OUT,IN)	;PEP - Returns detail information for a specified Concept Id
	;
	; See CNCLKP^BSTSAPIB for a detailed description of the input parameters
	; and the output format.
	;
	Q $$CNCLKP^BSTSAPIB(OUT,$G(IN))
	;
ASSOC(IN)	;PEP - Returns the associations for each type (SMD, RxNorm, UNII)
	;
	; See ASSOC^BSTSAPIF for a detailed description of the input parameters
	; and the output format.
	;
	Q $$ASSOC^BSTSAPIF($G(IN))
	;
DI2RX(IN)	;PEP - Performs a drug ingredient lookup on a specified value
	;
	; Returns only the first RxNorm mapping as a function call output
	; See DI2RX^BSTSAPIF for a detailed description of the input parameters
	; and the output format.
	;
	Q $$DI2RX^BSTSAPIF($G(IN))
	;
MPADVICE(OUT,IN)	;PEP - Returns ICD-10 mapping information for a specified Concept Id
	;
	; See MPADVICE^BSTSAPIC for a detailed description of the input parameters
	; and the output format.
	;
	Q $$MPADVICE^BSTSAPIC(OUT,$G(IN))
	;
SUBLST(OUT,IN)	;PEP - Returns a list of concepts in a specified subset
	;
	; See SUBLST^BSTSAPIC for a detailed description of the input parameters
	; and the output format.
	;
	Q $$SUBLST^BSTSAPIC(OUT,$G(IN))
	;
VALSBTRM(OUT,IN)	;PEP - Returns whether a given term is in a particular subset
	;
	; See VALSBTRM^BSTSAPIB for a detailed description of the input parameters
	; and the output format.
	;
	Q $$VALSBTRM^BSTSAPIB(OUT,IN)
	;
ICD2SMD(OUT,IN)	;PEP - Returns the SNOMED terms which map to a given ICD9 code
	;
	; See ICD2SMD^BSTSAPID for a detailed description of the input parameters
	; and the output format.
	;
	Q $$ICD2SMD^BSTSAPID(OUT,IN)
	;
DILKP(OUT,IN)	;PEP - Performs a drug ingredient lookup on a specified value
	;
	; See DILKP^BSTSAPIF for a detailed description of the input parameters
	; and the output format.
	Q $$DILKP^BSTSAPIF(OUT,IN)
	;
VSBTRMF(IN)	;PEP - Function Call: Returns whether a given term is in a particular subset
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 - Description Id of term to check
	;     - P2 - The subset to look in
	;     - P3 (Optional) - The code set Id (default SNOMED '36')
	;     - P4 (Optional) - LOCAL - Pass 1 or blank to perform local listing,
	;                       Pass 2 for remote DTS listing
	;     - P5 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	;
	; VAR = 1:Term is in the provided subset
	;       0:Term is not in the provided subset
	;
	NEW FOUT,STS,%D
	;
	S STS=$$VALSBTRM^BSTSAPIB("FOUT",IN)
	Q FOUT
	;
DESC(IN)	;PEP - Function Call: Returns detail information for a specified Description Id
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 - The Description Id to look up
	;     - P2 (Optional) - The code set Id (default SNOMED '36')
	;     - P3 (Optional) - LOCAL - Pass 1 or blank to perform local listing,
	;                       Pass 2 for remote DTS listing
	;     - P4 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	; Function returns - [1]^[2]^[3]
	; [1] - Concept Id
	; [2] - Term Description
	;
	Q $$DESC^BSTSAPIA($G(IN))
	;
CONC(IN)	;PEP - Returns detail information for a specified Concept Id
	;
	;Input
	; OUT - Output variable/global to return information in (VAR)
	;  IN - P1 - The Concept Id to look up
	;     - P2 (Optional) - The code set Id (default SNOMED '36')
	;     - P3 (Optional) - Snapshot Date to check (default DT)
	;     - P4 (Optional) - LOCAL - Pass 1 or blank to perform local listing,
	;                       Pass 2 for remote DTS listing
	;     - P5 (Optional) - DEBUG - Pass 1 to display debug information
	;
	;Output
	; Function returns - [1]^[2]^[3]^[4]
	; [1] - Description Id of Fully Specified Name
	; [2] - Fully Specified Name
	; [3] - Description Id of Preferred Term
	; [4] - Preferred Term
	;
	Q $$CONC^BSTSAPIA($G(IN))
