BSTSDTS3	;GDIT/HS/BEE-Standard Terminology DTS Calls/Processing ; 5 Nov 2012  9:53 AM
	;;1.0;IHS STANDARD TERMINOLOGY;**2**;Sep 10, 2014;Build 59
	;
	Q
	;
SUPDATE(NMID,ROUT)	;EP - Add/Update Special Codeset Entries
	;
	;This update section only applies to special codesets
	I ($G(NMID)=5180)!($G(NMID)=1552)!($G(NMID)=36) Q 1
	;
	N GL,CONCDA,BSTSC,INMID,ERROR,TCNT,I,CVRSN,ST,NROUT,TLIST,STYPE,RTR
	;
	S GL=$NA(^TMP("BSTSCMCL",$J,1))
	S ROUT=$G(ROUT,"")
	;
	;Look for Concept Id
	I $P($G(@GL@("CONCEPTID")),U)="" Q 0
	;
	;Look for existing entry
	I $G(@GL@("DTSID"))="" Q 0
	S CONCDA=$O(^BSTS(9002318.4,"D",NMID,@GL@("DTSID"),""))
	;
	;Pull internal Code Set ID
	S INMID=$O(^BSTS(9002318.1,"B",NMID,"")) Q:INMID="" "0"
	;
	;Pull the current version
	S CVRSN=$$GET1^DIQ(9002318.1,INMID_",",.04,"I")
	;
	;Handle retired concepts
	I CONCDA]"",'$$RET(CONCDA,CVRSN,GL) Q 0
	;
	;None found - create new entry
	I CONCDA="" S CONCDA=$$NEWC^BSTSDTS0()
	;
	;Verify entry found/created
	I +CONCDA<0 Q 0
	;
	;Get Revision Out
	S NROUT=$P(@GL@("CONCEPTID"),U,3) S:NROUT="" NROUT=ROUT
	;
	;Set up top level concept fields
	S BSTSC(9002318.4,CONCDA_",",.02)=$P(@GL@("CONCEPTID"),U) ;Concept ID
	S BSTSC(9002318.4,CONCDA_",",.08)=@GL@("DTSID") ;DTS ID
	S BSTSC(9002318.4,CONCDA_",",.07)=INMID ;Code Set
	S BSTSC(9002318.4,CONCDA_",",.03)="N"
	S BSTSC(9002318.4,CONCDA_",",.05)=$$EP2FMDT^BSTSUTIL($P(@GL@("CONCEPTID"),U,2),1)
	S BSTSC(9002318.4,CONCDA_",",.06)=$$EP2FMDT^BSTSUTIL(NROUT,1)
	S BSTSC(9002318.4,CONCDA_",",.11)="N"
	S BSTSC(9002318.4,CONCDA_",",.04)=CVRSN
	S BSTSC(9002318.4,CONCDA_",",.12)=DT
	S BSTSC(9002318.4,CONCDA_",",1)=$G(@GL@("FSN",1))
	;
	;Need to interim save because subsets look at .07
	I $D(BSTSC) D FILE^DIE("","BSTSC","ERROR")
	;
	;Save Subsets
	;
	;Clear out existing entries
	D
	. NEW SB
	. S SB=0 F  S SB=$O(^BSTS(9002318.4,CONCDA,4,SB)) Q:'SB  D
	.. NEW DA,DIK
	.. S DA(1)=CONCDA,DA=SB
	.. S DIK="^BSTS(9002318.4,"_DA(1)_",4," D ^DIK
	I $D(@GL@("SUB"))>1 D
	. ;
	. NEW SB
	. S SB="" F  S SB=$O(@GL@("SUB",SB)) Q:SB=""  D
	.. ;
	.. NEW DIC,X,Y,DA,X,Y,IENS,DLAYGO
	.. S DA(1)=CONCDA
	.. S DIC(0)="L",DIC="^BSTS(9002318.4,"_DA(1)_",4,"
	.. S X=$P($G(@GL@("SUB",SB)),U) Q:X=""
	.. S DLAYGO=9002318.44 D ^DIC
	.. I +Y<0 Q
	.. S DA=+Y
	.. S IENS=$$IENS^DILF(.DA)
	.. S BSTSC(9002318.44,IENS,".02")=$$DTS2FMDT^BSTSUTIL($P($G(@GL@("SUB",SB)),U,2))
	;
	;Save Associations
	;
	;Clear out existing entries
	D
	. NEW AS
	. S AS=0 F  S AS=$O(^BSTS(9002318.4,CONCDA,9,AS)) Q:'AS  D
	.. NEW DA,DIK
	.. S DA(1)=CONCDA,DA=AS
	.. S DIK="^BSTS(9002318.4,"_DA(1)_",9," D ^DIK
	I $D(@GL@("ASC"))>1 D
	. ;
	. ;
	. NEW AS
	. S AS="" F  S AS=$O(@GL@("ASC",AS)) Q:AS=""  D
	.. ;
	.. NEW DIC,X,Y,DA,X,Y,IENS,DLAYGO
	.. S DA(1)=CONCDA
	.. S DIC(0)="L",DIC="^BSTS(9002318.4,"_DA(1)_",9,"
	.. S X=$P($G(@GL@("ASC",AS)),U) Q:X=""
	.. S DLAYGO=9002318.49 D ^DIC
	.. I +Y<0 Q
	.. S DA=+Y
	.. S IENS=$$IENS^DILF(.DA)
	.. S BSTSC(9002318.49,IENS,".02")=$P($G(@GL@("ASC",AS)),U,2)
	.. S BSTSC(9002318.49,IENS,".03")=$P($G(@GL@("ASC",AS)),U,3)
	;
	I $D(BSTSC) D FILE^DIE("","BSTSC","ERROR")
	;
	;Now save Terminology entries
	;
	;Synonyms/Preferred/FSN
	;
	S STYPE="" F  S STYPE=$O(@GL@("SYN",STYPE)) Q:STYPE=""  S TCNT="" F  S TCNT=$O(@GL@("SYN",STYPE,TCNT)) Q:TCNT=""  D
	. ;
	. N TERM,TYPE,DESC,BSTST,ERROR,TMIEN,AIN
	. ;
	. ;Pull values
	. S TERM=$G(@GL@("SYN",STYPE,TCNT,1)) Q:TERM=""
	. ;
	. ;Quit if already found
	. I $D(TLIST(TERM)) Q
	. S TLIST(TERM)=""
	. ;
	. S TYPE=$P($G(@GL@("SYN",STYPE,TCNT,0)),U,2)
	. S TYPE=$S(TYPE=1:"P",1:"S")
	. I TERM=$G(@GL@("FSN",1)) S TYPE="F"
	. S DESC=$P($G(@GL@("SYN",STYPE,TCNT,0)),U) Q:DESC=""
	. S AIN=$$EP2FMDT^BSTSUTIL($P($G(@GL@("SYN",STYPE,TCNT,0)),U,3))
	. ;
	. ;Look up entry
	. S TMIEN=$O(^BSTS(9002318.3,"D",INMID,DESC,""))
	. ;
	. ;Entry not found - create new one
	. I TMIEN="" S TMIEN=$$NEWT^BSTSDTS0()
	. I TMIEN<0 Q
	. ;
	. ;Save/update other fields
	. S BSTST(9002318.3,TMIEN_",",.02)=DESC
	. S BSTST(9002318.3,TMIEN_",",.09)=TYPE
	. S BSTST(9002318.3,TMIEN_",",1)=TERM
	. S BSTST(9002318.3,TMIEN_",",.04)="N"
	. S BSTST(9002318.3,TMIEN_",",.05)=CVRSN
	. S BSTST(9002318.3,TMIEN_",",.08)=INMID
	. S BSTST(9002318.3,TMIEN_",",.03)=CONCDA
	. S BSTST(9002318.3,TMIEN_",",.06)=AIN
	. S BSTST(9002318.3,TMIEN_",",.1)=DT
	. S BSTST(9002318.3,TMIEN_",",.11)="N"
	. D FILE^DIE("","BSTST","ERROR")
	. ;
	. ;Reindex - needed for custom indices
	. D
	.. NEW DIK,DA
	.. S DIK="^BSTS(9002318.3,",DA=TMIEN
	.. D IX^DIK
	;
	;Need to check for retired concepts again since it may have just been added
	S RTR=$$RET^BSTSDTS3(CONCDA,CVRSN,GL)
	;
	Q $S($D(ERROR):"0^Update Failed",1:1)
	;
USEARCH(OUT,BSTSWS)	;EP - DTS4 UNIVERSE Search Call
	;
	NEW STS,II,SEARCH,STYPE,SLIST,DLIST,OCNT,MAX,NMID,RES
	;
	S SEARCH=$G(BSTSWS("SEARCH"))
	S STYPE=$G(BSTSWS("STYPE"))
	S SLIST=$NA(^TMP("BSTSSLST",$J)) ;Sorted List
	S DLIST=$NA(^TMP("BSTSCMCL",$J)) ;DTS Return List
	K @SLIST,@DLIST,@OUT
	S OCNT=0
	;
	;Determine maximum to return
	S MAX=$G(BSTSWS("MAXRECS")) S:MAX="" MAX=25
	S NMID=$G(BSTSWS("NAMESPACEID")) S:NMID="" NMID=36 S:NMID=30 NMID=36
	;
	;Loop through each word
	S BSTSWS("SEARCH")=SEARCH
	;
	;Perform DTS Search
	I STYPE="S" S STS=$$TRMSRCH^BSTSCMCL(.BSTSWS,.RES) I $G(BSTSWS("DEBUG")) W !!,STS
	;
	;Perform DTS concept search
	I STYPE="F" S STS=$$CONSRCH^BSTSCMCL(.BSTSWS,.RES) I $G(BSTSWS("DEBUG")) W !!,STS
	;
	;Loop through results and retrieve detail
	M @SLIST=@DLIST
	;
	I $O(@SLIST@(""))]"" S II="" F  S II=$O(@SLIST@(II)) Q:II=""  D
	. NEW DTSID,DSCID,CONC,STATUS,CONCID,FSNT,FSND,REL,SYN
	. NEW SUB,ERSLT,PRD,PRT,PRSY,ASSOC,MAPP
	. ;
	. S DTSID=$P(@SLIST@(II),U) Q:DTSID=""
	. S DSCID=$P(@SLIST@(II),U,2) I STYPE="S",DSCID="" Q
	. ;
	. I $G(BSTSWS("DEBUG")) W !,"DTSID: ",DTSID
	. ;
	. ;Check for maximum
	. I $G(OCNT)'<MAX Q
	. ;
	. ;Look for detail stored locally
	. S CONC=$$CONC^BSTSDTS0(DTSID,.BSTSWS)
	. ;
	. I $G(BSTSWS("DEBUG")) W !!,"DETAIL CONC: ",CONC
	. ;
	. ;Now get the detail
	. ;
	. ;Not Found or in need of update
	. S BSTSWS("DTSID")=DTSID
	. ;
	. ;Clear result file
	. K @DLIST
	. ;
	. ;Get Detail for concept
	. S STATUS=$$DETAIL^BSTSCMCL(.BSTSWS,.ERSLT)
	. ;
	. ;Concept ID
	. S CONCID=$P($G(@DLIST@(1,"CONCEPTID")),U)
	. ;
	. ;FSN
	. S FSNT=$P($G(@DLIST@(1,"FSN",1)),U)
	. S FSND=""
	. ;
	. ;ISA
	. S REL="" I $D(@DLIST@(1,"ISA")) D
	.. NEW ICNT
	.. S ICNT="" F  S ICNT=$O(@DLIST@(1,"ISA",ICNT)) Q:ICNT=""  D
	... NEW DTS,TRM
	... S DTS=$P($G(@DLIST@(1,"ISA",ICNT,0)),"^")
	... S TRM=$P($G(@DLIST@(1,"ISA",ICNT,1)),"^")
	... S REL=REL_$S(REL]"":$C(28),1:"")_DTS_$C(29)_TRM_$C(29)_"ISA"
	. ;
	. ;Child
	. I $D(@DLIST@(1,"SUBC")) D
	.. NEW ICNT
	.. S ICNT="" F  S ICNT=$O(@DLIST@(1,"SUBC",ICNT)) Q:ICNT=""  D
	... NEW DTS,TRM
	... S DTS=$P($G(@DLIST@(1,"SUBC",ICNT,0)),"^")
	... S TRM=$P($G(@DLIST@(1,"SUBC",ICNT,1)),"^")
	... S REL=REL_$S(REL]"":$C(28),1:"")_DTS_$C(29)_TRM_$C(29)_"CHD"
	. ;
	. ;Synonyms
	. S SYN="",(PRT,PRD)="",PRSY="S" I $D(@DLIST@(1,"SYN")) D
	.. NEW ICNT
	.. S ICNT="" F  S ICNT=$O(@DLIST@(1,"SYN",ICNT)) Q:ICNT=""  D
	... NEW TRM,DSC,PS
	... S TRM=$P($G(@DLIST@(1,"SYN",ICNT,1)),"^")
	... S PS=$P($G(@DLIST@(1,"SYN",ICNT,0)),"^",2)
	... S DSC=$P($G(@DLIST@(1,"SYN",ICNT,0)),"^")
	... ;
	... ;Look for FSN
	... I TRM]"",FSNT=TRM D  I $G(@DLIST@(1,"NAMESP"))=36 Q
	.... S FSND=DSC
	... ;
	... I DSCID]"",DSCID=DSC D
	.... S PRD=DSCID,PRT=TRM
	.... S:PS=1 PRSY="P"
	... I DSCID="",PS=1 S PRD=DSC,PRT=TRM
	... S SYN=SYN_$S(SYN]"":$C(28),1:"")_DSC_$C(29)_TRM_$C(29)_$S(PS=1:"Preferred",1:"Synonym")
	. ;
	. ;Subsets
	. S SUB="" I $D(@DLIST@(1,"SUB")) D
	.. NEW ICNT
	.. S ICNT="" F  S ICNT=$O(@DLIST@(1,"SUB",ICNT)) Q:ICNT=""  D
	... NEW SB
	... S SB=$P($G(@DLIST@(1,"SUB",ICNT)),U)
	... S SUB=SUB_$S(SUB]"":$C(28),1:"")_SB
	. ;
	. ;Associations
	. S ASSOC="" I $D(@DLIST@(1,"ASC")) D
	.. NEW ICNT
	.. S ICNT="" F  S ICNT=$O(@DLIST@(1,"ASC",ICNT)) Q:ICNT=""  D
	... NEW ANODE,ASC,ASN,AST,CDIEN
	... S ANODE=$G(@DLIST@(1,"ASC",ICNT))
	... S ASC=$P(ANODE,U)
	... S ASN=$P(ANODE,U,2) Q:ASN=""
	... S CDIEN=$O(^BSTS(9002318.1,"B",ASN,"")) Q:CDIEN=""
	... S ASN=$$GET1^DIQ(9002318.1,CDIEN_",",.03,"I") Q:ASN=""
	... S AST=$P(ANODE,U,4) S:AST["[" ASC=""
	... S ASSOC=ASSOC_$S(ASSOC]"":$C(28),1:"")_ASN_": "_AST_$S(ASC="":"",1:" ["_ASC_"]")
	. ;
	. ;Mappings
	. S MAPP="" I $D(@DLIST@(1,"NDC")) D
	.. NEW ICNT
	.. S ICNT="" F  S ICNT=$O(@DLIST@(1,"NDC",ICNT)) Q:ICNT=""  D
	... NEW NDC
	... S NDC=$P($G(@DLIST@(1,"NDC",ICNT)),U)
	... S MAPP=MAPP_$S(MAPP]"":$C(28),1:"")_"NDC: "_NDC
	. ;
	. ;VUID
	. I $D(@DLIST@(1,"VUID")) D
	.. NEW ICNT
	.. S ICNT="" F  S ICNT=$O(@DLIST@(1,"VUID",ICNT)) Q:ICNT=""  D
	... NEW VUID
	... S VUID=$P($G(@DLIST@(1,"VUID",ICNT)),U)
	... S MAPP=MAPP_$S(MAPP]"":$C(28),1:"")_"VUID: "_VUID
	. ;
	. ;Save the detail
	. S OCNT=OCNT+1
	. S @OUT@(OCNT)=CONCID_U_PRT_U_PRD_U_FSNT_U_FSND_U_SYN_U_REL_U_DTSID_U_SUB_U_PRSY_U_ASSOC_U_MAPP
	;
	K @DLIST,@SLIST
	;
	Q STS
	;
RET(CONCDA,CVRSN,GL)	;Handle retired concepts
	;
	;Input
	; CONCDA - Pointer to concept file, if populated
	; CVRSN - Current codeset version
	; GL - Name of scratch global
	;
	;Output - 1 - Retired Concept
	;         0 - Active Concept
	;
	NEW CURRENT,STATUS
	;
	S CURRENT=$G(@GL@("CURRENT"))
	S STATUS=$G(@GL@("STS"))
	;
	I STATUS'="A" D  Q 0
	. ;
	. ;Skip if not already defined
	. I CONCDA="" Q
	. ;
	. ;Entry is defined - Mark as out of date
	. NEW NROUT,BSTSC,ERR,NRIN,TIEN
	. S NRIN=$$DTS2FMDT^BSTSUTIL($P($G(@GL@("CONCEPTID")),U,2))
	. S NROUT=$$DTS2FMDT^BSTSUTIL($P(CURRENT,U))
	. ;
	. ;Update the concept
	. S BSTSC(9002318.4,CONCDA_",",.05)=NRIN
	. S BSTSC(9002318.4,CONCDA_",",.06)=NROUT
	. S BSTSC(9002318.4,CONCDA_",",.11)="N"
	. S BSTSC(9002318.4,CONCDA_",",.04)=CVRSN
	. S BSTSC(9002318.4,CONCDA_",",.12)=DT
	. D FILE^DIE("","BSTSC","ERR")
	. ;
	. ;Now mark the terms as out of date
	. ;
	. ;Set up FSN, Synonyms, Preferred
	. S TIEN="" F  S TIEN=$O(^BSTS(9002318.3,"C",NMID,CONCDA,TIEN),-1) Q:TIEN=""  D
	.. ;
	.. ;Skip if not the same Concept Id
	.. I CONCDA'=$$GET1^DIQ(9002318.3,TIEN_",",".03","I") Q
	.. ;
	.. NEW BSTST,ERR
	.. ;
	.. ;Save/update other fields
	.. S BSTST(9002318.3,TIEN_",",.05)=CVRSN
	.. S BSTST(9002318.3,TIEN_",",.06)=NRIN
	.. S BSTST(9002318.3,TIEN_",",.07)=NROUT
	.. S BSTST(9002318.3,TIEN_",",.1)=DT
	.. S BSTST(9002318.3,TIEN_",",.11)="N"
	.. D FILE^DIE("","BSTST","ERROR")
	.. ;
	.. ;Reindex - needed for custom indices
	.. D
	... NEW DIK,DA
	... S DIK="^BSTS(9002318.3,",DA=TIEN
	... D IX^DIK
	;
	Q 1
