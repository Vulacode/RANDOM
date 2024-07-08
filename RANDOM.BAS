$ERROR 	PARAM+,		BOUNDS-, 	NUMERIC-,		OVERFLOW-, 		STACK-
$LIB 		LPT-, 		COM-, 		GRAPH-, 		FULLFLOAT+,  IPRINT-
$OPTION	CNTLBREAK-,					AUTODIM-
$STACK 3072
$FLOAT PROCEDURE

DEFINT A-Z

$INCLUDE "BIOS.INC"
$INCLUDE "CHKERR.INC"
$INCLUDE "DOS.INC"
$INCLUDE "KONST.INC"
$INCLUDE "RAND.INC"

DIM FileEntry$(140),FileLen&(140),DR$(1:5),KeyFun$(1:10),CKS(99),DDFile$(1:3)

StartPath$=CURDIR$

OPEN "A:\RANDOM.CNF" FOR INPUT AS #1
FOR N=1 TO 5
  LINE INPUT #1,DR$(N)
NEXT N
CLOSE

CALL SetParams

Jump=0:Ext=1

%Exists=1		:%NoDir=2		:%BadName=3		:%NoFile=4		:%NoSpec=5	:%NoCopy=6
%NoSpace=7	:%Full=8		:%Failed=9	:%TooShort=10	:%BadDisk=11	:%BadDat=12
%BadFile=13	:%NoDiff=14

FOR N=1 TO 10
	READ KeyFun$(N)
NEXT N
DATA "Copy ",Renam,"Del  ",Merge,"Prep ",Creat,Randz,Check,Clone,Quit

COLOR Ink,Paper
PRINT BORDER$;

MENU:
	DO
		BF=0:M=0:LenBite=32750
		KeyOn=%TRUE:Wild=%TRUE
		NoDraw=%FALSE
		Title$="":WhichFiles$="*.*"
		ON ERROR GOTO ERMESG
		IF Jump THEN
			Drive$=RTRIM$(CURDIR$,"\")
			GOTO SkipMenu
		ELSE
			Drive$=DD1$
		END IF
		HL=0:LH=0:XF=%FALSE:XFH=%FALSE:XHL=0
		PRINT TOPLINE$;
		CALL ClearWindow(%TopLeft1,%BottomRight1)
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		COLOR %Yellow,%Cyan
		PRINT CENTRE$(3," R A N D O M ");
		PRINT CENTRE$(5," MAIN MENU ");
		COLOR ,Paper
		Y=8:X=29:XX=4
		RESTORE MENU
		FOR ROW=1 TO 13
			LOCATE Y,X
			COLOR %LCyan
			READ MenuItem$:PRINT MenuItem$
			COLOR Ink
			LOCATE Y,X+XX
			READ MenuItem$:PRINT MenuItem$
			INCR Y
		NEXT ROW
		DATA "F1",": Copy files"
		DATA "F2",": Rename files"
		DATA "F3",": Delete files"
		DATA "F4",": Merge files"
		DATA " "," "
		DATA "F5",": Prepare Data Disk"
		DATA "F6",": Create random data"
		DATA "F7",": Randomise data"
		DATA " "," "
		DATA "F8",": Check data"
		DATA "F9",": Create second disk"
		DATA " "," "
		DATA "F10",": Quit"
		COLOR %Yellow
		PRINT CENTRE$(23,"Version 2.05");
		COLOR Ink
		DO
			DO
				CH$=INKEY$
				IF CH$=CHR$(%Esc) OR CH$=CHR$(%Ent) THEN QUITTA
			LOOP WHILE LEN(CH$)<2
			M=ASC(RIGHT$(CH$,1))
			SkipMenu:
			IF Jump THEN M=Jump
			SELECT CASE M
				CASE %F1:GOSUB COPYA
				CASE %F2:GOSUB RENNA
				CASE %F3:GOSUB DELLA
				CASE %F4:GOSUB MERJA
				CASE %F5:GOSUB PREPA
				CASE %F6:GOSUB MAKEA
				CASE %F7:GOSUB RANDA
				CASE %F8:GOSUB CHEKA
				CASE %F9:GOSUB CLONA
				CASE %F10:GOSUB QUITTA
				CASE %AltF9:GOSUB CONFIG
				CASE %AltF10:GOSUB DOS
			END SELECT
		LOOP UNTIL M=>%F1 AND M<=%F10 OR M=>%AltF9 AND M<=%AltF10
	LOOP
:
COPYA:
	Heading$="COPY FILES"
	Title$=SPACE$(43)+"To: ["+SPACE$(27)+"]"
	Wild=%FALSE
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	DO
		IF FF THEN PRINT TITEL$(Title$);
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		OutFIle$=OUTNM$(NM$,OutName$,OutPath$)
		IF OutFile$="" THEN RETURN
		DO
			YN$="Y"
			IF EXIST(OutPath$,OutName$,0) THEN
				PRINT CLEARLINE$(%SelLine,77,1);
				PRINT "Overwrite ";OutFile$;" ? (Y/N) Y";
				LOCATE %SelLine,POS-1
				SOUND 800,5
				DO
					YN$=UCASE$(INPUT$(1))
				LOOP UNTIL INSTR(YN$, ANY "NY"+CHR$(%Ent)+CHR$(%Esc))
				IF YN$=CHR$(%Esc) THEN LOCATE ,,0:RETURN
				IF YN$="N" THEN
					PRINT CLEARLINE$(%SelLine,77,1);
					EXIT LOOP
				END IF
			ELSE
				DO
					BF=0
					IF FileLen&(XHL)>FREESPACE&(OutPath$) THEN
						BF=%Full
						IF ERRA(BF) THEN RETURN
					END IF
				LOOP WHILE BF
			END IF
			PRINT CLEARLINE$(%SelLine,77,1);
			PRINT "Copying '";NM$;"' to '";OutFile$;"'...";
			SHELL "COPY "+NM$+" "+OutFile$+" > NUL"
			IF INSTR(NM$,"*")=0 THEN
				IF NOT EXIST(OutPath$,OutName$,0) THEN
					IF ERRA(%Failed) THEN RETURN
				END IF
			END IF
		LOOP UNTIL YN$="Y" OR YN$=CHR$(%Ent)
		IF RTRIM$(CURDIR$,"\")=OutPath$ THEN CALL ReFiles(WhichFiles$)
	LOOP
RETURN
:
RENNA:
	Heading$="RENAME FILES"
	Title$=SPACE$(40)+"New name: [            ]"
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	DO
		IF FF THEN PRINT TITEL$(Title$);
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			BF=0:X=54
			LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(11)
			NNM$=UCASE$(KEYA$(%SelLine,X,33,126,12))
			IF NNM$=CHR$(%Esc) THEN RETURN
			IF NNM$="" THEN EXIT LOOP
			IF INSTR(NM$,"*") OR INSTR(NNM$,"*") THEN BF=%BadName
			BF=CHECKNAME(NNM$)
			IF NNM$=NM$ THEN BF=%Exists
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		IF NNM$<>"" THEN
			NAME NM$ AS NNM$
			CALL ReFiles(WhichFiles$)
		END IF
	LOOP
RETURN
:
DELLA:
	Heading$="DELETE FILES"
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	Wild=%FALSE
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			PRINT CLEARLINE$(%SelLine,77,1);
			PRINT "Are you sure? (Y/N) Y"
			LOCATE %SelLine,23
			SOUND 800,5
			DO
				YN$=UCASE$(INPUT$(1))
				IF YN$=CHR$(%Esc) THEN LOCATE ,,0:RETURN
			LOOP UNTIL YN$="N" OR YN$="Y" OR YN$=CHR$(%Ent)
			PRINT CLEARLINE$(%SelLine,77,1);
			IF YN$="N" THEN EXIT LOOP
			PRINT "Deleting '";NM$;"'...";
			KILL NM$
			CALL ReFiles(WhichFiles$)
		LOOP UNTIL YN$="Y" OR YN$=CHR$(%Ent)
	LOOP
RETURN
:
MERJA:
	Heading$="CREATE MERGED FILES"
	IF EXIST(DD1$,"MERGER",0) THEN
		OPEN DD1$+"MERGER" FOR INPUT AS #1
		LenMrg&=LOF(1)
		CLOSE
	ELSE
		LenMrg&=0
	END IF
	Title$=SPACE$(38)+"Merged file is now"+STR$(LenMrg&)+" bytes"
	LenMrg&=0
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	DO
		DO
			BF=0
			NM$=GETNM$(Drive$)
			IF NM$="" THEN RETURN
			IF FREESPACE&(DD1$)<(360000-LenMrg&) THEN BF=%NoSpace
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		PRINT CLEARLINE$(%SelLine,77,1);
		PRINT "Merging '";NM$;"'...";
		OPEN NM$ FOR BINARY AS #1
		OPEN DD1$+"MERGER" FOR BINARY AS #2
		LenFile&=LOF(1)
		CALL GetPut(0,LenFile&)
		INCR LenMrg&,LenFile&
		IF RTRIM$(CURDIR$,"\")=DD1$ THEN CALL ReFiles(WhichFiles$)
		Title$=SPACE$(38)+"Merged file is now"+STR$(LOF(2))+" bytes"
		CLOSE
		PRINT TITEL$(Title$);
	LOOP
RETURN
:
PREPA:
	Heading$="PREPARE DATA DISKS"
	Instruct$="Place a blank, formatted disk in drive "+CHR$(ASCII(DD4$))
	IF Jump THEN CALL ChPath(DD4$)
	Jump=0:NoDraw=%TRUE
	CALL DoScreen(Heading$,Title$,DD4$,WhichFiles$)
	DO
		Go=GETGOING(Instruct$)
		IF Go=%FALSE OR Go=>%F1 AND Go<=%F10 OR Go=>%AltF9 AND Go<=%AltF10 THEN
			Jump=Go
			PRINT CLEARLINE$(%SelLine,77,0);
			RETURN
		END IF
		DO
			IF FREESPACE&(CURDIR$)<720005 THEN BF=%NoSpace ELSE BF=0
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		PRINT CLEARLINE$(%SelLine,77,1);
		PRINT "Preparing disk...";
		OPEN DD4$+"\DATAPTR" FOR OUTPUT AS #1
		PRINT #1,"0"
		CLOSE
		CALL Signal
		CALL ReFiles(WhichFiles$)
		MSG$="Prepare another disk (Y/N)? N"
		IF PROCEED$(MSG$)="" THEN EXIT LOOP
	LOOP
RETURN
:
MAKEA:
	Heading$="CREATE RANDOM DATA"
	NoDraw=%TRUE
	CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	DO
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		DO
			IF FREESPACE&(CURDIR$)<360000 THEN BF=%NoSpace ELSE BF=0
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		CKSUM=0
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		IF GETKEY$(CKS(),CKSUM)="" THEN JUMP=0:RETURN
		LOCATE %ActLine,3
		PRINT "Percentage done:"
		PRINT CLEARLINE$(%SelLine,77,0);
		PRINT "Creating random data..."
		Y&=TIMER+CKSUM
		RANDOMIZE Y&
		LS$=STRING$(4000,0)
		LSSeg=STRSEG(LS$)
		RandNM$="RANDATA."+RIGHT$(STR$(Ext),1)
		OPEN RandNM$ FOR BINARY AS #1
		FOR N=0 TO 99
			LOCATE %ActLine,19:PRINT N
			DEF SEG=LSSeg
			LSPos&=STRPTR(LS$)
			LenStr=0
			DO
				Gen=INT(RND*3)+1
				NumChars=INT(RND*256)+1
				SELECT CASE Gen
					CASE 1:RS$=RAND1$(NumChars)
					CASE 2:RS$=RAND2$(NumChars)
					CASE 3:RS$=RAND3$(NumChars)
				END SELECT
				POKE$ LSPos&,RS$
				INCR LSPos&,NumChars
				INCR LenStr,NumChars
			LOOP UNTIL LenStr=>3600
			PUT$ #1,LEFT$(LS$,3600)
			Y&=TIMER+CKS(N)+Ext+N
			RANDOMIZE Y&
			DEF SEG
		NEXT N
		CLOSE:INCR Ext
		PRINT CLEARLINE$(%SelLine,77,0);
		PRINT CLEARLINE$(%ActLine,40,0);
		CALL Signal
		CALL ReFiles(WhichFiles$)
		MSG$="Create more random files (Y/N)? N"
		IF PROCEED$(MSG$)="" THEN EXIT LOOP
		PRINT CLEARLINE$(%SelLine,77,0);
	LOOP
RETURN
:
RANDA:
	Heading$="RANDOMISE FILES"
	Drive1$=DD2$
	Drive2$=DD3$
	CALL DoScreen(Heading$,Title$,Drive1$,WhichFiles$)
	DO
		DO
			BF=0
			PRINT CLEARLINE$(%ActLine,40,0);
			PRINT SELFILE$("first");
			NM1$=GETNM$(Drive1$)
			IF NM1$="" THEN RETURN
			IF FileLen&(XHL)<360000 THEN BF=%TooShort
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		CALL ReScreen(Heading$,Title$,WhichFiles$)
		DO
			BF=0
			PRINT SELFILE$("second");
			NM2$=GETNM$(Drive2$)
			IF NM2$="" THEN RETURN
			IF FileLen&(XHL)<360000 THEN BF=%TooShort
			IF NM1$=NM2$ THEN BF=%NoDiff
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		PRINT CLEARLINE$(%SelLine,77,0);
		LOCATE %SelLine,3:PRINT "Creating randomised file..."
		OPEN Drive1$+"\"+NM1$ FOR BINARY AS #1
		OPEN Drive2$+"\"+NM2$ FOR BINARY AS #2
		OPEN DD1$+"\RANDOM" FOR BINARY AS #3
		PRINT CLEARLINE$(%ActLine,45,0);
		PRINT "Percentage done:"
		MrgStr$=STRING$(3600,0)
		FOR N=1 TO 100
			LOCATE %ActLine,19:PRINT N-1
			DEF SEG=STRSEG(MrgStr$)
			GET$ #1,3600,RS1$
			GET$ #2,3600,RS2$
			SP1&=STRPTR(RS1$)
			SP2&=STRPTR(RS2$)
			SPM&=STRPTR(MrgStr$)
			FOR B=1 TO 3600 STEP 8
				A$=PEEK$(SP1&,8)
				B$=PEEK$(SP2&,8)
				A&&=CVQ(A$)
				B&&=CVQ(B$)
				X&&=A&& XOR B&&
				BitMrg$=MKQ$(X&&)
				POKE$ SPM&,BitMrg$
				INCR SP1&,8:INCR SP2&,8:INCR SPM&,8
			NEXT B
			PUT$ #3,MrgStr$
			DEF SEG
		NEXT N
		CLOSE
		CALL Signal
		CALL ReFiles(WhichFiles$)
		LOCATE %ActLine,20:PRINT "100"
		MSG$="Randomise more files (Y/N)? N"
		IF PROCEED$(MSG$)="" THEN EXIT LOOP
	LOOP
RETURN
:
CHEKA:
	Heading$="CHECK RANDOM FILES"
	ReChek:
	DO
		CALL DoScreen(Heading$,Title$,Drive$,WhichFiles$)
		NM$=GETNM$(Drive$)
		IF NM$="" THEN RETURN
		KeyOn=%FALSE
		CALL RandCheck(NM$)
	LOOP
RETURN
:
CLONA:
	Heading$="CREATE SECOND DATA DISKS"
	Instruct$="Place completed data disk in drive "+CHR$(ASCII(DD4$))
	IF Jump THEN CALL ChPath(DD4$)
	Jump=0:NoDraw=%TRUE
	CALL DoScreen(Heading$,Title$,DD4$,WhichFiles$)
	ReClon:
	DO
		Go=GETGOING(Instruct$)
		IF Go=%FALSE OR Go=>%F1 AND Go<=%F10 OR Go=>%AltF9 AND Go<=%AltF10 THEN
			Jump=Go
			PRINT CLEARLINE$(%SelLine,77,0);
			RETURN
		END IF
		DDFile$(1)="DATAPTR":DDFile$(2)="SNUM":DDFile$(3)="RNUM"
		DO
			BF=0:FS&=FREESPACE&(DD1$):KeyOn=%FALSE
			IF FS&>720005 THEN BigDisk=%TRUE ELSE BigDisk=%FALSE
			FOR N=1 TO 3
				IF NOT EXIST(DD4$,DDFile$(N),0) THEN BF=%BadDat:EXIT FOR
				OPEN DDFile$(N) FOR INPUT AS #1:LenFile&=LOF(1):CLOSE
				IF N=1 THEN IF LenFile& <> 3 THEN BF=%BadFile:EXIT FOR
				IF N>1 THEN IF LenFile& <> 360000 THEN BF=%BadFile:EXIT FOR
			NEXT N
			IF FS&<360005 THEN BF=%NoSpace
			IF ERRA(BF) THEN RETURN MENU
			CALL ReFiles(WhichFiles$)
		LOOP WHILE BF
		NoDraw=%TRUE
		PRINT CLEARLINE$(%SelLine,77,1);
		CALL ClearWindow(%TopLeft3,%BottomRight3)
		PRINT "Copying data...";
		SHELL "COPY DATAPTR "+DD1$+"\DATAPTR > NUL"
		SHELL "COPY SNUM "+DD1$+"\SNUM > NUL"
		IF BigDisk THEN
			SHELL "COPY RNUM "+DD1$+"\RNUM > NUL"
		END IF
		LOCATE %SelLine,3,0
		PRINT "Place a blank, formatted disk in drive ";
		PRINT CHR$(ASCII(DD4$));", then press a key to start"
		CNT$=INPUT$(1)
		IF CNT$=CHR$(%Esc) THEN RETURN MENU
		DO
			IF FREESPACE&(CURDIR$)<720005 THEN BF=%NoSpace ELSE BF=0
			IF ERRA(BF) THEN RETURN
		LOOP WHILE BF
		PRINT CLEARLINE$(%SelLine,77,1);
		PRINT "Copying data...";
		SHELL "COPY "+DD1$+"\DATAPTR > NUL"
		SHELL "COPY "+DD1$+"\SNUM RNUM > NUL"
		IF BigDisk THEN
			SHELL "COPY "+DD1$+"\RNUM SNUM > NUL"
		END IF
		KILL DD1$+"\DATAPTR"
		KILL DD1$+"\SNUM"
		IF BigDisk THEN
			KILL DD1$+"\RNUM"
		END IF
		IF BigDisk=%FALSE THEN
			LOCATE %SelLine,3,0
			PRINT "Replace first disk in drive ";
			PRINT CHR$(ASCII(DD4$));", then press a key to start"
			CNT$=INPUT$(1)
			IF CNT$=CHR$(%Esc) THEN RETURN MENU
			DO
				IF NOT EXIST(DD4$,DDFile$(1),0) THEN BF=%BadDisk ELSE BF=0
			LOOP WHILE BF
			PRINT CLEARLINE$(%SelLine,77,1);
			PRINT "Copying data...";
			SHELL "COPY RNUM "+DD1$+"\RNUM > NUL"
			BF=0
			LOCATE %SelLine,3,0
			PRINT "Replace second disk in drive ";
			PRINT CHR$(ASCII(DD4$));", then press a key to start"
			CNT$=INPUT$(1)
			IF CNT$=CHR$(%Esc) THEN RETURN MENU
			DO
				BF=0
				IF NOT EXIST(DD4$,DDFile$(1),0) THEN BF=%BadDisk
				IF NOT EXIST(DD4$,DDFile$(3),0) THEN BF=%BadDisk
				IF FREESPACE&(DD4$)<360000 THEN BF=%NoSpace
				IF ERRA(BF) THEN RETURN
			LOOP WHILE BF
			PRINT CLEARLINE$(%SelLine,77,1);
			PRINT "Copying data...";
			SHELL "COPY "+DD1$+"\RNUM SNUM > NUL"
			KILL DD1$+"\RNUM"
		END IF
		CALL Signal
		CALL ReFiles(WhichFiles$)
		MSG$="Clone more disks (Y/N)? N"
		IF PROCEED$(MSG$)="" THEN EXIT LOOP
	LOOP
RETURN
:
QUITTA:
	Jump=0:KeyOn=%FALSE
	IF WARNDISK("A:","RANDOM.CNF","PROGRAM") THEN RETURN
	CALL ChPath(StartPath$)
	CLS:END
RETURN
:
CONFIG:
	PRINT TOPLINE$
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	Jump=0:KeyOn=%FALSE
	RESTORE CONFIG
	FOR N=1 TO 5
		READ ConfigList$
		COLOR %LCyan
		LOCATE N+1,3:PRINT CHR$(N+64);
		COLOR Ink
		PRINT TAB(6);ConfigList$;TAB(36);"-   ";
		PRINT DR$(N)
	NEXT N
	DATA "Color, B&W OR LCD",Data area 1,Data area 2,Data area 3,Storage disk
	PRINT HORLINE$;
	LOCATE %SelLine,3:PRINT "Press letter of option to change";TAB(52);
	COLOR %LCyan:PRINT "S";
	COLOR Ink:PRINT " - Save changes"
	LOCATE %HeadLine,3:COLOR %Yellow,%Cyan:PRINT "CHANGE CONFIGURATION":COLOR ,Paper
	PRINT ESCMAIN$;
	COLOR Ink
	DO
		DO
			LOCATE %SelLine,36
			COLOR %Flash+Ink:PRINT CHR$(219):COLOR Ink
			Choice$=UCASE$(INPUT$(1))
			IF ASC(Choice$)=>65 AND ASC(Choice$)<=73 THEN
				LOCATE %SelLine,36:PRINT " "
				TempName$=""
				Opt=ASC(Choice$)-64
				Y=Opt+1:X=40
				LOCATE Y,X,1,0,7
				DO
					DO
						Char$=INKEY$
					LOOP WHILE Char$=""
					IF LEN(Char$)=2 THEN
						IF ASC(RIGHT$(Char$,1))=%LfAr THEN
							Char$=CHR$(%Bsp)
						ELSE
							Char$=" "
						END IF
					END IF
					SELECT CASE Char$
						CASE CHR$(%Ent)
							IF TempName$<>"" THEN	DR$(Opt)=LTRIM$(RTRIM$(TempName$,ANY "\ "))
							CALL SetParams
							LOCATE ,,0
							EXIT LOOP
						CASE CHR$(%Esc)
							LOCATE Y,40:PRINT SPACE$(39)
							LOCATE Y,40:PRINT DR$(Opt)
							LOCATE ,,0
							EXIT LOOP
						CASE CHR$(%Bsp)
							DECR X
							IF X<40 THEN X=40:EXIT SELECT
							LOCATE Y,X:PRINT CHR$(32):LOCATE Y,X
							TempName$=LEFT$(TempName$,LEN(TempName$)-1)
						CASE ELSE
							IF X<78 THEN
								PRINT UCASE$(Char$);SPACE$(39-LEN(TempName$))
								INCR X:LOCATE Y,X
								TempName$=TempName$+UCASE$(Char$)
							ELSE
								SOUND 88,8
							END IF
					END SELECT
				LOOP
			END IF
			IF Choice$="S" THEN
				IF WARNDISK("A:","RANDOM.CNF","PROGRAM") THEN RETURN
				OPEN "A:\RANDOM.CNF" FOR OUTPUT AS #1
				FOR N=1 TO 5
					PRINT #1,DR$(N)
				NEXT N
				CLOSE
			END IF
		LOOP UNTIL Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent)
		IF Choice$=CHR$(%Esc) OR Choice$=CHR$(%Ent) THEN
			EXIT LOOP
		END IF
	LOOP
RETURN
:
DOS:
	Jump=0
	COLOR Ink,%Black
	CLS
	PRINT "Type EXIT to return to program"
	SHELL
	COLOR Ink,Paper
	PRINT BORDER$;
RETURN
:
FUNCTION ERRA(BF)
	ERRA=%FALSE
	IF BF=0 THEN EXIT FUNCTION
	SHARED KeyOn,KeyFun$()
	SOUND 88,8:CLOSE
	SELECT CASE BF
		CASE %Exists	:ERRMSG$="File already exists. Use another name"
		CASE %NoDir		:ERRMSG$="Invalid path specification"
		CASE %BadName	:ERRMSG$="Unacceptable characters in name. Use another name"
		CASE %NoFile	:ERRMSG$="File not found"
		CASE %NoSpec	:ERRMSG$="Drive not specified"
		CASE %NoCopy	:ERRMSG$="File cannot be copied onto itself"
		CASE %NoSpace	:ERRMSG$="Insufficient space to create file(s)"
		CASE %Full		:ERRMSG$="Disk full"
		CASE %Failed	:ERRMSG$="Copy failed"
		CASE %TooShort:ERRMSG$="File too short. Minimum length 360,000 bytes"
		CASE %BadDisk	:ERRMSG$="Wrong disk"
		CASE %BadDat	:ERRMSG$="Data file(s) missing"
		CASE %BadFile	:ERRMSG$="Data disk file faulty"
		CASE %NoDiff	:ERRMSG$="Second file must differ from the first"
	END SELECT
	IF SHOWERR(ERRMSG$) THEN
		ERRA=%TRUE
	ELSE
		IF KeyOn THEN PRINT FUNKEY$;
	END IF
END FUNCTION
:
ERMESG:
	DEF SEG:SOUND 88,8:CLOSE
	SELECT CASE ERR
		CASE 53:ERRMSG$="File not found"
		CASE 58:ERRMSG$="File already exists. Choose another name"
		CASE 61:ERRMSG$="Disk full. Use new disk or delete files to make space"
		CASE 64:ERRMSG$="Invalid name"
		CASE 70:ERRMSG$="Disk write-protected. Switch tab to continue"
		CASE 71:ERRMSG$="Disk not in drive or door open"
		CASE 72:ERRMSG$="Disk corrupted"
		CASE 75:ERRMSG$="Path/file access error"
		CASE 76:ERRMSG$="Path not found"
		CASE ELSE:ERRMSG$="Error number"+STR$(ERR)+" at address"+STR$(ERADR)
	END SELECT
	IF SHOWERR(ERRMSG$) THEN RESUME MENU
RESUME MENU
:
FUNCTION BORDER$
	SHARED Ink
	CLS:COLOR Ink
	BDR$=STRING$(78,205)
	LOCATE 1,1:PRINT CHR$(201);BDR$;CHR$(187);
	FOR V=2 TO 21
		LOCATE V,1:PRINT CHR$(186)
		LOCATE V,80:PRINT CHR$(186)
	NEXT V
	LOCATE 22,1:PRINT CHR$(200);BDR$;CHR$(188);
END FUNCTION
:
FUNCTION KEYA$(Y,X,LOLIM,UPLIM,CRS)
	SHARED Ink,Paper
	LOCAL L,STNG$
	DO
		K=%FALSE
		DO
			I=ASC(INPUT$(1))
			SELECT CASE I
				CASE %Ent
					K=%TRUE
					IF L=0 AND PW=%FALSE THEN
						KEYA$=""
						EXIT LOOP
					END IF
					IF L<CRS THEN
						LOCATE Y,X:PRINT CHR$(32)
						EXIT LOOP
					ELSE
						LOCATE Y,X-1:PRINT RIGHT$(STNG$,1)
						EXIT LOOP
					END IF
				CASE %Esc
					IF PW=%FALSE THEN
						KEYA$=CHR$(I)
						EXIT FUNCTION
					END IF
				CASE %Bsp
					IF L THEN
						STNG$=LEFT$(STNG$,LEN(STNG$)-1)
						LOCATE Y,X-1:PRINT CHR$(219);
						IF L<CRS-1 THEN PRINT " "
						IF L=CRS-1 THEN LOCATE Y,X:PRINT " "
						DECR L:DECR X:EXIT LOOP
					ELSE
						SOUND 88,8
						EXIT LOOP
					END IF
				CASE <LOLIM,>UPLIM
					SOUND 88,8:EXIT LOOP
			END SELECT
			LOCATE Y,X
			IF L<CRS-1 THEN
				PRINT CHR$(I);CHR$(219)
			ELSEIF L=CRS-1 THEN
				COLOR Paper,Ink
				PRINT CHR$(I)
				COLOR Ink,Paper
			ELSE
				SOUND 88,8
				EXIT LOOP
			END IF
			STNG$=STNG$+CHR$(I)
			INCR L:INCR X
		LOOP
	LOOP UNTIL K
	KEYA$=STNG$
END FUNCTION
:
FUNCTION WARNDISK(WPath$,WProg$,Disk$)
	SHARED Ink,Jump
	DO
		WARNDISK=%FALSE
		IF NOT EXIST(WPath$,WProg$,0) THEN
			WARNDISK=%TRUE
			CALL ClearWindow(%TopLeft3,%BottomRight3)
			SOUND 800,5:Jump=0
			WarnMsg$="Place "+Disk$+" disk in drive"
			LOCATE 23,((80-LEN(WarnMsg$))/2)-3
			COLOR %Flash+%RED,Ink:PRINT "** ";:COLOR %Red
			PRINT WarnMsg$;:COLOR %Flash+%Red:PRINT " **"
			COLOR Ink,Paper:LOCATE 25,13,0
			PRINT "Press any key to continue. ESCape to return to Main Menu";
			CNT$=INPUT$(1)
			CALL ClearWindow(%TopLeft3,%BottomRight3)
			IF CNT$=CHR$(%Esc) THEN EXIT LOOP
		ELSE
			WARNDISK=%FALSE
			EXIT LOOP
		END IF
	LOOP
END FUNCTION
:
FUNCTION FUNKEY$
	SHARED Ink,M,Paper,KeyFun$()
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	A=1:X=2
	IF M>103 THEN
		INCR A,10
		COLOR %Yellow,Ink
		LOCATE 23,3:PRINT "ALT Menu"
		COLOR Ink,Paper
	END IF
	FOR N=A TO A+9
		LOCATE 25,X
		COLOR %LCyan
		PRINT LTRIM$(STR$(N MOD 10));
		COLOR Paper,Ink
		PRINT KeyFun$(N);
		COLOR ,Paper
		INCR X,8
	NEXT N
	COLOR Ink
END FUNCTION
:
SUB GetDirra(WhichFiles$) SHARED
	FF=0:SubD=%FALSE:XF=%FALSE:DTA$=SPACE$(43)
	ERASE FileEntry$,FileLen&
	CALL SETDTA(DTA$)
	Nom$=FINDFIRST$(WhichFiles$)
	IF Nom$="" THEN
		EXIT SUB
	ELSE
		FileEntry$(0)=RIGHT$(Nom$,LEN(Nom$)-4)
		FileLen&(0)=CVL(LEFT$(Nom$,4))
		IF SubD THEN FileEntry$(0)="<"+FileEntry$(0)+">"
		FOR FF=1 TO 139
			Nom$=FINDNEXT$
			IF Nom$="" THEN EXIT FOR
			FileEntry$(FF)=RIGHT$(Nom$,LEN(Nom$)-4)
			FileLen&(FF)=CVL(LEFT$(Nom$,4))
			IF SubD THEN FileEntry$(FF)="<"+FileEntry$(FF)+">"
		NEXT FF
		ARRAY SORT FileEntry$() FOR FF, TAGARRAY FileLen&()
		IF FF>70 THEN XF=%TRUE
	END IF
END SUB
:
SUB PrintName(FileEntry$())
	SHARED Ink,FF,HL,Paper,XFH,FileLen&()
	LOCATE 16,77
	FirstFile=0
	IF FF>70 THEN
		COLOR Paper,%Red
		IF XFH THEN
			FirstFile=70
			PRINT "PgUp"
		ELSE
			PRINT "PgDn"
		END IF
		COLOR Ink,paper
	ELSE
		COLOR Ink,paper
		PRINT STRING$(3,196)+CHR$(182);
	END IF
	LOCATE 2,3:PRINT FileEntry$(FirstFile)
	FOR N=1 TO FF-FirstFile
		LOCATE (N\5)+2,((N MOD 5)*16)+3
		PRINT FileEntry$(FirstFile+N)
		IF N=FirstFile+69 THEN EXIT FOR
	NEXT N
END SUB
:
FUNCTION NEWDIR$(Path$)
	SHARED HL,LH,XFH,XHL
	HL=0:LH=0:XFH=%FALSE:XHL=0
	Path$=UCASE$(RTRIM$(Path$,"\"))
	OldPath$=RTRIM$(CURDIR$,"\")
	IF MID$(Path$,2,1)=":" THEN
		IF INSTR(Path$,"\")=0 THEN
			CHDRIVE Path$
			CHDIR "\"
			EXIT IF
		ELSEIF INSTR(Path$,"\") THEN
			CHDRIVE Path$
			CHDIR RIGHT$(Path$,LEN(Path$)-2)
			EXIT IF
		END IF
	ELSEIF INSTR(Path$,"\") THEN
		IF INSTR(Path$,"\.") THEN
			IF INSTR(Path$,"\..") THEN
				CHDIR ".."
			ELSE
				CHDIR "\"
			END IF
		ELSE
			CHDIR RIGHT$(OldPath$,LEN(OldPath$)-2)+Path$
		END IF
	END IF
	NEWDIR$=RTRIM$(CURDIR$,"\")
END FUNCTION
:
SUB SELECTA(Naam$) SHARED
	Jump=0:Choice=0:TempName$=""
	IF XFH=%FALSE THEN
		IF FF THEN
			IF XF THEN
				FPOS=69
			ELSE
				FPOS=FF-1
				IF HL>=FPOS THEN HL=FPOS:XHL=HL
			END IF
		END IF
	ELSE
		FPOS=FF-71
		IF HL>=FPOS THEN HL=FPOS:XHL=HL+70
	END IF
	Row=(HL\5)+2:Col=((HL MOD 5)*16)+3
	COLOR Ink,Paper
	IF NoDraw=%FALSE THEN
		IF FF THEN
			COLOR Paper,Ink
			IF XFH THEN HiLite=HL+70 ELSE HiLite=HL
			LOCATE Row,Col:PRINT FileEntry$(HiLite)
			PRINT CLEARLINE$(%SelLine,32,0);
			PRINT FileEntry$(HiLite);TAB(18);FileLen&(HiLite);
		END IF
	ELSE
  	LOCATE %SelLine,3
		IF M=%F5 OR M=%F6 THEN
			PRINT "Press Enter to start";
		END IF
	END IF
	Col=3:LOCATE %SelLine,3,1,0,7
	DO
		DO
			Choice$=INKEY$
		LOOP WHILE Choice$=""
		Choice=ASC(RIGHT$(Choice$,1))
		IF LEN(Choice$)=2 THEN
			IF FF THEN
				Col=3:LH=HL:TempName$=""
				SELECT CASE Choice
					CASE %RtAr:INCR HL
					CASE %LfAr:DECR HL
					CASE %DnAr:INCR HL,5
					CASE %UpAr:DECR HL,5
					CASE %Home:HL=0
					CASE %End :HL=FPOS
					CASE %PgDn:
						IF XF THEN
							XFH=%TRUE
							FPOS=FF-71
							LOCATE ,,0
							CALL ClearWindow(%TopLeft2,%BottomRight2)
							CALL PrintName(FileEntry$())
							HL=0:LH=HL
						END IF
					CASE %PgUp
						IF XF THEN
							XFH=%FALSE
							FPOS=69
							LOCATE ,,0
							CALL ClearWindow(%TopLeft2,%BottomRight2)
							CALL PrintName(FileEntry$())
							HL=0:LH=HL
						END IF
					CASE %F1 TO %F10, %AltF1 TO %AltF10
						Jump=Choice
						IF ABS(M-Choice)>9 THEN ChangeMenu=%TRUE ELSE ChangeMenu=%FALSE
						EXIT LOOP
				END SELECT
				IF HL<0 OR HL>FPOS THEN HL=LH
				XHL=HL:XLH=LH
				IF XFH THEN INCR XHL,70:INCR XLH,70
				LOCATE (LH\5)+2,((LH MOD 5)*16)+3
				COLOR Ink,Paper
				PRINT FileEntry$(XLH)
				LOCATE (HL\5)+2,((HL MOD 5)*16)+3
				COLOR Paper,Ink
				PRINT FileEntry$(XHL)
				COLOR Ink,Paper
				LOCATE %SelLine,3:PRINT SPACE$(24):LOCATE %SelLine,3
				PRINT FileEntry$(XHL);TAB(18);FileLen&(XHL)
				LOCATE %SelLine,3
				LH=HL
			ELSEIF FF<2 THEN
				IF Choice>=%F1 AND Choice<=%F10 OR Choice>=%AltF1 AND Choice<=%AltF10 THEN
					Jump=Choice
					IF ABS(M-Choice)>9 THEN ChangeMenu=%TRUE ELSE ChangeMenu=%FALSE
					EXIT LOOP
				END IF
			END IF
		ELSE
			SELECT CASE Choice
				CASE %Ent
					IF TempName$="" THEN
						IF M=%F5 OR M=%F6 THEN
							Naam$="*"
						ELSE
							Naam$=FileEntry$(XHL)
						END IF
						IF ASCII(Naam$)=60 THEN
							Naam$="\"+REMOVE$(Naam$, ANY "<>")
						END IF
					ELSE
						Naam$=TempName$
					END IF
					EXIT LOOP
				CASE %Esc:EXIT IF
				CASE %Bsp
					DECR Col
					IF Col<3 THEN Col=3:EXIT SELECT
					LOCATE %SelLine,Col:PRINT CHR$(32):LOCATE %SelLine,Col
					TempName$=LEFT$(TempName$,LEN(TempName$)-1)
				CASE ELSE
					PRINT UCASE$(CHR$(Choice));SPACE$(31-LEN(TempName$))
					INCR Col:LOCATE %SelLine,Col
					TempName$=TempName$+UCASE$(CHR$(Choice))
			END SELECT
		END IF
	LOOP UNTIL (Choice$)=CHR$(%Esc)
	IF Choice$=CHR$(%Esc) THEN
		Jump=0:Naam$=""
		LOCATE 16,80:PRINT CHR$(186);
	END IF
	LOCATE ,,0
END SUB
:
SUB DoScreen(Heading$,Title$,Drive$,WhichFiles$)
	SHARED FF,Jump,KeyOn,FileEntry$()
	IF Jump THEN
		CALL ReScreen(Heading$,Title$,WhichFiles$)
	ELSE
		CALL ChPath(Drive$)
		CALL GetDirra(WhichFiles$)
		CALL NewScreen(Heading$,Title$)
		IF FF THEN CALL PrintName(FileEntry$())
		KeyOn=%TRUE
	END IF
END SUB
:
SUB NewScreen(Heading$,Title$)
	SHARED FF,KeyOn,NoDraw
	CALL ClearWindow(%TopLeft1,%BottomRight1)
	PRINT PATHEAD$;
	PRINT HORLINE$;
	IF KeyOn THEN PRINT FUNKEY$;
	PRINT HEAD$(Heading$);
	PRINT SPACEFREE$(0);
	IF FF=0 THEN
		PRINT NOFILES$;
	ELSEIF NoDraw=%FALSE THEN
		PRINT ARROWS$;
		PRINT TITEL$(Title$);
	END IF
	PRINT ESCMAIN$;
END SUB
:
SUB ReScreen(Heading$,Title$,WhichFiles$)
	SHARED ChangeMenu,FF,Jump,KeyOn,NoDraw
	DiskChanged=%FALSE
	PRINT PATHEAD$;
	PRINT CLEARLINE$(%HeadLine,55,0);
	PRINT HEAD$(Heading$);
	PRINT SPACEFREE$(DiskChanged);
	IF Jump=0 OR DiskChanged THEN
		CALL ReFiles(WhichFiles$)
	END IF
	IF SCREEN(%ActLine,3)<>27 THEN
		IF FF AND NoDraw=%FALSE THEN
			PRINT ARROWS$;
			PRINT TITEL$(Title$);
		END IF
	ELSEIF FF=0 OR NoDraw THEN
		PRINT CLEARLINE$(%ActLine,62,0);
	END IF
	IF ChangeMenu THEN
		IF KeyOn THEN PRINT FUNKEY$;
	END IF
END SUB
:
SUB ReFiles(WhichFiles$)
	SHARED FF,FileEntry$()
	CALL ClearWindow(%TopLeft2,%BottomRight2)
	CALL GetDirra(WhichFiles$)
	IF FF=0 THEN
		PRINT NOFILES$;
	ELSE
		CALL PrintName(FileEntry$())
	END IF
	PRINT SPACEFREE$(0);
END SUB
:
SUB ChPath(Path$)
	CHDRIVE Path$
	IF INSTR (Path$,"\") THEN
		CHDIR RIGHT$(Path$,LEN(Path$)-2)
	ELSE
		CHDIR "\"
	END IF
END SUB
:
FUNCTION GETNM$(Drive$)
	SHARED BF,FF,Jump,M,Wild,Heading$,Title$,WhichFiles$
	File$=""
	DO
		BF=0
		DO
			CD=%FALSE
			CALL SELECTA(File$)
			IF File$="" OR Jump THEN
				IF FF THEN PRINT CLEARLINE$(%SelLine,77,0);
				GETNM$="":EXIT FUNCTION
			END IF
			IF MID$(FIle$,2,1)=":" OR INSTR(File$,"\") THEN
				IF M=%F5 OR M=%F6 THEN
					IF ASCII(File$)=92 THEN EXIT LOOP
				END IF
				CD=%TRUE:Drive$=NEWDIR$(File$)
				CALL ReScreen(Heading$,Title$,WhichFiles$)
			END IF
		LOOP WHILE CD
		IF M=%F5 OR M=%F6 THEN EXIT LOOP
		BF=CHECKFILE(File$)
		IF Wild THEN IF INSTR(File$,"*") THEN BF=%BadName
		IF ERRA(BF) THEN GETNM$="":EXIT FUNCTION
	LOOP WHILE BF
	GETNM$=File$
END FUNCTION
:
FUNCTION SPACEFREE$(DC)
	STATIC OldFree&
	SHARED Ink,Paper
	FS&=FREESPACE&(CURDIR$)
	LOCATE %HeadLine,58
	COLOR %Yellow,Paper
	FS$=STR$(FS&)+" bytes free"
	PRINT FS$;SPACE$(79-(57+LEN(FS$)))
	COLOR Ink,Paper
	IF FS&<>OldFree& THEN
		DC=%TRUE
	ELSE
		DC=%FALSE
	END IF
	OldFree&=FS&
END FUNCTION
:
FUNCTION CLEARLINE$(WhichLine,Blanks,Curs)
	SHARED Ink,Paper
	COLOR Ink,Paper
	LOCATE WhichLine,3
	PRINT SPACE$(Blanks);
	LOCATE WhichLine,3,Curs
END FUNCTION
:
FUNCTION CHKLEN(Repeat,Extra,Length&)
	SHARED LenBite
	IF Length&>LenBite THEN
		CHKLEN=%TRUE
		Repeat=Length&\LenBite
		Extra=Length& MOD LenBite
	ELSE
		CHKLEN=%FALSE
	END IF
END FUNCTION
:
SUB GetPut(Start&,Length&)
	Repeat=0:Extra=0
	SHARED LenBite,LL,M,FL$
	LongFile=CHKLEN(Repeat,Extra,Length&)
	SEEK #1,Start&
	FOR LN=1 TO Repeat+1
		IF LongFile THEN
			IF LN>Repeat THEN
				GET$ #1,Extra,PutStr$
			ELSE
				GET$ #1,LenBite,PutStr$
			END IF
		ELSE
			GET$ #1,Length&,PutStr$
		END IF
		IF M=%F4 THEN
			LenJoin&=LOF(2):SEEK #2,LenJoin&:PUT$ #2,REMOVE$(PutStr$,CHR$(0))
		END IF
		PutStr$=""
	NEXT LN
END SUB
:
SUB SetParams SHARED
  StartInk=ASCII(DR$(1))
	SELECT CASE StartInk
		CASE 66
			StartInk=&H0000
			Ink=%HWhite:Paper=%Black
		CASE 67
			StartInk=&H1000
			Ink=%HWhite:Paper=%Blue
		CASE 76
			StartInk=&H0000
			Ink=%White:Paper=%Black
	END SELECT
	DD1$=DR$(2)		'Main RAM area
	DD2$=DR$(3)
	DD3$=DR$(4)
	DD4$=DR$(5)		'Actual data disk
END SUB
:
FUNCTION SHOWERR(ERRMSG$)
	SHARED Ink,Paper
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	LOCATE 23,((80-LEN(ERRMSG$))/2)-3,0
	COLOR %Flash+Ink,%Red:PRINT "** ";:COLOR Ink
	PRINT ERRMSG$;:COLOR %Flash+Ink:PRINT " **";
	COLOR Ink,Paper:LOCATE 25,13
	PRINT "Press any key to continue. ESCape to return to Main Menu";
	CNT$=INPUT$(1)
	CALL ClearWindow(%TopLeft3,%BottomRight3)
	IF CNT$=CHR$(%Esc) THEN
		SHOWERR=%TRUE
	ELSE
		SHOWERR=%FALSE
	END IF
END FUNCTION
:
FUNCTION HEAD$(Heading$)
	COLOR %Yellow,%Cyan
	LOCATE %HeadLine,3
	PRINT Heading$;
END FUNCTION
:
FUNCTION TITEL$(Title$)
	SHARED FF
	IF FF THEN
		SHARED Ink,Paper
		COLOR Ink,Paper
		LOCATE %SelLine,3
		PRINT Title$;
	END IF
END FUNCTION
:
FUNCTION ARROWS$
	SHARED Paper,NoDraw
	IF NoDraw=%FALSE THEN
		COLOR %LCyan,Paper
		LOCATE %ActLine,3
		PRINT CHR$(%Esc)+CHR$(24)+CHR$(25)+CHR$(26);
		COLOR %Yellow
		PRINT " to highlight file, ENTER selects";
	END IF
END FUNCTION
:
FUNCTION ESCMAIN$
	SHARED Paper
	COLOR %LCyan,Paper
	LOCATE %ActLine,70
	PRINT "ESC:";
	COLOR %Yellow:PRINT " Menu";
END FUNCTION
:
FUNCTION CONT$
	SHARED Ink,Paper
	COLOR %Black,Ink
	LOCATE 25,53
	PRINT CHR$(179)+" Press any key to continue ";
	CNT$=INPUT$(1)
	COLOR Ink,Paper
END FUNCTION
:
FUNCTION HORLINE$
	SHARED Ink,Paper
	COLOR Ink,Paper
	LOCATE 16,1
	PRINT CHR$(199)+STRING$(78,196)+CHR$(182);
END FUNCTION
:
FUNCTION NOFILES$
	SHARED Ink,Paper,NoDraw
	COLOR %Flash+Ink,Paper
	LOCATE 2,3:PRINT "No files!"
	COLOR Ink
	LOCATE %SelLine,3
	IF NoDraw=%FALSE THEN PRINT "Change drive?";SPACE$(64);
	PRINT CLEARLINE$(%ActLine,67,1);
END FUNCTION
:
FUNCTION SELFILE$(NumFile$)
	SHARED Ink,Paper
	LOCATE %SelLine,45
	COLOR Ink+%Flash,Paper
	PRINT "<==";
	COLOR Ink,Paper
	PRINT " Select ";NumFile$;" file";
END FUNCTION
:
FUNCTION GETKEY$(CKS(),CKSUM)
	LOCAL Kee$,N,S
	SHARED Ink,Paper
	COLOR Ink,Paper
	LOCATE %SelLine,3,0:PRINT "Enter randomising key: [";SPACE$(50);"]"
	DO
		X=27:CRS=50
		LOCATE %SelLine,X:PRINT CHR$(219);SPACE$(CRS-1);
		Kee$=KEYA$(%SelLine,X,0,255,CRS)
		IF Kee$="" OR Kee$=CHR$(%Esc) THEN GETKEY$="":EXIT FUNCTION
		IF LEN(Kee$)<>CRS THEN SOUND 88,8
	LOOP UNTIL LEN(Kee$)=CRS
	RANDOMIZE TIMER
	DO
		VLet=ASC(MID$(Kee$,N\2+1,1)) XOR INT(RND*256)
		INCR CKSUM,Vlet
		CKS(N)=CKSUM
		INCR N
		CKS(N)=CKSUM+(Vlet XOR INT(RND*256))
		RANDOMIZE CKS(N)
		INCR N
	LOOP UNTIL N=100
	GETKEY$=Kee$
END FUNCTION
:
FUNCTION RAND1$(NumChars)
	RandStr$=CHR$(NumChars-1)
	A&=16807
	M&=2147483647
	Q&=127773
	R&=2836
	Seed&=(RND*M&)
	FOR N=1 TO NumChars
		HI&=Seed&\Q&
		LO&=Seed&-Q&*INT(Seed&/Q&)
		TEST&=A&*LO&-R&*HI&
		IF TEST&>0 THEN
			Seed&=TEST&
		ELSE
			Seed&=TEST&+M&
		END IF
		R!=Seed&/M&
		RandStr$=RandStr$+CHR$(INT(R!*256))
	NEXT N
	RAND1$=RandStr$
END FUNCTION
:
FUNCTION RAND2$(NumChars)
	RandStr$=CHR$(NumChars-1)
	FOR N=1 TO NumChars
		RandStr$=RandStr$+CHR$(INT(RND*256))
	NEXT N
	RAND2$=RandStr$
END FUNCTION
:
FUNCTION RAND3$(NumChars)
	RandStr$=CHR$(NumChars-1)
	A&=16807
	M&=2147483647
	Seed&=(RND*M&)
	FOR N=1 TO NumChars
		Seed&=(A&*Seed&) MOD M&
		RandStr$=RandStr$+CHR$(INT((Seed&/M&)*256))
	NEXT N
	RAND3$=RandStr$
END FUNCTION
:
SUB Signal
	Snd=600
	FOR N=1 TO 3
		SOUND Snd,3
		INCR Snd,150
	NEXT N
END SUB
:
FUNCTION GETGOING(Instruct$)
	SHARED Ink,Paper
	COLOR Ink,Paper
	LOCATE %SelLine,3,0:PRINT Instruct$;
	PRINT ", then press a key to start"
	DO
		CNT$=INKEY$
	LOOP WHILE CNT$=""
	IF CNT$=CHR$(%Esc) THEN GETGOING=%FALSE:EXIT FUNCTION
	IF LEN(CNT$)=2 THEN
		GETGOING=ASC(RIGHT$(CNT$,1))
	ELSE
		GETGOING=%TRUE
	END IF
END FUNCTION
:
FUNCTION CENTRE$(Y,MSG$)
	X=40-(LEN(MSG$)\2)
	LOCATE Y,X
	PRINT MSG$;
END FUNCTION
:
FUNCTION PATHEAD$
	SHARED WhichFiles$
	PRINT TOPLINE$;
	COLOR %Yellow,%Cyan
	PRINT CENTRE$(1,RTRIM$(CURDIR$,"\")+"\"+RTRIM$(WhichFiles$,CHR$(0)));
END FUNCTION
:
FUNCTION TOPLINE$
	SHARED Ink,Paper
	COLOR Ink,Paper
	LOCATE 1,2
	PRINT STRING$(78,205);
END FUNCTION
:
FUNCTION PROCEED$(MSG$)
	LOCATE %SelLine,3,1
	PRINT MSG$;
	LOCATE %SelLine,POS-1
	DO
		YN$=UCASE$(INPUT$(1))
		IF YN$="Y" THEN
			PRINT FUNKEY$;
			EXIT LOOP
		ELSEIF YN$="N" OR YN$=CHR$(%Ent) THEN
			YN$=""
			EXIT LOOP
		END IF
	LOOP
	LOCATE ,,0
	PROCEED$=YN$
END FUNCTION
