pN$ = COMMAND$(1)
OPEN _CWD$ + "\Content\Stats\" + pN$ + "Inv.txt" FOR OUTPUT AS #1
FOR i = 1 TO 15
    PRINT #1, 0, 0
NEXT
CLOSE #1
OPEN _CWD$ + "\Content\Stats\" + pN$ + "RInv.txt" FOR OUTPUT AS #1
FOR i = 1 TO 1
    PRINT #1, 0, 0
NEXT
CLOSE #1

OPEN _CWD$ + "\Content\Stats\" + pN$ + "eqItem.txt" FOR OUTPUT AS #1
FOR i = 1 TO 7
    PRINT #1, 0, 0
NEXT
CLOSE #1
OPEN _CWD$ + "\Content\Stats\" + pN$ + "Stats.txt" FOR OUTPUT AS #1
PRINT #1, 1, 5, 5, 10, 1, 1, 1, 1, 0, 1
CLOSE #1

OPEN _CWD$ + "\Content\Stats\" + pN$ + "Sizes.txt" FOR OUTPUT AS #1
PRINT #1, 15
PRINT #1, 1
CLOSE #1



OPEN _CWD$ + "\Content\Stats\" + pN$ + "QList.txt" FOR OUTPUT AS #1
OPEN _CWD$ + "\Content\Stats\QList.csv" FOR INPUT AS #2
DO
    totalQ% = totalQ% + 1
    INPUT #2, n$, t, x, y, z
LOOP UNTIL EOF(2)
CLOSE #2

FOR i = 1 TO totalQ%
    PRINT #1, 0
NEXT
CLOSE #1

OPEN _CWD$ + "\Content\Stats\" + pN$ + "CurQuest.txt" FOR OUTPUT AS #1
PRINT #1, 0, 0
CLOSE #1


toRun$ = "RPGProgram.exe " + pN$
RUN toRun$
