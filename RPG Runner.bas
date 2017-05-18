OPEN _CWD$ + "\Content\Stats\MT.txt" FOR RANDOM AS #1
PUT #1, 1, a$
CLOSE #1


DO
    CLS
    PRINT "1) New Game/Load Game"
    PRINT "2) Quit Game"
    c$ = INPUT$(1)
    IF VAL(c$) = 1 THEN
        GOTO 1:
    ELSEIF VAL(c$) = 2 THEN
        SYSTEM
    END IF
LOOP


1:

DO

    INPUT "Name: ", pN$
    GOSUB cVN
    IF isNV = 1 THEN
        IF NOT _FILEEXISTS(_CWD$ + "\Content\Stats\" + pN$ + "Inv.txt") THEN
            PRINT "Inventory not found, creating players' stats.."

            toRun$ = "Make Player.exe " + pN$
            RUN toRun$
        ELSE
            toRun$ = "RPGProgram.exe " + pN$
            SHELL "CD > " + _CWD$
            SHELL "start " + toRun$
            SHELL "start bgm.exe"
            SYSTEM
        END IF
    END IF
    PRINT "That name was not valid"
LOOP


cVN:
IF INSTR(pN$, "~") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "#") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "%") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "&") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "*") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "{") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "}") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "\") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, ":") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "<") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, ">") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "?") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "/") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "+") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, "|") >= 1 THEN
    isNV = 0
ELSEIF INSTR(pN$, CHR$(37)) >= 1 THEN
    isNV = 0
ELSE
    isNV = 1
END IF
RETURN


