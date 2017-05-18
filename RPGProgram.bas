DIM SHARED pN$
pN$ = COMMAND$(1)
'pN$ = "MercuryKun"

preventexit = _EXIT
SCREEN _NEWIMAGE(600, 500, 32)
'_FULLSCREEN _SQUAREPIXELS
_MOUSEHIDE



TYPE ModifierType
    NumID AS INTEGER
    Name AS STRING * 20
    HPPerc AS INTEGER
    ATKPerc AS INTEGER
    DEPerc AS INTEGER
    LUKPerc AS INTEGER
END TYPE

TYPE ItemType

    HP AS INTEGER
    ATK AS INTEGER
    DE AS INTEGER
    LUK AS INTEGER
    Name AS STRING * 50
    NumID AS INTEGER
    Type AS SINGLE
    CTD AS INTEGER
    Stack AS INTEGER
    set AS INTEGER
    Picture AS LONG
    Modifier AS ModifierType
    ModifierChance AS INTEGER
    'FlavTXT AS STRING * 100

END TYPE
'---
TYPE SetType

    NumID AS INTEGER
    HP AS INTEGER
    ATK AS INTEGER
    DE AS INTEGER
    LUK AS INTEGER
    FlavTXT AS STRING * 100

END TYPE
'---
TYPE MapType

    NumID AS INTEGER
    PortalUpX AS INTEGER
    PortalUpY AS INTEGER
    PortalDownX AS INTEGER
    PortalDownY AS INTEGER
    TileType AS STRING * 1
    QStart AS INTEGER

END TYPE
'---
TYPE SpriteType

    Typ AS LONG
    Map AS INTEGER
    Level AS INTEGER
    X AS INTEGER
    Y AS INTEGER

    bHP AS INTEGER
    bATK AS INTEGER
    bDE AS INTEGER
    bLUK AS INTEGER

    HP AS INTEGER
    ATK AS INTEGER
    DE AS INTEGER
    LUK AS INTEGER
    XP AS INTEGER
    NeededXP AS INTEGER
    Dir AS INTEGER '1 up, 2 down, 3 left, 4 right

END TYPE
'---
TYPE MonsterType

    Typ AS LONG 'pic
    NumID AS INTEGER
    Name AS STRING * 50
    HP AS INTEGER
    ATK AS INTEGER
    DE AS INTEGER
    LUK AS INTEGER
    CTS AS DOUBLE
    Alive AS SINGLE
    XP AS INTEGER
    Picture AS LONG

END TYPE
'---
TYPE QT
    NumID AS INTEGER
    Name AS STRING * 50
    Type AS SINGLE '1 Killing, 2 Gather, 3 Travel
    monitemID AS INTEGER 'THE X VALUE, KILLCOLLECT IS Y VALUE FOR TRAVEL TYPE QUESTS
    killCollect AS INTEGER 'kill would be kill NumToDo amount of WhatToDo [5 of 1], [1 of 7]
    completed AS INTEGER
    qRequired AS INTEGER
    mapStart AS INTEGER
    'seperate array for quest strings to display
END TYPE
'--



REDIM SHARED MSpawn(1) AS INTEGER
REDIM SHARED MonOnTile(8) AS MonsterType

DIM SHARED OnPortalNum AS SINGLE
DIM SHARED ToWhatX AS SINGLE
DIM SHARED ToWhatY AS SINGLE
DIM SHARED WherePortalLead AS SINGLE
DIM SHARED questPromptItem AS SINGLE

DIM SHARED totalMonsters%
DIM SHARED true%
DIM SHARED false%
DIM SHARED setCount AS INTEGER
DIM SHARED qCount AS INTEGER
DIM SHARED qli AS INTEGER
DIM SHARED moved AS INTEGER
DIM SHARED totalModifiers AS INTEGER
DIM SHARED totalModifiersChance AS INTEGER

DIM SHARED Strings$
DIM SHARED c$
DIM SHARED worldUp$
DIM SHARED worldDown$
DIM SHARED worldLeft$
DIM SHARED worldRight$
DIM SHARED worldInv$
DIM SHARED worldQuit$
DIM SHARED worldQuest$
DIM SHARED worldMonStat$
DIM SHARED invExit$
DIM SHARED invEq$
DIM SHARED invUnEq$
DIM SHARED invDelete$
DIM SHARED invDisplayItem$
DIM globalKeybinds AS SINGLE

OPEN _CWD$ + "\Content\Config\Keybinds.csv" FOR INPUT AS #1
INPUT #1, globalKeybinds, worldUp$, worldLeft$, worldDown$, worldRight$, worldInv$, worldQuit$, worldQuest$, worldMonStat$, invExit$, invEq$, invUnEq$, invDelete$, invDisplayItem$
CLOSE #1

IF globalKeybinds = 1 THEN 'CURRENTLY THE ONLY GLOBAL KEYBIND IS EXAMINATION
    invDisplayItem$ = worldMonStat$
    invExit$ = worldQuit$
END IF

DIM SHARED heroUp&
DIM SHARED heroDown&
DIM SHARED heroRight&
DIM SHARED heroLeft&
DIM SHARED block&
DIM SHARED portal&
DIM SHARED normTile&
DIM SHARED normQ&
DIM SHARED NoMap&

DIM SHARED Hero AS SpriteType
DIM SHARED CombatHero AS SpriteType
DIM SHARED MonToFight AS MonsterType
DIM SHARED MapInfo AS MapType 'for current map
DIM SHARED curQuest AS QT

OPEN _CWD$ + "\Content\Stats\SetList.csv" FOR INPUT AS #1
DO
    setCount = setCount + 1
    LINE INPUT #1, Strings$
LOOP UNTIL EOF(1)
CLOSE #1
DIM SHARED SetListArray(setCount) AS SetType
DIM SHARED SetListBooleans(7, setCount) AS INTEGER
'INIT QUEST ARRAY FOR STUFF TO USE
OPEN _CWD$ + "\Content\Stats\QList.csv" FOR INPUT AS #1
qCount = 0
DO
    qCount = qCount + 1
    LINE INPUT #1, Strings$
LOOP UNTIL EOF(1)
CLOSE #1
DIM SHARED QList(qCount) AS QT
'---
OPEN _CWD$ + "\Content\Stats\QText.csv" FOR INPUT AS #1
DO
    qli = qli + 1
    INPUT #1, Strings$, Strings$, Strings$
LOOP UNTIL EOF(1)
CLOSE #1
DIM SHARED QText(qli, 3) AS STRING
'---
REDIM SHARED Map(1, 1) AS LONG
'---
OPEN _CWD$ + "\Content\Stats\Monsters.csv" FOR INPUT AS #1
DO
    totalMonsters% = totalMonsters% + 1
    LINE INPUT #1, Strings$
LOOP UNTIL EOF(1)
CLOSE #1
DIM SHARED MonList(totalMonsters%) AS MonsterType
'---
OPEN _CWD$ + "\Content\Stats\Items.csv" FOR INPUT AS #1
DIM SHARED totalItems%
DO
    totalItems% = totalItems% + 1
    LINE INPUT #1, Strings$
LOOP UNTIL EOF(1)
CLOSE #1
DIM SHARED ItemList(totalItems%) AS ItemType
DIM SHARED EquipList(7) AS ItemType
'---
OPEN _CWD$ + "\Content\Stats\" + pN$ + "Sizes.txt" FOR INPUT AS #1
INPUT #1, invSize
INPUT #1, rinvSize
CLOSE #1
REDIM SHARED Inventory(invSize) AS ItemType
REDIM SHARED ResInventory(rinvSize) AS ItemType
'---
'init modifiers
'init chance along with item
OPEN _CWD$ + "\Content\Stats\ModifierList.csv" FOR INPUT AS #1
DO
    totalModifiers = totalModifiers + 1
    LINE INPUT #1, Strings$
LOOP UNTIL EOF(1)
CLOSE #1
DIM SHARED Modifiers(totalModifiers) AS ModifierType
OPEN _CWD$ + "\Content\Stats\ModifierChances.csv" FOR INPUT AS #1
DO
    totalModifiersChance = totalModifiersChance + 1
    LINE INPUT #1, Strings$
LOOP UNTIL EOF(1)
CLOSE #1
DIM SHARED ModifierChances(totalModifiersChance, totalModifiers) AS INTEGER '3x2, assuming 3 modifier chance rates and 2 different modifiers
'want to set the actual chances here, so if Item.ModifierChance = 1 then ModifierChance(1,x) will go through
'THIS ENDS UP ALL 0?
'---
DIM SHARED dummyItem AS ItemType
dummyItem.HP = 0
dummyItem.ATK = 0
dummyItem.DE = 0
dummyItem.LUK = 0
dummyItem.Name = "No Item"
dummyItem.NumID = 0
dummyItem.Type = 0
dummyItem.CTD = 0
'---


'END VARIABLE CREATION
'THINGS THAT NEED TO BE RUN
initCall
WorldLoop
saveCall
SYSTEM






'----
SUB WorldLoop STATIC
    DO

        Engine.DrawScreen Hero
        c$ = LCASE$(INPUT$(1))
        c% = VAL(c$)
        SELECT CASE c$
            CASE worldUp$
                Hero.Dir = 1
                Engine.UpdatePlayer Hero
            CASE worldLeft$
                Hero.Dir = 3
                Engine.UpdatePlayer Hero
            CASE worldRight$
                Hero.Dir = 2
                Engine.UpdatePlayer Hero
            CASE worldDown$
                Hero.Dir = 4
                Engine.UpdatePlayer Hero
            CASE worldInv$
                ProfileLoop
            CASE worldQuit$
                gameOver = 1
            CASE "0"
                IF Map(Hero.X, Hero.Y) = portal& THEN
                    Port
                    SelectTileMonsters
                END IF
            CASE worldQuest$
                IF Map(Hero.X, Hero.Y) = normQ& THEN Quests
            CASE worldMonStat$
                DisplayMonStat
        END SELECT

        IF c% > 0 AND c% <= UBOUND(MonOnTile) THEN Combat

        IF curQuest.Type = 3 THEN
            IF (Hero.X = curQuest.monitemID) AND (Hero.Y = curQuest.killCollect) AND Hero.Map = curQuest.mapStart THEN
                qComplete
            END IF
        ELSEIF curQuest.Type = 1 OR curQuest.Type = 2 THEN
            IF curQuest.killCollect = 0 THEN
                qComplete
            END IF
        END IF
    LOOP UNTIL gameOver = 1
END SUB

SUB QRewards STATIC
    itemAmt = 0
    OPEN _CWD$ + "\Content\Stats\QRewards.csv" FOR INPUT AS #1
    FOR i = 1 TO curQuest.NumID - 1
        LINE INPUT #1, Strings$
    NEXT
    INPUT #1, xp
    Hero.XP = Hero.XP + xp
    CheckLevelUp

    'check amt of open inv spaces
    FOR i = 1 TO UBOUND(inventory)
        IF Inventory(i).NumID = 0 THEN itemAmt = itemAmt + 1
    NEXT
    REDIM freespaces(itemAmt) AS SINGLE
    fsc = 1
    FOR i = 1 TO UBOUND(inventory)
        IF Inventory(i).NumID = 0 THEN freespaces(fsc) = i: fsc = fsc + 1
    NEXT
    fsc = 1
    INPUT #1, item
    WHILE item <> 0
        Inventory(freespaces(fsc)) = ItemList(item)
        fsc = fsc + 1
        INPUT #1, item
    WEND

    CLOSE #1
END SUB

SUB qComplete STATIC
    cqc = curquestitemcheck
    IF cqc = 1 THEN
        QList(curQuest.NumID).completed = 1
        PRINT QText(curQuest.NumID, 3)
        QRewards
        PRINT "Any key to continue..."
        _DISPLAY
        Strings$ = INPUT$(1)
        curQuest.NumID = 0
        curQuest.Name = "No Quest"
        curQuest.Type = 0
        curQuest.monitemID = 0
        curQuest.killCollect = 0
        questPromptItem = 0
    ELSE
        IF questPromptItem = 0 THEN
            PRINT "Unable to complete quest due to item slots!"
            _DISPLAY
            Strings$ = INPUT$(1)
            questPromptItem = 1
        END IF
    END IF
    ' NumID AS INTEGER
    ' Name AS STRING * 50
    ' Type AS SINGLE '1 Killing, 2 Gather, 3 Travel
    ' monitemID AS INTEGER 'THE X VALUE, KILLCOLLECT IS Y VALUE FOR TRAVEL TYPE QUESTS
    ' killCollect AS INTEGER 'kill would be kill NumToDo amount of WhatToDo [5 of 1], [1 of 7]
    ' completed AS INTEGER
    ' qRequired AS INTEGER
END SUB

FUNCTION curquestitemcheck
    OPEN _CWD$ + "\Content\Stats\QRewards.csv" FOR INPUT AS #1
    INPUT #1, xp
    DO
        INPUT #1, xp
        itemAmt = itemAmt + 1
    LOOP WHILE xp <> 0
    CLOSE #1
    FOR i = 1 TO UBOUND(Inventory)
        IF Inventory(i).NumID = 0 THEN itemAmt = itemAmt - 1
    NEXT
    IF itemAmt > 0 THEN PRINT "There are " + STR$(itemAmt) + " missing item slots."
    IF itemAmt <= 0 THEN curquestitemcheck = 1 ELSE curquestitemcheck = 0
END FUNCTION

SUB Quests STATIC
    CLS
    PRINT "You may accept a quest by pressing A, and deny by going back to the world (w)"
    PRINT "Quest progress will be displayed below your map"
    PRINT "Quests will be automatically turned in once the conditions have been met"
    PRINT "At the moment this cannot be turned off"
    OnQNum = 0
    FOR r = 1 TO 50
        FOR c = 1 TO 50
            IF (POINT(r, c) = _RGB(20, 20, 20)) THEN
                OnQNum = OnQNum + 1
            END IF
            IF (r = Hero.X) AND (c = Hero.Y) THEN
                GOTO PIsNP:
            END IF
        NEXT
    NEXT 'FINDS QUEST NUMBER FOR MAP
    PIsNP:
    PRINT QText(OnQNum, 1)
    _DISPLAY

    DO
        c$ = LCASE$(INPUT$(1))
        IF c$ = "a" THEN
            AcceptQ
            EXIT SUB
        END IF
    LOOP UNTIL c$ = "w"
END SUB

SUB AcceptQ STATIC 'DIM SHARED curQuest AS QT
    testimage& = _LOADIMAGE(_CWD$ + "\Content\Pictures\map" + LTRIM$(STR$(MapInfo.NumID)) + ".png")
    _SOURCE testimage&

    OnQNum = -1
    FOR r = 1 TO 50
        FOR c = 1 TO 50
            IF (POINT(r, c) = _RGB(20, 20, 20)) THEN
                OnQNum = OnQNum + 1
            END IF
            IF (r = Hero.X) AND (c = Hero.Y) THEN
                GOTO PIsNP:
            END IF
        NEXT
    NEXT 'FINDS QUEST NUMBER FOR MAP
    PIsNP:
    quNum = MapInfo.QStart + OnQNum
    IF QList(quNum).completed = 0 AND (QList(QList(quNum).qRequired).completed = 1 OR QList(quNum).qRequired = 0) THEN
        curQuest = QList(quNum)
        curQuest.mapStart = Hero.Map
        CLS
        PRINT QText(curQuest.NumID, 2)
        PRINT "Any key to continue..."
        _DISPLAY
        x$ = INPUT$(1)
    ELSE
        AcceptQFailed
    END IF
END SUB

SUB AcceptQFailed
    CLS
    PRINT "You have done one of the following:"
    PRINT "Completed the quest previously, or"
    PRINT "not finished a required quest."
    PRINT "An option to repeat quests may be added later.."
    _DISPLAY
    Strings$ = INPUT$(1)
END SUB

SUB ProfileLoop STATIC
    DO

        Engine.DrawProfile
        x$ = LCASE$(INPUT$(1))
        SELECT CASE x$
            CASE invExit$
                EXIT DO
            CASE invEq$
                tryToEquip
            CASE invUnEq$
                tryToUnEquip
            CASE invDelete$
                TryToDeleteItem
            CASE invDisplayItem$
                DisplayItemStats
        END SELECT
    LOOP
END SUB

SUB DisplayMonStat
    CLS
    FOR i = 1 TO UBOUND(MonOnTile)
        _PRINTSTRING (305, 5 + (i * 20)), STR$(cnt + 1) + ": " + MonOnTile(i).Name
        cnt = cnt + 1
    NEXT
    PRINT "Which monster's stats would you like to see?"
    _DISPLAY
    DO
        dis = VAL(INPUT$(1))
        IF dis = 0 THEN EXIT DO
        IF dis > 0 AND dis < UBOUND(MonOnTile) THEN
            IF MonOnTile(dis).Alive > 0 THEN
                DisMStat (MonOnTile(dis).NumID)
                EXIT DO
            END IF
        END IF
        CLS
        PRINT "Couldn't show that monster, try again, 0 to exit"
        _DISPLAY
    LOOP
END SUB


SUB DisMStat (NumID)
    CLS
    PRINT "Item Stats"
    PRINT "----------"
    PRINT "Name   : " + MonList(NumID).Name
    PRINT "Health : " + STR$(MonList(NumID).HP)
    PRINT "Str/Atk: " + STR$(MonList(NumID).ATK)
    PRINT "Defense: " + STR$(MonList(NumID).DE)
    PRINT "Luck   : " + STR$(MonList(NumID).LUK)
    PRINT ""
    PRINT "Item Drops"
    PRINT "----------"

    OPEN _CWD$ + "\Content\Stats\MDrop.csv" FOR INPUT AS #1
    FOR i = 1 TO NumID - 1
        LINE INPUT #1, Strings$
    NEXT
    DO
        INPUT #1, in
        IF in <> 0 THEN
            count = count + 1
            PRINT STR$(count) + ": "; ItemList(in).Name
        END IF
    LOOP UNTIL in = 0
    CLOSE #1

    _PUTIMAGE (600 - _WIDTH(MonList(NumID).Picture), 500 - _HEIGHT(MonList(NumID).Picture)), MonList(NumID).Picture '600X500
    _DISPLAY
    getchh$ = INPUT$(1)
END SUB

SUB DisplayItemStats
    CLS
    Engine.DisplayInventory
    PRINT "Which item's stats would you like to see?"
    _DISPLAY
    DO
        dis = VAL(INPUT$(2))
        IF dis > 0 AND dis < UBOUND(Inventory) THEN
            IF Inventory(dis).NumID > 0 THEN
                DispStat (Inventory(dis).NumID)
                EXIT DO
            END IF
        ELSE
            CLS
            Engine.DisplayInventory
            PRINT "Unable to display that item's stats (Not a number/item there?)"
            _DISPLAY
        END IF
    LOOP
END SUB
SUB DispStat (NumID)
    CLS
    PRINT "Item Stats"
    PRINT "----------"
    PRINT "Name   : " + ItemList(NumID).Name
    PRINT "Item   : ";
    SELECT CASE ItemList(NumID).Type
        CASE 1
            PRINT "Chestplate"
        CASE 2
            PRINT "Helmet"
        CASE 3
            PRINT "Gloves"
        CASE 4
            PRINT "Boots"
        CASE 5
            PRINT "Ring"
        CASE 6
            PRINT "Weapon"
        CASE 7
            PRINT "Shield"
    END SELECT
    PRINT "Health : " + STR$(ItemList(NumID).HP)
    PRINT "Str/Atk: " + STR$(ItemList(NumID).ATK)
    PRINT "Defense: " + STR$(ItemList(NumID).DE)
    PRINT "Luck   : " + STR$(ItemList(NumID).LUK)
    PRINT "Set No.: " + STR$(ItemList(NumID).set)
    PRINT "A Set No. of 0 indicates no set"
    PRINT "Any items in a set will also have their set stats displayed below"
    PRINT ""
    IF ItemList(NumID).set > 0 THEN
        PRINT "Item Set Stats"
        PRINT "--------------"
        PRINT "Health : " + STR$(SetListArray(ItemList(NumID).set).HP)
        PRINT "Str/Atk: " + STR$(SetListArray(ItemList(NumID).set).ATK)
        PRINT "Defense: " + STR$(SetListArray(ItemList(NumID).set).DE)
        PRINT "Luck   : " + STR$(SetListArray(ItemList(NumID).set).LUK)
        PRINT RTRIM$(SetListArray(ItemList(NumID).set).FlavTXT)
    END IF
    PRINT ""
    PRINT "Press any key to continue"
    _PUTIMAGE (600 - _WIDTH(ItemList(NumID).Picture), 500 - _HEIGHT(ItemList(NumID).Picture)), ItemList(NumID).Picture '600X500
    _DISPLAY
    getch$ = INPUT$(1)
END SUB

'THINGS THAT CALL OTHER THINGS
SUB Engine.DrawScreen (Hero AS SpriteType) STATIC
    CLS
    Engine.DrawMap
    Engine.DrawPlayer Hero
    IF moved = true% THEN
        SelectTileMonsters
        moved = false%
    END IF
    Engine.DrawOptions
    Engine.DrawQuests
    _DISPLAY
END SUB

SUB Engine.DrawQuests STATIC 'START ABOUT 350
    SELECT CASE curQuest.Type
        CASE 0
            cqt$ = "QType: No Quest"
            cqmi$ = "QP1:   No Quest"
            cqkc$ = "QP2:   No Quest"
        CASE 1
            cqt$ = "QType: Killing"
            cqmi$ = "Mon:   " + MonList(curQuest.monitemID).Name
            cqkc$ = "Amt:   " + LTRIM$(STR$(curQuest.killCollect))
        CASE 2
            cqt$ = "QType: Gathering"
            cqmi$ = "Item:  " + ItemList(curQuest.monitemID).Name
            cqkc$ = "Amt:   " + LTRIM$(STR$(curQuest.killCollect))
        CASE 3
            cqt$ = "QType: Travel"
            cqmi$ = "XLoc:  " + LTRIM$(STR$(curQuest.monitemID))
            cqkc$ = "YLoc:  " + LTRIM$(STR$(curQuest.killCollect))
    END SELECT

    _PRINTSTRING (5, 350), LTRIM$(curQuest.Name)
    _PRINTSTRING (5, 365), cqt$
    _PRINTSTRING (5, 380), cqmi$
    _PRINTSTRING (5, 395), cqkc$

    'INPUT #1, curQuest.Name, curQuest.Type, curQuest.monitemID, curQuest.killCollect
END SUB


SUB Engine.DrawProfile STATIC
    CLS
    Engine.DisplayStats
    Engine.DisplayEquipped
    Engine.DisplayInventory
    _DISPLAY
END SUB

FUNCTION itemHP
    itemHPt = 0
    FOR i = 1 TO 7
        itemHPt = itemHPt + EquipList(i).HP
    NEXT
    itemHP = itemHPt
END FUNCTION
FUNCTION itemATK
    itemATKt = 0
    FOR i = 1 TO 7
        itemATKt = itemATKt + EquipList(i).ATK
    NEXT
    itemATK = itemATKt
END FUNCTION
FUNCTION itemDE
    itemDE = 0
    FOR i = 1 TO 7
        itemDEt = itemDEt + EquipList(i).DE
    NEXT
    itemDE = itemDEt
END FUNCTION
FUNCTION itemLUK
    itemLUKt = 0
    FOR i = 1 TO 7
        itemLUKt = itemLUKt + EquipList(i).LUK
    NEXT
    itemLUK = itemLUKt
END FUNCTION

SUB BaseToItem STATIC

    Hero.HP = Hero.bHP + itemHP
    Hero.ATK = Hero.bATK + itemATK
    Hero.DE = Hero.bDE + itemDE
    Hero.LUK = Hero.bLUK + itemLUK

END SUB

'INITIALIZATION FUNCTIONS
SUB InitVariables STATIC
    true% = 1
    false% = 0
    heroUp& = _LOADIMAGE(_CWD$ + "\Content\Pictures\player.png")
    heroDown& = _LOADIMAGE(_CWD$ + "\Content\Pictures\player.png")
    heroLeft& = _LOADIMAGE(_CWD$ + "\Content\Pictures\player.png")
    heroRight& = _LOADIMAGE(_CWD$ + "\Content\Pictures\player.png")
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "Stats.txt" FOR INPUT AS #1
    INPUT #1, Hero.Map, Hero.X, Hero.Y, Hero.bHP, Hero.bATK, Hero.bDE, Hero.bLUK, Hero.Dir, Hero.XP, Hero.Level
    CLOSE #1
    Hero.Typ = heroDown&

    Hero.NeededXP = xptonextlevelfunc
    gameOver = false%
    moved = false%

    loadmap
END SUB


SUB InitMonList () STATIC
    OPEN _CWD$ + "\Content\Stats\Monsters.csv" FOR INPUT AS #1
    FOR i = 1 TO totalMonsters%
        MonList(i).NumID = i
        MonList(i).Alive = 1
        INPUT #1, MonList(i).Name, MonList(i).HP, MonList(i).ATK, MonList(i).DE, MonList(i).LUK, MonList(i).XP
        IF _FILEEXISTS(_CWD$ + "\Content\Pictures\Monsters\" + RTRIM$(MonList(i).Name) + ".png") THEN
            MonList(i).Picture = _LOADIMAGE(_CWD$ + "\Content\Pictures\Monsters\" + RTRIM$(MonList(i).Name) + ".png")
        ELSE
            MonList(i).Picture = _LOADIMAGE(_CWD$ + "\Content\Pictures\Monsters\NoMon.png")
        END IF

    NEXT
    CLOSE #1
END SUB


SUB initItemList STATIC
    OPEN _CWD$ + "\Content\Stats\Items.csv" FOR INPUT AS #1
    FOR i = 1 TO totalItems%
        ItemList(i).NumID = i
        INPUT #1, ItemList(i).Name, ItemList(i).Type, ItemList(i).HP, ItemList(i).ATK, ItemList(i).DE, ItemList(i).LUK, ItemList(i).CTD, ItemList(i).set, ItemList(i).ModifierChance
        IF _FILEEXISTS(_CWD$ + "\Content\Pictures\Items\" + RTRIM$(ItemList(i).Name) + ".png") THEN
            ItemList(i).Picture = _LOADIMAGE(_CWD$ + "\Content\Pictures\Items\" + RTRIM$(ItemList(i).Name) + ".png")
        ELSE
            ItemList(i).Picture = _LOADIMAGE(_CWD$ + "\Content\Pictures\Items\NoItem.png")
        END IF
    NEXT
    CLOSE #1
END SUB

SUB initEquipped STATIC
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "eqItem.txt" FOR INPUT AS #1
    FOR i = 1 TO 7
        INPUT #1, placeholder%
        IF placeholder% > 0 THEN EquipList(i) = ItemList(placeholder%) ELSE EquipList(i) = dummyItem
        INPUT #1, EquipList(i).Modifier.NumID
    NEXT
    CLOSE #1
    FOR i = 1 TO 7
        EquipList(i).Modifier.Name = Modifiers(EquipList(i).Modifier.NumID).Name
        EquipList(i).Modifier.HPPerc = Modifiers(EquipList(i).Modifier.NumID).HPPerc
        EquipList(i).Modifier.ATKPerc = Modifiers(EquipList(i).Modifier.NumID).ATKPerc
        EquipList(i).Modifier.DEPerc = Modifiers(EquipList(i).Modifier.NumID).DEPerc
        EquipList(i).Modifier.LUKPerc = Modifiers(EquipList(i).Modifier.NumID).LUKPerc
    NEXT

END SUB

SUB initInv STATIC
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "Inv.txt" FOR INPUT AS #1
    FOR i = 1 TO UBOUND(Inventory)
        INPUT #1, placeholder%
        IF placeholder% > 0 THEN Inventory(i) = ItemList(placeholder%) ELSE Inventory(i) = dummyItem
        INPUT #1, Inventory(i).Modifier.NumID
    NEXT
    CLOSE #1
    FOR i = 1 TO 7
        Inventory(i).Modifier.Name = Modifiers(Inventory(i).Modifier.NumID).Name
        Inventory(i).Modifier.HPPerc = Modifiers(Inventory(i).Modifier.NumID).HPPerc
        Inventory(i).Modifier.ATKPerc = Modifiers(Inventory(i).Modifier.NumID).ATKPerc
        Inventory(i).Modifier.DEPerc = Modifiers(Inventory(i).Modifier.NumID).DEPerc
        Inventory(i).Modifier.LUKPerc = Modifiers(Inventory(i).Modifier.NumID).LUKPerc
    NEXT

    OPEN _CWD$ + "\Content\Stats\" + pN$ + "RInv.txt" FOR INPUT AS #1
    FOR i = 1 TO UBOUND(ResInventory)
        INPUT #1, ph%, st%
        ResInventory(i) = ItemList(ph%)
        ResInventory(i).Stack = st%
    NEXT
    CLOSE #1

END SUB

SUB loadmap STATIC 'TO PROPERLY LOAD MAP YOU HAVE TO SET THE MAP ID AGAIN AND THEN USE LOADMA

    MapInfo.NumID = Hero.Map
    OPEN _CWD$ + "\Content\Stats\Maps.csv" FOR INPUT AS #3
    FOR i = 1 TO MapInfo.NumID - 1
        LINE INPUT #3, Strings$
    NEXT
    INPUT #3, MapInfo.TileType, MapInfo.QStart
    CLOSE #3

    block& = _LOADIMAGE(_CWD$ + "\Content\Pictures\block" + LTRIM$(MapInfo.TileType + ".png"))
    normTile& = _LOADIMAGE(_CWD$ + "\Content\Pictures\norm" + LTRIM$(MapInfo.TileType + ".png"))
    normQ& = _LOADIMAGE(_CWD$ + "\Content\Pictures\norm" + LTRIM$(MapInfo.TileType) + "Q.png")
    portal& = _LOADIMAGE(_CWD$ + "\Content\Pictures\portalUp.png")
    NoMap& = _LOADIMAGE(_CWD$ + "\Content\Pictures\NoMap.png")
    endOfLine = 0
    numOfValues = 0


    OPEN _CWD$ + "\Content\Stats\MSpawn.csv" FOR INPUT AS #1

    FOR i = 1 TO MapInfo.NumID - 1
        LINE INPUT #1, uselessstring$
    NEXT

    WHILE endOfLine = 0
        INPUT #1, counter
        IF counter <> 0 THEN
            numOfValues = numOfValues + 1
        ELSE
            endOfLine = 1
        END IF
        INPUT #1, counter2
    WEND
    CLOSE #1

    REDIM MSpawn(numOfValues) AS INTEGER

    OPEN _CWD$ + "\Content\Stats\MSpawn.csv" FOR INPUT AS #1

    FOR i = 1 TO MapInfo.NumID - 1
        LINE INPUT #1, Strings$
    NEXT

    FOR i = 1 TO numOfValues
        INPUT #1, MSpawn(i), MonList(MSpawn(i)).CTS
    NEXT

    CLOSE #1

END SUB


SUB InitMapTiles STATIC
    testimage& = _LOADIMAGE(_CWD$ + "\Content\Pictures\map" + LTRIM$(STR$(MapInfo.NumID)) + ".png")
    _SOURCE testimage&
    cisheight& = _HEIGHT(testimage&)
    rislength& = _WIDTH(testimage&)
    REDIM Map(rislength&, cisheight&)
    FOR r = 1 TO rislength&
        FOR c = 1 TO cisheight&
            IF (POINT(r, c) = _RGB(0, 0, 0)) THEN
                Map(r, c) = block&
            ELSEIF (POINT(r, c) = _RGB(56, 56, 56)) THEN
                Map(r, c) = portal&
            ELSEIF (POINT(r, c) = _RGB(256, 32, 32)) THEN
                Map(r, c) = portal&
            ELSEIF (POINT(r, c) = _RGB(20, 20, 20)) THEN
                Map(r, c) = normQ&
            ELSE
                Map(r, c) = normTile&
            END IF
        NEXT
    NEXT
END SUB


'SAVING FUNCTIONS
SUB SaveStats STATIC
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "Stats.txt" FOR OUTPUT AS #1
    PRINT #1, Hero.Map, Hero.X, Hero.Y, Hero.bHP, Hero.bATK, Hero.bDE, Hero.bLUK, Hero.Dir, Hero.XP, Hero.Level
    CLOSE #1
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "Sizes.txt" FOR OUTPUT AS #1
    PRINT #1, STR$(UBOUND(Inventory))
    PRINT #1, STR$(UBOUND(ResInventory))
    CLOSE #1

END SUB 'MARINE SANDWICHES
'NO MORE SANDWICHES
'ABOLISH THE SANDWICH BOURGEOISIE

SUB saveQuests STATIC
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "QList.txt" FOR OUTPUT AS #1
    FOR i = 1 TO UBOUND(QList)
        PRINT #1, QList(i).completed
    NEXT
    CLOSE #1
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "CurQuest.txt" FOR OUTPUT AS #1
    PRINT #1, curQuest.NumID
    IF curQuest.Type = 1 OR curQuest.Type = 2 THEN PRINT #1, curQuest.killCollect
    IF curQuest.Type = 3 THEN PRINT #1, curQuest.mapStart
    CLOSE #1
END SUB


SUB saveItems STATIC
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "eqItem.txt" FOR OUTPUT AS #1
    FOR i = 1 TO 7
        PRINT #1, EquipList(i).NumID, EquipList(i).Modifier.NumID
    NEXT
    CLOSE #1
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "Inv.txt" FOR OUTPUT AS #1
    FOR i = 1 TO UBOUND(Inventory)
        PRINT #1, Inventory(i).NumID, Inventory(i).Modifier.NumID
    NEXT
    CLOSE #1
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "RInv.txt" FOR OUTPUT AS #1
    FOR i = 1 TO UBOUND(ResInventory)
        PRINT #1, ResInventory(i).NumID, ResInventory(i).Stack
    NEXT
    CLOSE #1
END SUB

'WORLD FUNCTIONS
SUB Engine.UpdatePlayer (Hero AS SpriteType) STATIC
    SELECT CASE Hero.Dir 'UPDATES PLAYER DIRECTION
        CASE 1 'up
            Hero.Typ = heroUp&
            IF Map(Hero.X, Hero.Y - 1) <> block& AND Hero.Y - 1 >= 1 THEN
                Hero.Y = Hero.Y - 1
                moved = true%
            END IF
        CASE 2 'down
            Hero.Typ = heroDown&
            IF Hero.Y + 1 <= UBOUND(Map, 2) THEN
                IF Map(Hero.X, Hero.Y + 1) <> block& THEN
                    Hero.Y = Hero.Y + 1
                    moved = true%
                END IF
            END IF
        CASE 3 'left
            Hero.Typ = heroLeft&
            IF Map(Hero.X - 1, Hero.Y) <> block& AND Hero.X - 1 >= 1 THEN
                Hero.X = Hero.X - 1
                moved = true%
            END IF
        CASE 4 'right
            Hero.Typ = heroRight&
            IF Hero.X + 1 <= UBOUND(Map, 1) THEN
                IF Map(Hero.X + 1, Hero.Y) <> block& THEN
                    Hero.X = Hero.X + 1
                    moved = true%
                END IF
            END IF
    END SELECT
END SUB

SUB Engine.DrawMap STATIC
    FOR r = 1 TO 5
        FOR c = 1 TO 5

            c1x = 60 * (r - 1) + 1
            c1y = 60 * (c - 1)
            c2x = (60 * r)
            c2y = (60 * c) - 1
            wr = Hero.X - (3 - r)
            wc = Hero.Y - (3 - c)

            IF ((wr >= 1) AND (wc >= 1)) AND ((wr <= UBOUND(Map, 1)) AND (wc <= UBOUND(Map, 2))) THEN _PUTIMAGE (c1x, c1y)-(c2x, c2y), Map(wr, wc) ELSE _PUTIMAGE (c1x, c1y)-(c2x, c2y), NoMap&


        NEXT
    NEXT



END SUB

SUB Engine.DrawPlayer (Hero AS SpriteType) STATIC
    _PUTIMAGE (121, 120)-(180, 181), Hero.Typ
END SUB

SUB Engine.DrawOptions STATIC 'TECHNICALLY JUST DRAW ACTIONS....

    _PRINTSTRING (5, 305), "Coordinates: " + STR$(Hero.X) + ", " + STR$(Hero.Y)

    cnt = 0
    IF Map(Hero.X, Hero.Y) = portal& THEN
        _PRINTSTRING (305, 5 + (cnt * 20)), " 0: Portal"
    END IF
    FOR i = 1 TO UBOUND(MonOnTile)
        IF MonOnTile(i).Alive = 0 THEN
            _PRINTSTRING (305, 5 + (i * 20)), STR$(cnt + 1) + ": " + LTRIM$(STR$(MonOnTile(i).XP)) + " EXP Gain"
        ELSE
            _PRINTSTRING (305, 5 + (i * 20)), STR$(cnt + 1) + ": " + MonOnTile(i).Name
        END IF
        cnt = cnt + 1
    NEXT
END SUB


SUB SelectTileMonsters STATIC
    DIM DummyMonOnTile(8) AS MonsterType
    FOR i = 1 TO 8
        didSpawn = 0
        RANDOMIZE TIMER
        FOR zz = 1 TO UBOUND(MSpawn)
            MonChoose = INT(RND * 101) + 1
            IF MonChoose < MonList(MSpawn(zz)).CTS THEN
                DummyMonOnTile(i) = MonList(MSpawn(zz))
                didSpawn = 1
                RANDOMIZE TIMER
                EXIT FOR
            END IF
        NEXT

        IF didSpawn = 0 THEN
            DummyMonOnTile(i).Alive = 0
        END IF
    NEXT

    count = 0
    FOR i = 1 TO 8
        IF DummyMonOnTile(i).Alive <> 0 THEN
            count = count + 1
        END IF
    NEXT
    REDIM MonOnTile(count) AS MonsterType
    count = 1
    FOR i = 1 TO 8
        IF DummyMonOnTile(i).Alive <> 0 THEN
            MonOnTile(count) = DummyMonOnTile(i)
            count = count + 1
        END IF
    NEXT



END SUB

'PROFILE FUNCTIONS
SUB Engine.DisplayStats STATIC
    _PRINTSTRING (5, 5), "HP       : " + STR$(Hero.HP)
    _PRINTSTRING (5, 20), "Strength : " + STR$(Hero.ATK)
    _PRINTSTRING (5, 35), "Defense  : " + STR$(Hero.DE)
    _PRINTSTRING (5, 50), "Luck     : " + STR$(Hero.LUK)
    _PRINTSTRING (5, 65), "Exp      : " + STR$(Hero.XP) + ", (" + LTRIM$(STR$(_ROUND((Hero.XP / Hero.NeededXP) * 100))) + "%)"
    _PRINTSTRING (5, 80), "Level    : " + STR$(Hero.Level)
END SUB


SUB Engine.DisplayEquipped STATIC
    _PRINTSTRING (5, 100), "Equipped Items:"
    FOR i = 2 TO 8
        _PRINTSTRING (5, 85 + (i * 15)), LTRIM$(STR$(i - 1)) + ": " + RTRIM$(EquipList(i - 1).Name)
    NEXT
END SUB

SUB Engine.DisplayInventory STATIC
    _PRINTSTRING (250, 5), "INVENTORY"
    FOR i = 1 TO UBOUND(Inventory)
        _PRINTSTRING (250, 5 + (i * 15)), LTRIM$(STR$(i)) + ": " + RTRIM$(Inventory(i).Name)
    NEXT
    FOR ii = 1 TO UBOUND(ResInventory)
        _PRINTSTRING (250, 5 + (i * 15) + (ii * 15)), LTRIM$(STR$(ii)) + ": " + RTRIM$(ResInventory(ii).Name) + " Stk: " + LTRIM$(STR$(ResInventory(ii).Stack))
    NEXT

END SUB

SUB tryToEquip STATIC
    _PRINTSTRING (5, 250), "What would you like to equip? Add a 0 before a single-digit, e.g. 01, 02."
    _DISPLAY
    f$ = INPUT$(2)
    IF VAL(f$) > 0 AND VAL(f$) < UBOUND(Inventory) THEN
        IF Inventory(VAL(f$)).NumID > 0 THEN
            OnEquipItem (VAL(f$))
        ELSE
            _PRINTSTRING (5, 265), "There is no item to equip.. Any key to continue."
            _DISPLAY
            Strings$ = INPUT$(1)
        END IF
    END IF
END SUB
SUB tryToUnEquip STATIC
    _PRINTSTRING (5, 250), "What to dequip? Add a 0 before a single-digit"
    _DISPLAY
    f$ = INPUT$(2)
    IF VAL(f$) > 0 AND VAL(f$) < 8 THEN
        IF EquipList(VAL(f$)).NumID > 0 THEN
            _PRINTSTRING (5, 265), "To where?"
            _DISPLAY
            as$ = INPUT$(2)
            IF VAL(as$) > 0 AND VAL(f$) < UBOUND(Inventory) THEN
                OnUnEquip VAL(f$), VAL(as$)
            END IF
        END IF
    ELSE
        _PRINTSTRING (5, 265), "There is no item to unequip.. Any key to continue."
        _DISPLAY
        Strings$ = INPUT$(1)
    END IF
END SUB
SUB TryToDeleteItem STATIC
    _PRINTSTRING (5, 250), "What would you like to destroy? Add a 0 before a single-digit, e.g. 01, 02."
    _DISPLAY
    f$ = INPUT$(2)
    IF VAL(f$) > 0 AND VAL(f$) < UBOUND(Inventory) THEN
        IF Inventory(VAL(f$)).NumID > 0 THEN
            _PRINTSTRING (5, 265), "Are you sure you want to delete this? y/anything but y"
            _DISPLAY
            yn$ = LCASE$(INPUT$(1))
            IF yn$ = "y" THEN
                Inventory(VAL(f$)) = dummyItem
            END IF
        END IF
    END IF
END SUB


SUB OnEquipItem (WhereInInv) STATIC
    DIM PlaceholderItem AS ItemType
    eq = Inventory(WhereInInv).Type

    PlaceholderItem = EquipList(eq)
    EquipList(eq) = Inventory(WhereInInv)
    Inventory(WhereInInv) = PlaceholderItem

    BaseUpdateStats

END SUB
SUB OnUnEquip (WhereInEquip, toWhatSlot) STATIC
    IF Inventory(toWhatSlot).NumID = 0 THEN
        Inventory(toWhatSlot) = EquipList(WhereInEquip)
        EquipList(WhereInEquip) = dummyItem
        BaseUpdateStats
    ELSE
        CLS
        PRINT "Can't unequip the item to that slot!"
        _DISPLAY
        Strings$ = INPUT$(1)
    END IF
END SUB

FUNCTION xptonextlevelfunc STATIC
    IF Hero.Level = 1 THEN
        xptonextlevelfunc = 100
    ELSE
        xptonextlevelfunc = 1.5 * (((Hero.Level - 1) ^ 3) * 5) + 100
    END IF
END FUNCTION


'COMBAT FUNCTIONS
SUB Combat STATIC
    CombatHero = Hero
    MonToFight = MonOnTile(VAL(c$))
    IF MonToFight.Alive = 0 THEN
        EXIT SUB
    END IF
    CLS
    ActualFighting
END SUB

SUB ActualFighting STATIC
    CLS
    count = 0
    turns = 0

    WHILE (CombatHero.HP > 0 AND MonToFight.HP > 0 AND turns < 20)
        '_LIMIT 1
        GOSUB PlayerAttack:
        IF MonToFight.HP > 0 THEN
            GOSUB MonsterAttack:
        END IF



        PRINT "--------------------------------------------------------------"
        turns = turns + 1
        _DISPLAY
        SLEEP 1
    WEND

    IF MonToFight.HP <= 0 THEN
        PRINT "You have successfully defeated the " + MonToFight.Name
        PRINT "Any key to continue.."
        _DISPLAY
        uselessstring$ = INPUT$(1)
        IF curQuest.Type = 1 THEN
            IF MonToFight.NumID = curQuest.monitemID THEN
                curQuest.killCollect = curQuest.killCollect - 1
            END IF
        END IF
        AwardEXP (MonToFight.XP)
        AwardItemMonsterDrop (MonToFight.NumID)
        MonOnTile(VAL(c$)).Alive = 0
    ELSEIF CombatHero.HP <= 0 THEN
        AwardEXP -3 * MonToFight.XP
        DisplayBattleLossWarning
    END IF

    EXIT SUB
    PlayerAttack:
    combatCalch = INT((MonToFight.DE - (CombatHero.ATK / 2) + INT(RND * (CombatHero.ATK * 1.1 + (CombatHero.LUK / 2)) + (CombatHero.ATK * .9 - (CombatHero.LUK / 2)))))

    IF combatCalch > 0 THEN
        MonToFight.HP = MonToFight.HP - combatCalch
        PRINT "You dealt " + LTRIM$(STR$(combatCalch)) + " damage to the " + RTRIM$(MonToFight.Name)
        PRINT "The " + RTRIM$(MonToFight.Name) + " has " + LTRIM$(STR$(MonToFight.HP)) + " health remaining"
        PRINT ""

    ELSE
        MonToFight.HP = MonToFight.HP - 1
        PRINT "You dealt 1 damage to the " + RTRIM$(MonToFight.Name)
        PRINT "The " + RTRIM$(MonToFight.Name) + " has " + LTRIM$(STR$(MonToFight.HP)) + " health remaining"
        PRINT ""

    END IF
    RETURN
    MonsterAttack:
    combatCalc = INT((CombatHero.DE - (MonToFight.ATK / 2) + INT(RND * (MonToFight.ATK * 1.1 + (MonToFight.LUK / 2)) + (MonToFight.ATK * .9 - (MonToFight.LUK / 2)))))
    IF combatCalc > 0 THEN
        CombatHero.HP = CombatHero.HP - combatCalc
        PRINT "The " + RTRIM$(MonToFight.Name) + " dealt " + LTRIM$(STR$(combatCalc)) + " damage to you "
        PRINT "You have " + LTRIM$(STR$(CombatHero.HP)) + " health remaining"

    ELSE
        CombatHero.HP = CombatHero.HP - 1
        PRINT RTRIM$(MonToFight.Name) + " dealt 1 damage to you "
        PRINT "You have " + LTRIM$(STR$(CombatHero.HP)) + " health remaining."

    END IF
    RETURN
END SUB

SUB DisplayBattleLossWarning STATIC
    PRINT "You lost your fight against (a) " + RTRIM$(MonToFight.Name)
    _DISPLAY
    Strings$ = INPUT$(1)
END SUB



'AWARD FUNCTIONS (EXP, ITEM, ETC)
SUB AllocateLevelUp (ToAllocate) STATIC
    CLS
    PRINT "You have " + LTRIM$(STR$(ToAllocate)) + " skill points to allocate"
    PRINT "S for Strength, D for Defense, L for Luck, H for HP.": PRINT " Every point in HP gives 3 Health, every 2 in STR gives 1 DMG."
    PRINT "Every 5 points in base strength gives you an extra Inventory slot."
    _DISPLAY
    WHILE ToAllocate > 0
        aloevera$ = LCASE$(INPUT$(1))
        IF aloevera$ = "s" THEN
            Hero.bATK = Hero.bATK + 1
            ToAllocate = ToAllocate - 1
            PRINT "1 point to Strength (+.5 Damage)"
            PRINT "You have " + LTRIM$(STR$(ToAllocate)) + " points remaining"
        ELSEIF aloevera$ = "d" THEN
            Hero.bDE = Hero.bDE + 1
            ToAllocate = ToAllocate - 1
            PRINT "1 point to Defense"
            PRINT "You have " + LTRIM$(STR$(ToAllocate)) + " points remaining"
        ELSEIF aloevera$ = "l" THEN
            Hero.bLUK = Hero.bLUK + 1
            ToAllocate = ToAllocate - 1
            PRINT "1 point to Luck"
            PRINT "You have " + LTRIM$(STR$(ToAllocate)) + " points remaining"
        ELSEIF aloevera$ = "h" THEN
            Hero.bHP = Hero.bHP + 3
            ToAllocate = ToAllocate - 1
            PRINT "1 point to HP (+3 HP)"
            PRINT "You have " + LTRIM$(STR$(ToAllocate)) + " points remaining"
        END IF
        _DISPLAY
    WEND

    addedAtk = 0
    FOR i = 1 TO 7
        addedAtk = addedAtk + EquipList(i).ATK
    NEXT

    IF (INT((Hero.ATK - addedAtk) / 5) + 15) > UBOUND(Inventory) THEN 'has -1 in the thing to account for the +1 strength we start with
        IncreaseInventorySize INT((Hero.ATK - addedAtk) / 5)
    END IF

    BaseUpdateStats
END SUB


SUB AwardItems (ItemNumID) STATIC
    IF ItemList(ItemNumID).Type < 8 THEN
        FOR f = 1 TO UBOUND(Inventory)
            IF Inventory(f).NumID = 0 THEN
                Inventory(f) = ItemList(ItemNumID)
                GiveModifiers (f)
                ApplyModifiers (f)
                DisplayItemObtained Inventory(f).Name
                EXIT SUB
            END IF
        NEXT
    ELSE
        FOR f = 1 TO UBOUND(ResInventory)
            IF ResInventory(f).NumID = 0 THEN
                ResInventory(f) = ItemList(ItemNumID)
                ResInventory(f).Stack = ResInventory(f).Stack + 1
                DisplayItemObtained ResInventory(f).Name
                EXIT SUB
            ELSEIF ResInventory(f).Name = ItemList(ItemNumID).Name THEN
                ResInventory(f).Stack = ResInventory(f).Stack + 1
                DisplayItemObtained ResInventory(f).Name
                EXIT SUB
            END IF
        NEXT
        REDIM _PRESERVE ResInventory(UBOUND(ResInventory) + 1) AS ItemType
        ResInventory(UBOUND(ResInventory)) = ItemList(ItemNumID)
        ResInventory(UBOUND(ResInventory)).Stack = 1
        DisplayItemObtained ResInventory(UBOUND(ResInventory)).Name
    END IF
END SUB
SUB DisplayItemObtained (InvItem$)
    CLS
    PRINT RTRIM$(InvItem$) + " recieved."
    PRINT "Any key to continue..."
    _DISPLAY
    Strings$ = INPUT$(1)
END SUB

SUB AwardEXP (XP) STATIC
    IF Hero.XP + XP > 0 THEN
        Hero.XP = Hero.XP + XP
    ELSE
        Hero.XP = 0
    END IF
    CheckLevelUp
END SUB
SUB CheckLevelUp STATIC
    IF Hero.XP > Hero.NeededXP THEN
        Hero.XP = Hero.XP - Hero.NeededXP
        Hero.Level = Hero.Level + 1
        Hero.NeededXP = xptonextlevelfunc
        AllocateLevelUp (2)
    END IF
END SUB


SUB IncreaseInventorySize (byHowMuch) STATIC
    OrigSize = UBOUND(Inventory)
    REDIM _PRESERVE Inventory(UBOUND(Inventory) + byHowMuch) AS ItemType
    FOR i = OrigSize TO UBOUND(Inventory)
        Inventory(i) = dummyItem
    NEXT
END SUB

SUB AwardItemMonsterDrop (NumID)
    totalItems = 0
    OPEN _CWD$ + "\Content\Stats\MDrop.csv" FOR INPUT AS #1
    FOR i = 1 TO NumID - 1
        LINE INPUT #1, Strings$
    NEXT
    DO
        INPUT #1, in
        IF in <> 0 THEN
            totalItems = totalItems + 1
        END IF
    LOOP UNTIL in = 0
    CLOSE #1
    IF totalItems = 0 THEN EXIT SUB
    REDIM ItemsPossible(totalItems) AS INTEGER
    OPEN _CWD$ + "\Content\Stats\MDrop.csv" FOR INPUT AS #1
    FOR i = 1 TO NumID - 1
        LINE INPUT #1, Strings$
    NEXT

    FOR i = 1 TO totalItems
        INPUT #1, ItemsPossible(i)
    NEXT

    CLOSE #1

    FOR i = 1 TO totalItems
        ItemChoose = INT(RND * 101) + 1
        IF ItemChoose < ItemList(ItemsPossible(i)).CTD THEN

            IF ItemList(ItemsPossible(i)).NumID = curQuest.monitemID AND curQuest.Type = 2 THEN
                curQuest.killCollect = curQuest.killCollect - 1
                EXIT FOR
            END IF

            AwardItems (ItemsPossible(i))
            EXIT FOR
        END IF
    NEXT


END SUB
'PORTAL FUNCTIONS
SUB Port STATIC
    FindPort
    FindPortalsStats
    ChangeMap
END SUB

SUB FindPortalsStats STATIC
    OPEN _CWD$ + "\Content\Stats\Portals.csv" FOR INPUT AS #1


    FOR i = 1 TO (Hero.Map - 1)
        LINE INPUT #1, Strings$
    NEXT
    CLS
    FOR i = 1 TO OnPortalNum
        INPUT #1, WherePortalLead, ToWhatX, ToWhatY
    NEXT
    CLOSE #1
END SUB


SUB ChangeMap STATIC
    Hero.Map = WherePortalLead
    loadmap
    Hero.X = ToWhatX
    Hero.Y = ToWhatY
    InitMapTiles
END SUB


SUB FindPort STATIC
    OnPortalNum = 0
    FOR r = 1 TO 50
        FOR c = 1 TO 50
            IF (POINT(r, c) = _RGB(56, 56, 56)) THEN
                OnPortalNum = OnPortalNum + 1
            ELSEIF (POINT(r, c) = _RGB(256, 32, 32)) THEN
                OnPortalNum = OnPortalNum + 1
            END IF
            IF (r = Hero.X) AND (c = Hero.Y) THEN
                EXIT SUB
            END IF
        NEXT
    NEXT

END SUB


'----------------------------------------------

SUB initCall
    InitVariables
    InitModifiers
    initQuest
    InitSetList
    InitMonList
    InitMapTiles
    initItemList
    initEquipped
    initInv
    InitAllItemsModifiers
    BaseUpdateStats
    SelectTileMonsters
END SUB

SUB saveCall
    SaveStats
    saveItems
    saveQuests
END SUB

SUB BaseUpdateStats STATIC
    BaseToItem
    ItemSets
END SUB

SUB InitSetList STATIC
    OPEN _CWD$ + "\Content\Stats\SetList.csv" FOR INPUT AS #1
    FOR i = 1 TO setCount
        FOR k = 1 TO 7
            INPUT #1, SetListBooleans(k, i) 'BOOL FIRST SET NEXT, (1,1) = BOOL,SET
        NEXT
        INPUT #1, SetListArray(i).HP
        INPUT #1, SetListArray(i).ATK
        INPUT #1, SetListArray(i).DE
        INPUT #1, SetListArray(i).LUK
        INPUT #1, SetListArray(i).FlavTXT
    NEXT
    CLOSE #1
END SUB

SUB InitModifiers STATIC
    OPEN _CWD$ + "\Content\Stats\ModifierList.csv" FOR INPUT AS #1
    FOR i = 1 TO totalModifiers
        Modifiers(i).NumID = i
        INPUT #1, Modifiers(i).Name, Modifiers(i).HPPerc, Modifiers(i).ATKPerc, Modifiers(i).DEPerc, Modifiers(i).LUKPerc
    NEXT
    CLOSE #1
    OPEN _CWD$ + "\Content\Stats\ModifierChances.csv" FOR INPUT AS #1
    FOR i = 1 TO totalModifiersChance
        FOR k = 1 TO totalModifiers
            INPUT #1, ModifiersChances(i, k) 'inputs 1,1-1,2-2,1-2,2-3,1-3,2- makes a 3x2. if i = 1 then its modifier chance list 1 and k is where in that we are
        NEXT
    NEXT
    CLOSE #1


END SUB
SUB initQuest STATIC
    OPEN _CWD$ + "\Content\Stats\QList.csv" FOR INPUT AS #1
    FOR i = 1 TO qCount
        QList(i).NumID = i
        INPUT #1, QList(i).Name, QList(i).Type, QList(i).monitemID, QList(i).killCollect, QList(i).qRequired
    NEXT
    CLOSE #1
    OPEN _CWD$ + "\Content\Stats\" + pN$ + "QList.txt" FOR INPUT AS #1 'QLIST.CSV AND .TXT ARE DIFFERENT I PROBABLY SHOULDVE NAMED THEM DIFFERENTLYZZZZ
    QL = 0
    DO
        QL = QL + 1
        INPUT #1, QList(QL).completed
    LOOP UNTIL EOF(1)
    FOR i = QL TO (qCount - 1)
        QList(QL + 1).completed = 0
    NEXT
    CLOSE #1


    OPEN _CWD$ + "\Content\Stats\QText.csv" FOR INPUT AS #1
    FOR i = 1 TO qli
        INPUT #1, QText(i, 1), QText(i, 2), QText(i, 3)
    NEXT
    CLOSE #1

    OPEN _CWD$ + "\Content\Stats\" + pN$ + "CurQuest.txt" FOR INPUT AS #1
    INPUT #1, cqst
    IF cqst > 0 THEN
        curQuest = QList(cqst)
        IF curQuest.Type = 1 OR curQuest.Type = 2 THEN
            INPUT #1, cqkc
            curQuest.killCollect = cqkc
        END IF
        IF curQuest.Type = 3 THEN
            INPUT #1, curQuest.mapStart
        END IF
    ELSE
        curQuest.Name = "No Quest"
        curQuest.Type = 0
        curQuest.monitemID = 0
        curQuest.killCollect = 0
    END IF
    CLOSE #1

END SUB

SUB ItemSets STATIC
    tempHP = 0
    tempATK = 0
    tempDE = 0
    tempLUK = 0
    ts = 0
    FOR i = 1 TO 7
        IF EquipList(i).set > 0 THEN ts = ts + 1
    NEXT
    REDIM totset(ts) AS INTEGER
    tscount = 1
    FOR i = 1 TO 7
        IF EquipList(i).set > 0 THEN
            totset(tscount) = EquipList(i).set
            tscount = tscount + 1
        END IF
    NEXT

    REDIM diffsetp1(7) AS INTEGER
    diffsetcount = 1
    FOR i = 1 TO tscount - 1
        FOR k = 1 TO 7 'UBOUND DIFFSETP
            IF totset(i) = diffsetp1(k) THEN

                GOTO Breaker
            END IF
        NEXT
        diffsetp1(diffsetcount) = totset(i)

        diffsetcount = diffsetcount + 1
        Breaker:
    NEXT



    actdiffset = 0

    FOR i = 1 TO 7
        IF diffsetp1(i) > 0 THEN actdiffset = actdiffset + 1
    NEXT

    REDIM DiffSetArr(actdiffset) AS INTEGER
    'PROBLEM IS BELOW THIS ALL ABOVE IS CORRECT
    'SETUP DIFFSETARR
    counter = 1
    FOR i = 1 TO diffsetcount - 1
        FOR k = 1 TO UBOUND(DiffSetArr)
            IF diffsetp1(i) = DiffSetArr(k) THEN
                GOTO Breakerr
            END IF
        NEXT
        DiffSetArr(counter) = diffsetp1(i)
        counter = counter + 1
        Breakerr:
    NEXT



    FOR i = 1 TO UBOUND(DiffSetArr)
        totalItems = 0
        equipTotal = 0

        FOR k = 1 TO 7
            IF SetListBooleans(k, DiffSetArr(i)) = 1 THEN 'BOOL THEN SET NUMBER
                totalItems = totalItems + 1
                IF EquipList(k).set = DiffSetArr(i) THEN equipTotal = equipTotal + 1
            END IF

        NEXT

        IF totalItems = equipTotal THEN
            tempHP = tempHP + SetListArray(DiffSetArr(i)).HP
            tempATK = tempATK + SetListArray(DiffSetArr(i)).ATK
            tempDE = tempDE + SetListArray(DiffSetArr(i)).DE
            tempLUK = tempLUK + SetListArray(DiffSetArr(i)).LUK
        END IF
    NEXT

    Hero.HP = Hero.HP + tempHP
    Hero.ATK = Hero.ATK + tempATK
    Hero.DE = Hero.DE + tempDE
    Hero.LUK = Hero.LUK + tempLUK

END SUB


SUB InitAllItemsModifiers STATIC
    'eqlist
    FOR i = 1 TO 7
        IF EquipList(i).NumID > 0 THEN
            EquipList(i).Name = RTRIM$(EquipList(i).Modifier.Name) + " " + EquipList(i).Name
            EquipList(i).HP = EquipList(i).HP * EquipList(i).Modifier.HPPerc
            EquipList(i).ATK = EquipList(i).ATK * EquipList(i).Modifier.ATKPerc
            EquipList(i).DE = EquipList(i).DE * EquipList(i).Modifier.DEPerc
            EquipList(i).LUK = EquipList(i).LUK * EquipList(i).Modifier.LUKPerc
        END IF
    NEXT
    'inv
    FOR i = 1 TO UBOUND(Inventory)
        IF Inventory(i).NumID > 0 THEN
            Inventory(i).Name = RTRIM$(Inventory(i).Modifier.Name) + " " + Inventory(i).Name
            Inventory(i).HP = Inventory(i).HP * Inventory(i).Modifier.HPPerc
            Inventory(i).ATK = Inventory(i).ATK * Inventory(i).Modifier.ATKPerc
            Inventory(i).DE = Inventory(i).DE * Inventory(i).Modifier.DEPerc
            Inventory(i).LUK = Inventory(i).LUK * Inventory(i).Modifier.LUKPerc
        END IF
    NEXT
    'new sub for when an item drops
END SUB

SUB GiveModifiers (whereInInv) STATIC
    FOR i = 1 TO totalModifiers
        ModChoose = INT(RND * 101) + 1
        IF ModChoose < ModifiersChances(Inventory(whereInInv).ModifierChance, i) THEN Inventory(whereInInv).Modifier.NumID = i: EXIT FOR
    NEXT
    IF Inventory(whereInInv).Modifier.NumID = 0 THEN Inventory(whereInInv).Modifier.NumID = 1
    SetupModifierStats (whereInInv)
END SUB
'ModifierChances(totalModifiersChance, totalModifiers)
SUB SetupModifierStats (whereInInv) STATIC
    Inventory(whereInInv).Modifier.Name = Modifiers(Inventory(whereInInv).Modifier.NumID).Name
    Inventory(whereInInv).Modifier.HPPerc = Modifiers(Inventory(whereInInv).Modifier.NumID).HPPerc
    Inventory(whereInInv).Modifier.ATKPerc = Modifiers(Inventory(whereInInv).Modifier.NumID).ATKPerc
    Inventory(whereInInv).Modifier.DEPerc = Modifiers(Inventory(whereInInv).Modifier.NumID).DEPerc
    Inventory(whereInInv).Modifier.LUKPerc = Modifiers(Inventory(whereInInv).Modifier.NumID).LUKPerc
END SUB
SUB ApplyModifiers (whereInInv) STATIC

    Inventory(whereInInv).Name = RTRIM$(Inventory(whereInInv).Modifier.Name) + " " + Inventory(whereInInv).Name
    Inventory(whereInInv).HP = Inventory(whereInInv).HP * Inventory(whereInInv).Modifier.HPPerc
    Inventory(whereInInv).ATK = Inventory(whereInInv).ATK * Inventory(whereInInv).Modifier.ATKPerc
    Inventory(whereInInv).DE = Inventory(whereInInv).DE * Inventory(whereInInv).Modifier.DEPerc
    Inventory(whereInInv).LUK = Inventory(whereInInv).LUK * Inventory(whereInInv).Modifier.LUKPerc
END SUB

'pretty deep in the shithole that is bad programming
'man this thing actually sucks but i guess its good enough
'i think later im going to just make this item setup work off of the modifiers array
'i dont know why i did it this way nowi just started and ic ant ogv back
'man programming is bad






