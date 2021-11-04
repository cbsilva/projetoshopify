/*----------------------------------------------------------------------------------------------/
 Programa..: esint0023.p
 Objetivo..: Interface Integracao Titulos SHOPIFY - Importacao
 Data......: 08/09/2021
 Autor.....: 4Make Consultoria
 Vers∆o....: 2.000.001
-----------------------------------------------------------------------------------------------*/


/******************************* Definitions **************************************************/
/* --------------------------------------------------------------------------------------------
    Import Objects - JSON Definitions
----------------------------------------------------------------------------------------------*/
USING PROGRESS.Json.ObjectModel.*.

/* --------------------------------------------------------------------------------------------
    Temp-Tables Definitions
----------------------------------------------------------------------------------------------*/
{esp/esspf023.i}
{method/dbotterr.i}

/* --------------------------------------------------------------------------------------------
    Global  Variable Definitions
----------------------------------------------------------------------------------------------*/


/* --------------------------------------------------------------------------------------------
    Local Variable Definitions
----------------------------------------------------------------------------------------------*/
DEFINE VARIABLE cLongJson        AS LONGCHAR          NO-UNDO.
DEFINE VARIABLE lRetJson         AS LOGICAL           NO-UNDO.
DEFINE VARIABLE body             AS JsonObject        NO-UNDO.
DEFINE VARIABLE jsonOutput       AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonObject      AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonObjectMain  AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonObjectSec   AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonArrayMain   AS JsonArray         NO-UNDO.
DEFINE VARIABLE oJsonArraySec    AS JsonArray         NO-UNDO.
DEFINE VARIABLE iCountMain       AS INTEGER           NO-UNDO.
DEFINE VARIABLE iCountSec        AS INTEGER           NO-UNDO.
DEFINE VARIABLE mJson            AS MEMPTR            NO-UNDO.
DEFINE VARIABLE myParser         AS ObjectModelParser NO-UNDO. 
DEFINE VARIABLE pJsonInput       AS JsonObject        NO-UNDO.

DEF VAR pErro AS CHAR.
/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/


/******************************* Main Block **************************************************/

SESSION:DEBUG-ALERT = YES.
LOG-MANAGER:LOGFILE-NAME = "c:\temp\clientlog-BASIC.txt".
LOG-MANAGER:LOGGING-LEVEL = 4.
LOG-MANAGER:LOG-ENTRY-TYPES = "4GLMessages,4GLTrace,DB.Connects,DynObjects.DB,DynObjects.XML,DynObjects.Other,DynObjects.CLASS,DynObjects.UI,FileID,ProEvents.UI.CHAR,ProEvents.UI.COMMAND,ProEvents.Other,SAX".

RUN pi-criaTTPayments.


IF NOT TEMP-TABLE ttPayments:HAS-RECORDS THEN
DO:
    ASSIGN pErro = "ERRO: N∆o encontrado titulo no arquivo importado.".
    RETURN "NOK":U.
END.
ELSE
DO:

        RUN esp/esspf023a.p (INPUT TABLE ttPayments,
                             OUTPUT TABLE RowErrors). 

        IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN
        DO:
            FOR EACH RowErrors NO-LOCK:
                ASSIGN pErro = pErro + " " + RowErrors.ErrorDescription.
            END.

            MESSAGE pErro.
            RETURN "NOK":U.
        END.
        ELSE DO:

            MESSAGE "Sucesso".
        END.
END.


SESSION:DEBUG-ALERT = NO.
LOG-MANAGER:CLOSE-LOG().

PROCEDURE pi-criaTTPayments:

    CREATE ttPayments.
    ASSIGN ttPayments.cpfCnpj      = "61793907000140"
           ttPayments.nrPedido     = "881005"
           ttPayments.vlPedido     = 8810.05
           ttPayments.dtPedido     = "2021-09-30"
           ttPayments.dtPagamento  = "2021-09-30"
    .

    MESSAGE "ALTERNATIVO" SKIP
            ttPayments.cpfCnpj      SKIP
            ttPayments.nrPedido     SKIP
            ttPayments.vlPedido     SKIP
            ttPayments.dtPedido     SKIP
            ttPayments.dtPagamento	SKIP(1)
        VIEW-AS ALERT-BOX .       

END PROCEDURE.
