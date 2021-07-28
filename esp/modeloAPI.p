/*----------------------------------------------------------------------------------------------/
 Programa..: esint0021.p
 Objetivo..: Interface Integraá∆o Clientes SHOPIFY - Importaá∆o
 Data......: 27/07/2021
 Autor.....: 4Make Consultoria
 Vers∆o....: 2.000.001
-----------------------------------------------------------------------------------------------*/


/******************************* Definitions **************************************************/
/* --------------------------------------------------------------------------------------------
    Import Objects - JSON Definitions
----------------------------------------------------------------------------------------------*/
USING PROGRESS.Json.ObjectModel.*



/* --------------------------------------------------------------------------------------------
    Global  Variable Definitions
----------------------------------------------------------------------------------------------*/



/* --------------------------------------------------------------------------------------------
    Local Variable Definitions
----------------------------------------------------------------------------------------------*/
DEFINE VARIABLE cLongJson        AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lRetJson         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE body             AS JsonObject NO-UNDO.
DEFINE VARIABLE jsonOutput       AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObject      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectMain  AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectSec   AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain   AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonArraySec    AS JsonArray  NO-UNDO.
DEFINE VARIABLE iCountMain       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCountSec        AS INTEGER    NO-UNDO.

/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/





/******************************* Main Block **************************************************/
