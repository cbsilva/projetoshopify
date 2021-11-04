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

/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM pRowid AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAM pErro  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAM pChave AS CHARACTER NO-UNDO.


/******************************* Main Block **************************************************/

//OUTPUT TO VALUE ("\\192.168.0.131\datasul\Teste\ERP\quarentena\Spf\logIntegracao\esint023.txt").

/* MESSAGE "inciando programa recuperacao json" VIEW-AS ALERT-BOX. */
//LOG-MANAGER:WRITE-MESSAGE("esspf023.p - inciando programa recuperacao json").

FOR FIRST es-api-import-spf NO-LOCK
    WHERE ROWID(es-api-import-spf) = pRowid:
END.

IF NOT AVAIL es-api-import-spf THEN
DO:
    ASSIGN pErro = "Registro n∆o encontrado.".
    RETURN "NOK":U.
END.

FIX-CODEPAGE(cLongJson) = "UTF-8".

//MESSAGE "COPIANDO O OBJETO PARA VARIAVEL DO TIPO MEMPTR".

COPY-LOB es-api-import-spf.c-json TO mJson.  
COPY-LOB mJson TO cLongJson NO-CONVERT.  

//MESSAGE "CRIANDO UM OBJETO DO TIPO MODEL PARSER".

myParser = NEW ObjectModelParser().                              
pJsonInput = CAST(myParser:Parse(cLongJson),JsonObject).         
oJsonArrayMain = pJsonInput:GetJsonArray("payments":U).  

//LOG-MANAGER:WRITE-MESSAGE("esspf023.p - CRIANDO UM OBJETO DO TIPO MODEL PARSER").

DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

    EMPTY TEMP-TABLE ttPayments. /* bhfs */
    CREATE ttPayments.

    oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

  //  MESSAGE "CHAMANDO ROTINA PARA CRIAR OS DADOS NA TEMP-TABLE".

    //LOG-MANAGER:WRITE-MESSAGE("esspf023.p - CHAMANDO ROTINA PARA CRIAR OS DADOS NA TEMP-TABLE").

    RUN pi-criaTTPayments.

END.

//LOG-MANAGER:WRITE-MESSAGE("esspf023.p - NOT TEMP-TABLE ttPayments:HAS-RECORDS " + STRING(NOT TEMP-TABLE ttPayments:HAS-RECORDS) ).

IF NOT TEMP-TABLE ttPayments:HAS-RECORDS THEN
DO:
    ASSIGN pErro = "ERRO: N∆o encontrado titulo no arquivo importado.".
    RETURN "NOK":U.
END.
ELSE
DO:

/*
        MESSAGE "CHAMADO PROGRAM PARA CRIAR REGISTRO NO BANCO".
		/* MUDAR*/

        MESSAGE "Info recebidas:" SKIP
                ttPayments.cpfCnpj      SKIP
                ttPayments.nrPedido     SKIP
                ttPayments.vlPedido     SKIP
                ttPayments.dtPedido     SKIP
                ttPayments.dtPagamento	SKIP(1).
*/                

        /*
        RUN esp\esspf023a-02.p.
        
        MESSAGE "Retorno: " RETURN-VALUE.
        
        RETURN RETURN-VALUE.
        */
        
        //LOG-MANAGER:WRITE-MESSAGE("esspf023.p - Chamando 23a -  Pedido: " + ttPayments.nrPedido ).


            
        RUN esp/esspf023a.p (INPUT TABLE ttPayments,
                             OUTPUT TABLE RowErrors).
        
        //LOG-MANAGER:WRITE-MESSAGE("esspf023.p - Retornando da 23a - Pedido: " + ttPayments.nrPedido + ". H† erros?" + STRING(CAN-FIND(FIRST RowErrors NO-LOCK)) ).

        IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN
        DO:
            FOR EACH RowErrors NO-LOCK:
                ASSIGN pErro = pErro + " " + RowErrors.ErrorDescription.
            END.

/*
            IF ttPayments.cpfCnpj  = "61793907000140" THEN
                MESSAGE pErro.
*/
            RETURN "NOK":U.
        END.
        ELSE DO:

//            MESSAGE "Sucesso - fim esspf023".

            RETURN "OK".
        END.
END.


PROCEDURE pi-criaTTPayments:

    IF oJsonObjectMain:Has(TRIM("customerCNPJ"))    THEN ASSIGN ttPayments.cpfCnpj      = oJsonObjectMain:GetCharacter(TRIM("customerCNPJ"))    NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("orderNumber"))     THEN ASSIGN ttPayments.nrPedido 	= oJsonObjectMain:GetCharacter(TRIM("orderNumber"))	    NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("orderValue")) 	    THEN ASSIGN ttPayments.vlPedido 	= oJsonObjectMain:GetDecimal(TRIM("orderValue")) 	    NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("orderDate")) 	    THEN ASSIGN ttPayments.dtPedido     = oJsonObjectMain:GetCharacter(TRIM("orderDate")) 	    NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("paymentDate"))	    THEN ASSIGN ttPayments.dtPagamento  = oJsonObjectMain:GetCharacter(TRIM("paymentDate"))     NO-ERROR.    

END PROCEDURE.


