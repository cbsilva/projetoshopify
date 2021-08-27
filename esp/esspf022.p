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
USING PROGRESS.Json.ObjectModel.*.

/* --------------------------------------------------------------------------------------------
    Temp-Tables Definitions
----------------------------------------------------------------------------------------------*/
{esp/esspf022.i}
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

OUTPUT TO VALUE ("\\192.168.0.131\datasul\Teste\ERP\quarentena\spf\logIntegracao\esspf022.log").

MESSAGE "inciando programa recuperacao json".

FOR FIRST es-api-import-spf NO-LOCK
    WHERE ROWID(es-api-import-spf) = pRowid:
END.

IF NOT AVAIL es-api-import-spf THEN
DO:
    ASSIGN pErro = "Registro n∆o encontrado.".
    RETURN "NOK":U.
END.


FIX-CODEPAGE(cLongJson) = "UTF-8".

MESSAGE "COPIANDO O OBJETO PARA VARIAVEL DO TIPO MEMPTR".

COPY-LOB es-api-import-spf.c-json TO mJson.  
COPY-LOB mJson TO cLongJson NO-CONVERT.  

MESSAGE "CRIANDO UM OBJETO DO TIPO MODEL PARSER".


myParser = NEW ObjectModelParser().                              
pJsonInput = CAST(myParser:Parse(cLongJson),JsonObject).         
oJsonArrayMain = pJsonInput:GetJsonArray("order":U).  


DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

   CREATE ttPedido.

   MESSAGE 'CRIANDO TT-PEDIDO'.

   oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

   if oJsonObjectMain:Has("customerCNPJ") THEN ASSIGN ttPedido.cnpjEmitente = oJsonObjectMain:GetCharacter(TRIM("cnpjEmitente"))  NO-ERROR. 
   if oJsonObjectMain:Has("orderNumber") THEN ASSIGN ttPedido.pedidoCliente = oJsonObjectMain:GetCharacter(TRIM("orderNumber "))  NO-ERROR. 

   IF oJsonObjectMain:Has("ItemOrderList") THEN 
   DO: 
         MESSAGE 'LENDO OS ITENS DO PEDIDO'. 
         oJsonArraySec = oJsonObjectMain:GetJsonArray("ItemOrderList").
         
         DO iCountSec = 1 TO oJsonArraySec:LENGTH:
            oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec).           

            CREATE ttItensPedido.
            ASSIGN ttItensPedido.nrSeqPed = iCountSec.
            if oJsonObjectSec:Has("ItemCodee") THEN ASSIGN	ttItensPedido.codigoItem  = oJsonObjectSec:GetCharacter("ItemCode") NO-ERROR.
            if oJsonObjectSec:Has("Quantity" ) THEN ASSIGN	ttItensPedido.qtdPedida   = oJsonObjectSec:GetInteger("Quantity"  ) NO-ERROR.
            if oJsonObjectSec:Has("Price"    ) THEN ASSIGN	ttItensPedido.precoUnit   = oJsonObjectSec:GetDecimal("Price"     ) NO-ERROR.
         END.  

         /* Rotina de sa°da da validaá∆o */
         IF ERROR-STATUS:ERROR THEN DO:
            pErro = "TI | Ocorreram erros durante o processamento: " + ERROR-STATUS:GET-MESSAGE(1).
            RETURN "NOK".
        END.       

        RETURN "OK".      
                
   END.   

END.

IF NOT TEMP-TABLE ttPedido:HAS-RECORDS THEN
DO:
    ASSIGN pErro = "ERRO: N∆o encontrado emitente no arquivo importado.".
    RETURN "NOK":U.
END.
ELSE
DO:
        MESSAGE "CHAMADO PROGRAM PARA CRIAR REGISTRO NO BANCO".
        RUN esp/esspf022a.p (INPUT TABLE ttPedido,
                             INPUT TABLE ttItensPedido,
                             OUTPUT TABLE RowErrors).


        IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN
        DO:
            FOR EACH RowErrors NO-LOCK:
                ASSIGN pErro = pErro + " " + RowErrors.ErrorDescription.
            END.
            RETURN "NOK":U.
        END.

END.





