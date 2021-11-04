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
{esp/esspf021.i}
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
    Functions
----------------------------------------------------------------------------------------------*/

FUNCTION fnRemoveCaracter RETURN CHARACTER (INPUT pValor AS CHAR):
    DEFINE VARIABLE cReturnValor AS CHARACTER   NO-UNDO.

    ASSIGN cReturnValor = REPLACE(pValor,"/","")
           cReturnValor = REPLACE(cReturnValor,".","")
           cReturnValor = REPLACE(cReturnValor,"-","")
           cReturnValor = REPLACE(cReturnValor,"\","")
           cReturnValor = REPLACE(cReturnValor,",","")
           cReturnValor = REPLACE(cReturnValor,"_","").

    RETURN cReturnValor.
END.

/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM pRowid AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAM pErro  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAM pChave AS CHARACTER NO-UNDO.


/******************************* Main Block **************************************************/


LOG-MANAGER:WRITE-MESSAGE(">> INCIANDO A TRANSFORMAÄ«O DO JSON PARA TEMP-TABLE").

FOR FIRST es-api-import-spf NO-LOCK
    WHERE ROWID(es-api-import-spf) = pRowid:
END.

IF NOT AVAIL es-api-import-spf THEN
DO:
    ASSIGN pErro = "Registro n∆o encontrado.".
    RETURN "NOK":U.
END.


FIX-CODEPAGE(cLongJson) = "UTF-8".

LOG-MANAGER:WRITE-MESSAGE("COPIANDO O OBJETO PARA VARIAVEL DO TIPO MEMPTR").

COPY-LOB es-api-import-spf.c-json TO mJson.  
COPY-LOB mJson TO cLongJson NO-CONVERT.  

IF ERROR-STATUS:ERROR THEN
DO:
    LOG-MANAGER:WRITE-MESSAGE("ERRO AO RECUPERAR VARIAVEIS " + RETURN-VALUE).
    ASSIGN pErro = "TI - Erro Interno ao criar cliente " + RETURN-VALUE.
    RETURN "NOK":U.
END.



LOG-MANAGER:WRITE-MESSAGE("CRIANDO UM OBJETO DO TIPO MODEL PARSER").


myParser = NEW ObjectModelParser().                              
pJsonInput = CAST(myParser:Parse(cLongJson),JsonObject).         
oJsonArrayMain = pJsonInput:GetJsonArray("customer":U).  


DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

    CREATE ttCustomer.

    oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

    IF ERROR-STATUS:ERROR THEN
    DO:
        LOG-MANAGER:WRITE-MESSAGE("ERRO AO RECUPERAR VARIAVEIS " + RETURN-VALUE).
        ASSIGN pErro = "TI - Erro Interno ao criar cliente " + RETURN-VALUE.
        RETURN "NOK":U.
    END.
    ELSE
    DO:

        LOG-MANAGER:WRITE-MESSAGE("CHAMANDO ROTINA PARA CRIAR OS DADOS NA TEMP-TABLE").
    
        RUN pi-criaTTEmitente.

    END.


END.

IF NOT TEMP-TABLE ttCustomer:HAS-RECORDS THEN
DO:
    ASSIGN pErro = "ERRO: N∆o encontrado emitente no arquivo importado.".
    RETURN "NOK":U.
END.
ELSE
DO:

    FIND FIRST ttCustomer NO-ERROR.
    IF AVAIL ttCustomer AND (ttCustomer.CNPJ <> "") THEN
    DO:
        
        RUN esp/esspf021a.p (INPUT TABLE ttCustomer,
                             OUTPUT TABLE RowErrors).


        IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN
        DO:
            FOR EACH RowErrors NO-LOCK:
                ASSIGN pErro = pErro + " " + RowErrors.ErrorDescription.
            END.
            RETURN "NOK":U.
        END.

    END.
    ELSE
    DO:
         ASSIGN pErro = "TI - Ocorreu erro durante a transformaá∆o dos dados".
         RETURN "NOK":U.
    END.
END.



PROCEDURE pi-criaTTEmitente:

    IF oJsonObjectMain:Has(TRIM("SocialReason   ")) THEN ASSIGN ttCustomer.RazaoSocial     = oJsonObjectMain:GetCharacter(TRIM("SocialReason  "))                     NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("customerId     ")) THEN ASSIGN ttCustomer.CNPJ            = fnRemoveCaracter(oJsonObjectMain:GetCharacter(TRIM("customerId    ")))   NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("IE             ")) THEN ASSIGN ttCustomer.IE              = oJsonObjectMain:GetCharacter(TRIM("IE            "))                     NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("Email          ")) THEN ASSIGN ttCustomer.Email           = oJsonObjectMain:GetCharacter(TRIM("Email         "))                     NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("Phone          ")) THEN ASSIGN ttCustomer.Telefone        = oJsonObjectMain:GetCharacter(TRIM("Phone         "))                     NO-ERROR.    
    IF oJsonObjectMain:Has(TRIM("InscMunicipal  ")) THEN ASSIGN ttCustomer.InscMunicipal   = oJsonObjectMain:GetCharacter(TRIM("InscMunicipal "))                     NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Address        ")) THEN ASSIGN ttCustomer.Endereco        = oJsonObjectMain:GetCharacter (TRIM("Address      "))                     NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Neighborhood   ")) THEN ASSIGN ttCustomer.Bairro          = oJsonObjectMain:GetCharacter (TRIM("Neighborhood "))                     NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Zip            ")) THEN ASSIGN ttCustomer.Cep             = fnRemoveCaracter(oJsonObjectMain:GetCharacter(TRIM("Zip")))              NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("State          ")) THEN ASSIGN ttCustomer.Estado          = oJsonObjectMain:GetCharacter (TRIM("State        "))                     NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("City           ")) THEN ASSIGN ttCustomer.Cidade          = oJsonObjectMain:GetCharacter (TRIM("City         "))                     NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Country        ")) THEN ASSIGN ttCustomer.Pais            = oJsonObjectMain:GetCharacter (TRIM("Country      "))                     NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Complement     ")) THEN ASSIGN ttCustomer.Complemento     = oJsonObjectMain:GetCharacter (TRIM("Complement   "))                     NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("shopifyId      ")) THEN ASSIGN ttCustomer.ShopifyId       = oJsonObjectMain:GetCharacter (TRIM("shopifyId    "))                     NO-ERROR.
                                                                                                                                                                     
                                                                                                                                                                     
                                                                                                                                                                     



END PROCEDURE.


