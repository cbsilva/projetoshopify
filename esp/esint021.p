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
{esp/esint021.i}
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

DEFINE INPUT PARAM pRowid  AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAM pErro  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAM pChave AS CHARACTER NO-UNDO.


/******************************* Main Block **************************************************/


FOR FIRST es-api-import NO-LOCK
    WHERE ROWID(es-api-import) = pRowid:
END.

IF NOT AVAIL es-api-import THEN
DO:
    ASSIGN pErro = "Registro n∆o encontrado.".
    RETURN "NOK":U.
END.


FIX-CODEPAGE(cLongJson) = "UTF-8".


COPY-LOB es-api-import.c-json TO mJson.  
COPY-LOB mJson TO cLongJson NO-CONVERT.  

myParser = NEW ObjectModelParser().                              
pJsonInput = CAST(myParser:Parse(cLongJson),JsonObject).         
oJsonArrayMain = pJsonInput:GetJsonArray("customer":U).  


DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

    CREATE ttCustomer.

    oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).

    RUN pi-criaEmitente.
    RUN pi-criaEndereco.

END.

IF NOT TEMP-TABLE ttCustomer:HAS-RECORDS THEN
    ASSIGN pErro = "TI - N∆o existem registros para serem processados".
ELSE
DO:
        RUN esp/esint021a.p (INPUT TABLE ttCustomer, 
                             INPUT TABLE ttEnderecoList,
                             OUTPUT TABLE RowErrors).


        IF CAN-FIND(FIRST RowErrors NO-LOCK) THEN
        DO:
            FOR EACH RowErrors NO-LOCK:
                ASSIGN pErro = pErro + " " + RowErrors.ErrorDescription.
            END.
        END.

END.



PROCEDURE pi-criaEmitente:

    IF oJsonObjectMain:Has(TRIM("RazaoSocial        "))  THEN ASSIGN     ttCustomer.RazaoSocial         = oJsonObjectMain:GetCharacter(TRIM("RazaoSocial       ")) NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("CNPJ               "))  THEN ASSIGN     ttCustomer.CNPJ                = oJsonObjectMain:GetCharacter(TRIM("CNPJ              ")) NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("IE                 "))  THEN ASSIGN     ttCustomer.IE                  = oJsonObjectMain:GetCharacter(TRIM("IE                ")) NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("Email              "))  THEN ASSIGN     ttCustomer.Email               = oJsonObjectMain:GetCharacter(TRIM("Email             ")) NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("Telefone           "))  THEN ASSIGN     ttCustomer.Telefone            = oJsonObjectMain:GetCharacter(TRIM("Telefone          ")) NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("GrupoCliente       "))  THEN ASSIGN     ttCustomer.GrupoCliente        = oJsonObjectMain:GetInteger  (TRIM("GrupoCliente      ")) NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("EmailXML           "))  THEN ASSIGN     ttCustomer.EmailXML            = oJsonObjectMain:GetCharacter(TRIM("EmailXML          ")) NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("TelefoneFinanceiro "))  THEN ASSIGN     ttCustomer.TelefoneFinanceiro  = oJsonObjectMain:GetCharacter(TRIM("TelefoneFinanceiro")) NO-ERROR. 

    /*--- campos nao existentes no shopify, mas necessarios para API ---*/
    /*-- deverao ser incluidos posteriormente na tela de parametros ---
    ASSIGN ttCustomer.LimiteCredito     = 9999999.99
           ttCustomer.AvalicaoCredito   = 1
           ttCustomer.TipoCredito       = 1
           ttCustomer.DataLimiteCredito = 12/31/9999.*/

END PROCEDURE.



PROCEDURE pi-criaEndereco:

    IF oJsonObjectMain:Has("EnderecoList") THEN DO:                                                                                                                                       
        oJsonArraySec = oJsonObjectMain:GetJsonArray("EnderecoList").                                                                                                                     
                                                                                                                                                                                            
        DO iCountSec = 1 TO oJsonArraySec:LENGTH:                                                                                                                                           
            oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec).                                                                                                                       
                                                                                                                                                                                            
            CREATE ttEnderecoList.                                                                                                                                                      
                                                                                                                                                                                            
            IF oJsonObjectSec:Has(TRIM("Bairro      ")) THEN ASSIGN ttEnderecoList.Bairro        = oJsonObjectSec:GetCharacter (TRIM("Bairro       "))  NO-ERROR.  
            IF oJsonObjectSec:Has(TRIM("Cep         ")) THEN ASSIGN ttEnderecoList.Cep           = oJsonObjectSec:GetCharacter (TRIM("Cep          "))  NO-ERROR.  
            IF oJsonObjectSec:Has(TRIM("Estado      ")) THEN ASSIGN ttEnderecoList.Estado        = oJsonObjectSec:GetCharacter (TRIM("Estado       "))  NO-ERROR.  
            IF oJsonObjectSec:Has(TRIM("Cidade      ")) THEN ASSIGN ttEnderecoList.Cidade        = oJsonObjectSec:GetCharacter (TRIM("Cidade       "))  NO-ERROR.  
            IF oJsonObjectSec:Has(TRIM("Pais        ")) THEN ASSIGN ttEnderecoList.Pais          = oJsonObjectSec:GetCharacter (TRIM("Pais         "))  NO-ERROR.  
            IF oJsonObjectSec:Has(TRIM("Complemento ")) THEN ASSIGN ttEnderecoList.Complemento   = oJsonObjectSec:GetCharacter (TRIM("Complemento  "))  NO-ERROR.  
            IF oJsonObjectSec:Has(TRIM("Logradouro  ")) THEN ASSIGN ttEnderecoList.Logradouro    = oJsonObjectSec:GetCharacter (TRIM("Logradouro   "))  NO-ERROR.  
            IF oJsonObjectSec:Has(TRIM("Padrao      ")) THEN ASSIGN ttEnderecoList.Padrao        = oJsonObjectSec:GetCharacter (TRIM("Padrao       ")) NO-ERROR.  
        END.                                                                                                                                                                               
                                                                                                                                                                                            
    END.    

END PROCEDURE.
