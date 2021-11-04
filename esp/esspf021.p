/*----------------------------------------------------------------------------------------------/
 Programa..: esint0021.p
 Objetivo..: Interface Integra��o Clientes SHOPIFY - Importa��o
 Data......: 27/07/2021
 Autor.....: 4Make Consultoria
 Vers�o....: 2.000.001
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


LOG-MANAGER:WRITE-MESSAGE(">> INCIANDO A TRANSFORMA��O DO JSON PARA TEMP-TABLE").

FOR FIRST es-api-import-spf NO-LOCK
    WHERE ROWID(es-api-import-spf) = pRowid:
END.

IF NOT AVAIL es-api-import-spf THEN
DO:
    ASSIGN pErro = "Registro n�o encontrado.".
    RETURN "NOK":U.
END.


FIX-CODEPAGE(cLongJson) = "UTF-8".

LOG-MANAGER:WRITE-MESSAGE("COPIANDO O OBJETO PARA VARIAVEL DO TIPO MEMPTR").

COPY-LOB FROM OBJECT  es-api-import-spf.c-json TO mJson.
 
LOG-MANAGER:WRITE-MESSAGE("COPIANDO MEMPTR PARA LONGCHAR").

COPY-LOB FROM OBJECT mJson TO cLongJson CONVERT SOURCE CODEPAGE "iso8859-1" TARGET CODEPAGE "utf-8".  

IF ERROR-STATUS:ERROR OR ERROR-STATUS:NUM-MESSAGES > 0 THEN
DO:
    LOG-MANAGER:WRITE-MESSAGE("ERRO AO RECUPERAR VARIAVEIS " + ERROR-STATUS:GET-MESSAGE(1)).
    ASSIGN pErro = "TI - Erro Interno ao criar cliente " + ERROR-STATUS:GET-MESSAGE(1).
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
        LOG-MANAGER:WRITE-MESSAGE("ERRO AO RECUPERAR VARIAVEIS " + ERROR-STATUS:GET-MESSAGE(1)).
        ASSIGN pErro = "TI - Erro Interno ao criar cliente " + ERROR-STATUS:GET-MESSAGE(1).
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
    ASSIGN pErro = "ERRO: N�o encontrado emitente no arquivo importado.".
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
         ASSIGN pErro = "TI - Ocorreu erro durante a transforma��o dos dados".
         RETURN "NOK":U.
    END.
END.



PROCEDURE pi-criaTTEmitente:

    IF oJsonObjectMain:Has(TRIM("SocialReason   ")) THEN ASSIGN ttCustomer.RazaoSocial     = oJsonObjectMain:GetCharacter(TRIM("SocialReason  "))                     NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("customerId     ")) THEN ASSIGN ttCustomer.CNPJ            = fnRemoveCaracter(oJsonObjectMain:GetCharacter(TRIM("customerId")))       NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("IE             ")) THEN ASSIGN ttCustomer.IE              = oJsonObjectMain:GetCharacter(TRIM("IE            "))                     NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("Email          ")) THEN ASSIGN ttCustomer.Email           = oJsonObjectMain:GetCharacter(TRIM("Email         "))                     NO-ERROR. 
    IF oJsonObjectMain:Has(TRIM("phone          ")) THEN ASSIGN ttCustomer.Telefone        = REPLACE(oJsonObjectMain:GetCharacter(TRIM("phone")),"+55","")            .    
    IF oJsonObjectMain:Has(TRIM("InscMunicipal  ")) THEN ASSIGN ttCustomer.InscMunicipal   = oJsonObjectMain:GetCharacter(TRIM("InscMunicipal "))                     NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Address        ")) THEN ASSIGN ttCustomer.Endereco        = oJsonObjectMain:GetCharacter(TRIM("Address      "))                      NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Neighborhood   ")) THEN ASSIGN ttCustomer.Bairro          = oJsonObjectMain:GetCharacter(TRIM("Neighborhood "))                      NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Zip            ")) THEN ASSIGN ttCustomer.Cep             = fnRemoveCaracter(oJsonObjectMain:GetCharacter(TRIM("Zip")))              NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("State          ")) THEN ASSIGN ttCustomer.Estado          = oJsonObjectMain:GetCharacter(TRIM("State        "))                      NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("City           ")) THEN ASSIGN ttCustomer.Cidade          = oJsonObjectMain:GetCharacter(TRIM("City         "))                      NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Country        ")) THEN ASSIGN ttCustomer.Pais            = REPLACE(oJsonObjectMain:GetCharacter(TRIM("Country")),"BRAZIL","BRASIL") NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("Complement     ")) THEN ASSIGN ttCustomer.Complemento     = oJsonObjectMain:GetCharacter (TRIM("Complement   "))                     NO-ERROR.
    IF oJsonObjectMain:Has(TRIM("shopifyID      ")) THEN ASSIGN ttCustomer.ShopifyId       = STRING(oJsonObjectMain:GetCharacter("shopifyID"))                        NO-ERROR.

    IF ttCustomer.ShopifyId = "" THEN 
        IF oJsonObjectMain:Has(TRIM("shopifyID")) THEN ASSIGN ttCustomer.ShopifyId  = oJsonObjectMain:GetCharacter("shopifyID") NO-ERROR.
                                                                                                                                                                    
   LOG-MANAGER:WRITE-MESSAGE("#####INFORMACOES RECEBIDAS PELO JSON DE CLIENTES ###") NO-ERROR.

   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.RazaoSocial    " + STRING(ttCustomer.RazaoSocial  )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.ShopifyId      " + STRING(ttCustomer.ShopifyId    )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.CNPJ           " + STRING(ttCustomer.CNPJ         )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.IE             " + STRING(ttCustomer.IE           )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.Email          " + STRING(ttCustomer.Email        )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.Telefone       " + STRING(ttCustomer.Telefone     )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.InscMunicipal  " + STRING(ttCustomer.InscMunicipal)) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.Endereco       " + STRING(ttCustomer.Endereco     )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.Complemento    " + STRING(ttCustomer.Complemento  )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.Bairro         " + STRING(ttCustomer.Bairro       )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.Cep            " + STRING(ttCustomer.Cep          )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.Estado         " + STRING(ttCustomer.Estado       )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.Cidade         " + STRING(ttCustomer.Cidade       )) NO-ERROR. 
   LOG-MANAGER:WRITE-MESSAGE("ttCustomer.Pais           " + STRING(ttCustomer.Pais         )) NO-ERROR. 



















                                                                                                                                                                     
                                                                                                                                                                     
                                                                                                                                                                     



END PROCEDURE.


