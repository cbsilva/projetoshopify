/*----------------------------------------------------------------------------------------------/
 Programa..: esint0021.i
 Objetivo..: Include com Definiá∆o das Tabelas Tempor†rias SHOPIFY
 Data......: 27/07/2021
 Autor.....: 4Make Consultoria
 Vers∆o....: 2.000.001
-----------------------------------------------------------------------------------------------*/


/*************************** Temp-Table Definitions ********************************************/

DEFINE TEMP-TABLE ttCustomer NO-UNDO SERIALIZE-NAME "Customer"
    FIELD RazaoSocial      AS CHARACTER 
    FIELD CNPJ             AS CHARACTER 
    FIELD IE               AS CHARACTER 
    FIELD Email            AS CHARACTER 
    FIELD Telefone         AS CHARACTER 
    FIELD InscMunicipal    AS CHARACTER
    FIELD Endereco         AS CHARACTER
    FIELD Pais             AS CHARACTER
    FIELD Complemento      AS CHARACTER
    FIELD Cidade           AS CHARACTER
    FIELD Bairro           AS CHARACTER
    FIELD Estado           AS CHARACTER
    FIELD Cep              AS CHARACTER
    FIELD padrao           AS CHARACTER  
    FIELD shopifyId        AS CHARACTER.
    .

DEFINE TEMP-TABLE tt-emitente NO-UNDO LIKE emitente
    FIELD r-rowid AS ROWID.


DEFINE TEMP-TABLE tt-erro  NO-UNDO
   FIELD cd-erro  AS INTEGER
   FIELD mensagem AS CHARACTER.

DEFINE TEMP-TABLE tt_erros_conexao no-undo
   field i-sequen as int             
   field cd-erro  as int
   field mensagem as char format "x(255)"
   field param-1 as char
   field param-2 as char
   field param-3 as char
   field param-4 as char
   field param-5 as char
   field param-6 as char
   field param-7 as char
   field param-8 as char
   field param-9 as char.

