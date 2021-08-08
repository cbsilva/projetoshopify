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
    .

DEFINE TEMP-TABLE tt-emitente NO-UNDO LIKE emitente
    FIELD r-rowid AS ROWID.
