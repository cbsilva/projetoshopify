/*----------------------------------------------------------------------------------------------/
 Programa..: esint0021.i
 Objetivo..: Include com Definiá∆o das Tabelas Tempor†rias SHOPIFY
 Data......: 27/07/2021
 Autor.....: 4Make Consultoria
 Vers∆o....: 2.000.001
-----------------------------------------------------------------------------------------------*/


/*************************** Temp-Table Definitions ********************************************/

DEFINE TEMP-TABLE ttCustomer NO-UNDO SERIALIZE-NAME "Customer"
    FIELD RazaoSocial                   AS CHARACTER 
    FIELD CNPJ                          AS CHARACTER 
    FIELD IE                            AS CHARACTER 
    FIELD Email                         AS CHARACTER 
    FIELD Telefone                      AS CHARACTER 
    FIELD CodigoCliente                 AS INTEGER   
    FIELD GrupoCliente                  AS INTEGER 
    FIELD EmailXML                      AS CHARACTER 
    FIELD EmailFinanceiro               AS CHARACTER 
    FIELD TelefoneFinanceiro            AS CHARACTER 
    FIELD RegimeTributario              AS LOGICAL    INITIAL NO 
    FIELD ContribuinteICMS              AS LOGICAL    INITIAL NO
    FIELD Suframa                       AS CHARACTER 
    FIELD DataValidadeSuframa           AS DATE       INITIAL ?   
    FIELD NomeAbreviado                 AS CHARACTER 
    FIELD LimiteCredito                 AS INTEGER    INITIAL 9999999.99
    FIELD AvaliacaoCredito              AS INTEGER    INITIAL 1
    FIELD TipoCredito                   AS INTEGER    INITIAL 1
    FIELD DataLimiteCredito             AS DATE       INITIAL 12/31/9999.



DEFINE TEMP-TABLE ttContactList NO-UNDO SERIALIZE-NAME "ContactList"
    FIELD CodigoContato   AS INTEGER
    FIELD Sobrenome       AS CHARACTER
    FIELD AreaContato     AS CHARACTER
    FIELD Cargo           AS CHARACTER
    FIELD Email           AS CHARACTER
    FIELD Telefone        AS CHARACTER
    FIELD Aplicacao       AS CHARACTER
    FIELD Descricao       AS CHARACTER
    FIELD CNPJ_CPF        AS CHARACTER
    FIELD Fax             AS CHARACTER
    FIELD Ramal           AS CHARACTER
    FIELD RamalFAX        AS CHARACTER.



DEFINE TEMP-TABLE ttEnderecoList NO-UNDO SERIALIZE-NAME "EnderecoList"
    FIELD codEntrega    AS CHARACTER
    field Pais          AS CHARACTER
    field Logradouro    AS CHARACTER
    field Complemento   AS CHARACTER
    field Cidade        AS CHARACTER
    field Bairro        AS CHARACTER
    field Estado        AS CHARACTER
    field Cep           AS CHARACTER
    FIELD padrao        AS CHARACTER  
    .

DEFINE TEMP-TABLE tt-emitente NO-UNDO LIKE emitente
    FIELD r-rowid AS ROWID.
