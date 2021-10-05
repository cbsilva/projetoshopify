/******************************************************************************
 *              Definicao de temp-tables para customer API
 ******************************************************************************/
DEFINE TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

DEFINE TEMP-TABLE ttRetorno 
    FIELD chave       AS CHARACTER
    FIELD situacao    AS CHARACTER 
    FIELD descricao   AS CHARACTER FORMAT "x(200)".
