/******************************************************************************
 *              Definicao de temp-tables para customer API
 ******************************************************************************/

DEFINE TEMP-TABLE ttEmitente NO-UNDO
    FIELD cod-emitente LIKE emitente.cod-emitente SERIALIZE-NAME "CodEmitente"
    FIELD nome-abrev   LIKE emitente.nome-abrev   SERIALIZE-NAME "NomeEmitente".





DEFINE TEMP-TABLE tt-erros
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "x(256)"
    FIELD desc-arq  AS CHARACTER.

DEFINE TEMP-TABLE ttRetorno 
    FIELD chave       AS CHARACTER
    FIELD situacao    AS CHARACTER 
    FIELD descricao   AS CHARACTER FORMAT "x(200)".
