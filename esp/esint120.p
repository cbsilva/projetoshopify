/*
 *------------------------------------------------------------------------------
 *  PROGRAMA        esp/esint120.p
 *  OBJETIVO        Exporta‡Æo altera‡Æo Pedido / Nota fiscal para DATALOGIX
 *  AUTOR           TOTVS - LASF
 *  DATA            04/2021
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *                                DEFINI€åES
 *
 *------------------------------------------------------------------------------
 */
/* ------ Defini?’o das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

{lib/Utilidades.i}

//{include/i-prgvrs.i esint001HE 2.09.00.003} 

/* ------- Defini?’o de Par?metros ----- */
DEFINE INPUT  PARAMETER r-table AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro  AS CHARACTER NO-UNDO.

/* ------- Defini?’o de Variÿveis ----- */
DEFINE VARIABLE oJsonObjMain      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjMain1      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain    AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonObjIni       AS jsonObject NO-UNDO.
DEFINE VARIABLE ojsonArrayIni     AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonObjIni1      AS jsonObject NO-UNDO.
DEFINE VARIABLE ojsonObjAux       AS JsonObject NO-UNDO.
DEFINE VARIABLE ojsonArrayAux     AS JsonArray  NO-UNDO.

DEFINE VARIABLE hComunicacaoAPI AS HANDLE      NO-UNDO.

DEFINE VARIABLE h-temp              AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-temp1             AS HANDLE     NO-UNDO.

DEFINE VARIABLE c-json              AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lEnviou             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-arq-json          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lresp               AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cResposta           AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE cErro               AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE tt-req    NO-UNDO SERIALIZE-NAME "req"  
    FIELD operacao       AS CHAR SERIALIZE-NAME "Operacao" 
    FIELD nota-fiscal    AS CHAR SERIALIZE-NAME "NrNota"
    FIELD serie          AS CHAR SERIALIZE-NAME "Serie"
    FIELD cod-estabel    AS CHAR SERIALIZE-NAME "Estabelecimento"
    FIELD nr-pedido      AS CHAR SERIALIZE-NAME "NrPedido"
    FIELD cnpj           AS CHAR SERIALIZE-NAME "Cliente"
    FIELD data           AS DATETIME SERIALIZE-NAME "Data"
    .

DEF TEMP-TABLE tt-req-it    NO-UNDO SERIALIZE-NAME "ListaProdutos" 
    FIELD it-codigo         AS CHAR SERIALIZE-NAME "CodigoProduto"
    FIELD quantidade        AS DEC  SERIALIZE-NAME "Quantidade"
    .
         
DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.


/*
 *------------------------------------------------------------------------------
 *
 *                                  BLOCO PRINCIPAL
 *
 *------------------------------------------------------------------------------
 */


/*------------------------------ Main Begin ----------------------------*/

ASSIGN c-erro = "".

    oJsonObjMain    = NEW JsonObject().

    FIND FIRST es-api-export NO-LOCK WHERE ROWID(es-api-export) = r-table NO-ERROR.
    IF NOT AVAIL es-api-export THEN DO:
        ASSIGN c-erro = "Registro de Exporta‡Æo nÆo encontrado".
        RETURN "NOK".
    END.
    IF es-api-export.chave = "" THEN DO:
        ASSIGN c-erro = "Registro de Exporta‡Æo sem chave definida".
        RETURN "NOK".
    END.

    FIND FIRST es-api-param WHERE es-api-param.cd-tipo-integr = es-api-export.cd-tipo-integr NO-LOCK NO-ERROR. 

    IF ENTRY(1,es-api-export.chave,"|") = "Faturamento" OR
       ENTRY(1,es-api-export.chave,"|") = "Cancelamento"  THEN DO:

        RUN piGravaTempitensNota(OUTPUT h-temp,
                                 OUTPUT h-temp1,
                                 OUTPUT c-erro).

    END.
    IF ENTRY(1,es-api-export.chave,"|") = "Cancelamento Pedido" OR
       ENTRY(1,es-api-export.chave,"|") = "Altera‡Æo Pedido"  THEN DO:

        RUN piGravaTempitensPedido(OUTPUT h-temp,
                                   OUTPUT h-temp1,
                                   OUTPUT c-erro).

    END.
    IF RETURN-VALUE <> "OK" THEN
    DO:
        ASSIGN c-erro = "Erro ao gerar Itens para envio. " + tratarString(c-erro).
        RETURN "NOK".
    END.
    IF NOT VALID-HANDLE(h-temp)  THEN
    DO:
        ASSIGN c-erro = "Erro ao gerar dados para envio".
        RETURN "NOK".
    END.
    IF valid-handle(h-temp) THEN DO:

        RUN lib/ComunicacaoAPI.p PERSISTENT SET hComunicacaoAPI.

        RUN criarObjetoJSON IN hComunicacaoAPI (INPUT h-temp,
                                                OUTPUT ojsonObjIni,
                                                OUTPUT ojsonArrayIni,
                                                INPUT NO) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-temp.

            ASSIGN c-erro = "Erro ao criar objeto JSON do Cabe‡alho. " + tratarString(c-erro).

            RETURN "NOK".
        END.            
        DELETE OBJECT h-temp.

        RUN criarObjetoJSON IN hComunicacaoAPI (INPUT h-temp1,
                                                OUTPUT ojsonObjIni1,
                                                OUTPUT ojsonArrayIni,
                                                INPUT YES) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            DELETE OBJECT h-temp1.

            ASSIGN c-erro = "Erro ao criar objeto JSON do Item. " + tratarString(c-erro).

            RETURN "NOK".
        END.            
        DELETE OBJECT h-temp1.

    END.
    ojsonObjIni:ADD("ListaProdutos",ojsonArrayIni).

    oJsonArrayMain = NEW JsonArray().
    oJsonArrayMain:ADD(ojsonObjIni).

    /* ----- Cria Json Principal ------- */
    oJsonObjMain = NEW JsonObject().
    oJsonObjMain:ADD("req",oJsonArrayMain).

    /* oJsonObjMain:WriteFile("c:\temp\json-PED-ITEM.json",YES,"UTF-8"). */

    oJsonObjMain:WRITE(c-json,YES,"UTF-8").

    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
        DELETE OBJECT hComunicacaoAPI.

        ASSIGN c-erro = "Erro ao preparar objeto JSON. " + tratarString(c-erro).

        RETURN "NOK".
    END.

    RUN lib/ComunicacaoAPI.p PERSISTENT SET hComunicacaoAPI.
    RUN criarObjetoJSON IN hComunicacaoAPI (INPUT h-temp,
                                            OUTPUT ojsonObjIni,
                                            OUTPUT ojsonArrayIni,
                                            INPUT NO) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
        DELETE OBJECT h-temp.

        ASSIGN c-erro = "Erro ao compor objeto JSON. " + tratarString(c-erro).

        RETURN "NOK".
    END.            
    DELETE OBJECT h-temp.


    /* ------------ Envia Objeto Json --------- */
    RUN RealizarPost IN hComunicacaoAPI (INPUT ROWID(es-api-param),
                                         INPUT oJsonObjMain,
                                         OUTPUT cErro,
                                         OUTPUT cResposta
                                         ).
/*     IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:                                                          */
/*         FOR EACH rowErrors:                                                                               */
/*             ASSIGN c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription. */
/*             DELETE OBJECT hComunicacaoAPI.                                                                */
/*             RETURN "NOK".                                                                                 */
/*         END.                                                                                              */
/*     END.                                                                                                  */
    IF RETURN-VALUE <> "OK" THEN
    DO:
        IF VALID-HANDLE(hComunicacaoAPI) THEN
            DELETE OBJECT hComunicacaoAPI.
        c-erro = cErro.
        
        ASSIGN c-erro = "Erro ao Enviar JSON. " + tratarString(c-erro).

        RETURN "NOK".
    END.


    FIND CURRENT es-api-export EXCLUSIVE-LOCK.
    ASSIGN es-api-export.c-json             = c-Json
           es-api-export.text-retorno       = cResposta.
    FIND CURRENT es-api-export NO-LOCK.
    RELEASE es-api-export.

    
    IF VALID-HANDLE(hComunicacaoAPI) THEN
        DELETE OBJECT hComunicacaoAPI.

/*
 *------------------------------------------------------------------------------
 *
 *                                  PROCEDURES
 *
 *------------------------------------------------------------------------------
 */

PROCEDURE piGravaTempitensNota:

    DEFINE OUTPUT PARAMETER pTemp  AS HANDLE     NO-UNDO.
    DEFINE OUTPUT PARAMETER pTemp1 AS HANDLE     NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro  AS CHARACTER  NO-UNDO.

    DEF VAR pnr-nota-fis   LIKE nota-fiscal.nr-nota-fis  NO-UNDO.    
    DEF VAR pserie         LIKE nota-fiscal.serie        NO-UNDO.  
    DEF VAR pOper          AS CHAR                       NO-UNDO.
    DEF VAR pcod-estabel   LIKE nota-fiscal.cod-estabel  NO-UNDO.    

    ASSIGN  pOper         = ENTRY(1,es-api-export.chave,"|")
            pcod-estabel  = ENTRY(2,es-api-export.chave,"|")
            pserie        = ENTRY(3,es-api-export.chave,"|")
            pnr-nota-fis  = ENTRY(4,es-api-export.chave,"|").

    IF pOper = "Cancelamento" THEN 
       ASSIGN pOper = TRIM(pOper) + " do Faturamento".

    FOR FIRST nota-fiscal NO-LOCK 
        WHERE nota-fiscal.cod-estabel = pcod-estabel  
          AND nota-fiscal.serie       = pserie        
          AND nota-fiscal.nr-nota-fis = pnr-nota-fis:

    END.
    IF NOT AVAIL nota-fiscal THEN DO:

        pErro = SUBSTITUTE("Nota  nÆo encontrado (&1)", es-api-export.chave ).      
        RETURN "NOK".                                                              

    END.

    CREATE tt-req.
    ASSIGN tt-req.operacao     = pOper
           tt-req.nota-fiscal  = nota-fiscal.nr-nota-fis
           tt-req.serie        = nota-fiscal.serie
           tt-req.cod-estabel  = nota-fiscal.cod-estabel
           tt-req.nr-pedido    = nota-fiscal.nr-pedcli
           tt-req.cnpj         = nota-fiscal.cgc
        
           tt-req.data         = es-api-export.data-movto
        .

    FOR EACH it-nota-fisc NO-LOCK 
        WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel 
          AND it-nota-fisc.serie       = nota-fiscal.serie       
          AND it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
          /*,
        EACH ext_item 
        WHERE ext_item.it_codigo =  it-nota-fisc.it-codigo
          AND ext_item.flg_doavel = YES
          */
          :

        CREATE tt-req-it.
        ASSIGN tt-req-it.it-codigo    = it-nota-fisc.it-codigo
               tt-req-it.quantidade   = it-nota-fisc.qt-faturada[1].

    END.
    IF CAN-FIND(FIRST tt-req-it) THEN DO:

        IF TEMP-TABLE tt-req:HAS-RECORDS THEN
            ASSIGN pTemp = BUFFER tt-req:HANDLE.

        IF TEMP-TABLE tt-req-it:HAS-RECORDS THEN
            ASSIGN pTemp1 = BUFFER tt-req-it:HANDLE.

        RETURN "OK".
    END.
    ELSE
    DO:
        ASSIGN pErro = "Nenhum item encontrado".
        RETURN "NOK".    
    END.

END PROCEDURE.

PROCEDURE piGravaTempitensPedido:
    DEFINE OUTPUT PARAMETER pTemp  AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pTemp1 AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER  NO-UNDO.

    DEF VAR pOper          AS CHAR                       NO-UNDO.
    DEF VAR pnomeAbrev     LIKE ped-venda.nome-abrev     NO-UNDO.    
    DEF VAR pnrPedcli      LIKE ped-venda.nr-pedcli      NO-UNDO. 
    DEF VAR pitem          LIKE ped-item.it-codigo       NO-UNDO.
    DEF VAR pseq           LIKE ped-item.nr-sequencia    NO-UNDO.
    DEFINE VARIABLE deQuantidade AS DECIMAL     NO-UNDO.

    ASSIGN  pOper       = ENTRY(1,es-api-export.chave,"|")
            pnomeAbrev  = ENTRY(2,es-api-export.chave,"|")
            pnrPedcli   = ENTRY(3,es-api-export.chave,"|")
            pseq        = INT(ENTRY(4,es-api-export.chave,"|"))
            pitem       = ENTRY(5,es-api-export.chave,"|").

    IF pOper = "Cancelamento" THEN 
       ASSIGN pOper = TRIM(pOper) + " do Pedido".


    IF NUM-ENTRIES(es-api-export.chave,"|") >= 6 THEN
        ASSIGN  deQuantidade     = DEC(ENTRY(6,es-api-export.chave,"|")).
    ELSE
    DO:

        FOR FIRST ped-item FIELDS(nome-abrev nr-pedcli nr-sequencia it-codigo qt-pedida) NO-LOCK 
            WHERE ped-item.nome-abrev    = pnomeAbrev  
              AND ped-item.nr-pedcli     = pnrPedcli   
              AND ped-item.nr-sequencia  = pseq        
              AND ped-item.it-codigo     = pitem:     
        END.
        IF NOT AVAIL ped-item THEN DO:
    
            pErro = SUBSTITUTE("Item do Pedido nÆo encontrado (&1).", pitem ).
            RETURN "NOK".
    
        END.

    END.


    FOR FIRST ped-venda FIELDS(nome-abrev nr-pedcli cod-estabel cgc)
        WHERE ped-venda.nome-abrev = pnomeAbrev 
          AND ped-venda.nr-pedcli  = pnrPedcli  :
    END.
    IF AVAIL ped-venda THEN DO:
        CREATE tt-req.
        ASSIGN tt-req.operacao     = pOper
               tt-req.nota-fiscal  = ""
               tt-req.serie        = ""
               tt-req.cod-estabel  = ped-venda.cod-estabel
               tt-req.nr-pedido    = ped-venda.nr-pedcli
               tt-req.cnpj         = ped-venda.cgc
            
               tt-req.data         = es-api-export.data-movto
            .

        CREATE tt-req-it.
        IF AVAIL ped-item THEN
            ASSIGN tt-req-it.it-codigo    = ped-item.it-codigo
                   tt-req-it.quantidade   = ped-item.qt-pedida.
        ELSE
            ASSIGN tt-req-it.it-codigo    = pitem
                   tt-req-it.quantidade   = deQuantidade
                .
    END.
    IF CAN-FIND(FIRST tt-req-it) THEN DO:

        IF TEMP-TABLE tt-req:HAS-RECORDS THEN
            ASSIGN pTemp = BUFFER tt-req:HANDLE.

        IF TEMP-TABLE tt-req-it:HAS-RECORDS THEN
            ASSIGN pTemp1 = BUFFER tt-req-it:HANDLE.

        RETURN "OK".
    END.
    ELSE
    DO:
         pErro = "Nenhum item encontrado".
        RETURN "NOK".
    END.
        


END PROCEDURE.




