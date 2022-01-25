/*---------------------------------------------------------------------------------------------------------------------------------
  AUTHOR : DRG-SP DWC
  Object : ENVIA DADOS DO MEDIO ITEM DOA€ÇO.
  DATA   : ABRIL/2021  - INTEGRA€ÇO DATALOGIX bonifica‡Æo.
/* C:\progress\dlc116\gui\netlib\OpenEdge.Net.pl */  
-----------------------------------------------------------------------------------------------------------------------------------*/
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

{include/i-prgvrs.i esint121 2.00.00.016} /*** 010016 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esint121 MPD}
&ENDIF

{include/i_dbinst.i}  /* versao das bases e bases instaladas */
/* ------- Defini»’o de Par³metros ----- */
    
{lib/Utilidades.i}

DEFINE OUTPUT PARAMETER c-erro  AS CHARACTER NO-UNDO.

DEF VAR de-ult-fech-dia     LIKE estab-mat.ult-fech-dia NO-UNDO.


/* ------- Defini»’o de Variÿveis ----- */
DEFINE VARIABLE oJsonObjMain      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjMain1     AS JsonObject NO-UNDO.

DEFINE VARIABLE oJsonArrayMain    AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonObjIni       AS jsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjIni1      AS jsonObject NO-UNDO.

DEFINE VARIABLE ojsonArrayIni     AS JsonArray  NO-UNDO.
DEFINE VARIABLE ojsonArrayIni1    AS JsonArray  NO-UNDO.
DEFINE VARIABLE ojsonObjAux       AS JsonObject NO-UNDO.
DEFINE VARIABLE ojsonArrayAux     AS JsonArray  NO-UNDO.

DEFINE VARIABLE h-temp              AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-temp1             AS HANDLE     NO-UNDO.
DEFINE VARIABLE hComunicacaoAPI     AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-json              AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lEnviou             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-arq-json          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-retorno           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lresp               AS LOGICAL    NO-UNDO.
DEFINE VARIABLE l-envia-medio       AS LOG        NO-UNDO.
DEFINE VARIABLE cResposta           AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE cErro               AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE tt-req    NO-UNDO SERIALIZE-NAME "req"  
    FIELD data-medio     AS DATE SERIALIZE-NAME "Data" 
           INDEX ch_01
                 data-medio
    .

DEF TEMP-TABLE tt-req-it    NO-UNDO SERIALIZE-NAME "ListaProdutos" 
    FIELD it-codigo         AS CHAR SERIALIZE-NAME "CodigoProduto"
    FIELD val-mat           AS DEC  SERIALIZE-NAME "Valor"
            INDEX ch_01
                  it-codigo.

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.


DEF BUFFER b-es-api-export FOR es-api-export.

EMPTY TEMP-TABLE tt-req.
EMPTY TEMP-TABLE tt-req-it.

FIND FIRST para-ped NO-LOCK NO-ERROR.
IF NOT AVAIL para-ped THEN RETURN.

FOR FIRST estab-mat NO-LOCK 
    WHERE estab-mat.cod-estabel = para-ped.estab-pad:
     
    ASSIGN de-ult-fech-dia = estab-mat.ult-fech-dia.

END.

ASSIGN l-envia-medio = YES.

FOR LAST es-api-export NO-LOCK 
    WHERE  es-api-export.cd-tipo-integr = 7:

    IF DATE(es-api-export.data-movto) < de-ult-fech-dia
        AND es-api-export.ind-situacao = 2 THEN 
       ASSIGN l-envia-medio = YES.
    ELSE
       ASSIGN l-envia-medio = NO.

END.

IF l-envia-medio = NO THEN DO:
    ASSIGN c-erro = "NÆo existe itens a serem enviados".
    RETURN "NOK".
END.

IF l-envia-medio THEN DO:

    RUN piGravaTempitens(OUTPUT h-temp,
                         OUTPUT h-temp1,
                         OUTPUT c-erro).

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
        RUN criarObjetoJSON  IN hComunicacaoAPI (INPUT h-temp,
                                                 OUTPUT ojsonObjIni,
                                                 OUTPUT ojsonArrayIni,
                                                 INPUT NO) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            ASSIGN c-erro = "Erro ao criar objeto JSON do Cabe‡alho. " + tratarString(c-erro).
            DELETE OBJECT h-temp.
            RETURN "NOK".
        END.            
        DELETE OBJECT h-temp.

        RUN criarObjetoJSON  IN hComunicacaoAPI (INPUT h-temp1,
                                                 OUTPUT ojsonObjIni1,
                                                 OUTPUT ojsonArrayIni1, 
                                                 INPUT YES) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
            ASSIGN c-erro = "Erro ao criar objeto JSON do Item. " + tratarString(c-erro).
            DELETE OBJECT h-temp1.
            RETURN "NOK".
        END.            
        DELETE OBJECT h-temp1.

    END.
    ojsonObjIni:ADD("ListaProdutos",ojsonArrayIni1).

    oJsonArrayMain = NEW JsonArray().
    oJsonArrayMain:ADD(ojsonObjIni).

    /* ----- Cria Json Principal ------- */
    oJsonObjMain = NEW JsonObject().
    oJsonObjMain:ADD("req",oJsonArrayMain).

   /* oJsonObjMain:WriteFile("c:\temp\json-custo.json",YES,"UTF-8"). */ 
    oJsonObjMain:WRITE(c-json,YES,"UTF-8") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
        DELETE OBJECT hComunicacaoAPI.

        ASSIGN c-erro = "Erro ao preparar objeto JSON. " + tratarString(c-erro).

        RETURN "NOK".
    END.

    FIND FIRST es-api-param WHERE 
               es-api-param.ind-tipo-trans = 2 /*---- Saida ----*/                 AND 
               es-api-param.cd-tipo-integr = 7 /*---- Integra»’o atualiza‡Æo medio  ------*/ NO-LOCK NO-ERROR.
    IF AVAIL es-api-param THEN 
    DO:
        IF NOT CAN-FIND(FIRST es-api-export WHERE 
                              es-api-export.chave        = STRING(de-ult-fech-dia,"99/99/9999") AND 
                              es-api-export.ind-situacao < 2) THEN 
        DO:    
            FIND LAST b-es-api-export NO-LOCK NO-ERROR.

            CREATE es-api-export.
            ASSIGN es-api-export.id-movto       = IF NOT AVAIL b-es-api-export THEN 1 ELSE b-es-api-export.id-movto + 1 //NEXT-VALUE(seq-export)
                   es-api-export.chave          = STRING(de-ult-fech-dia,"99/99/9999")
                   es-api-export.data-movto     = NOW
                   es-api-export.cd-tipo-integr = es-api-param.cd-tipo-integr     
                   es-api-export.ind-situacao   = 2 /*Processado*/
                   es-api-export.cod-status     = 0 /*nÆo integrado*/
                   es-api-export.text-retorno   = ""
                   es-api-export.c-json         = c-json.

            CREATE es-api-export-log.
            ASSIGN es-api-export-log.id-movto       = es-api-export.id-movto
                   es-api-export-log.nr-seq         = 1
                   es-api-export-log.cd-tipo-integr = es-api-param.cd-tipo-integr
                   es-api-export-log.data-log       = NOW
                   es-api-export-log.des-log        = ""      /*---- Pendente -----*/.

        END.
        RUN RealizarPost IN hComunicacaoAPI (INPUT ROWID(es-api-param),
                                             INPUT oJsonObjMain,
                                             OUTPUT cResposta,
                                             OUTPUT cErro).

         IF RETURN-VALUE <> "OK" THEN
         DO:
             IF VALID-HANDLE(hComunicacaoAPI) THEN
                DELETE OBJECT hComunicacaoAPI.
             c-erro = cErro.
             ASSIGN c-erro = "Erro ao Enviar JSON. " + tratarString(c-erro).
             FIND CURRENT es-api-export EXCLUSIVE-LOCK NO-ERROR.
             IF AVAIL es-api-export THEN 
                ASSIGN  es-api-export.data-inicio   = NOW
                        es-api-export.data-fim      = NOW
                        es-api-export.ind-situacao  = 2 // Processado
                        es-api-export.cod-status    = 2. // Com erro
             RETURN "NOK".
         END.

         FIND CURRENT es-api-export EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL es-api-export THEN 
            ASSIGN es-api-export.clob-retorno       = cResposta
                   es-api-export.ind-situacao       = 2
                   es-api-export.cod-status         = 1
                   es-api-export.data-inicio        = NOW
                   es-api-export.data-fim           = NOW.

         FIND CURRENT es-api-export NO-LOCK NO-ERROR.
         RELEASE es-api-export.

         IF VALID-HANDLE(hComunicacaoAPI) THEN
             DELETE OBJECT hComunicacaoAPI.
    END.
    /*
    IF TEMP-TABLE rowErrors:HAS-RECORDS THEN DO:
        FOR EACH rowErrors:
            ASSIGN c-erro = c-erro + string(rowerrors.ErrorNumber)  + " - " + rowerrors.ErrorDescription.
            DELETE OBJECT hComunicacaoAPI.
            RETURN "NOK".
        END.
    END.
    */
END.

PROCEDURE piGravaTempitens:
    DEFINE OUTPUT PARAMETER pTemp  AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pTemp1 AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER  NO-UNDO.

    DEF VAR i AS INT NO-UNDO.

    ASSIGN i = 0.
    FOR EACH ITEM NO-LOCK 
    /*,
        EACH ext_item NO-LOCK 
        WHERE ext_item.it_codigo = ITEM.it-codigo
          AND ext_item.flg_doavel = YES*/
          :

        FIND LAST pr-it-per NO-LOCK 
             WHERE pr-it-per.it-codigo   = ITEM.it-codigo
               AND pr-it-per.cod-estabel = estab-mat.cod-estabel
               AND pr-it-per.periodo     = de-ult-fech-dia       NO-ERROR.
        IF AVAIL pr-it-per THEN DO:
    
           CREATE tt-req-it.
           ASSIGN tt-req-it.it-codigo  = ITEM.it-codigo
                  tt-req-it.val-mat    = pr-it-per.val-unit-mat-m[1].
        END.
    END.
    IF CAN-FIND(FIRST tt-req-it) THEN DO:
        CREATE tt-req.
        ASSIGN tt-req.data-medio = de-ult-fech-dia. 

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
