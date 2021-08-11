/*
 *------------------------------------------------------------------------------
 *  PROGRAMA        esp/esspf101.p
 *  OBJETIVO        Exportaá∆o do Status do pedido para DATALOGIX
 *  AUTOR           TOTVS - LASF
 *  DATA            03/2021
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *                                DEFINIÄÂES
 *
 *------------------------------------------------------------------------------
 */
/* ------ Defini?ío das classes de objetos ------ */
using Progress.Json.OBJECTModel.JsonOBJECT.
using Progress.Json.OBJECTModel.JsonArray.

//{include/i-prgvrs.i esspf001HE 2.09.00.003} 

/* ------- Defini?ío de Par?metros ----- */
DEFINE INPUT  PARAMETER r-table AS ROWID     NO-UNDO.
DEFINE OUTPUT PARAMETER c-erro  AS CHARACTER NO-UNDO.

/* ------- Defini?ío de Variˇveis ----- */
DEFINE VARIABLE oJsonObjMain      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain    AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonObjIni       AS jsonObject NO-UNDO.
DEFINE VARIABLE ojsonArrayIni     AS JsonArray  NO-UNDO.
DEFINE VARIABLE ojsonObjAux       AS JsonObject NO-UNDO.
DEFINE VARIABLE ojsonArrayAux     AS JsonArray  NO-UNDO.

DEFINE VARIABLE hComunicacaoAPI AS HANDLE      NO-UNDO.

DEFINE VARIABLE h-temp              AS HANDLE     NO-UNDO.
DEFINE VARIABLE cJson               AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lEnviou             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE c-arq-json          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lresp               AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cResposta           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErro               AS CHARACTER   NO-UNDO.


DEF TEMP-TABLE ttStatusNota 
    FIELD Estabelecimento       AS CHARACTER            SERIALIZE-NAME "Estabelecimento"                                                  
    FIELD NumeroPedidoCliente   AS CHARACTER            SERIALIZE-NAME "NumeroPedidoCliente"  
    field nrNotaFis             AS CHARACTER            SERIALIZE-NAME "NrNotaFis"
    field statusNota            AS CHARACTER            SERIALIZE-NAME "StatusNota"                                              
    field DataHoraStatus        AS CHARACTER            SERIALIZE-NAME "DataHoraStatus"                                                
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
 *                                  FUNÄÂES
 *
 *------------------------------------------------------------------------------
 */

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

    FIND FIRST es-api-export-spf NO-LOCK WHERE ROWID(es-api-export) = r-table NO-ERROR.
    IF NOT AVAIL es-api-export-spf THEN DO:
        ASSIGN c-erro = "Registro de Exportaá∆o n∆o encontrado".
        RETURN "NOK".
    END.

    FIND FIRST es-api-param-spf WHERE es-api-param-spf.cd-tipo-integr = es-api-export-spf.cd-tipo-integr NO-LOCK NO-ERROR. 

    // Cria TT serializada
    RUN piGravaTT   (OUTPUT h-temp,
                     OUTPUT c-erro).
    IF RETURN-VALUE <> "OK" THEN
        RETURN "NOK".
    IF NOT VALID-HANDLE(h-temp)  THEN
    DO:
        ASSIGN c-erro = "Erro ao gerar dados para envio".
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
        RETURN "NOK".
    END.            
    DELETE OBJECT h-temp.

    oJsonArrayMain = NEW JsonArray().
    oJsonArrayMain:ADD(ojsonObjIni).
    
    /* ----- Cria Json Principal ------- */
    oJsonObjMain = NEW JsonObject().
    oJsonObjMain:ADD("req",oJsonArrayMain).

    /* ------ Grava conteudo do Json em variavel -----*/
    oJsonObjMain:WRITE(INPUT-OUTPUT cJSON, TRUE).
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN c-erro = ERROR-STATUS:GET-MESSAGE(1).
        DELETE OBJECT hComunicacaoAPI.
        RETURN "NOK".
    END.


    /* ------------ Envia Objeto Json --------- */
    RUN RealizarPost IN hComunicacaoAPI (INPUT ROWID(es-api-param-spf),
                                         INPUT oJsonObjMain,
                                         OUTPUT cResposta,
                                         OUTPUT cErro
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
        RETURN "NOK".
    END.

    oJsonObjMain:WRITE(cJSON, TRUE  ).
    FIND CURRENT es-api-export-spf EXCLUSIVE-LOCK.
    ASSIGN es-api-export-spf.c-json             = cJSON
           es-api-export-spf.text-retorno       = cResposta.
    FIND CURRENT es-api-export-spf NO-LOCK.
    RELEASE es-api-export-spf.

    
    IF VALID-HANDLE(hComunicacaoAPI) THEN
        DELETE OBJECT hComunicacaoAPI.

RETURN "OK".


/*
 *------------------------------------------------------------------------------
 *
 *                                  PROCEDURES
 *
 *------------------------------------------------------------------------------
 */

PROCEDURE piGravaTT:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

DEFINE VARIABLE cPedido AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE dePreco AS DECIMAL     NO-UNDO.

                                                                                   
    FIND LAST es-nota-status-transp WHERE 
         es-nota-status-transp.cod-estabel = ENTRY(1,es-api-export-spf.chave,"|") AND 
         es-nota-status-transp.serie       = ENTRY(2,es-api-export-spf.chave,"|") AND 
         es-nota-status-transp.nr-nota-fis = ENTRY(3,es-api-export-spf.chave,"|") AND
         es-nota-status-transp.nr-pedcli   = ENTRY(4,es-api-export-spf.chave,"|") AND
         es-nota-status-transp.status-nf   = ENTRY(5,es-api-export-spf.chave,"|") NO-LOCK NO-ERROR.
    IF NOT AVAIL es-nota-status-transp THEN
    DO:
        ASSIGN pErro = "Registo n∆o encontrado para tabela es-nota-status-transp".
        RETURN "NOK".
    END.
    
    cPedido = es-nota-status-transp.nr-pedcli.
    IF es-nota-status-transp.nr-pedcli = "" or es-nota-status-transp.nr-pedcli = ? THEN 
    DO:
        FOR FIRST it-nota-fisc NO-LOCK WHERE 
                  it-nota-fisc.serie            = es-nota-status-transp.serie            
              AND it-nota-fisc.cod-estabel      = es-nota-status-transp.cod-estabel      
              AND it-nota-fisc.nr-nota-fis       = es-nota-status-transp.nr-nota-fis       :
            ASSIGN cPedido   = it-nota-fisc.nr-pedcli.
        END.
        
    END.

    IF cPedido = "" OR cPedido = ? THEN
    DO:
        ASSIGN pErro = "Pedido n∆o encontrado para o registro de status".
        RETURN "NOK".
    END.

    CREATE ttStatusNota.
    ASSIGN ttStatusNota.Estabelecimento     = es-nota-status-transp.cod-estabel
           ttStatusNota.NumeroPedidoCliente = es-nota-status-transp.nr-pedcli
           ttStatusNota.nrNotaFis           = es-nota-status-transp.nr-nota-fisc
           ttStatusNota.statusNota          = es-nota-status-transp.status-nf
           ttStatusNota.DataHoraStatus      = es-nota-status-transp.data-hora-status.

    IF TEMP-TABLE ttStatusNota:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER ttStatusNota:HANDLE.

    RETURN "OK".

END PROCEDURE.




