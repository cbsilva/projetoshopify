/*
 *------------------------------------------------------------------------------
 *  PROGRAMA        esp/esspf102.p
 *  OBJETIVO        Exportaá∆o do Item para DATALOGIX
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


DEFINE TEMP-TABLE tt_item NO-UNDO
    field ISBN              LIKE ITEM.it-codigo         SERIALIZE-NAME "ISBN"
    field Descricao         LIKE ITEM.desc-item         SERIALIZE-NAME "Descriá∆o"
    field PrecoVenda        AS DECIMAL                  SERIALIZE-NAME "PrecoVenda"
    field Vendavel          AS CHARACTER                SERIALIZE-NAME "Vendavel"
    field Doavel            AS CHARACTER                SERIALIZE-NAME "Doavel"
    field Idioma            AS CHARACTER                SERIALIZE-NAME "Idioma"
    field cStatus           AS CHARACTER                SERIALIZE-NAME "Status"
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
 * ------------------------------------------------------------------------------
 */
FUNCTION obterPrecoItem2 RETURNS DECIMAL
    (INPUT pTabpre      AS CHAR,
     INPUT pItem        AS CHAR,
     INPUT pRefer       AS CHAR,
     INPUT pUn          AS CHAR,
     INPUT pQuantidade  AS DECIMAL
     ):

                                                          
    FOR FIRST   preco-item  NO-LOCK WHERE                 
                preco-item.nr-tabpre        =  pTabpre    
            AND preco-item.it-codigo        =  pItem      
            AND preco-item.cod-refer        =  pRefer     
            AND preco-item.cod-unid-med     =  pUn        
            AND preco-item.quant-min        <= pQuantidade
                USE-INDEX ch-itemtab:                     
        RETURN preco-item.preco-venda.                    
    END.                                                  
                                                          
    //RETURN 1.0.
    
    RETURN ?.


END FUNCTION.

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
    RUN piGravaTTItem   (OUTPUT h-temp,
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



/*
 *------------------------------------------------------------------------------
 *
 *                                  PROCEDURES
 *
 *------------------------------------------------------------------------------
 */

PROCEDURE piGravaTTItem:

    DEFINE OUTPUT PARAMETER pTemp AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER pErro AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dePreco AS DECIMAL     NO-UNDO.

    FOR FIRST es-api-param-ped-spf NO-LOCK:                                            
    END.                                                                           
    IF NOT AVAIL es-api-param-ped-spf THEN                                                 
    DO:                                                                            
        pErro = SUBSTITUTE("Parametro de integraá∆o de pedidos n∆o encontrado" ).  
        RETURN "NOK".                                                              
    END.                                                                           
                                                                                   
    FIND FIRST ITEM  NO-LOCK WHERE item.it-codigo   = es-api-export-spf.chave NO-ERROR.
    IF NOT AVAIL ITEM THEN                                                         
    DO:                                                                            
        pErro = SUBSTITUTE("Item n∆o encontrado (&1)", es-api-export-spf.chave ).      
        RETURN "NOK".                                                              
    END.                                                                           
                                                                                   
    FOR FIRST ext_item NO-LOCK WHERE                                               
              ext_item.it_codigo        = ITEM.it-codigo:                          
    END.                                                                           
    IF NOT AVAIL ext_ITEM THEN                                                     
    DO:                                                                            
        pErro = SUBSTITUTE("Extens∆o do item n∆o encontrada (&1)", ITEM.it-codigo).
        RETURN "NOK".                                                              
    END.                                                                           
                                                                                   
    FOR FIRST es_idioma NO-LOCK WHERE                                              
              es_idioma.cod_idioma      = ext_item.cod_idioma:                     
    END.                                                                           
    IF NOT AVAIL es_idioma THEN                                                    
    DO:                                                                            
        pErro = SUBSTITUTE("Idioma n∆o encontrado (&1)", ext_item.cod_idioma).     
        RETURN "NOK".                                                              
    END.                                                                           
                                                                                   
    ASSIGN  dePreco = obterPrecoItem2   (INPUT es-api-param-ped-spf.nr_tabpre          ,
                                         INPUT ITEM.it-codigo ,
                                         INPUT "",
                                         input item.un,
                                         INPUT 1
                                         ).
    IF ext_item.flg_vendavel AND (dePreco = 0 OR dePreco = ?) THEN
    DO:
        pErro = SUBSTITUTE("Preáo do item n∆o encontrado" ).
        RETURN "NOK".
    END.

    IF dePreco = ? THEN dePreco = 0.

    CREATE tt_item.
    ASSIGN  tt_item.ISBN        = ITEM.it-codigo                               
            tt_item.Descricao   = ITEM.desc-item                               
            tt_item.PrecoVenda  = dePreco                                      
            tt_item.Vendavel    = STRING(ext_item.flg_vendavel, "Sim/N∆o")
            tt_item.Doavel      = STRING(ext_item.flg_doavel, "Sim/N∆o")
            tt_item.Idioma      = es_idioma.des_Sigla
            tt_item.cStatus     = IF ITEM.cod-obsoleto = 4 THEN "Inativo" ELSE "Ativo"
    .                

    IF TEMP-TABLE tt_item:HAS-RECORDS THEN
        ASSIGN pTemp = BUFFER tt_item:HANDLE.

    RETURN "OK".

END PROCEDURE.



