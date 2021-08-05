/*
 *------------------------------------------------------------------------------
 *  PROGRAMA        LIB/PedidoVenda.p
 *  OBJETIVO        Rotinas para integraá∆o de pedidos de venda MACMILLAN
 *  AUTOR           TOTVS - LASF
 *  DATA            09/2020
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *                                DEFINIÄÂES
 *
 *------------------------------------------------------------------------------
 */

&SCOPED-DEFINE chavePedidoVenda "(Cli/Ped: " + tratarString(STRING(ped-venda.nome-abrev)) + "/"  + tratarString(STRING(ped-venda.nr-pedcli)) +  ")"
&SCOPED-DEFINE chavePedidoItem "(Cli/Ped/Seq/Item: " + tratarString(STRING(ped-item.nome-abrev)) + "/"  + tratarString(STRING(ped-item.nr-pedcli)) + "/" + tratarString(STRING(ped-item.it-codigo)) + "/" + tratarString(STRING(ped-item.cod-refer)) +  ")"
&SCOPED-DEFINE chaveSaldo "(Dep/Item/Lote: " + tratarString(STRING(ttLote.cod-depos)) + "/"  + tratarString(STRING(ttLote.it-codigo)) + "/" + tratarString(STRING(ttLote.nr-serlote)) +  ")"

DEFINE BUFFER b-ped-venda-orig  FOR ped-venda.
DEFINE BUFFER b-ped-item-orig     FOR ped-item.
DEFINE BUFFER b-ped-venda       FOR ped-venda.

    DEFINE VARIABLE dedisponivel AS DECIMAL     NO-UNDO.


{utp/utapi019.i}  /* tabela de erros da rotina de envio de e-mails */

DEFINE VARIABLE lParamValidarPreco  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lParamValidarTrib   AS LOGICAL     NO-UNDO.

DEFINE BUFFER b-web-ped-venda   FOR web-ped-venda.
DEFINE BUFFER b-web-ped-doacao  FOR web-ped-doacao.


DEFINE VARIABLE cNumPedidoBO  AS CHARACTER   NO-UNDO.

/* Definicao temp-table RowErrors */
//{method/dbotterr.i}     
{lib/rowErrors.i}
// DEFINE TEMP-TABLE rowErrorsaux LIKE rowErrors.

/* DEFINE TEMP-TABLE RowErrors NO-UNDO      */
/*     FIELD ErrorSequence    AS INTEGER    */
/*     FIELD ErrorNumber      AS INTEGER    */
/*     FIELD ErrorDescription AS CHARACTER  */
/*     FIELD ErrorParameters  AS CHARACTER  */
/*     FIELD ErrorType        AS CHARACTER  */
/*     FIELD ErrorHelp        AS CHARACTER  */
/*     FIELD ErrorSubType     AS CHARACTER. */
/*                                          */
        
{lib/utilidades.i}
{lib/log2.i}
{lib/mensagens2.i}

{lib/PedidoVenda.i}



/* Definicao de variaveis globais - Padrío */
{utp/ut-glob.i}
  

DEFINE VARIABLE deAloc AS DECIMAL     NO-UNDO.

DEFINE TEMP-TABLE tt-ped-venda  NO-UNDO LIKE ped-venda
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ped-repre  NO-UNDO LIKE ped-repre
    FIELD r-rowid AS ROWID.
  
DEFINE TEMP-TABLE tt-ped-item   NO-UNDO LIKE ped-item
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ped-item-aux LIKE tt-ped-item.

DEFINE TEMP-TABLE tt-item-pedido NO-UNDO
    FIELD it-codigo     LIKE ped-item.it-codigo
    FIELD qt-pedida     LIKE ped-item.qt-pedida
    FIELD vl-preuni     LIKE ped-item.vl-preuni
    .

DEFINE TEMP-TABLE tt-ped-param NO-UNDO
    FIELD relacao-item-cli     AS LOG INIT YES
    FIELD tp-relacao-item-cli  AS INT INIT 1
    FIELD qtde-un-medida-cli   AS LOG INIT YES
    FIELD multiplicar-qtde     AS LOG INIT YES
    FIELD atribuir-preco-comp  AS LOG INIT NO
    FIELD tp-exp-nat-oper      AS INT INIT 1
    FIELD tp-exp-dt-entrega    AS INT INIT 1
    FIELD exp-nat-cons-final   AS LOG INIT NO
    FIELD exp-nat-cod-mensagem AS LOG INIT NO
    FIELD atualizar-entregas   AS LOG INIT YES
    FIELD arredondar-qtde-lote AS LOG INIT NO
    FIELD gerar-proc-exp       AS LOG INIT NO
    FIELD itinerario           AS INT.

DEFINE VARIABLE h_bodi159sdf AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi159com AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi159cal AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi159    AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi154sdf AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi154    AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi157    AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi154can AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi159can AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi159sus AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi159rct AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi159del AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bodi018    AS HANDLE NO-UNDO.
DEFINE VARIABLE hAlocacao   AS HANDLE      NO-UNDO.

DEFINE VARIABLE hShowMsg     AS HANDLE NO-UNDO.

DEFINE VARIABLE i-nr-pedido     LIKE ped-venda.nr-pedido.
DEFINE VARIABLE i-nr-sequencia  LIKE ped-item.nr-sequencia.

/*
 *------------------------------------------------------------------------------
 *
 *                                FUNÄÂES
 *
 *------------------------------------------------------------------------------
 */
FUNCTION PedidoVenda RETURNS LOGICAL
    ():

    IF NOT AVAIL tt-ped-venda-import THEN
        RETURN ?.

    RETURN LOOKUP(tt-ped-venda-import.tipoPedido, "S,E") > 0.

END FUNCTION.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
FUNCTION obterTextoNotificacao RETURNS CHARACTER
    (INPUT pTexto AS CHAR):

DEFINE VARIABLE cConfirmado         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFuturo             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRejeitado          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescItem           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErro               AS CHARACTER   NO-UNDO.

    ASSIGN pTexto       = REPLACE(pTexto, "&Ciente", 
                                  IF AVAIL emitente 
                                  THEN emitente.nome-emit 
                                  ELSE IF AVAIL ped-venda 
                                       THEN ped-venda.nome-abrev
                                       ELSE IF AVAIL tt-ped-venda-import
                                            THEN tt-ped-venda-import.cnpj
                                            ELSE ""
                                        )     .

    ASSIGN pTexto       = REPLACE(pTexto, "&Pedido",
                                  IF AVAIL ped-venda 
                                  THEN ped-venda.nr-pedcli
                                  ELSE IF AVAIL tt-ped-venda-import 
                                       THEN tt-ped-venda-import.NumeroPedidoCliente
                                       ELSE ""
                                           ).

    IF AVAIL tt-ped-venda-import THEN
        ASSIGN  pTexto      = REPLACE(pTexto, "&Email",            tratarString(tt-ped-venda-import.email    ) ) 
                pTexto      = REPLACE(pTexto, "&Telefone",         tratarString(tt-ped-venda-import.telefone ) )
            .
    ELSE
    DO:
        IF AVAIL web-ped-venda THEN
            ASSIGN  pTexto      = REPLACE(pTexto, "&Email",       tratarString(web-ped-venda.e-mail    ) )
                    pTexto      = REPLACE(pTexto, "&Telefone",    tratarString(web-ped-venda.telefone  ) ).
                                                                                                       
        IF AVAIL web-ped-doacao THEN                                                                   
            ASSIGN  pTexto      = REPLACE(pTexto, "&Email",       tratarString(web-ped-doacao.e-mail   ) )
                    pTexto      = REPLACE(pTexto, "&Telefone",    tratarString(web-ped-doacao.telefone ) ).
                
    END.

    ASSIGN  pTexto      = REPLACE(pTexto, "&DataPedido",      STRING(TODAY,"99/99/9999")    )
            pTexto      = REPLACE(pTexto, "&HoraPedido",      STRING(TIME,"HH:MM:SS")       ).
                                                                                     
    ASSIGN  pTexto      = REPLACE(pTexto, "&DataPedido",      STRING(TODAY,"99/99/9999")    )
            .
                                                                                     
    IF AVAIL ped-venda THEN
    DO:

        FOR EACH ped-item OF ped-venda NO-LOCK:

            FOR FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = ped-item.it-codigo:
            END.
            ASSIGN cDescItem        =  IF AVAIL ITEM THEN ITEM.desc-item ELSE "".

            IF ped-item.cod-sit-item <> 6 THEN
                ASSIGN  cConfirmado  = cConfirmado  + SUBSTITUTE("&1 - &2 - &3 pedidos", ped-item.it-codigo, cDescItem, tratarString(STRING(ped-item.qt-pedida)) ) + CHR(10).
            
                ASSIGN  cFuturo      = cFuturo      + SUBSTITUTE("&1 - &2 - &3 pedidos", ped-item.it-codigo, cDescItem, tratarString(STRING(ped-item.qt-pedida)) ) + CHR(10).
                                                                                                                                                                   
        END.

        FOR EACH web-ped-venda OF ped-venda NO-LOCK WHERE web-ped-venda.flg_Restricao
                :

            FOR FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = ped-item.it-codigo:
            END.
            ASSIGN cDescItem        =  IF AVAIL ITEM THEN ITEM.desc-item ELSE "".

            ASSIGN  cRejeitado      = cRejeitado    + SUBSTITUTE("&1 - &2 - &3 pedidos", web-ped-venda.it-codigo, cDescItem, tratarString(STRING(web-ped-venda.quantidade)) ) + CHR(10).

        END.

    END.

    FOR EACH rowErrors:

        cErro = cErro + rowErrors.errorDesc + CHR(10).
        
    END.




    ASSIGN  pTexto      = REPLACE(pTexto, TRIM("&ITENS_FUTURO     "),   cFuturo      )
            pTexto      = REPLACE(pTexto, TRIM("&ITENS_CONFIRMADOS"),   cConfirmado  )
            pTexto      = REPLACE(pTexto, TRIM("&ITENS_REJEITADOS "),   cRejeitado   )
            pTexto      = REPLACE(pTexto, "&MENSAGENS",                 cErro        )
.


    RETURN pTexto.

END FUNCTION.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
FUNCTION AcrescentarEmail RETURNS CHARACTER
    (INPUT pDestino AS CHAR,
     INPUT pLIsta   AS CHAR):

DEFINE VARIABLE iIdx AS INTEGER     NO-UNDO.

    DO iIDx = 1 TO NUM-ENTRIES(pLista):

        IF LOOKUP(ENTRY(iIdx, pLista), pDestino ) = 0  THEN
        DO:
            IF pDestino <> "" AND pDestino <> ?  THEN
                pDestino = pDestino + "," .
                
            pDestino = pDestino + ENTRY(iIdx, pLista).
        END.
            

    END.


    RETURN pDestino.
    


END FUNCTION.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
FUNCTION obterNaturezaOperacao RETURNS CHARACTER
    (INPUT pGE          AS CHARACTER,
     INPUT pCdTrib      AS INTEGER ,
     INPUT pTpPedido    AS INTEGER,
     INPUT pUFPedido    AS CHAR,
     INPUT pUFCliente   AS CHAR
     ):

    FOR FIRST es_regra_natur_oper NO-LOCK WHERE
              es_regra_natur_oper.ge_codigo         = pGE        
          AND es_regra_natur_oper.cd_trib           = pCdTrib     
          AND es_regra_natur_oper.tip_pedido        = pTpPedido   
          AND es_regra_natur_oper.uf_cliente        = pUFCliente  
          AND es_regra_natur_oper.uf_pedido         = pUFPedido   :
    END.
    IF AVAIL es_regra_natur_oper THEN
        RETURN es_regra_natur_oper.nat_oper.
    
    FOR FIRST es_regra_natur_oper NO-LOCK WHERE
              es_regra_natur_oper.ge_codigo         = pGE         
          AND es_regra_natur_oper.cd_trib           = pCdTrib     
          AND es_regra_natur_oper.tip_pedido        = pTpPedido   
          AND es_regra_natur_oper.uf_cliente        = "*"
          AND es_regra_natur_oper.uf_pedido         = pUFPedido   :
    END.
    IF AVAIL es_regra_natur_oper THEN
        RETURN es_regra_natur_oper.nat_oper.

    FOR FIRST es_regra_natur_oper NO-LOCK WHERE
              es_regra_natur_oper.ge_codigo         = "*"
          AND es_regra_natur_oper.cd_trib           = pCdTrib     
          AND es_regra_natur_oper.tip_pedido        = pTpPedido   
          AND es_regra_natur_oper.uf_cliente        = pUFCliente  
          AND es_regra_natur_oper.uf_pedido         = pUFPedido   :
    END.
    IF AVAIL es_regra_natur_oper THEN
        RETURN es_regra_natur_oper.nat_oper.

    FOR FIRST es_regra_natur_oper NO-LOCK WHERE
              es_regra_natur_oper.ge_codigo         = "*"
          AND es_regra_natur_oper.cd_trib           = pCdTrib    
          AND es_regra_natur_oper.tip_pedido        = pTpPedido   
          AND es_regra_natur_oper.uf_cliente        = "*"
          AND es_regra_natur_oper.uf_pedido         = pUFPedido   :
    END.
    IF AVAIL es_regra_natur_oper THEN
        RETURN es_regra_natur_oper.nat_oper.
    

END FUNCTION.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */

FUNCTION obterCondPag RETURNS INTEGER 
    ():


END FUNCTION.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
FUNCTION obterNrPedcli RETURNS CHARACTER
    (INPUT pNomeAbrev   AS CHAR,
     INPUT pPedcli      AS CHAR,
     INPUT pTipoPedido  AS CHAR
     ):
DEFINE VARIABLE cPrefixo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSufixo AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cAux AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.



    IF pTipoPedido  =  "B" THEN
    DO:
        RETURN cNumPedidoBO.
/*         ASSIGN cAux         = pPedCli.                                      */
/*         IF pPedCli MATCHES "*BO*" THEN                                      */
/*             ASSIGN cAux = SUBSTRING(pPedCli,  1, INDEX(pPedCli, "BO") - 1). */
/*                                                                             */
/*         iCont = 0.                                                          */
/*         FOR EACH b-ped-venda NO-LOCK WHERE                                  */
/*                  b-ped-venda.nome-abrev         = pNomeAbrev                */
/*              AND b-ped-venda.nr-pedcli          BEGINS cAux:                */
/*             ASSIGN iCont = iCont  + 1.                                      */
/*         END.                                                                */
/*                                                                             */
/*         RETURN cAux + "BO" + STRING(iCont - 1, "99").                       */
    END.

    CASE pTipoPedido :
        WHEN "S" THEN     ASSIGN cPrefixo     = "WS".
        WHEN "D" THEN     ASSIGN cPrefixo     = "WD".
    END CASE.

    RETURN cPrefixo + pPedcli + cSufixo.


END FUNCTION.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
FUNCTION itemTributado RETURNS LOGICAL
    (INPUT pItem AS CHAR):
   
    FOR FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = pItem:
    END.

    IF NOT AVAIL ITEM THEN
        RETURN ?.

    RETURN (ITEM.cd-trib-icm <> 2 OR ITEM.cd-trib-iss <> 2 AND ITEM.cd-trib-ipi <> 2) .


END FUNCTION.


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

/*         MESSAGE                                */
/*             preco-item.desco-quant             */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */

        RETURN preco-item.preco-venda.
    END.

    RETURN ?.


END FUNCTION.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
FUNCTION obterPrecoItem RETURNS DECIMAL
    ():


    FOR FIRST   preco-item  NO-LOCK WHERE
                preco-item.nr-tabpre        = tt-ped-venda.nr-tabpre
            AND preco-item.it-codigo        = tt-ped-item-import.codigoItem    
            AND preco-item.cod-refer        = ITEM.cod-refer
            AND preco-item.cod-unid-med     = ITEM.un    
            AND preco-item.quant-min        <= tt-ped-item-import.quantidadepedida
                USE-INDEX ch-itemtab:
        RETURN preco-item.preco-venda.
    END.

    RETURN ?.


END FUNCTION.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
FUNCTION retornarSitPed RETURNS CHARACTER
    (INPUT pSituacao AS INTEGER):

    CASE pSituacao:
        WHEN 1 THEN RETURN "Aberto".
        WHEN 2 THEN RETURN "Atendido Parcial".
        WHEN 3 THEN RETURN "Atendido Total".
        WHEN 4 THEN RETURN "Pendente".
        WHEN 5 THEN RETURN "Suspenso".
        WHEN 6 THEN RETURN "Cancelado".
        WHEN 7 THEN RETURN "Fat Balc∆o".
       OTHERWISE RETURN "Inv†lida".
    END CASE.

END FUNCTION.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
FUNCTION obterSaldoItem RETURNS DECIMAL
    (INPUT pItem AS CHAR):

DEFINE VARIABLE deQtdDisponivel   AS DECIMAL     NO-UNDO.


    FOR EACH saldo-estoq WHERE
             saldo-estoq.it-codigo = pitem ,
          FIRST deposito WHERE
                deposito.cod-depos      = saldo-estoq.cod-depos   AND
                // deposito.ind-disp-saldo
                deposito.cons-saldo
        NO-LOCK:

        deQtdDisponivel        = deQtdDisponivel  
                                    + saldo-estoq.qtidade-atu  - 
                                      saldo-estoq.qt-alocada   - 
                                      saldo-estoq.qt-aloc-prod - 
                                      saldo-estoq.qt-aloc-ped    
                    .


    END. 

    RETURN deQtdDisponivel.


END FUNCTION.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
/* FUNCTION ChecarSeAlocaPedido RETURNS LOGICAL */
/*     ():                                      */
/*                                              */
/*                                              */
/*                                              */
/*                                              */
/* END FUNCTION.                                */

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
FUNCTION PedidoVendaAbaixoLimiteTransp RETURNS LOGICAL
    ():

DEFINE VARIABLE deValLiq AS DECIMAL     NO-UNDO.

    IF NOT AVAIL tt-ped-venda-import THEN 
        FOR FIRST tt-ped-venda-import NO-LOCK:
        END.

    IF AVAIL ped-venda THEN 
        ASSIGN deValLiq = ped-venda.vl-tot-ped.
    ELSE
        ASSIGN deValLiq = IF AVAIL tt-ped-venda-import THEN tt-ped-venda-import.valLiquido ELSE 0.
            
    IF AVAIL tt-ped-venda-import AND tt-ped-venda-import.tipoPedido = "S" AND deValLiq < 500 THEN
        RETURN YES.

    RETURN NO.

END FUNCTION.


/*
 *------------------------------------------------------------------------------
 *
 *                              BLOCO PRINCIPAL
 *                                MAIN BLOCK 
 *
 *------------------------------------------------------------------------------
 */

/*                                                             */
/* FOR FIRST emitente NO-LOCK WHERE                            */
/*          emitente.nome-abrev = "disal centro":              */
/* END.                                                        */
/*                                                             */
/* FOR FIRST ITEM NO-LOCK WHERE                                */
/*            ITEM.it-codigo = "9788551101285":                */
/* END.                                                        */
/*                                                             */
/*                                                             */
/*                                                             */
/* MESSAGE                                                     */
/*     obterPrecoItem2   (INPUT emitente.nr-tabpre           , */
/*                        INPUT ITEM.it-codigo,                */
/*                        INPUT ""                           , */
/*                        INPUT ITEM.un                      , */
/*                        INPUT 1                              */
/*                        )                                    */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                      */





/* DEFINE VARIABLE cDestino AS CHARACTER   NO-UNDO.                                                         */
/*                                                                                                          */
/* cDestino = "lukeasf@yahoo.com.br,luciano.santos@totvs.com.br".                                           */
/*                                                                                                          */
/*     MESSAGE acrescentarEmail(cDestino,"lukeasf@yahoo.com.br") SKIP                                       */
/*         acrescentarEmail(cDestino,"lukeracer@gmail.com,luciano.santos@totvs.com.br,outro@server.com.br") */
/*         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                                                        */


/*     FOR EACH ped-item NO-LOCK WHERE                                      */
/*              ped-item.nome-abrev    = "LIV GRANJA V"                     */
/*          AND ped-item.nr-pedcli     = "WS1020":                          */
/*                                                                          */
/*         DISPLAY ped-item.it-codigo                                       */
/*             ped-item.cod-sit-item WITH SCROLLABLE .                      */
/*                                                                          */
/*         //RUN cancelarPedItem .                                          */
/*     END.                                                                 */
    

/* DEFINE VARIABLE cNO AS CHARACTER   NO-UNDO.                                      */
/* cNO = obterNaturezaOperacao                                                      */
/*     ("46",                                                                       */
/*      2,                                                                          */
/*      1,                                                                          */
/*      "SP",                                                                       */
/*      "rj")                                                                       */
/* .                                                                                */
/*                                                                                  */
/* FOR FIRST natur-oper NO-LOCK WHERE                                               */
/*          natur-oper.nat-oper    = cNO:                                           */
/*     DISP natur-oper.nat-oper natur-oper.denominacao VIEW-AS EDITOR SIZE 60 BY 6. */
/* END.                                                                             */

 
/* FOR FIRST ped-venda NO-LOCK WHERE                                                  */
/*           ped-venda.dt-implant >= 10/01/2020:                                      */
/*                                                                                    */
/*     DISPLAY ped-venda.nr-pedcli                                                    */
/*             ped-venda.nome-abrev                                                   */
/*             ped-venda.cod-estabel                                                  */
/*             WITH SCROLLABLE.                                                       */
/*                                                                                    */
/*     FOR FIRST emitente NO-LOCK WHERE emitente.nome-abrev   = ped-venda.nome-abrev: */
/*     END.                                                                           */
/*                                                                                    */
/*     FOR FIRST ext-emitente WHERE                                                   */
/*         ext-emitente.cod-emitente = emitente.cod-emitente :                        */
/*         UPDATE ext-emitente.cod_estab.                                             */
/*     END.                                                                           */
/*                                                                                    */
/*                                                                                    */
/*     FOR EACH ped-item OF ped-venda NO-LOCK:                                        */
/*         DISPLAY ped-item.it-codigo                                                 */
/*                 ped-item.qt-pedida                                                 */
/*                 ped-item.vl-preori.                                                */
/*                                                                                    */
/*         FOR FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = ped-item.it-codigo:          */
/*         END.                                                                       */
/*                                                                                    */
/*         CREATE  tt-dados.                                                          */
/*         ASSIGN  tt-dados.nr-pedcli              = "WSTESTE4"                       */
/*                 tt-dados.data-hora              = "2020-10-09 23h00"               */
/*                 tt-dados.it-codigo              = ped-item.it-codigo               */
/*                 tt-dados.desc-item              = ITEM.desc-item                   */
/*                 tt-dados.vlr-unit               = ped-item.vl-preuni               */
/*                 tt-dados.quantidade             = ped-item.qt-pedida               */
/*                 //tt-dados.vlt-tot                = ped-item.vlt-tot               */
/*                 //tt-dados.nro-pedido             = ped-item.nro-pedido            */
/*                 //tt-dados.nome-emit              = ped-item.nome-emit             */
/*                 //tt-dados.e-mail                 = ped-item.e-mail                */
/*                 //tt-dados.telefone               = ped-item.telefone              */
/*                 tt-dados.cnpj                   = emitente.cgc                     */
/*                 tt-dados.cod-cond-pag           = ""    .                          */
/*                                                                                    */
/*     END.                                                                           */
/*                                                                                    */
/* END.                                                                               */
/*                                                                                    */
/* // RETURN.                                                                         */
/*                                                                                    */


/*
 *------------------------------------------------------------------------------
 *
 *                                PROCEDURES 
 *
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE selecionarPedido:
DEFINE INPUT  PARAMETER prPedido AS ROWID       NO-UNDO.

    FOR FIRST ped-venda NO-LOCK WHERE
             ROWID(ped-venda)    =   prPedido:
    END.
    IF NOT AVAIL ped-venda THEN DO:
        RUN gerarRowError("Pedido de Venda n∆o encontrado").
        RETURN "NOK".
    END.

    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      Recebe dados oriundos da integracao
 * ------------------------------------------------------------------------------
 */
PROCEDURE receberDados:
DEFINE INPUT PARAMETER TABLE FOR tt-dados.

END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE receberPedidos:
DEFINE INPUT PARAMETER TABLE FOR tt-ped-venda-import.
DEFINE INPUT PARAMETER TABLE FOR tt-ped-item-import.

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      PROCEURE PINCIPAL CHAMADA PELA INTEGRAÄ«O
 * ------------------------------------------------------------------------------
 */
PROCEDURE processarDados:
DEFINE OUTPUT PARAMETER pChave AS CHARACTER   NO-UNDO.

    ASSIGN  lParamValidarPreco      = YES
            lParamValidarTrib       = YES.

    RUN carregarBO.

    RUN validarPedido.
    IF RETURN-VALUE <> "OK" THEN
    DO:
        RUN enviarNotificacao("Validacao").
        RETURN "NOK".
    END.


    INTEGRACAO:
    DO TRANSACTION      
        ON ENDKEY       UNDO INTEGRACAO, RETURN "NOK"
        ON ERROR        UNDO INTEGRACAO, RETURN "NOK"
        ON STOP         UNDO INTEGRACAO, RETURN "NOK"
        : 

        RUN criarPedido (OUTPUT pChave).
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN descarregarBO.
            UNDO INTEGRACAO, RETURN "NOK".
        END.

        RUN descarregarBO.

        IF CAN-FIND(FIRST tt-ped-item-import WHERE NOT tt-ped-item-import.flgRestricao) THEN
        DO:
    
            FOR FIRST ped-venda NO-LOCK WHERE 
                      ped-venda.nome-abrev      = tt-ped-venda.nome-abrev
                  AND ped-venda.nr-pedcli       = tt-ped-venda.nr-pedcli:
            END.
            IF NOT AVAIL ped-venda THEN
            DO:
                RUN gerarRowError("Pedido de Venda n∆o encontrado ap¢s geraá∆o").
                UNDO INTEGRACAO, RETURN "NOK".
            END.
    
            RUN criarExtensoes.
            IF RETURN-VALUE <> "OK" THEN
            DO:
                UNDO INTEGRACAO, RETURN "NOK".
            END.
    
    
            RUN criarExtensoesSemIntegracao.
            IF RETURN-VALUE <> "OK" THEN
            DO:
                UNDO INTEGRACAO, RETURN "NOK".
            END.
    
/*             MESSAGE                                */
/*                 "-- 2 --  " SKIP                   */
/*                 ped-venda.vl-tot-ped SKIP          */
/*                 PedidoVendaAbaixoLimiteTransp()    */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            IF NOT PedidoVendaAbaixoLimiteTransp() THEN DO:
                RUN alocarPedido.
/*                 IF RETURN-VALUE <> "OK" THEN       */
/*                     UNDO INTEGRACAO, RETURN "NOK". */
            END.

            LOG-MANAGER:WRITE-MESSAGE("DEPOIS ALOCAÄ«O").  
            FOR EACH rowErrors:
                LOG-MANAGER:WRITE-MESSAGE("rowErrors.ErrorDescription     " + rowErrors.ErrorDescription     ).  
                LOG-MANAGER:WRITE-MESSAGE("rowErrors.ErrorNumber          " + STRING(rowErrors.ErrorNumber )  ).
                LOG-MANAGER:WRITE-MESSAGE("rowErrors.ErrorSubType         " + rowErrors.ErrorSubType         ).
            END.

    

/*             // Avalia Cancelamento             */
/*             RUN atualizarSituacaoPedido.       */
/*             IF RETURN-VALUE <> "OK" THEN       */
/*                 UNDO INTEGRACAO, RETURN "NOK". */
    
            RUN completarPedido.
            IF RETURN-VALUE <> "OK" THEN
                UNDO INTEGRACAO, RETURN "NOK".
    
            // Avalia alocaá∆o / avaliaá∆o crÇdito
            RUN atualizarSituacaoPedido.
            IF RETURN-VALUE <> "OK" THEN
                UNDO INTEGRACAO, RETURN "NOK".


            IF ped-venda.cod-sit-ped <> 6 AND tt-ped-venda-import.tipoPedido <> "d" THEN 
            DO:
                RUN ajustarPedido.
                IF RETURN-VALUE <> "OK" THEN
                    UNDO INTEGRACAO, RETURN "NOK".

                RUN completarPedido.
                IF RETURN-VALUE <> "OK" THEN
                    UNDO INTEGRACAO, RETURN "NOK".

            END.

            FOR FIRST ped-venda NO-LOCK WHERE 
                      ped-venda.nome-abrev      = tt-ped-venda.nome-abrev
                  AND ped-venda.nr-pedcli       = tt-ped-venda.nr-pedcli:
            END.
            IF NOT AVAIL ped-venda THEN
            DO:
                RUN gerarRowError("Pedido de Venda n∆o encontrado ap¢s geraá∆o").
                UNDO INTEGRACAO, RETURN "NOK".
            END.
            RUN integrarLogistica (ped-venda.nome-abrev, ped-venda.nr-pedcli).
            
            
        END.
        ELSE
        DO:
    
            RUN criarExtensoesSemIntegracao.
            IF RETURN-VALUE <> "OK" THEN
            DO:
                UNDO INTEGRACAO, RETURN "NOK".
            END.
    
        END.

    END. // END TRANS

/*     IF AVAIL tt-ped-venda THEN                          */
/*         MESSAGE                                         */
/*             'antes notific ' SKIP                       */
/*             tt-ped-venda.nome-abrev   SKIP              */
/*             tt-ped-venda.nr-pedcli                      */
/*             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.   */
/*     ELSE                                                */
/*         MESSAGE 'antes notific - n∆o encontrado pedido' */
/*             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.   */


    FOR FIRST ped-venda NO-LOCK WHERE 
              ped-venda.nome-abrev      = tt-ped-venda.nome-abrev
          AND ped-venda.nr-pedcli       = tt-ped-venda.nr-pedcli:
    END.
    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido de Venda n∆o encontrado ap¢s geraá∆o -- notificaá∆o n∆o ser† enviada").
        RETURN "NOK".
    END.
    RUN enviarNotificacoesPedido.
    
    RETURN "OK".


END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE receberDadosFacilitador:
DEFINE INPUT PARAMETER TABLE FOR ttParam.
DEFINE INPUT PARAMETER TABLE FOR tt-ped-venda-import.
DEFINE INPUT PARAMETER TABLE FOR tt-ped-item-import.

    FOR FIRST ttParam:
    END.
    IF NOT AVAIL ttParam THEN
    DO:
        RUN gerarRowError("Pedido de Venda n∆o encontrado ap¢s geraá∆o").
        RETURN "NOK".

    END.

    ASSIGN  lParamValidarPreco      = ttParam.flgValidarPreco  
            lParamValidarTrib       = ttParam.flgValidartrib   
           .

    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      PROCEURE CHAMADA PELO FACILITADOR
 * ------------------------------------------------------------------------------
 */
PROCEDURE processarDadosFacilitador:

    FOR FIRST tt-ped-venda-import:
    END.
    IF NOT AVAIL tt-ped-venda-import THEN
    DO:
        RUN gerarRowError ("Pedido de Venda Importado n∆o encontrado").
        RETURN "NOK".
    END.


    FOR FIRST tt-ped-item-import:
    END.
    IF NOT AVAIL tt-ped-item-import THEN
    DO:
        RUN gerarRowError("Nenhum item de pedido encontrado").
        RETURN "NOK".
    END.

    FOR FIRST es_api_param_ped NO-LOCK:
    END.
    IF NOT AVAIL es_api_param_ped THEN
    DO:
        RUN gerarRowError( "Parametros de integraá∆o n∆o cadastrados." ).
        RETURN "NOK".        
    END.


    FOR FIRST emitente NO-LOCK WHERE
              emitente.cod-emitente      = tt-ped-venda-import.codigoEmitente:
    END.
    IF NOT AVAIL emitente THEN
    DO:
        RUN gerarRowError("Emitente n∆o encontrado").
        RETURN "NOK".
    END.

    FOR FIRST ext-emitente NO-LOCK WHERE
              ext-emitente.cod-emitente     = emitente.cod-emitente:
    END.
    IF NOT AVAIL emitente THEN
    DO:
        RUN gerarRowError("Extens∆o do emitente n∆o encontrada").
        RETURN "NOK".
    END.

    FOR FIRST ped-venda NO-LOCK WHERE
              ped-venda.nome-abrev          = emitente.nome-abrev
          AND ped-venda.nr-pedcli           = tt-ped-venda-import.NumeroPedidoCliente:
             
    END.
    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError ("Pedido de Venda n∆o encontrado").
        RETURN "NOK".
    END.

    FOR EACH tt-ped-item-import:

        RUN validarItemImportado.
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN EnviarNotificacao ("Validacao"). 

            RETURN "NOK".
        END.
            

    END.
    

    BLOCO_FACILITADOR:
    DO TRANSACTION      
        ON ENDKEY       UNDO BLOCO_FACILITADOR, RETURN "NOK"
        ON ERROR        UNDO BLOCO_FACILITADOR, RETURN "NOK"
        ON STOP         UNDO BLOCO_FACILITADOR, RETURN "NOK"
        : 


        RUN carregarBO.

        RUN atualizarPedido.
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN descarregarBO.
            UNDO BLOCO_FACILITADOR, RETURN "NOK".
        END.

        RUN descarregarBO.

        IF CAN-FIND(FIRST tt-ped-item-import WHERE NOT tt-ped-item-import.flgRestricao) THEN
        DO:
           
            FOR EACH tt-ped-item-import WHERE NOT tt-ped-item-import.flgRestricao:

                FOR FIRST ped-item NO-LOCK WHERE
                          ped-item.nome-abrev       = ped-venda.nome-abrev
                      AND ped-item.nr-pedcli        = ped-venda.nr-pedcli
                      AND ped-item.it-codigo        = tt-ped-item-import.codigoItem
                      AND ped-item.nr-sequencia     = tt-ped-item-import.numseq     :
                END.
                IF NOT AVAIL ped-item THEN
                DO:
                    RUN gerarRowError (SUBSTITUTE("Item do pedido n∆o encontrado ap¢s geraá∆o (Item &1 /Seq &2 )",
                                                  tratarString(tt-ped-item-import.codigoItem ),
                                                  tratarString(STRING (tt-ped-item-import.numseq   )  ))
                                                  ).
                    UNDO BLOCO_FACILITADOR, RETURN "NOK".
                END.

                RUN alocarItemPedido IN hAlocacao (ROWID(ped-item)) .    
                IF RETURN-VALUE <> "OK" THEN
                DO:
                    RUN retornarErros IN hAlocacao (OUTPUT TABLE rowerrorsAux).
                    RUN obterMensagensAux.
                    DELETE PROCEDURE hAlocacao .
                    UNDO BLOCO_FACILITADOR, RETURN "NOK".
                END.


                RUN retornarErros IN hAlocacao (OUTPUT TABLE rowerrorsAux).
                DELETE PROCEDURE hAlocacao.

            END.

            RUN atualizarExtensoes.
            IF RETURN-VALUE <> "OK" THEN
            DO:
                UNDO BLOCO_FACILITADOR, RETURN "NOK".
            END.

            // Avalia Cancelamento
            RUN atualizarSituacaoPedido.
            IF RETURN-VALUE <> "OK" THEN
                UNDO BLOCO_FACILITADOR, RETURN "NOK".
    
            IF ttParam.flgCompletarPedido  THEN
            DO:
                
                RUN completarPedido.
                IF RETURN-VALUE <> "OK" THEN
                    UNDO BLOCO_FACILITADOR, RETURN "NOK".

                // Avalia alocaá∆o / avaliaá∆o crÇdito
                RUN atualizarSituacaoPedido.
                IF RETURN-VALUE <> "OK" THEN
                    UNDO BLOCO_FACILITADOR, RETURN "NOK".

            END.
            
            RUN integrarLogistica (ped-venda.nome-abrev, ped-venda.nr-pedcli).
    
            RUN enviarNotificacoesPedido.

        END.

    END.

    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE atualizarPedido:

    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido de Venda n∆o encontrado").
        RETURN 'NOK'.
    END.


    EMPTY TEMP-TABLE tt-ped-venda.
    CREATE tt-ped-venda.
    BUFFER-COPY ped-venda TO tt-ped-venda
        ASSIGN tt-ped-venda.r-rowid     = ROWID(ped-venda).


    FOR FIRST estabelec NO-LOCK WHERE
              estabelec.cod-estabel     = ped-venda.cod-estabel:
    END.
    IF NOT AVAIL estabelec THEN
    DO:
        RUN gerarRowError("Estabelecimento n∆o encontrado").
        RETURN "NOK".
    END.


    FOR EACH tt-ped-item-import :

        //ASSIGN deValTotal   = deValTotal + (tt-ped-item-import.quantidadePedida * tt-ped-item-import.valorUnitario) .
            

        FOR FIRST ITEM NO-LOCK WHERE
                  ITEM.it-codigo        = tt-ped-item-import.codigoItem:
        END.
        IF NOT AVAIL ITEM THEN
        DO:
            RUN gerarRowError(SUBSTITUTE("Item n∆o encontrado (&1)", tt-ped-item-import.CodigoItem)).
            RETURN "NOK".
        END.

        ASSIGN tt-ped-item-import.nat-oper  = obterNaturezaOperacao  (INPUT STRING(ITEM.ge-codigo),
                                                                      INPUT (IF itemTributado(ITEM.it-codigo) THEN 1 ELSE 2) ,
                                                                      INPUT (IF tt-ped-venda-import.tipoPedido = "S" OR tt-ped-venda-import.tipoPedido = "E" THEN 1 ELSE 2),
                                                                      INPUT estabelec.estado,
                                                                      INPUT emitente.estado
                                                                      ) .

/*         IF cNatOper = ? THEN                                                            */
/*         DO:                                                                             */
/*             FOR FIRST natur-oper NO-LOCK WHERE                                          */
/*                       natur-oper.nat-oper   = tt-ped-item-import.nat-oper:              */
/*             END.                                                                        */
/*             IF NOT AVAIL natur-oper THEN                                                */
/*             DO:                                                                         */
/*                 RUN gerarRowError( SUBSTITUTE("Natureza de Operaá∆o n∆o encontrada") ). */
/*                 RETURN "NOK".                                                           */
/*             END.                                                                        */
/*                                                                                         */
/*             ASSIGN  cNatOper   = tt-ped-item-import.nat-oper                            */
/*                     iMensagem  = natur-oper.cod-mensagem.                               */
/*                                                                                         */
/*                                                                                         */
/*         END.                                                                            */
/*                                                               */
/*         ASSIGN i-nr-sequencia = i-nr-sequencia + 10.          */
/*         ASSIGN tt-ped-item-import.numSeq    = i-nr-sequencia. */


    END.



            /* Cria Item de Pedido */
        //ASSIGN i-nr-sequencia = 0.
        FOR EACH tt-ped-item-import NO-LOCK WHERE NOT tt-ped-item-import.flgrestricao:
    
            RUN criarItemPedido.
            IF RETURN-VALUE <> "OK" THEN
            DO:

                RETURN "NOK".
            END.


        END.


END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE processarDadosBO:
DEFINE INPUT  PARAMETER pPedido AS ROWID       NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-ped-item-import.
DEFINE OUTPUT PARAMETER pChave AS CHARACTER   NO-UNDO.

DEFINE VARIABLE lAlocar AS LOGICAL     NO-UNDO.

DEFINE VARIABLE rPedido AS ROWID       NO-UNDO.

    FOR FIRST b-ped-venda-orig NO-LOCK WHERE
              ROWID(b-ped-venda-orig)     = pPedido:
    END.
    IF NOT AVAIL b-ped-venda-orig THEN
    DO:
        RUN gerarRowError( "Pedido de Venda de origem n∆o encontrado para geraá∆o de BACK ORDER." ).
        RETURN "NOK".
    END.

/*     FOR FIRST tt-ped-item:                                                                                                                                               */
/*     END.                                                                                                                                                                 */
/*     IF NOT AVAIL tt-ped-item THEN                                                                                                                                        */
/*     DO:                                                                                                                                                                  */
/*         RUN gerarRowError("Nenhum item encontrado para Back Order").                                                                                                     */
/*         RETURN "NOK".                                                                                                                                                    */
/*     END.                                                                                                                                                                 */
/*                                                                                                                                                                          */
/*     FOR FIRST b-ped-venda-orig NO-LOCK WHERE                                                                                                                               */
/*               b-ped-venda-orig.nome-abrev       = tt-ped-item.nome-abrev                                                                                                   */
/*         AND   b-ped-venda-orig.nr-pedcli        = tt-ped-item.nr-pedcli:                                                                                                   */
/*     END.                                                                                                                                                                 */
/*     IF NOT AVAIL b-ped-venda-orig THEN                                                                                                                                     */
/*     DO:                                                                                                                                                                  */
/*         RUN gerarRowError(SUBSTITUTE("Pedido n∆o encontrado (Cliente &1; Pedido &2) ",                                                                                   */
/*                                  tt-ped-item.nome-abrev  ,                                                                                                               */
/*                                  tt-ped-item.nr-pedcli                                                                                                                   */
/*                                  )).                                                                                                                                     */
/*         RETURN "NOK".                                                                                                                                                    */
/*     END.                                                                                                                                                                 */
/*                                                                                                                                                                          */
/*     FOR EACH tt-ped-item:                                                                                                                                                */
/*         IF tt-ped-item.nome-abrev   <> b-ped-venda-orig.nome-abrev OR                                                                                                      */
/*            tt-ped-item.nr-pedcli    <> b-ped-venda-orig.nr-pedcli                                                                                                          */
/*             THEN                                                                                                                                                         */
/*         DO:                                                                                                                                                              */
/*             RUN gerarRowError(SUBSTITUTE("Foram informados itens de pedidos diferentes (Cliente &1; Pedido &2 / Cliente &3; Pedido &4) ",                                */
/*                                      b-ped-venda-orig.nome-abrev  ,                                                                                                        */
/*                                      b-ped-venda-orig.nr-pedcli   ,                                                                                                        */
/*                                      tt-ped-item.nome-abrev  ,                                                                                                           */
/*                                      tt-ped-item.nr-pedcli                                                                                                               */
/*                                      )).                                                                                                                                 */
/*             RETURN "NOK".                                                                                                                                                */
/*                                                                                                                                                                          */
/*         END.                                                                                                                                                             */
/*                                                                                                                                                                          */
/*         FOR FIRST   ped-item NO-LOCK WHERE                                                                                                                               */
/*                     ped-item.nome-abrev         = tt-ped-item.nome-abrev                                                                                                 */
/*                 AND ped-item.nr-pedcli          = tt-ped-item.nr-pedcli                                                                                                  */
/*                 AND ped-item.nr-sequencia       = tt-ped-item.nr-sequencia                                                                                               */
/*                 AND ped-item.it-codigo          = tt-ped-item.it-codigo                                                                                                  */
/*                 AND ped-item.cod-refer          = tt-ped-item.cod-refer :                                                                                                */
/*         END.                                                                                                                                                             */
/*         IF NOT AVAIL ped-item THEN                                                                                                                                       */
/*         DO:                                                                                                                                                              */
/*             RUN gerarRowError(SUBSTITUTE("Item do pedido enviado para Back Order n∆o encontrado (Cliente &1; Pedido &2; Item &3; Seq &4) ",                              */
/*                                      tt-ped-item.nome-abrev  ,                                                                                                           */
/*                                      tt-ped-item.nr-pedcli   ,                                                                                                           */
/*                                      tt-ped-item.it-codigo   ,                                                                                                           */
/*                                      tt-ped-item.nr-sequencia                                                                                                            */
/*                                      )).                                                                                                                                 */
/*             RETURN "NOK".                                                                                                                                                */
/*         END.                                                                                                                                                             */
/*                                                                                                                                                                          */
/*         IF ped-item.cod-sit-item <> 6 THEN                                                                                                                               */
/*         DO:                                                                                                                                                              */
/*             RUN gerarRowError(SUBSTITUTE("Item do pedido enviado para Back Order n∆o est† na situaá∆o cancelado (Cliente &1; Pedido &2; Item &3; Seq &4; Situaá∆o &5) ", */
/*                                      tt-ped-item.nome-abrev  ,                                                                                                           */
/*                                      tt-ped-item.nr-pedcli   ,                                                                                                           */
/*                                      tt-ped-item.it-codigo   ,                                                                                                           */
/*                                      tt-ped-item.nr-sequencia,                                                                                                           */
/*                                      retornarSitPed(tt-ped-item.cod-sit-item)                                                                                            */
/*                                      )).                                                                                                                                 */
/*             RETURN "NOK".                                                                                                                                                */
/*         END.                                                                                                                                                             */
/*                                                                                                                                                                          */
/*         ASSIGN tt-ped-item.r-rowid   = ROWID(ped-item).                                                                                                                  */
/*                                                                                                                                                                          */
/*     END.                                                                                                                                                                 */
/*                                                                                                                                                                          */

    RUN carregarBO.

    INTEGRACAO:
    DO TRANSACTION      
        ON ENDKEY       UNDO INTEGRACAO, RETURN "NOK"
        ON ERROR        UNDO INTEGRACAO, RETURN "NOK"
        ON STOP         UNDO INTEGRACAO, RETURN "NOK"
        : 

        RUN criarPedidoBO (
            INPUT ROWID(b-ped-venda-orig),
            INPUT TABLE tt-ped-item-import,
            OUTPUT pChave).
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN descarregarBO.
            UNDO INTEGRACAO, RETURN "NOK".
        END.

        RUN descarregarBO.


        FOR FIRST ped-venda NO-LOCK WHERE 
                  ped-venda.nome-abrev      = tt-ped-venda.nome-abrev
              AND ped-venda.nr-pedcli       = tt-ped-venda.nr-pedcli:
        END.
        IF NOT AVAIL ped-venda THEN
        DO:
            RUN gerarRowError("Pedido de Venda n∆o encontrado ap¢s geraá∆o").
            UNDO INTEGRACAO, RETURN "NOK".
        END.

        //RUN criarExtensoes.
        RUN atualizarExtensoes.
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN gerarRowError("N∆o foi poss°vel atualizar extens‰es").
            UNDO INTEGRACAO, RETURN "NOK".
        END.

        RUN alocarPedido.
/*         IF RETURN-VALUE <> "OK" THEN       */
/*             UNDO INTEGRACAO, RETURN "NOK". */

        // Avalia Cancelamento
        RUN atualizarSituacaoPedido2.
        IF RETURN-VALUE <> "OK" THEN
            UNDO INTEGRACAO, RETURN "NOK".

        FOR FIRST ped-item OF ped-venda NO-LOCK WHERE ped-item.cod-sit-item <> 6:
        END.
        IF NOT AVAIL ped-item THEN DO:
            RUN gerarRowError("Erro ao gerar pedido de backorder - nenhum item_possui saldo em estoque").
            UNDO INTEGRACAO, RETURN "NOK".
        END.


        RUN completarPedido.
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN gerarRowError("Erro ao gerar pedido de backorder - N∆o foi poss°vel completar pedido").
            UNDO INTEGRACAO, RETURN "NOK".
        END.

        // Avalia alocaá∆o / avaliaá∆o crÇdito
        RUN atualizarSituacaoPedido2.
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN gerarRowError("Erro ao gerar pedido de backorder - N∆o foi poss°vel atualizar situaá∆o DO pedido").
            UNDO INTEGRACAO, RETURN "NOK".
        END.



    END.


    RETURN "OK".


END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE validarPedido:

        
    FOR FIRST tt-ped-venda-import NO-LOCK:
    END.

    //IF NOT tt-ped-venda-import.tipoPedido >= 1 AND tt-ped-venda-import.tipoPedido <= 3 THEN


    IF LOOKUP(tt-ped-venda-import.tipoPedido, "s,d,e,b") = 0 THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Tipo do Pedido Ç Inv†lido (&1)", tt-ped-venda-import.tipoPedido) , 500001, "ERROR").
        RETURN "NOK".
    END.

    IF tt-ped-venda-import.tipoPedido = "D" AND tt-ped-venda-import.NomeTransportador = ?  THEN
    DO:
        RUN gerarMensagem("Transportador deve ser informado para pedido de doaá∆o", 500002, "ERROR").
        RETURN "NOK".
    END.
   

/*     IF NOT CAN-FIND(FIRST emitente NO-LOCK WHERE emitente.cod-emitente  = tt-ped-venda-import.codigoEmitente) AND                                      */
/*        NOT CAN-FIND(FIRST emitente NO-LOCK WHERE emitente.cgc           = tt-ped-venda-import.CNPJ)                                                    */
/*          THEN                                                                                                                                          */
/*     DO:                                                                                                                                                */
/*         RUN gerarMensagem(SUBSTITUTE("Emitente n∆o encontrado (C¢digo:&1 | CNPJ:&2) ", tt-ped-venda-import.codigoEmitente, tt-ped-venda-import.cnpj)). */
/*         RETURN "NOK".                                                                                                                                  */
/*     END.                                                                                                                                               */

    IF AVAIL emitente  THEN
        RELEASE emitente.
    FOR FIRST emitente NO-LOCK WHERE FALSE:
    END.
    IF tt-ped-venda-import.codigoEmitente <> 0 AND tt-ped-venda-import.codigoEmitente <> ? THEN
        FOR FIRST emitente NO-LOCK WHERE
                  emitente.cod-emitente    = tt-ped-venda-import.codigoEmitente:
        END.
    IF NOT AVAIL emitente THEN
    DO:
        DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
        FOR EACH emitente NO-LOCK WHERE
                 emitente.cgc       = tt-ped-venda-import.cnpj:
            iCont = iCont + 1.
        END.
        IF iCont > 1 THEN
        DO:
            RUN gerarMensagem(SUBSTITUTE("Encontrado mais de um Emitente com mesmo CNPJ (Importados: C¢digo:&1 | CNPJ:&2) ", tt-ped-venda-import.codigoEmitente, tt-ped-venda-import.cnpj), 500003, "ERROR").
            RETURN "NOK".
        END.

        FOR FIRST emitente NO-LOCK WHERE
                  emitente.cgc     = tt-ped-venda-import.cnpj:
        END.
        IF NOT AVAIL emitente THEN
        DO:
            IF tt-ped-venda-import.tipopedido = "D" THEN
            DO:
                RUN gerarDadosClienteNovo.
            END.

            RUN gerarMensagem("Emitente n∆o encontrado", 500015, "ERROR").
            RETURN "NOK".
        END.

    END.

    ASSIGN tt-ped-venda-import.codigoEmitente = emitente.cod-emitente.

    IF NOT AVAIL emitente THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Emitente n∆o encontrado (C¢digo:&1 | CNPJ:&2) ", tt-ped-venda-import.codigoEmitente, tt-ped-venda-import.cnpj) , 500004, "ERROR" ).
        RETURN "NOK".
    END.

    FOR FIRST ext-emitente NO-LOCK WHERE
              ext-emitente.cod-emitente     = emitente.cod-emitente:
    END.
    IF NOT AVAIL ext-emitente THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Emitente n∆o configurrado para receber pedidos de integraá∆o. Verifique CD1510 (C¢digo:&1 | CNPJ:&2) ", tt-ped-venda-import.codigoEmitente, tt-ped-venda-import.cnpj), 500005, "ERROR").
        RETURN "NOK".
    END.

    IF NOT (tt-ped-venda-import.NumeroPedidoCliente <> ? AND tt-ped-venda-import.NumeroPedidoCliente <> "") THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("N£mero do Pedido do Cliente Ç obrigat¢rio e n∆o foi informado. (&1)", tratarString(tt-ped-venda-import.NumeroPedidoCliente))).
        RETURN "NOK".
    END.

/*     FOR FIRST ped-venda NO-LOCK WHERE                                                                                                                   */
/*               ped-venda.nome-abrev      = emitente.nome-abrev                                                                                           */
/*          AND  ped-venda.nr-pedcli       = obterNrPedcli(emitente.nome-abrev , tt-ped-venda-import.NumeroPedidoCliente, tt-ped-venda-import.tipoPedido): */
/*     END.                                                                                                                                                */
    IF CAN-FIND(FIRST ped-venda NO-LOCK WHERE
              ped-venda.nome-abrev      = emitente.nome-abrev
         AND  ped-venda.nr-pedcli       = obterNrPedcli(emitente.nome-abrev , tt-ped-venda-import.NumeroPedidoCliente, tt-ped-venda-import.tipoPedido)
                ) THEN
    //IF AVAIL ped-venda THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Pedido j† existente (Cliente: &1 / N£mero Pedido: &2)", tratarString(emitente.nome-abrev), tratarString( obterNrPedcli(emitente.nome-abrev , tt-ped-venda-import.NumeroPedidoCliente, tt-ped-venda-import.tipoPedido)   )), 500006, "ERROR").
        RETURN "NOK".
    END.

    IF tt-ped-venda-import.DataEntrega <> ? AND tt-ped-venda-import.DataEntrega < TODAY THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Data de Entrega informada Ç anterior Ö atual (&1)", tt-ped-venda-import.dataentrega), 500007, "ERROR").
        RETURN "NOK".
    END.

    IF tt-ped-venda-import.Nomerepresentante <> ? AND tt-ped-venda-import.Nomerepresentante <> "" AND NOT CAN-FIND(FIRST repres NO-LOCK WHERE /*repres.nome-abrev*/ repres.cod-rep  = INT( tt-ped-venda-import.nomerepresentante) )
         
        THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Representante informado n∆o encontrado (&1)", tt-ped-venda-import.Nomerepresentante), 500008, "ERROR").
        RETURN "NOK".
    END.

    IF tt-ped-venda-import.NomeTransportador <> ? AND tt-ped-venda-import.NomeTransportador <> "" AND NOT CAN-FIND(FIRST transporte NO-LOCK WHERE transporte.nome-abrev  = tt-ped-venda-import.NomeTransportador) THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Tranportador informado n∆o encontrado (&1)", tt-ped-venda-import.NomeTransportador), 500009, "ERROR").
        RETURN "NOK".
    END.

    FOR EACH tt-ped-item-import NO-LOCK:

        RUN validarItemImportado.
        IF RETURN-VALUE <> "OK" THEN
            RETURN "NOK".


    END.


    RETURN "OK".


END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE validarItemImportado:
DEFINE VARIABLE dePreco AS DECIMAL     NO-UNDO.

    FOR FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = tt-ped-item-import.codigoItem:
    END.
    // IF NOT CAN-FIND(FIRST ITEM NO-LOCK WHERE item.it-codigo = tt-ped-item-import.CodigoItem ) THEN 
    IF NOT AVAIL ITEM THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Item n∆o encontrado (&1)", tt-ped-item-import.CodigoItem), 500010, "ERROR").
        RETURN "NOK".
    END.

    IF tt-ped-item-import.QuantidadePedida = 0 OR tt-ped-item-import.quantidadePedida = ? THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Item sem quantidade informada (&1)", tt-ped-item-import.CodigoItem), 500011, "ERROR").
        RETURN "NOK".
    END.

    FOR FIRST ext_item NO-LOCK WHERE
              ext_item.it_codigo            = tt-ped-item-import.codigoItem:
    END.
    IF NOT AVAIL ext_Item THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Item n∆o configurado para integraá∆o de venda/doaá∆o. Verifique CD0204  (&1)", tt-ped-item-import.CodigoItem), 500012, "ERROR").
        RETURN "NOK".
    END.

    IF (tt-ped-venda-import.tipoPedido = "s" OR tt-ped-venda-import.tipoPedido = "e" ) AND NOT ext_item.flg_vendavel THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Pedido Ç de venda mas item n∆o Ç vend†vel (&1)", tt-ped-item-import.CodigoItem), 500013, "ERROR").
        RETURN "NOK".
    END.

    IF (tt-ped-venda-import.tipoPedido = "d" ) AND NOT ext_item.flg_doavel THEN
    DO:
        RUN gerarMensagem(SUBSTITUTE("Pedido Ç de doaá∆o mas item n∆o Ç do†vel (&1)", tt-ped-item-import.CodigoItem), 500014, "ERROR").
        RETURN "NOK".
    END.

    // Restriá‰es -- INICIO
    ASSIGN tt-ped-item-import.flgRestricao = NO
           tt-ped-item-import.desRestricao     = ""
        .

    IF lParamValidarTrib THEN    
        IF ext-emitente.flg_valida_trib AND itemTributado(tt-ped-item-import.CodigoItem) THEN
            ASSIGN  tt-ped-item-import.flgRestricao     = YES
                    tt-ped-item-import.desRestricao     = "Tributaá∆o"
                    .


    IF lParamValidarPreco THEN
        IF ext-emitente.flg_valida_preco AND tt-ped-item-import.valorUnitario <> 0 THEN
        DO:                    
            ASSIGN  dePreco = obterPrecoItem2   (INPUT emitente.nr-tabpre           ,
                                                 INPUT tt-ped-item-import.codigoItem,
                                                 INPUT ""                           ,
                                                 INPUT ITEM.un                      ,
                                                 INPUT tt-ped-item-import.quantidadePedida
                                                 ).
    

/*             MESSAGE                                                                           */
/*                 "tt-ped-item-import.it-codigo     "  tt-ped-item-import.codigoitem       SKIP */
/*                 "ext-emitente.val_tolerancia      "  ext-emitente.val_tolerancia         SKIP */
/*                 "dePreco                          "  dePreco                             SKIP */
/*                 "tt-ped-item-import.valorUnitario "  tt-ped-item-import.valorUnitario    SKIP */
/*                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                                     */

            IF ABS(tt-ped-item-import.valorUnitario - dePreco) > ext-emitente.val_tolerancia THEN
                ASSIGN  tt-ped-item-import.flgRestricao     = YES
                        tt-ped-item-import.desRestricao     = "Preáo"
                        .
    
        END.


    IF ITEM.cod-obsoleto <> 1 THEN
        ASSIGN  tt-ped-item-import.flgRestricao     = YES
                tt-ped-item-import.desRestricao     = "Obsoleto"
                .
    // Restriá‰es -- FIM

    RETURN "OK".

END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      Criar Pedido de Venda
 * ------------------------------------------------------------------------------
 */
PROCEDURE criarPedido:
DEFINE OUTPUT PARAMETER pChave AS CHARACTER   NO-UNDO.

DEFINE VARIABLE deValTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE deValUnit  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cNatOper AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iMensagem AS INTEGER NO-UNDO.

/* DEFINE INPUT  PARAMETER pCodEstabel  LIKE estabelec.cod-estabel NO-UNDO.    */
/* DEFINE INPUT  PARAMETER pCodEmitente    LIKE emitente.cod-emitente NO-UNDO. */
/* DEFINE INPUT  PARAMETER TABLE FOR tt-item-pedido.                           */
/* DEFINE OUTPUT PARAMETER p-nr-pedido LIKE ped-venda.nr-pedido NO-UNDO. */

/*     FOR FIRST estabelec NO-LOCK                          */
/*         WHERE estabelec.cod-estabel     = pCodEstabel :  */
/*     END.                                                 */
/*                                                          */
/*     FOR FIRST emitente NO-LOCK                           */
/*          WHERE emitente.cod-emitente    = pCodEmitente : */
/*     END.                                                 */
/*                                                          */
/*     FOR FIRST repres NO-LOCK                             */
/*          WHERE repres.cod-rep = emitente.cod-rep :       */
/*     END.                                                 */    

/*     FOR FIRST tt-dados:                              */
/*     END.                                             */
/*     IF NOT AVAIL tt-dados THEN                       */
/*     DO:                                              */
/*         RUN gerarMensagem("Nenhum item encontrado"). */
/*         RETURN "NOK".                                */
/*     END.                                             */

    FOR FIRST tt-ped-venda-import:
    END.
    IF NOT AVAIL tt-ped-venda-import THEN
    DO:
        RUN gerarRowError( "Pedido de venda n∆o encontrado." ).
        RETURN "NOK".        
    END.

    FOR FIRST es_api_param_ped NO-LOCK:
    END.
    IF NOT AVAIL es_api_param_ped THEN
    DO:
        RUN gerarRowError( "Parametros de integraá∆o n∆o cadastrados." ).
        RETURN "NOK".        
    END.

    FOR FIRST emitente NO-LOCK
         WHERE emitente.cod-emitente= tt-ped-venda-import.codigoEmitente :
    END.
    IF NOT AVAIL emitente THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Emitente n∆o encontrado (CNPJ: &1)", STRING(tt-ped-venda-import.codigoEmitente)) ).
        RETURN "NOK".        
    END.
    
    FOR FIRST ped-venda NO-LOCK WHERE
              ped-venda.nome-abrev      = emitente.nome-abrev
         AND  ped-venda.nr-pedcli       = obterNrPedcli(emitente.nome-abrev, tt-ped-venda-import.NumeroPedidoCliente, tt-ped-venda-import.tipoPedido ):
    END.

    FOR FIRST ext-emitente NO-LOCK WHERE
              ext-emitente.cod-emitente     = emitente.cod-emitente:
    END.
    IF NOT AVAIL ext-emitente THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Extens∆o do emitente n∆o encontrada (Emitente: &1)", emitente.cod-emitente) ).
        RETURN "NOK".        
    END.


    FOR FIRST estabelec NO-LOCK                        
        WHERE estabelec.cod-estabel     = ext-emitente.cod_estabel:
    END.                                               
    IF NOT AVAIL estabelec THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Estabelecimento do emitente n∆o encontrado (Est: &1 / Emitente: &2)", ext-emitente.cod_estabel, emitente.nome-abrev) ).
        RETURN "NOK".        
    END.


    IF pedidoVenda() THEN 
        FOR FIRST repres NO-LOCK                         
             WHERE repres.cod-rep = emitente.cod-rep :   
        END.              
    ELSE
        FOR FIRST repres NO-LOCK WHERE
             /*repres.nome-abrev*/ repres.cod-rep = INT (tt-ped-venda-import.nomeRepresentante) :
        END.

    IF NOT AVAIL repres THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Representante n∆o encontrado (Cod: &1)", emitente.cod-rep) ).
        RETURN "NOK".        
    END.

    FOR FIRST loc-entr NO-LOCK WHERE
              loc-entr.nome-abrev       = emitente.nome-abrev
          AND loc-entr.cod-entrega      = emitente.cod-entrega:
    END.
    IF NOT AVAIL loc-entr THEN
    DO:
        RUN gerarRowError( "Local de entrega n∆o encontrado" ).
        RETURN "NOK".        
    END.
    
    FOR FIRST transporte NO-LOCK WHERE
              transporte.cod-transp    = emitente.cod-transp:
    END.
    IF pedidoVenda() AND NOT AVAIL transporte THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Transportador n∆o encontrado") ).
        RETURN "NOK".        
    END.

    IF tt-ped-venda-import.NomeTransportador  <> "" AND tt-ped-venda-import.NomeTransportador  <> ? THEN 
    DO:
        FOR FIRST transporte NO-LOCK WHERE
                  transporte.nome-abrev  = tt-ped-venda-import.NomeTransportador:
        END.
        RUN gerarRowError( SUBSTITUTE("Transportador informado n∆o encontrado (&1)"),  tratarString(tt-ped-venda-import.NomeTransportador ) ).
        RETURN "NOK".        
    END.

    FOR FIRST natur-oper NO-LOCK WHERE
           natur-oper.nat-oper   =  "" // obterNaturezaOperacao() 
        :
    END.




    CRIAR_PED_VENDA:
    DO TRANS: 

        // Obtem dados dos itens do pedido parra definiá∆o de campos
        ASSIGN i-nr-sequencia = 0.
        FOR EACH tt-ped-item-import :
            ASSIGN i-nr-sequencia = i-nr-sequencia + 10.
            ASSIGN tt-ped-item-import.numSeq    = i-nr-sequencia.
        END.

        ASSIGN deValTotal = 0.
        cNatOper = ?.
        FOR EACH tt-ped-item-import WHERE NOT tt-ped-item-import.flgrestricao:

            FOR FIRST ITEM NO-LOCK WHERE
                      ITEM.it-codigo        = tt-ped-item-import.codigoItem:
            END.
            IF NOT AVAIL ITEM THEN
            DO:
                RUN gerarRowError(SUBSTITUTE("Item n∆o encontrado (&1)", tt-ped-item-import.CodigoItem)).
                RETURN "NOK".
            END.
            
            ASSIGN tt-ped-item-import.nat-oper  = obterNaturezaOperacao  (INPUT STRING(ITEM.ge-codigo),
                                                                          INPUT (IF itemTributado(ITEM.it-codigo) THEN 1 ELSE 2) ,
                                                                          INPUT (IF tt-ped-venda-import.tipoPedido = "S" OR tt-ped-venda-import.tipoPedido = "E" THEN 1 ELSE 2),
                                                                          INPUT estabelec.estado,
                                                                          INPUT emitente.estado
                                                                          ) .

             ASSIGN  deValUnit   = IF tt-ped-item-import.valorUnitario <> 0 AND tt-ped-item-import.valorUnitario <> ? 
                                   THEN tt-ped-item-import.valorUnitario
                                   ELSE obterPrecoItem2   
                                         (INPUT emitente.nr-tabpre           ,
                                          INPUT tt-ped-item-import.codigoItem,
                                          INPUT ""                           ,
                                          INPUT ITEM.un                      ,
                                          INPUT tt-ped-item-import.quantidadePedida
                                          ).
        
                     deValTotal  = deValTotal + (tt-ped-item-import.quantidadePedida * deValUnit)
                 .
        



            IF cNatOper = ? THEN 
            DO:
                FOR FIRST natur-oper NO-LOCK WHERE
                          natur-oper.nat-oper   = tt-ped-item-import.nat-oper:
                END.
                IF NOT AVAIL natur-oper THEN
                DO:
                    RUN gerarRowError( SUBSTITUTE("Natureza de Operaá∆o n∆o encontrada") ).
                    RETURN "NOK".        
                END.

                ASSIGN  cNatOper   = tt-ped-item-import.nat-oper
                        iMensagem  = natur-oper.cod-mensagem.


            END.


        END.

        ASSIGN tt-ped-venda-import.valLiquido        = deValTotal.

/*         FOR EACH tt-ped-item-import:                       */
/*             MESSAGE                                        */
/*                 tt-ped-item-import.codigoitem         skip */
/*                 tt-ped-item-import.flgrestricao      skip  */
/*                 tt-ped-item-import.desrestricao      skip  */
/*                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.  */
/*         END.                                               */

        IF NOT CAN-FIND(FIRST tt-ped-item-import WHERE NOT tt-ped-item-import.flgRestricao) THEN
        DO:
            RETURN "OK".
        END.

        /* Cria tabela de parametros para criacao do pedido de venda/item do pedido de venda */
        CREATE tt-ped-param. /* para uso da BO */
        ASSIGN tt-ped-param.relacao-item-cli     = YES
               tt-ped-param.tp-relacao-item-cli  = 1
               tt-ped-param.qtde-un-medida-cli   = YES
               tt-ped-param.multiplicar-qtde     = YES
               tt-ped-param.atribuir-preco-comp  = NO
               tt-ped-param.tp-exp-nat-oper      = 2  
               tt-ped-param.tp-exp-dt-entrega    = 1
               tt-ped-param.exp-nat-cons-final   = NO
               tt-ped-param.exp-nat-cod-mensagem = YES
               tt-ped-param.atualizar-entregas   = YES
               tt-ped-param.arredondar-qtde-lote = NO
               tt-ped-param.gerar-proc-exp       = NO
               tt-ped-param.itinerario           = 1.
                
        /* Retorna Numero do Pedido de Venda do cliente */
        RUN setDefaultOrderNumber IN h_bodi159sdf (OUTPUT i-nr-pedido). 
        
        /* Cria tabela temporaria do pedido de venda */
        CREATE tt-ped-venda.
        ASSIGN tt-ped-venda.log-cotacao             = NO
               tt-ped-venda.nome-abrev              = emitente.nome-abrev
               tt-ped-venda.log-usa-tabela-desconto = NO
               tt-ped-venda.nr-pedido               = i-nr-pedido
               tt-ped-venda.nr-pedcli               = obterNrPedcli(emitente.nome-abrev , tt-ped-venda-import.NumeroPedidoCliente, tt-ped-venda-import.tipoPedido).


/*         MESSAGE                                       */
/*             'tt-ped-venda ' SKIP                      */
/*             tt-ped-venda.nome-abrev     SKIP          */
/*             tt-ped-venda.nr-pedcli                    */
/*                                                       */
/*             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */

        //tt-ped-venda-import.nr-pedcli       = tt-ped-venda.nr-pedcli
            
        /* Atualizacao de registros */
        RUN inputTable IN h_bodi159sdf (INPUT TABLE tt-ped-venda).
        RUN setDefaultCustomer IN h_bodi159sdf.
        RUN outputTable IN h_bodi159sdf (OUTPUT TABLE tt-ped-venda).
        FIND FIRST tt-ped-venda NO-ERROR.
            

        ASSIGN 
/*             tt-ped-venda.cod-cond-pag     = IF tt-ped-venda-import.tipoPedido <> "d"         */
/*                                             THEN IF deValTotal < 1000                        */
/*                                                  THEN es_api_param_ped.cod_cond_pag_bol      */
/*                                                  ELSE IF tt-ped-venda-import.PagamentoAV = 1 */
/*                                                       THEN es_api_param_ped.cod_cond_pag_av  */
/*                                                       ELSE emitente.cod-cond-pag             */
/*                                             ELSE 0                                           */
            tt-ped-venda.cod-cond-pag     = IF tt-ped-venda-import.tipoPedido <> "d"
                                            THEN es_api_param_ped.cod_cond_pag_bol  // Para pedido de venda assume a princ°pio que Ç menor que 1000
                                            ELSE 0
               tt-ped-venda.nat-operacao     = cNatOper
               tt-ped-venda.nr-tabpre        = emitente.nr-tabpre
               tt-ped-venda.cod-canal-venda  = 0
               tt-ped-venda.nr-tab-finan     = es_api_param_ped.nr_tab_finan
               tt-ped-venda.vl-tot-ped       = 0                                                                           
               tt-ped-venda.no-ab-reppri     = repres.nome-abrev  
               tt-ped-venda.cod-des-merc     = IF natur-oper.consum-final AND NOT emitente.contrib-icms THEN 2 ELSE 1      
               tt-ped-venda.mo-codigo        = 0                                                                           
               tt-ped-venda.nr-pedrep        = IF pedidoVenda()
                                               THEN tt-ped-venda.nr-pedcli  
                                               ELSE tt-ped-venda-import.NumeroPedidoSistemaCliente
               tt-ped-venda.contato          = ""                                                                          
               tt-ped-venda.cod-portador     = emitente.portador                                                           
               tt-ped-venda.modalidade       = emitente.modalidade                                                         
               tt-ped-venda.observacoes      = "Pedido de Venda - Web"
               tt-ped-venda.cond-espec       = ""                                                                          
               tt-ped-venda.cond-redespa     = ""                                                                          
               tt-ped-venda.tp-pedido        = es_api_param_ped.tp_pedido
               // tt-ped-venda.ind-tp-frete     = IF deValTotal > 500 THEN INTEGER(SUBSTRING(loc-entr.char-1,50,8)) ELSE 2
               tt-ped-venda.ind-tp-frete     = IF tt-ped-venda-import.tipoPedido <> "d"
                                               THEN 2       // Para pedido de venda assume a princ°pio que Ç menor que 500
                                               ELSE INTEGER(SUBSTRING(loc-entr.char-1,50,8))        
               tt-ped-venda.nome-transp      = IF pedidoVenda() THEN transporte.nome-abrev  ELSE tt-ped-venda-import.nomeTransportador
               tt-ped-venda.dt-implant       = TODAY                                                                       
               tt-ped-venda.cod-entrega      = loc-entr.cod-entrega                                                        
               tt-ped-venda.cgc              = emitente.cgc                                                                
               tt-ped-venda.ins-estadual     = emitente.ins-estadual                                                       
               tt-ped-venda.cep              = emitente.cep                                                                
               tt-ped-venda.local-entreg     = emitente.endereco                                                           
               tt-ped-venda.bairro           = emitente.bairro                                                             
               tt-ped-venda.cidade           = emitente.cidade                                                             
               tt-ped-venda.cidade-cif       = SUBSTRING(loc-entr.char-1,25,25)                                            
               tt-ped-venda.estado           = emitente.estado                                                             
               //tt-ped-venda.endereco         = emitente.endereco                                                         
               tt-ped-venda.pais             = emitente.pais                                                               
               tt-ped-venda.dt-entorig       = IF tt-ped-venda-import.dataentrega <> ? THEN tt-ped-venda-import.dataentrega ELSE TODAY
               tt-ped-venda.dt-entrega       = tt-ped-venda.dt-entorig
               tt-ped-venda.user-impl        = es_api_param_ped.USER_impl
               tt-ped-venda.cod-mensagem     = iMensagem
               //tt-ped-venda.cod-ped-clien-mp = emitente.cod-ped-clien-mp                                                  .
                   .

        // Completa observaá‰es
        IF tt-ped-venda-import.NumeroPedidoSistemaCliente <> "" THEN
            ASSIGN tt-ped-venda.Observacoes = tt-ped-venda.Observacoes + " | Pedido Cliente: " + tt-ped-venda-import.NumeroPedidoSistemaCliente.
        IF tt-ped-venda-import.DataEntrega <> ? THEN
            ASSIGN tt-ped-venda.observacoes = tt-ped-venda.observacoes + " | Entregar Dia: " + string(tt-ped-venda-import.DataEntrega, "99/99/9999").
        IF tt-ped-venda-import.Endereco <> ? AND tt-ped-venda-import.Endereco <> "" THEN
            ASSIGN tt-ped-venda.observacoes = tt-ped-venda.observacoes + " | Endereáo: " + tt-ped-venda-import.Endereco.



     
        /* Completa dados de ped-venda */
        ASSIGN tt-ped-venda.cod-estabel   = estabelec.cod-estabel
               // tt-ped-venda.tp-preco      = 1       /* Informado */
               tt-ped-venda.tp-preco      = IF tt-ped-venda-import.tipoPedido = "S" OR tt-ped-venda-import.tipoPedido = "E" THEN 2 ELSE 1
               tt-ped-venda.nr-ind-finan  = 0
               tt-ped-venda.perc-desco1   = 0

            // tt-ped-venda.cod-cond-pag = ""
            //tt-ped-venda.nome-abrev = ?

               .
        
        /* Efetiva as informacoes da tabela temporaria na tabela ped-venda */
        EMPTY TEMP-TABLE RowErrors.
        RUN emptyRowErrors  IN h_bodi159.
        RUN openQueryStatic IN h_bodi159 (INPUT "Default":U).
        RUN setrecord       IN h_bodi159 (INPUT TABLE tt-ped-venda).
        RUN createRecord    IN h_bodi159.
        RUN getRowErrors    IN h_bodi159 (OUTPUT TABLE RowErrors).
    
        IF CAN-FIND(FIRST RowErrors 
                    WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:
            RUN gerarRowError("Houve erro ao tentar gerar o pedido.").
            UNDO CRIAR_PED_VENDA, RETURN "NOK".
        END.
    
        /* Gera Representante */
        FIND FIRST tt-ped-venda NO-ERROR.
        RUN getRowid IN h_bodi159 (OUTPUT tt-ped-venda.r-rowid).
    
        CREATE tt-ped-repre.
        ASSIGN tt-ped-repre.nr-pedido   = tt-ped-venda.nr-pedido
               tt-ped-repre.nome-ab-rep = repres.nome-abrev
               tt-ped-repre.perc-comis  = repres.comis-direta
               tt-ped-repre.comis-emis  = repres.comis-emis
               tt-ped-repre.ind-repbase = YES.
    
        EMPTY TEMP-TABLE RowErrors.
        RUN emptyRowErrors  IN h_bodi157.
        RUN openQueryStatic IN h_bodi157(INPUT "DefaultPd4000":U).
        RUN setRecord       IN h_bodi157(INPUT TABLE tt-ped-repre).
        RUN createRecord    IN h_bodi157.
        RUN getRowErrors    IN h_bodi157(OUTPUT TABLE RowErrors).
        
        IF CAN-FIND(FIRST RowErrors
                    WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:
            RUN gerarRowerror("Houve erro ao tentar gerar o pedido.").
            UNDO CRIAR_PED_VENDA, RETURN "NOK".
        END.
       
        /* Cria Item de Pedido */
        //ASSIGN i-nr-sequencia = 0.
        FOR EACH tt-ped-item-import NO-LOCK WHERE NOT tt-ped-item-import.flgrestricao:
    
            RUN criarItemPedido.
            IF RETURN-VALUE <> "OK" THEN
            DO:

                RETURN "NOK".
            END.


        END.

        FOR FIRST ped-venda NO-LOCK WHERE 
                  ped-venda.nome-abrev      = tt-ped-venda.nome-abrev
              AND ped-venda.nr-pedcli       = tt-ped-venda.nr-pedcli:
        END.
        IF NOT AVAIL ped-venda THEN
        DO:
            RUN gerarRowError("Pedido de Venda n∆o encontrado ap¢s geraá∆o").
            UNDO CRIAR_PED_VENDA, RETURN "NOK".
        END.

         RUN calculateOrder IN h_bodi159cal (INPUT ROWID(ped-venda)). 


/*         RUN ajustarPedido.                                           */
/*         IF RETURN-VALUE <> "OK" THEN DO:                             */
/*             RUN gerarRowError("Erro ao ajustar pedido").             */
/*             RETURN "NOK".                                            */
/*         END.                                                         */






/*         /* Completa Pedido */                                                                       */
/*         IF NOT tt-ped-venda.completo THEN DO:                                                       */
/*             RUN completeOrder IN h_bodi159com (INPUT tt-ped-venda.r-rowid, OUTPUT TABLE rowErrors). */
/*             IF CAN-FIND (FIRST RowErrors                                                            */
/*                          WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:                     */
/*                 RUN gerarRowError("Erro ao completar pedido").                                      */
/*                 UNDO CRIAR_PED_VENDA, RETURN "NOK".                                                 */
/*             END.                                                                                    */
/*         END.                                                                                        */

        ASSIGN pChave       = tt-ped-venda.nome-abrev + "|" + tt-ped-venda.nr-pedcli .

        


//        ASSIGN p-nr-pedido = tt-ped-venda.nr-pedido. 
/*         UNDO Implanta, RETURN.  */
    END.

    RETURN "OK".

END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      Criar Pedido de Venda
 * ------------------------------------------------------------------------------
 */
PROCEDURE criarItemPedido:

    FOR FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = tt-ped-item-import.codigoItem:
    END.
    IF NOT AVAIL ITEM THEN
    DO:
        RUN gerarRowError(SUBSTITUTE("Item n∆o encontrado (&1)", tt-ped-item-import.CodigoItem)).
        RETURN "NOK".
    END.
    
    FOR FIRST es-item NO-LOCK WHERE
              es-item.it-codigo = ITEM.it-codigo:
    END.

   // ASSIGN i-nr-sequencia = i-nr-sequencia + 10.

    EMPTY TEMP-TABLE tt-ped-item.
    
    /*  cria temporaria de item */
    CREATE tt-ped-item.
    ASSIGN  tt-ped-item.it-codigo           = tt-ped-item-import.codigoItem
            tt-ped-item.nome-abrev          = tt-ped-venda.nome-abrev
            tt-ped-item.nr-pedcli           = tt-ped-venda.nr-pedcli
            tt-ped-item.nr-sequencia        = tt-ped-item-import.numSeq // i-nr-sequencia
            tt-ped-item.nat-operacao        = tt-ped-item-import.nat-oper
            .

    FOR FIRST natur-oper NO-LOCK WHERE
              natur-oper.nat-oper   = tt-ped-item-import.nat-oper:
    END.
    IF NOT AVAIL natur-oper THEN
    DO:
        RUN gerarRowError(SUBSTITUTE("Natureza de Operaá∆o do Item importado n∆o encontrada (&1)" , tt-ped-item-import.nat-oper)).
        RETURN "NOK".        
    END.

    /* executa criacao dos defaults */
    RUN inputParentTable          IN h_bodi154sdf (INPUT TABLE tt-ped-venda).
    RUN inputTable                IN h_bodi154sdf (INPUT TABLE tt-ped-item).
    RUN setDefaultItem            IN h_bodi154sdf.
    RUN outputTable               IN h_bodi154sdf (OUTPUT TABLE tt-ped-item).
    FIND FIRST tt-ped-item NO-ERROR.

    ASSIGN deDisponivel     = obterSaldoItem(tt-ped-item-import.codigoitem).

    ASSIGN tt-ped-item-import.QuantidadeDisponivel  = deDisponivel.

    ASSIGN  
            tt-ped-item.qt-pedida           = IF (deDisponivel <> 0 AND deDisponivel < tt-ped-item-import.QuantidadePedida) THEN deDisponivel ELSE tt-ped-item-import.QuantidadePedida  //tt-ped-item-import.QuantidadePedida
            tt-ped-item.qt-un-fat           = tt-ped-item.qt-pedida
            tt-ped-item.vl-preori           = IF tt-ped-item-import.ValorUnitario <> 0 AND tt-ped-item-import.ValorUnitario <> ? THEN tt-ped-item-import.ValorUnitario ELSE obterPrecoItem()
            tt-ped-item.vl-preuni           = tt-ped-item.vl-preori
            tt-ped-item.vl-pretab           = tt-ped-item.vl-preori
            tt-ped-item.vl-preori-un-fat    = tt-ped-item.vl-preori
            tt-ped-item.dt-entorig          = tt-ped-venda.dt-entorig  
            tt-ped-item.dt-entrega          = tt-ped-venda.dt-entrega  
            tt-ped-item.cod-entrega         = tt-ped-venda.cod-entrega
            tt-ped-item.observacao          = ""
            tt-ped-item.tipo-atend          = es_api_param_ped.tipo_atend
            tt-ped-item.aliquota-ipi        = ITEM.aliquota-ipi
            tt-ped-item.cod-unid-negoc      = item.cod-unid-negoc // if avail item-uni-estab then item-uni-estab.cod-unid-negoc else ""
            tt-ped-item.ind-icm-ret         = natur-oper.subs-trib
            tt-ped-item.dec-2               = 0
            SUBSTR(tt-ped-item.char-2,1,8)  = ITEM.class-fiscal
            SUBSTR(tt-ped-item.char-2,9,2)  = ITEM.un
            tt-ped-item.des-un-medida       = ITEM.un
            tt-ped-item.nr-tabpre           = tt-ped-venda.nr-tabpre
            tt-ped-item.user-impl           = es_api_param_ped.user_impl
            tt-ped-item.val-pct-desconto-tab-preco  = IF AVAIL es-item AND es-item.perc-desc <> 0 
                                                      THEN es-item.perc-desc 
                                                      ELSE IF emitente.bonificacao <> 0 
                                                          THEN emitente.bonificacao
                                                          ELSE  IF AVAIL preco-item 
                                                                THEN preco-item.desco-quant 
                                                                ELSE 0 
                .
            
    /* Efetiva as informacoes da tabela ped-item */
    EMPTY TEMP-TABLE RowErrors.
    RUN openQueryStatic IN h_bodi154 (INPUT "Default":U).
    RUN inputRowParam   IN h_bodi154 (INPUT TABLE tt-ped-param).
    RUN emptyRowErrors  IN h_bodi154.
    RUN setrecord       IN h_bodi154 (INPUT TABLE tt-ped-item).
    RUN createRecord    IN h_bodi154.
    RUN getRowErrors    IN h_bodi154 (OUTPUT TABLE RowErrors).

    IF CAN-FIND(FIRST RowErrors 
                WHERE RowErrors.ErrorSubType begins "Erro":U) THEN DO:
        RUN gerarRowError("Erro ao gerar item do pedido de venda").
        RETURN "NOK".
    END.

    RUN getRowid IN h_bodi154 (OUTPUT tt-ped-item.r-rowid).
    ASSIGN tt-ped-item-import.r-rowid = tt-ped-item.r-rowid.

/*                                                                        */
/*     /* Calcula Pedido */                                               */
/*     RUN calculateOrder IN h_bodi159cal (INPUT tt-ped-venda.r-rowid).   */
/*     IF CAN-FIND(FIRST RowErrors                                        */
/*                 WHERE RowErrors.ErrorSubType begins "Erro":U) THEN DO: */
/*         RUN gerarRowError("Erro ao calcular pedido").                  */
/*         RETURN "NOK".                                                  */
/*     END.                                                               */

    RETURN "OK".


END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE criarPedidoBO:
DEFINE INPUT  PARAMETER pPedido     AS ROWID       NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-ped-item-import.
DEFINE OUTPUT PARAMETER pChave      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE deValTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cNatOper AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iMensagem AS INTEGER NO-UNDO.
DEFINE VARIABLE deDisponivel AS DECIMAL     NO-UNDO.


/* DEFINE INPUT  PARAMETER pCodEstabel  LIKE estabelec.cod-estabel NO-UNDO.    */
/* DEFINE INPUT  PARAMETER pCodEmitente    LIKE emitente.cod-emitente NO-UNDO. */
/* DEFINE INPUT  PARAMETER TABLE FOR tt-item-pedido.                           */
/* DEFINE OUTPUT PARAMETER p-nr-pedido LIKE ped-venda.nr-pedido NO-UNDO. */

/*     FOR FIRST estabelec NO-LOCK                          */
/*         WHERE estabelec.cod-estabel     = pCodEstabel :  */
/*     END.                                                 */
/*                                                          */
/*     FOR FIRST emitente NO-LOCK                           */
/*          WHERE emitente.cod-emitente    = pCodEmitente : */
/*     END.                                                 */
/*                                                          */
/*     FOR FIRST repres NO-LOCK                             */
/*          WHERE repres.cod-rep = emitente.cod-rep :       */
/*     END.                                                 */    

/*     FOR FIRST tt-dados:                              */
/*     END.                                             */
/*     IF NOT AVAIL tt-dados THEN                       */
/*     DO:                                              */
/*         RUN gerarRowError("Nenhum item encontrado"). */
/*         RETURN "NOK".                                */
/*     END.                                             */

    FOR FIRST b-ped-venda-orig NO-LOCK WHERE
              ROWID(b-ped-venda-orig)     = pPedido:
    END.
    IF NOT AVAIL b-ped-venda-orig THEN
    DO:
        RUN gerarRowError( "Pedido de Venda n∆o encontrado para geraá∆o de BACK ORDER." ).
        RETURN "NOK".        
    END.

/*     FOR FIRST tt-ped-venda-import:                              */
/*     END.                                                        */
/*     IF NOT AVAIL tt-ped-venda-import THEN                       */
/*     DO:                                                         */
/*         RUN gerarRowError( "Pedido de venda n∆o encontrado." ). */
/*         RETURN "NOK".                                           */
/*     END.                                                        */
/*                                                                 */
    FOR FIRST es_api_param_ped NO-LOCK:
    END.
    IF NOT AVAIL es_api_param_ped THEN
    DO:
        RUN gerarRowError( "Parametros de integraá∆o n∆o cadastrados." ).
        RETURN "NOK".        
    END.

    FOR FIRST emitente NO-LOCK
         WHERE emitente.nome-abrev = b-ped-venda-orig.nome-abrev :
    END.
    IF NOT AVAIL emitente THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Emitente n∆o encontrado (CNPJ: &1)", STRING(emitente.cod-emitente)) ).
        RETURN "NOK".        
    END.
    

    FOR FIRST estabelec NO-LOCK WHERE
              estabelec.cod-estabel     = b-ped-venda-orig.cod-estabel:
    END.
    IF NOT AVAIL estabelec THEN
    DO:
        RUN gerarRowError( "Estabelecimento n∆o encontrado" ).
        RETURN "NOK".        
    END.
    


    FOR FIRST repres NO-LOCK                         
         WHERE repres.nome-abrev  = b-ped-venda-orig.no-ab-reppri :   
    END.              

    IF NOT AVAIL repres THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Representante n∆o encontrado (Cod: &1)", emitente.cod-rep) ).
        RETURN "NOK".        
    END.

    FOR FIRST loc-entr NO-LOCK WHERE
              loc-entr.nome-abrev       = emitente.nome-abrev
          AND loc-entr.cod-entrega      = emitente.cod-entrega:
    END.
    IF NOT AVAIL loc-entr THEN
    DO:
        RUN gerarRowError( "Local de entrega n∆o encontrado" ).
        RETURN "NOK".        
    END.
    
    FOR FIRST transporte NO-LOCK WHERE
              transporte.cod-transp    = emitente.cod-transp:
    END.
    IF pedidoVenda() AND NOT AVAIL transporte THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Transportador n∆o encontrado") ).
        RETURN "NOK".        
    END.

    FOR FIRST transporte NO-LOCK WHERE
              transporte.nome-abrev  = b-ped-venda-orig.nome-transp:
    END.

    FOR FIRST natur-oper NO-LOCK WHERE
           natur-oper.nat-oper   =  "" // obterNaturezaOperacao() 
        :
    END.


    CRIAR_PED_VENDA:
    DO TRANS: 


        // Obtem dados dos itens do pedido parra definiá∆o de campos
        ASSIGN deValTotal = 0.
        cNatOper = ?.
        ASSIGN i-nr-sequencia = 0.
        FOR EACH tt-ped-item-import WHERE NOT tt-ped-item-import.flgrestricao:
            
            FOR FIRST ITEM NO-LOCK WHERE
                      ITEM.it-codigo        = tt-ped-item-import.codigoItem:
            END.
            IF NOT AVAIL ITEM THEN
            DO:
                RUN gerarRowError(SUBSTITUTE("Item n∆o encontrado (&1)", tt-ped-item-import.CodigoItem)).
                RETURN "NOK".
            END.

            ASSIGN tt-ped-item-import.nat-oper  = obterNaturezaOperacao  (INPUT STRING(ITEM.ge-codigo),
                                                                          INPUT (IF itemTributado(ITEM.it-codigo) THEN 1 ELSE 2) ,
                                                                          INPUT 1, // (IF tt-ped-venda-import.tipoPedido = "S" OR tt-ped-venda-import.tipoPedido = "E" THEN 1 ELSE 2),  // DOAÄ«O N«O TEM BO
                                                                          INPUT estabelec.estado,
                                                                          INPUT emitente.estado
                                                                          ) .

            IF cNatOper = ? THEN 
            DO:
                FOR FIRST natur-oper NO-LOCK WHERE
                          natur-oper.nat-oper   = tt-ped-item-import.nat-oper:
                END.
                IF NOT AVAIL natur-oper THEN
                DO:
                    RUN gerarRowError( SUBSTITUTE("Natureza de Operaá∆o n∆o encontrada") ).
                    RETURN "NOK".        
                END.

                ASSIGN  cNatOper   = tt-ped-item-import.nat-oper
                        iMensagem  = natur-oper.cod-mensagem.


            END.

            ASSIGN i-nr-sequencia = i-nr-sequencia + 10.
            ASSIGN tt-ped-item-import.numSeq    = i-nr-sequencia.

        END.

/*         IF NOT CAN-FIND(FIRST tt-ped-item-import WHERE NOT tt-ped-item-import.flgRestricao) THEN */
/*         DO:                                                                                      */
/*             RETURN "OK".                                                                         */
/*         END.                                                                                     */

        /* Cria tabela de parametros para criacao do pedido de venda/item do pedido de venda */
        CREATE tt-ped-param. /* para uso da BO */
        ASSIGN tt-ped-param.relacao-item-cli     = YES
               tt-ped-param.tp-relacao-item-cli  = 1
               tt-ped-param.qtde-un-medida-cli   = YES
               tt-ped-param.multiplicar-qtde     = YES
               tt-ped-param.atribuir-preco-comp  = NO
               tt-ped-param.tp-exp-nat-oper      = 2  
               tt-ped-param.tp-exp-dt-entrega    = 1
               tt-ped-param.exp-nat-cons-final   = NO
               tt-ped-param.exp-nat-cod-mensagem = YES
               tt-ped-param.atualizar-entregas   = YES
               tt-ped-param.arredondar-qtde-lote = NO
               tt-ped-param.gerar-proc-exp       = NO
               tt-ped-param.itinerario           = 1.
                
        /* Retorna Numero do Pedido de Venda do cliente */
        RUN setDefaultOrderNumber IN h_bodi159sdf (OUTPUT i-nr-pedido). 
        
        /* Cria tabela temporaria do pedido de venda */
        CREATE tt-ped-venda.
        ASSIGN tt-ped-venda.log-cotacao             = NO
               tt-ped-venda.nome-abrev              = emitente.nome-abrev
               tt-ped-venda.log-usa-tabela-desconto = NO
               tt-ped-venda.nr-pedido               = i-nr-pedido
               tt-ped-venda.nr-pedcli               = obterNrPedcli(emitente.nome-abrev , "", "b").


        //tt-ped-venda-import.nr-pedcli       = tt-ped-venda.nr-pedcli
            
        /* Atualizacao de registros */
        RUN inputTable IN h_bodi159sdf (INPUT TABLE tt-ped-venda).
        RUN setDefaultCustomer IN h_bodi159sdf.
        RUN outputTable IN h_bodi159sdf (OUTPUT TABLE tt-ped-venda).
        FIND FIRST tt-ped-venda NO-ERROR.
            

        ASSIGN tt-ped-venda.cod-cond-pag     = b-ped-venda-orig.cod-cond-pag
               tt-ped-venda.nat-operacao     = b-ped-venda-orig.nat-operacao     
               tt-ped-venda.nr-tabpre        = b-ped-venda-orig.nr-tabpre        
               tt-ped-venda.cod-canal-venda  = 0
               tt-ped-venda.nr-tab-finan     = b-ped-venda-orig.nr-tab-finan
               tt-ped-venda.vl-tot-ped       = 0                                                                           
               tt-ped-venda.no-ab-reppri     = b-ped-venda-orig.no-ab-reppri 
               tt-ped-venda.cod-des-merc     = b-ped-venda-orig.cod-des-merc 
               tt-ped-venda.mo-codigo        = 0                                                                           
               tt-ped-venda.nr-pedrep        = b-ped-venda-orig.nr-pedrep 
               tt-ped-venda.contato          = ""                                                                          
               tt-ped-venda.cod-portador     = b-ped-venda-orig.cod-portador                                                           
               tt-ped-venda.modalidade       = b-ped-venda-orig.modalidade                                                         
               tt-ped-venda.observacoes      = b-ped-venda-orig.observacoes
               tt-ped-venda.cond-espec       = ""                                                                          
               tt-ped-venda.cond-redespa     = ""                                                                          
               tt-ped-venda.tp-pedido        = b-ped-venda-orig.tp-pedido
               tt-ped-venda.ind-tp-frete     = b-ped-venda-orig.ind-tp-frete   
               tt-ped-venda.nome-transp      = b-ped-venda-orig.nome-transp    
               tt-ped-venda.dt-implant       = TODAY                                                                       
               tt-ped-venda.cod-entrega      = b-ped-venda-orig.cod-entrega                                                        
               tt-ped-venda.cgc              = b-ped-venda-orig.cgc                                              
               tt-ped-venda.ins-estadual     = b-ped-venda-orig.ins-estadual                                     
               tt-ped-venda.cep              = b-ped-venda-orig.cep                                              
               tt-ped-venda.local-entreg     = b-ped-venda-orig.local-entreg                                     
               tt-ped-venda.bairro           = b-ped-venda-orig.bairro                                           
               tt-ped-venda.cidade           = b-ped-venda-orig.cidade                                           
               tt-ped-venda.cidade-cif       = b-ped-venda-orig.cidade-cif                                       
               tt-ped-venda.estado           = b-ped-venda-orig.estado                                           
               tt-ped-venda.pais             = b-ped-venda-orig.pais                                               
               tt-ped-venda.dt-entorig       = TODAY
               tt-ped-venda.dt-entrega       = TODAY // tt-ped-venda.dt-entorig
               tt-ped-venda.user-impl        = es_api_param_ped.USER_impl
               tt-ped-venda.cod-mensagem     = b-ped-venda-orig.cod-mensagem    
               //tt-ped-venda.cod-ped-clien-mp = emitente.cod-ped-clien-mp                                                  .
                   .

/*         // Completa observaá‰es                                                                                                                       */
/*         IF tt-ped-venda-import.NumeroPedidoSistemaCliente <> "" THEN                                                                                  */
/*             ASSIGN tt-ped-venda.Observacoes = tt-ped-venda.Observacoes + " | Pedido Cliente: " + tt-ped-venda-import.NumeroPedidoSistemaCliente.      */
/*         IF tt-ped-venda-import.DataEntrega <> ? THEN                                                                                                  */
/*             ASSIGN tt-ped-venda.observacoes = tt-ped-venda.observacoes + " | Entregar Dia: " + string(tt-ped-venda-import.DataEntrega, "99/99/9999"). */
/*         IF tt-ped-venda-import.Endereco <> ? AND tt-ped-venda-import.Endereco <> "" THEN                                                              */
/*             ASSIGN tt-ped-venda.observacoes = tt-ped-venda.observacoes + " | Endereáo: " + tt-ped-venda-import.Endereco.                              */



     
        /* Completa dados de ped-venda */
        ASSIGN tt-ped-venda.cod-estabel   = b-ped-venda-orig.cod-estabel
               // tt-ped-venda.tp-preco      = 1       /* Informado */
               tt-ped-venda.tp-preco      = b-ped-venda-orig.tp-preco
               tt-ped-venda.nr-ind-finan  = 0
               tt-ped-venda.perc-desco1   = 0

            // tt-ped-venda.cod-cond-pag = ""
            //tt-ped-venda.nome-abrev = ?

               .
        
        /* Efetiva as informacoes da tabela temporaria na tabela ped-venda */
        EMPTY TEMP-TABLE RowErrors.
        RUN emptyRowErrors  IN h_bodi159.
        RUN openQueryStatic IN h_bodi159 (INPUT "Default":U).
        RUN setrecord       IN h_bodi159 (INPUT TABLE tt-ped-venda).
        RUN createRecord    IN h_bodi159.
        RUN getRowErrors    IN h_bodi159 (OUTPUT TABLE RowErrors).
    
        IF CAN-FIND(FIRST RowErrors 
                    WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:
            RUN gerarRowError("Houve erro ao tentar gerar o pedido.").
            UNDO CRIAR_PED_VENDA, RETURN "NOK".
        END.
    
        /* Gera Representante */
        FIND FIRST tt-ped-venda NO-ERROR.
        RUN getRowid IN h_bodi159 (OUTPUT tt-ped-venda.r-rowid).
    
        CREATE tt-ped-repre.
        ASSIGN tt-ped-repre.nr-pedido   = tt-ped-venda.nr-pedido
               tt-ped-repre.nome-ab-rep = repres.nome-abrev
               tt-ped-repre.perc-comis  = repres.comis-direta
               tt-ped-repre.comis-emis  = repres.comis-emis
               tt-ped-repre.ind-repbase = YES.
    
        EMPTY TEMP-TABLE RowErrors.
        RUN emptyRowErrors  IN h_bodi157.
        RUN openQueryStatic IN h_bodi157(INPUT "DefaultPd4000":U).
        RUN setRecord       IN h_bodi157(INPUT TABLE tt-ped-repre).
        RUN createRecord    IN h_bodi157.
        RUN getRowErrors    IN h_bodi157(OUTPUT TABLE RowErrors).
        
        IF CAN-FIND(FIRST RowErrors
                    WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:
            RUN gerarRowerror("Houve erro ao tentar gerar o pedido.").
            UNDO CRIAR_PED_VENDA, RETURN "NOK".
        END.
       
        /* Cria Item de Pedido */
        //ASSIGN i-nr-sequencia = 0.
        //FOR EACH b-ped-item-orig OF b-ped-venda-orig WHERE b-ped-item-orig.cod-sit-item = 6 NO-LOCK :

/*                                                                                                                                                                                       */
/*         FOR EACH tt-ped-item :                                                                                                                                                        */
/*                                                                                                                                                                                       */
/*             FOR FIRST b-ped-item-orig NO-LOCK WHERE                                                                                                                                   */
/*                 // ROWID(b-ped-item-orig)     = tt-ped-item.r-rowid :                                                                                                                 */
/*                 b-ped-item.nome-abrev       = b-ped-venda-orig.nome-abrev                                                                                                             */
/*                 b-ped-item.nr-pedcli        = b-ped-venda-orig.nr-pedcli                                                                                                              */
/*                 b-ped-item.nr-sequencia     = tt-ped-item.nr-sequencia                                                                                                                */
/*                 b-ped-item.it-codigo        = tt-ped-item.it-codigo:                                                                                                                  */
/*             END.                                                                                                                                                                      */
/*             IF NOT AVAIL b-ped-item-orig THEN                                                                                                                                         */
/*             DO:                                                                                                                                                                       */
/*                 RUN gerarRowError("Item do pedido n∆o encontrado ao processar BO. ").                                                                                                 */
/*                 RETURN "NOK".                                                                                                                                                         */
/*             END.                                                                                                                                                                      */
/*                                                                                                                                                                                       */
/*                                                                                                                                                                                       */
/*             FOR FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = b-ped-item-orig.it-codigo:                                                                                                  */
/*             END.                                                                                                                                                                      */
/*             IF NOT AVAIL ITEM THEN                                                                                                                                                    */
/*             DO:                                                                                                                                                                       */
/*                 RUN gerarRowError(SUBSTITUTE("Item n∆o encontrado (&1)", b-ped-item-orig.it-codigo)).                                                                                 */
/*                 RETURN "NOK".                                                                                                                                                         */
/*             END.                                                                                                                                                                      */
/*                                                                                                                                                                                       */
/*             FOR FIRST es-item NO-LOCK WHERE                                                                                                                                           */
/*                       es-item.it-codigo = ITEM.it-codigo:                                                                                                                             */
/*             END.                                                                                                                                                                      */
/*                                                                                                                                                                                       */
/*            // ASSIGN i-nr-sequencia = i-nr-sequencia + 10.                                                                                                                            */
/*                                                                                                                                                                                       */
/*             EMPTY TEMP-TABLE tt-ped-item-aux.                                                                                                                                         */
/*                                                                                                                                                                                       */
/*             /*  cria temporaria de item */                                                                                                                                            */
/*             CREATE  tt-ped-item-aux.                                                                                                                                                  */
/*             ASSIGN  tt-ped-item-aux.it-codigo           = b-ped-item-orig.it-codigo                                                                                                   */
/*                     tt-ped-item-aux.nome-abrev          = b-ped-item-orig.nome-abrev                                                                                                  */
/*                     tt-ped-item-aux.nr-pedcli           = tt-ped-venda.nr-pedcli                                                                                                      */
/*                     tt-ped-item-aux.nr-sequencia        = b-ped-item-orig.nr-sequencia                                                                                                */
/*                     tt-ped-item-aux.nat-operacao        = b-ped-item-orig.nat-operacao                                                                                                */
/*                     .                                                                                                                                                                 */
/*                                                                                                                                                                                       */
/* /*             FOR FIRST natur-oper NO-LOCK WHERE                                                                                             */                                      */
/* /*                       natur-oper.nat-oper   = tt-ped-item-import.nat-oper:                                                                 */                                      */
/* /*             END.                                                                                                                           */                                      */
/* /*             IF NOT AVAIL natur-oper THEN                                                                                                   */                                      */
/* /*             DO:                                                                                                                            */                                      */
/* /*                 RUN gerarRowError(SUBSTITUTE("Natureza de Operaá∆o do Item importado n∆o encontrada (&1)" , tt-ped-item-import.nat-oper)). */                                      */
/* /*                 RETURN "NOK".                                                                                                              */                                      */
/* /*             END.                                                                                                                           */                                      */
/*                                                                                                                                                                                       */
/*             /* executa criacao dos defaults */                                                                                                                                        */
/*             RUN inputParentTable          IN h_bodi154sdf (INPUT TABLE tt-ped-venda).                                                                                                 */
/*             RUN inputTable                IN h_bodi154sdf (INPUT TABLE tt-ped-item-aux).                                                                                              */
/*             RUN setDefaultItem            IN h_bodi154sdf.                                                                                                                            */
/*             RUN outputTable               IN h_bodi154sdf (OUTPUT TABLE tt-ped-item-aux).                                                                                             */
/*             FIND FIRST tt-ped-item-aux NO-ERROR.                                                                                                                                      */
/*                                                                                                                                                                                       */
/*             ASSIGN deDisponivel     = obterSaldoItem(b-ped-item-orig.it-codigo).                                                                                                      */
/*                                                                                                                                                                                       */
/*             ASSIGN                                                                                                                                                                    */
/*                     tt-ped-item-aux.qt-pedida                   = IF (deDisponivel <> 0 OR deDisponivel < b-ped-item-orig.qt-pedida) THEN deDisponivel ELSE b-ped-item-orig.qt-pedida */
/*                     tt-ped-item-aux.qt-un-fat                   = tt-ped-item-aux.qt-pedida                                                                                           */
/*                     tt-ped-item-aux.vl-preori                   = b-ped-item-orig.vl-preori                                                                                           */
/*                     tt-ped-item-aux.vl-preuni                   = b-ped-item-orig.vl-preuni                                                                                           */
/*                     tt-ped-item-aux.vl-pretab                   = b-ped-item-orig.vl-pretab                                                                                           */
/*                     tt-ped-item-aux.vl-preori-un-fat            = b-ped-item-orig.vl-preori-un-fat                                                                                    */
/*                     tt-ped-item-aux.dt-entorig                  = b-ped-item-orig.dt-entorig                                                                                          */
/*                     tt-ped-item-aux.dt-entrega                  = TODAY // b-ped-item-orig.dt-entrega                                                                                 */
/*                     tt-ped-item-aux.cod-entrega                 = b-ped-item-orig.cod-entrega                                                                                         */
/*                     tt-ped-item-aux.observacao                  = b-ped-item-orig.observacao                                                                                          */
/*                     tt-ped-item-aux.tipo-atend                  = b-ped-item-orig.tipo-atend                                                                                          */
/*                     tt-ped-item-aux.aliquota-ipi                = b-ped-item-orig.aliquota-ipi                                                                                        */
/*                     tt-ped-item-aux.cod-unid-negoc              = b-ped-item-orig.cod-unid-negoc                                                                                      */
/*                     tt-ped-item-aux.ind-icm-ret                 = b-ped-item-orig.ind-icm-ret                                                                                         */
/*                     tt-ped-item-aux.dec-2                       = b-ped-item-orig.dec-2                                                                                               */
/*                     SUBSTR(tt-ped-item-aux.char-2,1,8)          = SUBSTR(b-ped-item-orig.char-2,1,8)                                                                                  */
/*                     SUBSTR(tt-ped-item-aux.char-2,9,2)          = SUBSTR(b-ped-item-orig.char-2,9,2)                                                                                  */
/*                     tt-ped-item-aux.des-un-medida               = b-ped-item-orig.des-un-medida                                                                                       */
/*                     tt-ped-item-aux.nr-tabpre                   = b-ped-item-orig.nr-tabpre                                                                                           */
/*                     tt-ped-item-aux.user-impl                   = b-ped-item-orig.user-impl                                                                                           */
/*                     tt-ped-item-aux.val-pct-desconto-tab-preco  = b-ped-item-orig.val-pct-desconto-tab-preco                                                                          */
/*                                                                                                                                                                                       */
/*                                                                                                                                                                                       */
/*                                                                                                                                                                                       */
/*                                                                                                                                                                                       */
/*                                                                                                                                                                                       */
/*                                                                                                                                                                                       */
/*                         .                                                                                                                                                             */
/*                                                                                                                                                                                       */
/*             /* Efetiva as informacoes da tabela ped-item */                                                                                                                           */
/*             EMPTY TEMP-TABLE RowErrors.                                                                                                                                               */
/*             RUN openQueryStatic IN h_bodi154 (INPUT "Default":U).                                                                                                                     */
/*             RUN inputRowParam   IN h_bodi154 (INPUT TABLE tt-ped-param).                                                                                                              */
/*             RUN emptyRowErrors  IN h_bodi154.                                                                                                                                         */
/*             RUN setrecord       IN h_bodi154 (INPUT TABLE tt-ped-item-aux).                                                                                                           */
/*             RUN createRecord    IN h_bodi154.                                                                                                                                         */
/*             RUN getRowErrors    IN h_bodi154 (OUTPUT TABLE RowErrors).                                                                                                                */
/*                                                                                                                                                                                       */
/*             IF CAN-FIND(FIRST RowErrors                                                                                                                                               */
/*                         WHERE RowErrors.ErrorSubType begins "Erro":U) THEN DO:                                                                                                        */
/*                 RUN gerarRowError("Erro ao gerar item do pedido de venda").                                                                                                           */
/*                 UNDO CRIAR_PED_VENDA, RETURN "NOK".                                                                                                                                   */
/*             END.                                                                                                                                                                      */
/*                                                                                                                                                                                       */
/*             RUN getRowid IN h_bodi154 (OUTPUT tt-ped-item-aux.r-rowid).                                                                                                               */
/*             // ASSIGN tt-ped-item-import.r-rowid = tt-ped-item.r-rowid.                                                                                                               */
/*                                                                                                                                                                                       */
/*                                                                                                                                                                                       */
/*                                                                                                                                                                                       */
/*             /* Calcula Pedido */                                                                                                                                                      */
/*             RUN calculateOrder IN h_bodi159cal (INPUT tt-ped-venda.r-rowid).                                                                                                          */
/*             IF CAN-FIND(FIRST RowErrors                                                                                                                                               */
/*                         WHERE RowErrors.ErrorSubType begins "Erro":U) THEN DO:                                                                                                        */
/*                 RUN gerarRowError("Erro ao calcular pedido").                                                                                                                         */
/*                 UNDO CRIAR_PED_VENDA, RETURN "NOK".                                                                                                                                   */
/*             END.                                                                                                                                                                      */
/*                                                                                                                                                                                       */
/*         END.                                                                                                                                                                          */

        /* Cria Item de Pedido */
        //ASSIGN i-nr-sequencia = 0.
        FOR EACH tt-ped-item-import NO-LOCK WHERE NOT tt-ped-item-import.flgrestricao:
    
            RUN criarItemPedido.
            IF RETURN-VALUE <> "OK" THEN
            DO:

                RETURN "NOK".
            END.


        END.




/*         /* Completa Pedido */                                                                       */
/*         IF NOT tt-ped-venda.completo THEN DO:                                                       */
/*             RUN completeOrder IN h_bodi159com (INPUT tt-ped-venda.r-rowid, OUTPUT TABLE rowErrors). */
/*             IF CAN-FIND (FIRST RowErrors                                                            */
/*                          WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:                     */
/*                 RUN gerarRowError("Erro ao completar pedido").                                      */
/*                 UNDO CRIAR_PED_VENDA, RETURN "NOK".                                                 */
/*             END.                                                                                    */
/*         END.                                                                                        */

        ASSIGN pChave       = tt-ped-venda.nome-abrev + "|" + tt-ped-venda.nr-pedcli .

        


//        ASSIGN p-nr-pedido = tt-ped-venda.nr-pedido. 
/*         UNDO Implanta, RETURN.  */
    END.

    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE criarExtensoesSemIntegracao:
DEFINE VARIABLE cPrefixo AS CHARACTER   NO-UNDO.

    FOR FIRST tt-ped-item-import:
    END.
    IF NOT AVAIL tt-ped-item-import THEN
    DO:
        RUN gerarRowError("Nenhum item encontrado").
        RETURN "NOK".
    END.

    IF NOT AVAIL tt-ped-venda-import THEN
    DO:
        RUN gerarRowError("Pedido de venda importado n∆o encontrado para gerar extens‰es").
        RETURN "NOK".
    END.

    IF AVAIL emitente  THEN
        RELEASE emitente.
    FOR FIRST emitente NO-LOCK WHERE FALSE:
    END.
    FOR FIRST emitente NO-LOCK WHERE
              emitente.cod-emitente    = tt-ped-venda-import.codigoEmitente:
    END.
    IF NOT AVAIL emitente THEN
    DO:
        DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
        FOR EACH emitente NO-LOCK WHERE
                 emitente.cgc       = tt-ped-venda-import.cnpj:
            iCont = iCont + 1.
        END.
        IF iCont > 1 THEN
        DO:
            RUN gerarRowError(SUBSTITUTE("Encontrado mais de um Emitente com mesmo CNPJ (Importados: C¢digo:&1 | CNPJ:&2) ", tt-ped-venda-import.codigoEmitente, tt-ped-venda-import.cnpj)).
            RETURN "NOK".
        END.

        FOR FIRST emitente NO-LOCK WHERE
                  emitente.cgc     = tt-ped-venda-import.cnpj:
        END.
    END.

    FOR FIRST ext-emitente NO-LOCK WHERE
              ext-emitente.cod-emitente     = emitente.cod-emitente:
    END.
    IF NOT AVAIL ext-emitente THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Extens∆o do emitente n∆o encontrada (Emitente: &1)", emitente.cod-emitente) ).
        RETURN "NOK".        
    END.


    FOR EACH 
        tt-ped-item-import WHERE tt-ped-item-import.flgRestricao
        //tt-ped-item
        :
        
/*         CASE tt-dados.tip-pedido :                      */
/*             WHEN 1 THEN     ASSIGN cPrefixo     = "WS". */
/*             WHEN 2 THEN     ASSIGN cPrefixo     = "WD". */
/*         END CASE.                                       */



        IF tt-ped-venda-import.tipoPedido <> "D" THEN 
        DO:

            CREATE web-ped-venda.
            ASSIGN 
                web-ped-venda.cod-estabel                = ext-emitente.cod_estabel
                web-ped-venda.arquivo-descricao          = "Pedido Web/REST API"         // N∆o h† mais arquivo
                web-ped-venda.nome-abrev                 = emitente.nome-abrev
                web-ped-venda.nr-pedcli                  = obterNrPedcli(emitente.nome-abrev , tt-ped-venda-import.NumeroPedidoCliente, tt-ped-venda-import.tipoPedido)
                web-ped-venda.nr-sequencia               = tt-ped-item-import.numSeq
                web-ped-venda.nr-nota-fisc               = ""
                web-ped-venda.serie                      = "" 
                web-ped-venda.nr-seq-fat                 = 0 
                web-ped-venda.situacao                   = "IMPORTADO" 
                web-ped-venda.dt-emis-ped-venda          = TODAY 
                web-ped-venda.dt-emis-nota-fiscal        = ? 
                web-ped-venda.log-saldo-fisico           = FALSE 
                web-ped-venda.dat-saldo-fisico           = ?
                web-ped-venda.aprov-saldo-fis            = "" 
                web-ped-venda.quantidade                 = tt-ped-item-import.QuantidadePedida
                web-ped-venda.it-codigo                  = tt-ped-item-import.CodigoItem
                web-ped-venda.qtde-saldo-fisico          = 0
                web-ped-venda.dt-gera-pedido             = IF AVAIL tt-ped-venda-import  AND tt-ped-venda-import.DataHoraEnvio <> ? AND tt-ped-venda-import.DataHoraEnvio <> ""                                                              
                                                            THEN DATE(INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 6, 2)) , INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 9, 2)) , INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 1, 4)) )   
                                                            ELSE TODAY                                                                                                                                                                       
                web-ped-venda.hr-pedido-web              = IF AVAIL tt-ped-venda-import  AND tt-ped-venda-import.DataHoraEnvio <> ? AND tt-ped-venda-import.DataHoraEnvio <> ""                                                              
                                                           THEN SUBSTR(tt-ped-venda-import.DataHoraEnvio, 12, 5)                                                                                                                             
                                                           ELSE STRING(TIME, "HH:MM")                                                                                                                                                        
                web-ped-venda.vlr-unit                   = tt-ped-item-import.ValorUnitario
                web-ped-venda.e-mail                     = tt-ped-venda-import.email
                web-ped-venda.telefone                   = tt-ped-venda-import.telefone
                web-ped-venda.cnpj                       = tt-ped-venda-import.cnpj
                web-ped-venda.nr-conhecimento            = ""
                web-ped-venda.dt-embarque                = ?
                web-ped-venda.nro-pedido                 = tt-ped-venda-import.NumeroPedidoSistemaCliente
                web-ped-venda.flg_restricao              = tt-ped-item-import.flgRestricao
                web-ped-venda.des_restricao              = tt-ped-item-import.desRestricao
                web-ped-venda.qtd_importada              = tt-ped-item-import.QuantidadePedida
                //web-ped-venda.cond-pag             = .
                .

        END. // IF tt-ped-venda-import.tipopedido = S 

        IF tt-ped-venda-import.tipoPedido = "D" THEN 
        DO:
    
            CREATE  web-ped-doacao.
            ASSIGN  web-ped-doacao.cod-estabel          = ext-emitente.cod_estabel                                                                                                                  
                    web-ped-doacao.arquivo-descricao    = "Pedido Web/REST API"           // N∆o h† mais arquivo                                                                                                                    
                    web-ped-doacao.nome-abrev           = emitente.nome-abrev
                    web-ped-doacao.natureza             = IF tt-ped-venda-import.natureza = "F" THEN 1 ELSE 2                                                                                            
                    web-ped-doacao.nr-pedcli            = obterNrPedcli(emitente.nome-abrev , tt-ped-venda-import.NumeroPedidoCliente, tt-ped-venda-import.tipoPedido)
                    web-ped-doacao.nr-sequencia         = tt-ped-item-import.numSeq                                                                                                                                              
                    web-ped-doacao.nr-nota-fisc         = ""                                                                                                                                 
                    web-ped-doacao.serie                = ""                                                                                                                                 
                    web-ped-doacao.nr-seq-fat           = 0                                                                                                                                  
                    web-ped-doacao.situacao             = "IMPORTADO"                                                                                                                        
                    web-ped-doacao.dt-emis-ped-venda    = TODAY /* ? */                                                                                                                      
                    web-ped-doacao.dt-emis-nota-fiscal  = ?                                                                                                                                  
                    web-ped-doacao.log-saldo-fisico     = FALSE                                                                                                                              
                    web-ped-doacao.dat-saldo-fisico     = ?                                                                                                                                  
                    web-ped-doacao.aprov-saldo-fis      = ""                                                                                                                                 
                    web-ped-doacao.quantidade           = tt-ped-item-import.QuantidadePedida                                                                                                     
                    web-ped-doacao.it-codigo            = tt-ped-item-import.CodigoItem                                                                                                                      
                    web-ped-doacao.qtde-saldo-fisico    = 0                                                                                                                                  
                    web-ped-doacao.dt-gera-pedido       = IF AVAIL tt-ped-venda-import  AND tt-ped-venda-import.DataHoraEnvio <> ? AND tt-ped-venda-import.DataHoraEnvio <> ""                                                                
                                                           THEN DATE(INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 6, 2)) , INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 9, 2)) , INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 1, 4)) )     
                                                           ELSE TODAY                                                                                                                                                                         
                    web-ped-doacao.hr-pedido-web        = IF AVAIL tt-ped-venda-import  AND tt-ped-venda-import.DataHoraEnvio <> ? AND tt-ped-venda-import.DataHoraEnvio <> ""                                                                
                                                          THEN SUBSTR(tt-ped-venda-import.DataHoraEnvio, 12, 5)                                                                                                                               
                                                          ELSE STRING(TIME, "HH:MM")                                                                                                                                                          
                    web-ped-doacao.vlr-unit             = tt-ped-item-import.ValorUnitario                                                                                                       
                    web-ped-doacao.e-mail               = tt-ped-venda-import.email                                                                                                              
                    web-ped-doacao.telefone             = tt-ped-venda-import.telefone                                                                                                           
                    web-ped-doacao.cnpj                 = tt-ped-venda-import.cnpj                                                                                                               
                    web-ped-doacao.nr-conhecimento      = ""                                                                                                                                 
                    web-ped-doacao.dt-embarque          = ?                                                                                                                                  
                    web-ped-doacao.nro-pedido           = tt-ped-venda-import.NumeroPedidoSistemaCliente                                                                                            
                    web-ped-doacao.cod-rep              = IF AVAIL repres THEN repres.cod-rep ELSE 1                                                                                         
                    web-ped-doacao.end-entrega          = tt-ped-venda-import.cnpj
                    web-ped-doacao.flg_restricao              = tt-ped-item-import.flgRestricao
                    web-ped-doacao.des_restricao              = tt-ped-item-import.desRestricao
                    web-ped-doacao.qtd_importada        = tt-ped-item-import.QuantidadePedida
                        .
                

        END. // IF tt-ped-venda-import.tipopedido = D



    END.

    IF CAN-FIND (FIRST rowErrors) THEN
    DO:
        FOR LAST tt-ped-item:

            RUN gerarPedVendaMSG
                (tt-ped-item.nome-abrev,
                 tt-ped-item.nr-pedcli,
                 tt-ped-item.it-codigo,
                 tt-ped-item.nr-sequencia
                 ).

        END.
    END.


    //RUN atribuirPendFin.


END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE criarExtensoes:
DEFINE VARIABLE cPrefixo AS CHARACTER   NO-UNDO.

    FOR FIRST emitente NO-LOCK WHERE
              emitente.nome-abrev    = ped-venda.nome-abrev:
    END.
    IF NOT AVAIL emitente THEN
    DO:
        RUN gerarRowError( "Cliente n∆o encontrado").
        RETURN "NOK".        
    END.


    FOR FIRST ext_ped_venda EXCLUSIVE-LOCK WHERE
              ext_ped_venda.nr_pedido       = ped-venda.nr-pedido:
    END.
    IF NOT AVAIL ext_ped_venda THEN
    DO:
        CREATE ext_ped_venda.
        ASSIGN ext_ped_venda.nr_pedido      = ped-venda.nr-pedido.
    END.
    IF AVAIL tt-ped-venda-import THEN
        ASSIGN ext_ped_venda.tip_pedido     = tt-ped-venda-import.tipopedido.
    

    FOR FIRST ext-emitente NO-LOCK WHERE
              ext-emitente.cod-emitente     = emitente.cod-emitente:
    END.
    IF NOT AVAIL ext-emitente THEN
    DO:
        RUN gerarRowError( SUBSTITUTE("Extens∆o do emitente n∆o encontrada (Emitente: &1)", emitente.cod-emitente) ).
        RETURN "NOK".        
    END.


    FOR EACH 
        ped-item NO-LOCK OF ped-venda:


        IF NOT AVAIL tt-ped-venda-import OR tt-ped-venda-import.tipoPedido <> "D" THEN 
        DO:
    
    
            CREATE web-ped-venda.
            ASSIGN 
                web-ped-venda.cod-estabel                = ped-venda.cod-estabel
                web-ped-venda.arquivo-descricao          = "Pedido Web/REST API"         // N∆o h† mais arquivo
                web-ped-venda.nome-abrev                 = ped-venda.nome-abrev
                web-ped-venda.nr-pedcli                  = ped-venda.nr-pedcli
                web-ped-venda.nr-sequencia               = ped-item.nr-sequencia
                web-ped-venda.nr-nota-fisc               = ""
                web-ped-venda.serie                      = "" 
                web-ped-venda.nr-seq-fat                 = 0 
                web-ped-venda.situacao                   = "IMPORTADO" 
                web-ped-venda.dt-emis-ped-venda          = TODAY 
                web-ped-venda.dt-emis-nota-fiscal        = ? 
                web-ped-venda.log-saldo-fisico           = FALSE 
                web-ped-venda.dat-saldo-fisico           = ?
                web-ped-venda.aprov-saldo-fis            = "" 
                web-ped-venda.quantidade                 = ped-item.qt-pedida
                web-ped-venda.it-codigo                  = ped-item.it-codigo
                web-ped-venda.qtde-saldo-fisico          = 0
                web-ped-venda.dt-gera-pedido             = IF AVAIL tt-ped-venda-import  AND tt-ped-venda-import.DataHoraEnvio <> ? AND tt-ped-venda-import.DataHoraEnvio <> ""
                                                            THEN DATE(INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 6, 2)) , INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 9, 2)) , INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 1, 4)) )
                                                            ELSE TODAY
                web-ped-venda.hr-pedido-web              = IF AVAIL tt-ped-venda-import  AND tt-ped-venda-import.DataHoraEnvio <> ? AND tt-ped-venda-import.DataHoraEnvio <> ""
                                                           THEN SUBSTR(tt-ped-venda-import.DataHoraEnvio, 12, 5)
                                                           ELSE STRING(TIME, "HH:MM")
                web-ped-venda.vlr-unit                   = ped-item.vl-preuni
                web-ped-venda.e-mail                     = IF AVAIL tt-ped-venda-import then tt-ped-venda-import.email      ELSE ""
                web-ped-venda.telefone                   = IF AVAIL tt-ped-venda-import then tt-ped-venda-import.telefone   ELSE ""
                web-ped-venda.cnpj                       = IF AVAIL tt-ped-venda-import then tt-ped-venda-import.cnpj       ELSE ""
                web-ped-venda.nr-conhecimento            = ""
                web-ped-venda.dt-embarque                = ?
                web-ped-venda.nro-pedido                 = IF AVAIL tt-ped-venda-import THEN tt-ped-venda-import.NumeroPedidoSistemaCliente ELSE ""
                web-ped-venda.flg_restricao              = IF AVAIL tt-ped-item-import THEN tt-ped-item-import.flgRestricao     ELSE NO
                web-ped-venda.des_restricao              = IF AVAIL tt-ped-item-import THEN tt-ped-item-import.desRestricao     ELSE ""
                web-ped-venda.qtd_importada              = ped-item.qt-pedida
                //web-ped-venda.cond-pag             = .
                .

        END.

        IF AVAIL tt-ped-venda-import AND  tt-ped-venda-import.tipoPedido = "D" THEN 
        DO:
    
            CREATE  web-ped-doacao.
            ASSIGN  web-ped-doacao.cod-estabel          = ped-venda.cod-estabel                                                                                                      
                    web-ped-doacao.arquivo-descricao    = "Pedido Web/REST API"         // N∆o h† mais arquivo                                                                                                       
                    web-ped-doacao.nome-abrev           = ped-venda.nome-abrev                                          
                    web-ped-doacao.natureza             = IF tt-ped-venda-import.natureza = "F" THEN 1 ELSE 2                                                                                            
                    web-ped-doacao.nr-pedcli            = ped-venda.nr-pedcli                                                                                                                                 
                    web-ped-doacao.nr-sequencia         = ped-item.nr-sequencia                                                                                                                                          
                    web-ped-doacao.nr-nota-fisc         = ""                                                                                                                                                  
                    web-ped-doacao.serie                = ""                                                                                                                                                  
                    web-ped-doacao.nr-seq-fat           = 0                                                                                                                                                   
                    web-ped-doacao.situacao             = "IMPORTADO"                                                                                                                                         
                    web-ped-doacao.dt-emis-ped-venda    = TODAY                                                                                                                                               
                    web-ped-doacao.dt-emis-nota-fiscal  = ?                                                                                                                                                   
                    web-ped-doacao.log-saldo-fisico     = FALSE                                                                                                                                               
                    web-ped-doacao.dat-saldo-fisico     = ?                                                                                                                                                   
                    web-ped-doacao.aprov-saldo-fis      = ""                                                                                                                                                  
                    web-ped-doacao.quantidade           = ped-item.qt-pedida                                                                                                                                  
                    web-ped-doacao.it-codigo            = ped-item.it-codigo                                                                                                                                  
                    web-ped-doacao.qtde-saldo-fisico    = 0                                                                                                                                                   
                    web-ped-doacao.dt-gera-pedido       = IF AVAIL tt-ped-venda-import 
                                                            THEN DATE(INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 6, 2)) , INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 9, 2)) , INT(SUBSTR(tt-ped-venda-import.DataHoraEnvio, 1, 4)) )
                                                            ELSE TODAY   
                    web-ped-doacao.hr-pedido-web        = IF AVAIL tt-ped-venda-import 
                                                           THEN SUBSTR(tt-ped-venda-import.DataHoraEnvio, 12, 5)
                                                           ELSE STRING(TIME, "HH:MM")
                    web-ped-doacao.vlr-unit             = ped-item.vl-preuni                                                                                                                                 
                    web-ped-doacao.e-mail               = IF AVAIL tt-ped-venda-import then tt-ped-venda-import.email      ELSE ""                                                                                    
                    web-ped-doacao.telefone             = IF AVAIL tt-ped-venda-import then tt-ped-venda-import.telefone   ELSE ""                                                                                    
                    web-ped-doacao.cnpj                 = IF AVAIL tt-ped-venda-import then tt-ped-venda-import.cnpj       ELSE ""                                                                                    
                    web-ped-doacao.nr-conhecimento      = ""                                                                                                                                                      
                    web-ped-doacao.dt-embarque          = ?                                                                                                                                                       
                    web-ped-doacao.nro-pedido           = IF AVAIL tt-ped-venda-import THEN tt-ped-venda-import.NumeroPedidoSistemaCliente ELSE ""
                    web-ped-doacao.cod-rep              = IF AVAIL repres THEN repres.cod-rep ELSE 0
                    web-ped-doacao.end-entrega          = IF AVAIL tt-ped-venda-import then tt-ped-venda-import.endereco ELSE ""         
                    web-ped-doacao.flg_restricao        = IF AVAIL tt-ped-item-import THEN tt-ped-item-import.flgRestricao     ELSE NO             
                    web-ped-doacao.des_restricao        = IF AVAIL tt-ped-item-import THEN tt-ped-item-import.desRestricao     ELSE ""             
                    web-ped-doacao.qtd_importada        = ped-item.qt-pedida
                        .
                

        END. // IF tt-ped-venda-import.tipopedido = D



    END.


    // Se isso for mesmo necess†rio usar procedure como receberDados para obter dados arquivo
/*     IF tt-ped-venda-import.tipoPedido = "E" THEN                       */
/*     DO:                                                                */
/*         CREATE web-log-arquivo.                                        */
/*         ASSIGN web-log-arquivo.cod-estabel   = tt-arquivos.cod-estabel */
/*                web-log-arquivo.data-arquivo  = d-data-arquivo          */
/*                web-log-arquivo.hora-arquivo  = c-hora-arquivo          */
/*                web-log-arquivo.data-processa = TODAY                   */
/*                web-log-arquivo.hora-processa = STRING(TIME,"HH:MM:SS") */
/*                web-log-arquivo.tipo          = c-tipo-arquivo          */
/*                web-log-arquivo.descricao     = pArquivo .              */
/*     END.                                                               */


    IF CAN-FIND (FIRST rowErrors) THEN
    DO:
        FOR LAST tt-ped-item:

            RUN gerarPedVendaMSG
                (tt-ped-item.nome-abrev,
                 tt-ped-item.nr-pedcli,
                 tt-ped-item.it-codigo,
                 tt-ped-item.nr-sequencia
                 ).

        END.
    END.




    //RUN atribuirPendFin.


END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE atualizarExtensoes:
DEFINE VARIABLE cPrefixo AS CHARACTER   NO-UNDO.

    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido de Venda n∆o dispon°vel").
        RETURN "NOK".        
    END.

    FOR FIRST emitente NO-LOCK WHERE
              emitente.nome-abrev    = ped-venda.nome-abrev:
    END.
    IF NOT AVAIL emitente THEN
    DO:
        RUN gerarRowError( "Cliente n∆o encontrado").
        RETURN "NOK".        
    END.

    FOR FIRST ext_ped_venda EXCLUSIVE-LOCK WHERE
              ext_ped_venda.nr_pedido       = ped-venda.nr-pedido:
    END.
    IF NOT AVAIL ext_ped_venda THEN
    DO:
        CREATE ext_ped_venda.
        ASSIGN ext_ped_venda.nr_pedido      = ped-venda.nr-pedido.
    END.
    IF NOT AVAIL ext_ped_venda THEN
    DO:
        RUN gerarRowError("Extens∆o do pedido de venda n∆o encontrada.").
        RETURN "NOK".        
    END.

    FOR EACH tt-ped-item-import NO-LOCK WHERE 
             NOT tt-ped-item-import.flgRestricao:
        

        FOR FIRST web-ped-venda EXCLUSIVE-LOCK WHERE
                  web-ped-venda.cod-estabel     = ped-venda.cod-estabel   
              AND web-ped-venda.nome-abrev      = ped-item.nome-abrev    
              AND web-ped-venda.nr-pedcli       = ped-item.nr-pedcli     
              AND web-ped-venda.it-codigo       = ped-item.it-codigo    :
        END.

        IF AVAIL web-ped-venda THEN
        DO:

            IF ext_ped_venda.tip_Pedido <> "D" THEN 
            DO:

                ASSIGN 
                    web-ped-venda.dt-gera-pedido             = TODAY
                    web-ped-venda.hr-pedido-web              = STRING(TIME, "HH:MM")
                    web-ped-venda.flg_restricao              = tt-ped-item-import.flgRestricao
                    web-ped-venda.des_restricao              = tt-ped-item-import.desRestricao
                    .
            
            END.
            
/*             IF AVAIL tt-ped-venda-import AND  tt-ped-venda-import.tipoPedido = "D" THEN        */
/*             DO:                                                                                */
/*                                                                                                */
/*                 ASSIGN  web-ped-doacao.dt-gera-pedido       = TODAY                            */
/*                         web-ped-doacao.hr-pedido-web        = STRING(TIME, "HH:MM")            */
/*                         web-ped-doacao.flg_restricao         = tt-ped-item-import.flgRestricao */
/*                         web-ped-doacao.des_restricao         = tt-ped-item-import.desRestricao */
/*                                                                                                */
/*                             .                                                                  */
/*                                                                                                */
/*                                                                                                */
/*             END. // IF tt-ped-venda-import.tipopedido = D                                      */


        END. // IF NOT AVAIL web-ped-venda

        FOR FIRST web-ped-doacao EXCLUSIVE-LOCK WHERE
                  web-ped-doacao.cod-estabel     = ped-venda.cod-estabel   
              AND web-ped-doacao.nome-abrev      = ped-item.nome-abrev    
              AND web-ped-doacao.nr-pedcli       = ped-item.nr-pedcli     
              AND web-ped-doacao.it-codigo       = ped-item.it-codigo    :

        END.

        IF AVAIL web-ped-venda OR AVAIL web-ped-doacao THEN DO:

            RUN avaliarAlocacao.
            IF RETURN-VALUE <> "OK" THEN 
                RETURN "NOK".


        END.

            
    END.

    IF CAN-FIND (FIRST rowErrors) THEN
    DO:
        FOR LAST tt-ped-item:

            RUN gerarPedVendaMSG
                (tt-ped-item.nome-abrev,
                 tt-ped-item.nr-pedcli,
                 tt-ped-item.it-codigo,
                 tt-ped-item.nr-sequencia
                 ).

        END.
    END.


    //RUN atribuirPendFin.

    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE gerarPedVendaMSG:
    DEF INPUT PARAM p-nome-abrev   like web-ped-venda.nome-abrev  .
    DEF INPUT PARAM p-nr-pedcli    like web-ped-venda.nr-pedcli   .
    DEF INPUT PARAM p-it-codigo    like web-ped-venda.it-codigo   .
    DEF INPUT PARAM p-nr-sequencia like web-ped-venda.nr-sequencia.

    FOR EACH RowErrors:
        CREATE web-ped-venda-msg.
        ASSIGN web-ped-venda-msg.nome-abrev       = p-nome-abrev   /*web-ped-venda.nome-abrev  */  
               web-ped-venda-msg.nr-pedcli        = p-nr-pedcli    /*web-ped-venda.nr-pedcli   */  
               web-ped-venda-msg.it-codigo        = p-it-codigo    /*web-ped-venda.it-codigo   */  
               web-ped-venda-msg.nr-sequencia     = p-nr-sequencia /*web-ped-venda.nr-sequencia*/  
               web-ped-venda-msg.ErrorSequence    = RowErrors.ErrorSequence
               web-ped-venda-msg.ErrorNumber      = RowErrors.ErrorNumber
               web-ped-venda-msg.ErrorDescription = RowErrors.ErrorDescription
               web-ped-venda-msg.errorparameters  = RowErrors.errorparameters
               web-ped-venda-msg.errortype        = RowErrors.errortype
               web-ped-venda-msg.ErrorHelp        = RowErrors.ErrorHelp
               web-ped-venda-msg.ErrorSubtype     = RowErrors.ErrorSubType
               web-ped-venda-msg.dt-registro = TODAY
               web-ped-venda-msg.hr-registro = STRING(TIME,"HH:MM:SS").
    
    END.
    
END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE atribuirPendFin:
DEFINE VARIABLE c-campo-aux AS CHARACTER   NO-UNDO.


    IF CAN-FIND (FIRST web-ped-venda-msg
                 WHERE web-ped-venda-msg.nome-abrev  = tt-ped-venda.nome-abrev
                   AND web-ped-venda-msg.nr-pedcli   = tt-ped-venda.nr-pedcli
                   AND web-ped-venda-msg.errornumber = 8259) OR 
           (    ped-venda.cod-sit-aval = 1  /* N∆o avaliado */
            AND ped-venda.quem-aprovou = "" /* Sem aprovador */
            AND ped-venda.dt-apr-cred  = ?  /* Sem data de aprovacao */)
    THEN DO:

        /* Acertar a situacao para PEND FIN */
        ASSIGN c-campo-aux = "".
        FOR EACH web-ped-venda
            WHERE web-ped-venda.nome-abrev  = ped-venda.nome-abrev
              AND web-ped-venda.nr-pedcli   = ped-venda.nr-pedcli 
              AND (web-ped-venda.situacao <> "OBSOLETO" 
/*                           AND web-ped-venda.situacao <> "ERRO IMPORTAÄ«O|ERRO VL UN" */
              AND NOT (web-ped-venda.situacao BEGINS "ERRO IMPORTAÄ«O")
              AND web-ped-venda.situacao <> "TRIBUTACAO") NO-LOCK:

            FIND CURRENT web-ped-venda EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN web-ped-venda.situacao = "PEND FIN".
            FIND CURRENT web-ped-venda NO-LOCK NO-ERROR.
            /* Gerando corpo do e-mail no campo c-campo-aux */
/*             ASSIGN c-campo-aux = "Informativo do Sistema WEBSALES " + CHR(10) + CHR(10) + ~                */
/*                                  "H† pendencias financeiras para o pedido abaixo:" + CHR(10) + CHR(10) + ~ */
/*                                  "Cliente.: " + web-ped-venda.nome-abrev + CHR(10) + ~                     */
/*                                  "Pedido..: " + web-ped-venda.nr-pedcli + CHR(10) + ~                      */
/*                                  "E-mail..: " + web-ped-venda.e-mail + CHR(10) + ~                         */
/*                                  "Telefone: " + web-ped-venda.telefone + CHR(10) + CHR(10)                 */
/*                                 c-email-anexo1 = "".                                                       */
        END.
    END.

END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE atualizarWebPedVenda:
DEFINE INPUT  PARAMETER pPedido AS ROWID       NO-UNDO.

    FOR FIRST ped-venda NO-LOCK WHERE
              ROWID(ped-venda)    = pPedido:
    END. 
    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("").
    END.

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      Carrega BOs de pedido pedido de venda
 * ------------------------------------------------------------------------------
 */
PROCEDURE carregarBO:

    IF NOT VALID-HANDLE(h_bodi159sdf) THEN
        RUN dibo/bodi159sdf.p PERSISTENT SET h_bodi159sdf NO-ERROR.
    
    IF NOT VALID-HANDLE(h_bodi159) THEN
        RUN dibo/bodi159.p PERSISTENT SET h_bodi159 NO-ERROR.
    
    IF NOT VALID-HANDLE(h_bodi157) THEN
        RUN dibo/bodi157.p PERSISTENT SET h_bodi157 NO-ERROR.
    
    IF NOT VALID-HANDLE(h_bodi154sdf) THEN
        RUN dibo/bodi154sdf.p PERSISTENT SET h_bodi154sdf NO-ERROR.
    
    IF NOT VALID-HANDLE(h_bodi154) THEN
        RUN dibo/bodi154.p PERSISTENT SET h_bodi154 NO-ERROR.
    
    IF NOT VALID-HANDLE(h_bodi159cal) THEN
        RUN dibo/bodi159cal.p PERSISTENT SET h_bodi159cal NO-ERROR.
    
    IF NOT VALID-HANDLE(h_bodi159com) THEN
        RUN dibo/bodi159com.p PERSISTENT SET h_bodi159com NO-ERROR.

    IF NOT VALID-HANDLE(hAlocacao) THEN
        RUN lib/PedidoVendaAlocacao.p PERSISTENT SET hAlocacao.

END PROCEDURE.
    
/*
 *------------------------------------------------------------------------------
 *      Descarrega BOs de pedido pedido de venda
 * ------------------------------------------------------------------------------
 */
PROCEDURE descarregarBO:

    IF NOT VALID-HANDLE(h_bodi159sdf) THEN DO:
        RUN destroyBO IN h_bodi159sdf.
        DELETE PROCEDURE h_bodi159sdf.
        ASSIGN h_bodi159sdf = ?.
    END.
    
    IF NOT VALID-HANDLE(h_bodi159) THEN DO:
        RUN destroyBO IN h_bodi159.
        DELETE PROCEDURE h_bodi159.
        ASSIGN h_bodi159 = ?.
    END.
    
    IF NOT VALID-HANDLE(h_bodi157) THEN DO:
        RUN destroyBO IN h_bodi157.
        DELETE PROCEDURE h_bodi157.
        ASSIGN h_bodi157 = ?.
    END.
    
    IF NOT VALID-HANDLE(h_bodi154sdf) THEN DO:
        RUN destroyBO IN h_bodi154sdf.
        DELETE PROCEDURE h_bodi154sdf.
        ASSIGN h_bodi154sdf = ?.
    END.
    
    IF NOT VALID-HANDLE(h_bodi154) THEN DO:
        RUN destroyBO IN h_bodi154.
        DELETE PROCEDURE h_bodi154.
        ASSIGN h_bodi154 = ?.
    END.
    
    IF NOT VALID-HANDLE(h_bodi159cal) THEN DO:
        RUN destroyBO IN h_bodi159cal.
        DELETE PROCEDURE h_bodi159cal.
        ASSIGN h_bodi159cal = ?.
    END.
    
    IF NOT VALID-HANDLE(h_bodi159com) THEN DO:
        RUN destroyBO IN h_bodi159com.
        DELETE PROCEDURE h_bodi159com.
        ASSIGN h_bodi159com = ?.
    END.

    IF NOT VALID-HANDLE(hAlocacao) THEN DO:
        RUN destroyBO IN hAlocacao.
        DELETE PROCEDURE hAlocacao.
        ASSIGN hAlocacao = ?.
    END.
    
END PROCEDURE.



/*
 *------------------------------------------------------------------------------
 *  Dado pediddo de venda, exclui pedido e itens via BO
 *------------------------------------------------------------------------------
 */
PROCEDURE EliminarPedidoVenda:
DEFINE INPUT  PARAMETER prPedido AS ROWID       NO-UNDO.

DEFINE VARIABLE rPedVenda AS ROWID       NO-UNDO.

    FOR FIRST ped-venda NO-LOCK WHERE 
              ROWID(ped-venda)  = prPedido:
    END.
    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido n∆o encontrado").
        RETURN "NOK".
    END.

    FOR EACH ROWERRORS:
        DELETE rowErrors.
    END.

/*     ELIMINAR_PEDIDO:                                     */
/*     DO TRANSACTION ON ERROR UNDO ELIMINAR_PEDIDO, LEAVE  */
/*                   ON ENDKEY UNDO ELIMINAR_PEDIDO, LEAVE: */

        FOR EACH ped-item NO-LOCK WHERE 
                 ped-item.nome-abrev        = ped-venda.nome-abrev
             AND ped-item.nr-pedcli         = ped-venda.nr-pedcli:

            RUN eliminarPedItem.
            IF RETURN-VALUE <> "OK" THEN
            DO:
                RUN eliminarBOs.
/*                 UNDO ELIMINAR_PEDIDO, RETURN "NOK". */
                RETURN "NOK".
            END.
                

        END.

        RUN dibo/bodi159.p PERSISTENT SET h_bodi159.

        RUN emptyRowErrors  IN h_bodi159.
        RUN openQueryStatic IN h_bodi159 (INPUT "Main":U).

/*         RUN setrecord       IN h_bodi159 (INPUT TABLE tt-ped-venda).  */

        DEFINE VARIABLE carqlog AS CHARACTER   NO-UNDO.
        carqlog = "v:\temp\EliminacaoPedido_log.txt".
        //RUN gerarLog(carqlog, "Vai eliminar pedido " + STRING(ped-venda.nome-abrev, ped-venda.nr-pedcli) ) .

        RUN gotoKey         IN h_bodi159 (ped-venda.nome-abrev, ped-venda.nr-pedcli ).
        rPedVenda = ROWID(ped-venda).

        run setUserLog in h_bodi159 (input v_cod_usuar_Corren).

        RUN deleteRecord    IN h_bodi159 .
        RUN getRowErrors    IN h_bodi159 (OUTPUT TABLE RowErrors).
        IF CAN-FIND(FIRST rowerrors) THEN
        DO:
           RUN eliminarBOs.
/*            UNDO ELIMINAR_PEDIDO, RETURN "NOK". */
           RETURN "NOK".
       END.

        DELETE PROCEDURE h_bodi159.

/*     END . */
    
    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *  Dado item do pedido de venda (RECORD-BUFFER), exclui via BO
 *------------------------------------------------------------------------------
 */
PROCEDURE eliminarPedItem:

DEFINE VARIABLE rPedItem AS ROWID       NO-UNDO.

    IF NOT AVAIL ped-item THEN
    DO:
        RUN gerarRowError("Item do Pedido nío encontrado").
        RETURN "NOK".
    END.

    ELIMINAR_ITEM_PEDIDO:
    DO TRANSACTION ON ERROR UNDO ELIMINAR_ITEM_PEDIDO, LEAVE
                    ON ENDKEY UNDO ELIMINAR_ITEM_PEDIDO, LEAVE:
    
        RUN dibo/bodi154.p PERSISTENT SET h_bodi154.
    
        RUN emptyRowErrors  IN h_bodi154.    
        RUN openQueryStatic IN h_bodi154 (INPUT "Main":U).
        RUN gotoKey         IN h_bodi154 (INPUT ped-item.nome-abrev  ,  
                                          INPUT ped-item.nr-pedcli   ,  
                                          INPUT ped-item.nr-sequencia,  
                                          INPUT ped-item.it-codigo   ,  
                                          INPUT ped-item.cod-refer   ).  

        rPedItem = ROWID(ped-item).
        RUN validateDelete  IN h_bodi154 (INPUT-OUTPUT rPedItem, OUTPUT TABLE RowErrors).
        IF CAN-FIND(FIRST rowerrors) THEN 
            UNDO ELIMINAR_ITEM_PEDIDO, RETURN "NOK".   
        
        DELETE PROCEDURE h_bodi154.
    
    END .
    
    RETURN "OK".

END PROCEDURE.

PROCEDURE eliminarBOs:

    IF VALID-HANDLE(h_bodi154) THEN
    DO:
        DELETE PROCEDURE h_bodi154.
        ASSIGN h_bodi154 = ?.
    END.

    IF VALID-HANDLE(h_bodi159) THEN
    DO:
        DELETE PROCEDURE h_bodi159.
        ASSIGN h_bodi159 = ?.
    END.


END PROCEDURE.


/*
*------------------------------------------------------------------------------
*      Gera mensagens de erro
* ------------------------------------------------------------------------------
*/
/* PROCEDURE gerarRowError:                                                      */
/* DEFINE INPUT  PARAMETER pcMensagem AS CHARACTER   NO-UNDO.                    */
/*                                                                               */
/* DEFINE VARIABLE iSeq AS INTEGER     NO-UNDO.                                  */
/*                                                                               */
/*     FIND LAST rowErrors NO-ERROR.                                             */
/*     ASSIGN iSeq = IF AVAIL rowErrors THEN rowErrors.ErrorSequence + 1 ELSE 1. */
/*                                                                               */
/*                                                                               */
/*     CREATE  rowErrors.                                                        */
/*     ASSIGN  rowErrors.ErrorSequence     = iSeq                                */
/*             rowErrors.ErrorNumber       = 17600                               */
/*             rowErrors.ErrorDescription  = pcMensagem                          */
/*             rowErrors.ErrorParameters   = ""                                  */
/*             rowErrors.ErrorType         = "EMS"                               */
/*             rowErrors.ErrorHelp         = ""                                  */
/*             rowErrors.ErrorSubType      = "ERROR".                            */
/*                                                                               */
/* /*                                                                */          */
/* /*     {method/svc/errors/inserr.i &ErrorNumber="17600"           */          */
/* /*                                 &ErrorType="EMS"               */          */
/* /*                                 &ErrorSubType="ERROR"          */          */
/* /*                                 &ErrorParameters="pcMensagem"} */          */
/*                                                                               */
/*                                                                               */
/* END PROCEDURE.                                                                */



/*
 *------------------------------------------------------------------------------
 *      Alterar Pedido de Venda
 * ------------------------------------------------------------------------------
 */
/* PROCEDURE alterarPedido:                                                                                                                  */
/* DEFINE INPUT  PARAMETER pNomeAbrev      LIKE ped-venda.nome-abrev   NO-UNDO.                                                              */
/* DEFINE INPUT  PARAMETER pNrPedcli       LIKE ped-venda.nr-pedcli    NO-UNDO.                                                              */
/* DEFINE INPUT  PARAMETER TABLE FOR tt-item-pedido.                                                                                         */
/* DEFINE OUTPUT PARAMETER p-nr-pedido LIKE ped-venda.nr-pedido NO-UNDO.                                                                     */
/*                                                                                                                                           */
/*                                                                                                                                           */
/* /*     FOR FIRST emitente NO-LOCK                       */                                                                                */
/* /*          WHERE emitente.nome-abrev    = pNomeAbrev : */                                                                                */
/* /*     END.                                             */                                                                                */
/* /*     IF NOT AVAIL emitente THEN                       */                                                                                */
/* /*     DO:                                              */                                                                                */
/* /*                                                      */                                                                                */
/* /*     END.                                             */                                                                                */
/*                                                                                                                                           */
/*     FOR EACH ped-venda NO-LOCK WHERE                                                                                                      */
/*              ped-venda.nome-abrev       = pNomeAbrev                                                                                      */
/*          AND ped-venda.nr-pedcli        = pNrPedcli:                                                                                      */
/*     END.                                                                                                                                  */
/*     IF NOT AVAIL ped-venda THEN                                                                                                           */
/*     DO:                                                                                                                                   */
/*         RUN gerarRowError("Pedido nío encontrado").                                                                                       */
/*         RETURN "NOK".                                                                                                                     */
/*     END.                                                                                                                                  */
/*                                                                                                                                           */
/*     Implanta:                                                                                                                             */
/*     DO TRANS:                                                                                                                             */
/*                                                                                                                                           */
/*         /* Cria tabela de parametros para criacao do pedido de venda/item do pedido de venda */                                           */
/*         CREATE tt-ped-param. /* para uso da BO */                                                                                         */
/*         ASSIGN tt-ped-param.relacao-item-cli     = YES                                                                                    */
/*                tt-ped-param.tp-relacao-item-cli  = 1                                                                                      */
/*                tt-ped-param.qtde-un-medida-cli   = YES                                                                                    */
/*                tt-ped-param.multiplicar-qtde     = YES                                                                                    */
/*                tt-ped-param.atribuir-preco-comp  = NO                                                                                     */
/*                tt-ped-param.tp-exp-nat-oper      = 2                                                                                      */
/*                tt-ped-param.tp-exp-dt-entrega    = 1                                                                                      */
/*                tt-ped-param.exp-nat-cons-final   = NO                                                                                     */
/*                tt-ped-param.exp-nat-cod-mensagem = YES                                                                                    */
/*                tt-ped-param.atualizar-entregas   = YES                                                                                    */
/*                tt-ped-param.arredondar-qtde-lote = NO                                                                                     */
/*                tt-ped-param.gerar-proc-exp       = NO                                                                                     */
/*                tt-ped-param.itinerario           = 1.                                                                                     */
/*                                                                                                                                           */
/*         /* Atualizacao de registros */                                                                                                    */
/*         RUN inputTable IN h_bodi159sdf (INPUT TABLE tt-ped-venda).                                                                        */
/*         RUN setDefaultCustomer IN h_bodi159sdf.                                                                                           */
/*         RUN outputTable IN h_bodi159sdf (OUTPUT TABLE tt-ped-venda).                                                                      */
/*                                                                                                                                           */
/*         FIND FIRST tt-ped-venda NO-ERROR.                                                                                                 */
/*                                                                                                                                           */
/*         /* Completa dados de ped-venda */                                                                                                 */
/*         ASSIGN tt-ped-venda.cod-estabel   = estabelec.cod-estabel                                                                         */
/*                tt-ped-venda.tp-preco      = IF tt-ped-venda-import.tipoPedido = "S" OR tt-ped-venda-import.tipoPedido = "E" THEN 2 ELSE 1 */
/*                tt-ped-venda.nr-ind-finan  = 0                                                                                             */
/*                tt-ped-venda.perc-desco1   = 0                                                                                             */
/*                .                                                                                                                          */
/*                                                                                                                                           */
/*         /* Efetiva as informacoes da tabela temporaria na tabela ped-venda */                                                             */
/*         EMPTY TEMP-TABLE RowErrors.                                                                                                       */
/*         RUN emptyRowErrors  IN h_bodi159.                                                                                                 */
/*         RUN openQueryStatic IN h_bodi159 (INPUT "Default":U).                                                                             */
/*         RUN setrecord       IN h_bodi159 (INPUT TABLE tt-ped-venda).                                                                      */
/*         RUN createRecord    IN h_bodi159.                                                                                                 */
/*         RUN getRowErrors    IN h_bodi159 (OUTPUT TABLE RowErrors).                                                                        */
/*                                                                                                                                           */
/*         IF CAN-FIND(FIRST RowErrors                                                                                                       */
/*                     WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:                                                                */
/*             RUN gerarRowError("Houve erro ao tentar gerar o pedido.").                                                                    */
/*             UNDO Implanta, RETURN "NOK".                                                                                                  */
/*         END.                                                                                                                              */
/*                                                                                                                                           */
/*         /* Gera Representante */                                                                                                          */
/*         FIND FIRST tt-ped-venda NO-ERROR.                                                                                                 */
/*         RUN getRowid IN h_bodi159 (OUTPUT tt-ped-venda.r-rowid).                                                                          */
/*                                                                                                                                           */
/*         CREATE tt-ped-repre.                                                                                                              */
/*         ASSIGN tt-ped-repre.nr-pedido   = tt-ped-venda.nr-pedido                                                                          */
/*                tt-ped-repre.nome-ab-rep = repres.nome-abrev                                                                               */
/*                tt-ped-repre.perc-comis  = repres.comis-direta                                                                             */
/*                tt-ped-repre.comis-emis  = repres.comis-emis                                                                               */
/*                tt-ped-repre.ind-repbase = YES.                                                                                            */
/*                                                                                                                                           */
/*         EMPTY TEMP-TABLE RowErrors.                                                                                                       */
/*         RUN emptyRowErrors  IN h_bodi157.                                                                                                 */
/*         RUN openQueryStatic IN h_bodi157(INPUT "DefaultPd4000":U).                                                                        */
/*         RUN setRecord       IN h_bodi157(INPUT TABLE tt-ped-repre).                                                                       */
/*         RUN createRecord    IN h_bodi157.                                                                                                 */
/*         RUN getRowErrors    IN h_bodi157(OUTPUT TABLE RowErrors).                                                                         */
/*                                                                                                                                           */
/*         IF CAN-FIND(FIRST RowErrors                                                                                                       */
/*                     WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:                                                                */
/*             RUN gerarRowError("Houve erro ao tentar gerar o pedido.").                                                                    */
/*             UNDO Implanta, RETURN "NOK".                                                                                                  */
/*         END.                                                                                                                              */
/*                                                                                                                                           */
/*         /* Cria Item de Pedido */                                                                                                         */
/*         ASSIGN i-nr-sequencia = 0.                                                                                                        */
/*         FOR EACH tt-item-pedido NO-LOCK:                                                                                                  */
/*                                                                                                                                           */
/*             FIND FIRST ITEM OF tt-item-pedido NO-LOCK NO-ERROR.                                                                           */
/*             ASSIGN i-nr-sequencia = i-nr-sequencia + 10.                                                                                  */
/*                                                                                                                                           */
/*             EMPTY TEMP-TABLE tt-ped-item.                                                                                                 */
/*             /*  cria temporaria de item */                                                                                                */
/*             CREATE tt-ped-item.                                                                                                           */
/*                                                                                                                                           */
/*             ASSIGN tt-ped-item.nome-abrev   = tt-ped-venda.nome-abrev                                                                     */
/*                    tt-ped-item.nr-pedcli    = tt-ped-venda.nr-pedcli                                                                      */
/*                    tt-ped-item.nr-sequencia = i-nr-sequencia                                                                              */
/*                    tt-ped-item.it-codigo    = tt-item-pedido.it-codigo                                                                    */
/*                    tt-ped-item.nat-operacao = tt-ped-venda.nat-oper                                                                       */
/*                    .                                                                                                                      */
/*                                                                                                                                           */
/*             /* executa criacao dos defaults */                                                                                            */
/*             RUN inputParentTable          IN h_bodi154sdf (INPUT TABLE tt-ped-venda).                                                     */
/*             RUN inputTable                IN h_bodi154sdf (INPUT TABLE tt-ped-item).                                                      */
/*             RUN setDefaultItem            IN h_bodi154sdf.                                                                                */
/*             RUN outputTable               IN h_bodi154sdf (OUTPUT TABLE tt-ped-item).                                                     */
/*             FIND FIRST tt-ped-item NO-ERROR.                                                                                              */
/*                                                                                                                                           */
/*                                                                                                                                           */
/*                                                                                                                                           */
/*             /* Efetiva as informacoes da tabela ped-item */                                                                               */
/*             EMPTY TEMP-TABLE RowErrors.                                                                                                   */
/*             RUN openQueryStatic IN h_bodi154 (INPUT "Default":U).                                                                         */
/*             RUN inputRowParam   IN h_bodi154 (INPUT TABLE tt-ped-param).                                                                  */
/*             RUN emptyRowErrors  IN h_bodi154.                                                                                             */
/*             RUN setrecord       IN h_bodi154 (INPUT TABLE tt-ped-item).                                                                   */
/*             RUN createRecord    IN h_bodi154.                                                                                             */
/*             RUN getRowErrors    IN h_bodi154 (OUTPUT TABLE RowErrors).                                                                    */
/*                                                                                                                                           */
/*             IF CAN-FIND(FIRST RowErrors                                                                                                   */
/*                         WHERE RowErrors.ErrorSubType begins "Erro":U) THEN DO:                                                            */
/*                 RUN gerarRowError("Erro ao criar item do pedido de venda.").                                                              */
/*                 UNDO Implanta, RETURN.                                                                                                    */
/*             END.                                                                                                                          */
/*                                                                                                                                           */
/*                                                                                                                                           */
/*             /* Calcula Pedido */                                                                                                          */
/*             RUN calculateOrder IN h_bodi159cal (INPUT tt-ped-venda.r-rowid).                                                              */
/*                                                                                                                                           */
/*             IF CAN-FIND(FIRST RowErrors                                                                                                   */
/*                         WHERE RowErrors.ErrorSubType begins "Erro":U) THEN DO:                                                            */
/*                 RUN gerarRowError("Erro ao calcular pedido de venda.").                                                                   */
/*                 UNDO Implanta, RETURN.                                                                                                    */
/*             END.                                                                                                                          */
/*         END.                                                                                                                              */
/*         /* Completa Pedido */                                                                                                             */
/*         IF NOT tt-ped-venda.completo THEN DO:                                                                                             */
/*             RUN completeOrder IN h_bodi159com (INPUT tt-ped-venda.r-rowid, OUTPUT TABLE rowErrors).                                       */
/*             IF CAN-FIND (FIRST RowErrors                                                                                                  */
/*                          WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:                                                           */
/*                 RUN gerarRowError("Erro ao completar pedido de venda.").                                                                  */
/*                 UNDO Implanta, RETURN.                                                                                                    */
/*             END.                                                                                                                          */
/*         END.                                                                                                                              */
/*         ASSIGN p-nr-pedido = tt-ped-venda.nr-pedido.                                                                                      */
/* /*         UNDO Implanta, RETURN.  */                                                                                                     */
/*     END.                                                                                                                                  */
/*                                                                                                                                           */
/* END PROCEDURE.                                                                                                                            */


/*
 *------------------------------------------------------------------------------
 *     
 * ------------------------------------------------------------------------------
 */
PROCEDURE ajustarPedido:                                                                                                                 

    ALTERAR_PED_VENDA:
    DO TRANSACTION:

        RUN calculateOrder IN h_bodi159cal (INPUT ROWID(ped-venda)).
        
        EMPTY TEMP-TABLE tt-ped-venda.

        /* Cria tabela temporaria do pedido de venda */
        CREATE tt-ped-venda.
        BUFFER-COPY ped-venda TO tt-ped-venda
            ASSIGN tt-ped-venda.r-rowid = ROWID(ped-venda).


        ASSIGN tt-ped-venda.cod-cond-pag     = IF tt-ped-venda-import.tipoPedido <> "d"
                                               THEN IF ped-venda.vl-tot-ped < 1000
                                                    THEN es_api_param_ped.cod_cond_pag_bol
                                                    ELSE IF tt-ped-venda-import.PagamentoAV = 1
                                                         THEN es_api_param_ped.cod_cond_pag_av
                                                         ELSE emitente.cod-cond-pag
                                               ELSE 0
               tt-ped-venda.ind-tp-frete     = IF NOT( PedidoVendaAbaixoLimiteTransp() ) 
                                                THEN INTEGER(SUBSTRING(loc-entr.char-1,50,8)) 
                                                ELSE 2
                   .



/*          MESSAGE                                                          */
/*              'ANTES AJUSTE '                                         SKIP */
/*              'ped-venda.nome-abrev   '     ped-venda.nome-abrev      SKIP */
/*              'ped-venda.nr-pedcli    '     ped-venda.nr-pedcli       SKIP */
/*              'ped-venda.cod-sit-ped  '     ped-venda.cod-sit-ped          */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                           */
/*                                                                           */

        RUN emptyRowErrors  IN h_bodi159.
        RUN openQueryStatic IN h_bodi159 (INPUT "Main":U).
        RUN gotoKey         IN h_bodi159 (tt-ped-venda.nome-abrev  ,
                                          tt-ped-venda.nr-pedcli).
        RUN setRecord               IN h_bodi159 (INPUT TABLE tt-ped-venda).
        RUN validateRecord       IN h_bodi159 (INPUT "UPDATE").
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN getRowErrors            IN h_bodi159 (OUTPUT TABLE RowErrors).
            RUN gerarRowError("Houve erro ao tentar gerar o pedido.").
            UNDO ALTERAR_PED_VENDA, RETURN "NOK".       
        END.
    


        RUN updateRecord        IN h_bodi159 .
        IF RETURN-VALUE <> "OK" THEN DO:
            RUN getRowErrors            IN h_bodi159 (OUTPUT TABLE RowErrors).
            RUN gerarRowError("Houve erro ao tentar gerar o pedido.").
            UNDO ALTERAR_PED_VENDA, RETURN "NOK".
        END.


    END.


    RETURN "OK".
                                                                                                   


END PROCEDURE.




/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE alocarPedido:

    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido n∆o dispon°vel").
        RETURN "NOK".
    END.

    RUN lib/PedidoVendaAlocacao.p PERSISTENT SET hAlocacao.
    
    RUN alocarpedido IN hAlocacao (ROWID(ped-venda)) .
    IF RETURN-VALUE <> "NOK" THEN
    DO:
        RUN retornarErros IN hAlocacao (OUTPUT TABLE rowerrorsAux).
        RUN obterMensagensAux.
        DELETE PROCEDURE hAlocacao.
        RETURN "NOK".
    END.

    RUN retornarErros IN hAlocacao (OUTPUT TABLE rowerrorsAux).
    RUN obterMensagensAux.
    
    DELETE PROCEDURE hAlocacao.


    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE alocarItemPedido:

    IF NOT AVAIL ped-item  THEN
    DO:
        RUN gerarRowError("Item do pedido n∆o dispon°vel para alocaá∆o").
        RETURN "NOK".
    END.

    RUN lib/PedidoVendaAlocacao.p PERSISTENT SET hAlocacao.
    
    RUN alocarItemPedido IN hAlocacao (ROWID(ped-item)) .
    
    RUN retornarErros IN hAlocacao (OUTPUT TABLE rowerrorsAux).

    IF RETURN-VALUE <> "NOK" THEN
    DO:
        DELETE PROCEDURE hAlocacao.
        RETURN "NOK".
    END.
    
    DELETE PROCEDURE hAlocacao.


    RETURN "OK".


END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE atualizarSituacaoPedido:


    FOR FIRST tt-ped-venda-import:
    END.
    IF AVAIL tt-ped-venda-import AND tt-ped-venda-import.tipopedido = "E" THEN
    DO:
          FOR EACH ped-item NO-LOCK  WHERE
                     ped-item.nome-abrev       = ped-venda.nome-abrev
                AND  ped-item.nr-pedcli         = ped-venda.nr-pedcli
            :
            RUN atualizarSituacaoWeb ("PEND VOL").
          END.
          RETURN "OK" .

    END.
       

    

    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido n∆o dispon°vel").
        RETURN "NOK".
    END.
    
    ITEM:
    FOR EACH ped-item NO-LOCK  WHERE
             ped-item.nome-abrev       = ped-venda.nome-abrev
        AND  ped-item.nr-pedcli         = ped-venda.nr-pedcli
        
        :


        IF ped-item.cod-sit-item = 6 THEN
            NEXT ITEM.


        IF ped-venda.cod-sit-aval = 4 THEN
        DO:
            RUN atualizarSituacaoWeb ("PEND FIN").
            NEXT ITEM.
        END.


        ASSIGN deAloc = 0.
        FOR EACH ped-ent OF ped-item NO-LOCK:
            
            ASSIGN deAloc   = deAloc + ped-ent.qt-log-aloca .

/*             MESSAGE                                */
/*                 ped-ent.qt-pedida SKIP             */
/*                 ped-ent.qt-log-aloca               */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

            IF ped-ent.qt-pedida <> ped-ent.qt-log-aloca THEN
            DO:

                //RUN gerarRowError("Houve um erro ao alocar o ITEM &1. Qt. Prevista: &2 / Qt. &3 ").

                RUN cancelarPedItem.
                IF RETURN-VALUE <> "OK" THEN
                    RETURN "NOK".

                RUN atualizarSituacaoWeb ("CANCELADO").

                NEXT ITEM.

            END.
            
            RUN atualizarSituacaoWeb ("PEND VOL").
            
            

        END.
        
        


        // ASSIGN web-ped-venda.situacao = "ERRO FN/CD".
        // ASSIGN bf-web-ped-venda.situacao = "BACK ORDER".
        // ASSIGN web-ped-venda.situacao = "ERRO IMPORTA?ÄO|ERRO TABELA PRE?O".
        //  ABSOLUTE(c-preco-tab - web-ped-venda.vlr-unit) > 0.01  web-ped-venda.situacao             = "ERRO IMPORTA?ÄO|ERRO VL UN".   

/*         IF AVAIL es-item THEN DO: /* find estˇ mais pra cima */                          */
/*         FIND es-componente                                                               */
/*             WHERE es-componente.cd-componente =  es-item.cd-componente NO-LOCK NO-ERROR. */
/*                                                                                          */
/*         IF AVAIL es-componente THEN DO:                                                  */
/*              IF (es-componente.cd-componente = "011" OR                                  */
/*                  es-componente.cd-componente = "013" OR                                  */
/*                  es-componente.cd-componente = "014" OR                                  */
/*                  es-componente.cd-componente = "030" OR                                  */
/*                  es-componente.cd-componente = "040" OR                                  */
/*                  es-componente.cd-componente = "070" OR                                  */
/*                  es-componente.cd-componente = "080" OR                                  */
/*                  es-componente.cd-componente = "090" OR                                  */
/*                  es-componente.cd-componente = "100" OR                                  */
/*                  es-componente.cd-componente = "190" OR                                  */
/*                  es-componente.cd-componente = "420" OR                                  */
/*                  es-componente.cd-componente = "500" OR                                  */
/*                  es-componente.cd-componente = "600" OR                                  */
/*                  es-componente.cd-componente = "970") THEN                               */
/*                web-ped-venda.situacao = "TRIBUTACAO".                                    */
/*         END.                                                                             */

        // web-ped-venda.situacao             = "ERRO IMPORTA?ÄO|OBSOLETO".  


    END. // EACH ped-item


    RUN calculateOrder IN h_bodi159cal (INPUT ROWID(ped-venda)). 

    FIND CURRENT ped-venda NO-LOCK.
    IF ped-venda.cod-sit-ped <> 6 
        AND PedidoVendaAbaixoLimiteTransp() 
        AND AVAIL tt-ped-venda-import
        AND tt-ped-venda-import.tipoPedido = "s"
        THEN DO:
        FOR EACH ped-item OF ped-venda:
            RUN atualizarSituacaoWeb ("PEND TRANS").
        END.
    END.



    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE atualizarSituacaoPedido2:


    FOR FIRST tt-ped-venda-import:
    END.
    IF AVAIL tt-ped-venda-import AND tt-ped-venda-import.tipopedido = "E" THEN
    DO:
          FOR EACH ped-item NO-LOCK  WHERE
                     ped-item.nome-abrev       = ped-venda.nome-abrev
                AND  ped-item.nr-pedcli         = ped-venda.nr-pedcli
            :
            RUN atualizarSituacaoWeb ("PEND VOL").
          END.
          RETURN "OK" .

    END.
       

    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido n∆o dispon°vel").
        RETURN "NOK".
    END.
    
    ITEM:
    FOR EACH ped-item NO-LOCK  WHERE
             ped-item.nome-abrev       = ped-venda.nome-abrev
        AND  ped-item.nr-pedcli         = ped-venda.nr-pedcli
        
        :
        IF ped-item.cod-sit-item = 6 THEN
            NEXT ITEM.



        ASSIGN deAloc = 0.
        FOR EACH ped-ent OF ped-item NO-LOCK:
            
            ASSIGN deAloc   = deAloc + ped-ent.qt-log-aloca .

/*             MESSAGE                                */
/*                 ped-ent.qt-pedida SKIP             */
/*                 ped-ent.qt-log-aloca               */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/*             IF ped-ent.qt-pedida <> ped-ent.qt-log-aloca THEN                                                                                                                 */
/*             DO:                                                                                                                                                               */
/*                 RUN gerarRowError(SUBSTITUTE("Houve um erro ao alocar o ITEM &1. Qt. Prevista: &2 / Qt. &3 ", ped-item.it-codigo, ped-ent.qt-pedida, ped-ent.qt-log-aloca) ). */
/*                 RETURN "NOK".                                                                                                                                                 */
/*             END.                                                                                                                                                              */
            

        END.

        // IF deAloc = 0 THEN DO:
        FOR FIRST tt-ped-item-import NO-LOCK WHERE
                  tt-ped-item-import.r-rowid        = ROWID(ped-item):
        END.
        IF AVAIL tt-ped-item-import AND tt-ped-item-import.QuantidadeDisponivel = 0 THEN 
        DO:

            RUN cancelarPedItem.
            IF RETURN-VALUE <> "OK" THEN
                RETURN "NOK".

            RUN atualizarSituacaoWeb ("CANCELADO").

            NEXT ITEM.

        END.

        &MESSAGE verificar
/*         IF deAloc <> ped-item.qt-pedida THEN                                                                                                                 */
/*         DO:                                                                                                                                                  */
/*             RUN gerarRowError(SUBSTITUTE("Houve um erro ao alocar o ITEM &1. Qt. Prevista: &2 / Qt. &3 ", ped-item.it-codigo, ped-item.qt-pedida, deAloc) ). */
/*             RETURN "NOK".                                                                                                                                    */
/*         END.                                                                                                                                                 */
        
        RUN atualizarSituacaoWeb ("PEND VOL").

        IF ped-venda.cod-sit-aval = 4 THEN
        DO:

            RUN atualizarSituacaoWeb ("PEND FIN").

        END.



    END. // EACH ped-item


    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE cancelarPedItem:

    IF NOT AVAIL ped-item THEN
    DO:
        RUN gerarRowError("Item de Pedido n∆o dispon°vel").
        RETURN "NOK".
    END.

    EMPTY TEMP-TABLE  rowErrorsAux.

    RUN dibo/bodi154can.p PERSISTENT SET h_bodi154can.
    RUN validateCancelation  IN h_bodi154can
        (INPUT ROWID(ped-item),
         INPUT "Sem Saldo para Atendimento",
         INPUT-OUTPUT TABLE rowErrorsAux).
    IF RETURN-VALUE <> "OK" THEN
    DO:
        RUN gerarRowError(SUBSTITUTE("Item do pedido n∆o poder ser cancelado (Seq: &1 | Item : &2)", tratarString(STRING(ped-item.nr-sequencia)), tratarString(STRING(ped-item.it-codigo)) ) ).
        FOR EACH rowErrorsAux NO-LOCK
            /* WHERE RowErrors.ErrorSubType BEGINS "Erro" */ 
            :
            RUN gerarMensagem
                (rowErrorsAux.errordescription,
                 rowErrorsaux.errornumber,
                 rowErrorsAux.errorsubtype).
        END.
        DELETE PROCEDURE h_bodi154can.
        RETURN "NOK".
    END.

    RUN updatecancelation IN h_bodi154can
        (ROWID(ped-item),
         "N∆o alocou o material no estoque. Sem estoque",
         TODAY,
         1
         ).
    IF RETURN-VALUE <> "OK" THEN
    DO:
        RUN gerarRowError(SUBSTITUTE("Erro ao Cancelar Item do pedido (Seq: &1 | Item : &2)", tratarString(STRING(ped-item.nr-sequencia)), tratarString(STRING(ped-item.it-codigo)) ) ).
        DELETE PROCEDURE h_bodi154can.
        RETURN "NOK".
    END.




    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE completarPedido:

    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido n∆o dispon°vel").
        RETURN "NOK".
    END.

    EMPTY TEMP-TABLE tt-ped-venda.
    
    CREATE tt-ped-venda.
    BUFFER-COPY ped-venda TO tt-ped-venda
        ASSIGN tt-ped-venda.r-rowid     = ROWID(ped-venda).
    
    /* Completa Pedido */
    IF NOT ped-venda.completo THEN DO:        
        RUN completeOrder IN h_bodi159com (INPUT tt-ped-venda.r-rowid, OUTPUT TABLE rowErrors).
        IF CAN-FIND (FIRST RowErrors
                     WHERE RowErrors.ErrorSubType BEGINS "Erro":U) THEN DO:
            RUN gerarRowError("Erro ao completar pedido").
            RETURN "NOK".
        END.
    END.


    DEF BUFFER bped FOR ped-venda.
    FOR FIRST bped NO-LOCK WHERE
              bped.nr-pedido   = ped-venda.nr-pedido:
    END.



END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE atualizarSituacaoWeb:
DEFINE INPUT  PARAMETER pSituacao AS CHARACTER   NO-UNDO.

    FOR FIRST web-ped-venda EXCLUSIVE-LOCK WHERE       
              web-ped-venda.cod-estabel         = ped-venda.cod-estabel     
          AND web-ped-venda.nome-abrev          = ped-item.nome-abrev      
          AND web-ped-venda.nr-pedcli           = ped-item.nr-pedcli       
          AND web-ped-venda.it-codigo           = ped-item.it-codigo       :
        ASSIGN web-ped-venda.situacao   = pSituacao.

/*         MESSAGE                                */
/*             web-ped-venda.it-codigo            */
/*             SKIP                               */
/*             web-ped-venda.quantidade           */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */


        IF pSituacao = "PEND VOL" THEN
            ASSIGN  web-ped-venda.qtde-saldo-fisico         = web-ped-venda.quantidade
                    web-ped-venda.qtde-confirmada-fat       = web-ped-venda.quantidade
                    
            .

    END.
    FOR FIRST web-ped-doacao EXCLUSIVE-LOCK WHERE       
              web-ped-doacao.cod-estabel         = ped-venda.cod-estabel     
          AND web-ped-doacao.nome-abrev          = ped-item.nome-abrev      
          AND web-ped-doacao.nr-pedcli           = ped-item.nr-pedcli       
          AND web-ped-doacao.it-codigo           = ped-item.it-codigo       :
        ASSIGN web-ped-doacao.situacao  = pSituacao.
        
        IF pSituacao = "PEND VOL" THEN
            ASSIGN web-ped-doacao.qtde-saldo-fisico            = web-ped-doacao.quantidade
                    web-ped-doacao.qtde-confirmada-fat           = web-ped-doacao.quantidade.

    END.

END PROCEDURE.
                                               
/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE integrarLogistica:
DEFINE INPUT  PARAMETER pNomeAbrev      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNrPedcli       AS CHARACTER   NO-UNDO.

    FOR FIRST ped-venda NO-LOCK WHERE 
              ped-venda.nome-abrev      = pNomeAbrev
        AND   ped-venda.nr-pedcli       = pNrPedcli:             
    END.
    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido de Venda n∆o encontrado").
        RETURN "NOK".
    END.

    FOR FIRST web-ped-venda NO-LOCK WHERE
              web-ped-venda.nr-pedcli       = ped-venda.nr-pedcli
        AND   web-ped-venda.nome-abrev      = ped-venda.nome-abrev
        AND   web-ped-venda.situacao        = "PEND VOL":
    END.
    IF AVAIL WEB-PED-VENDA THEN
    DO:
        RUN macp/mac0296.p (INPUT ped-venda.nome-abrev,
                            INPUT ped-venda.nr-pedcli,
                            INPUT "VENDA").
    END.
    

END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE obterMensagensAux:

    FOR EACH rowerrorsaux:
        
        RUN gerarMensagem (
            rowErrorsAux.ErrorDescription       ,
            rowErrorsAux.ErrorNumber            ,
            rowErrorsAux.ErrorSubType    
            ).


    END.

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE EnviarNotificacao:
DEFINE INPUT  PARAMETER pEvento AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cPara AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCopia      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAssunto    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCorpo      AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cErro AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPedido AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNomeAbrev AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCliente AS CHARACTER   NO-UNDO.


    FOR FIRST tt-ped-venda-import:
    END.
    


    //RUN abrirLog("c:\temp\EnviarNotificacao_log_" + stamp() + ".txt").
    carqlog = "c:\temp\EnviarNotificacao_log.txt".
    RUN gerarLog("pEvento " + tratarString(pevento) ).

    ASSIGN  cPedido     = ""
            cCliente    = ""
            cErro       = "".
    IF AVAIL ped-venda THEN
        ASSIGN   cPedido    = ped-venda.nr-pedcli  
                cCliente    = ped-venda.nome-abrev.
    ELSE
    DO:
        IF AVAIL tt-ped-venda-import THEN
            ASSIGN   cPedido    = tt-ped-venda-import.NumeroPedidoCliente
                     cCliente   = IF AVAIL emitente THEN emitente.nome-emit ELSE tt-ped-venda-import.cnpj.
    END.
/*     IF cPedido = "" THEN                                                                           */
/*     DO:                                                                                            */
/*         RUN gerarRowErro("Dados do pedido n∆o dispon°veis, n∆o foi poss°vel enviar notificaá‰es"). */
/*         RETURN "NOK".                                                                              */
/*     END.                                                                                           */
    
    RUN gerarLog("Cliente " + tratarString(cCliente ) ).
    RUN gerarLog("Pedido  " + tratarString(cPedido  ) ).


    IF pEvento = "Validacao" THEN
    DO:
        
        FOR FIRST es_api_notificacao_pedido EXCLUSIVE-LOCK WHERE   
                  es_api_notificacao_pedido.nome_abrev      = cCliente
             AND  es_api_notificacao_pedido.nr_pedcli       = cPedido:
        END.
        
        IF AVAIL es_api_notificacao_pedido AND es_api_notificacao_pedido.dat_ult_envio = TODAY THEN
            RETURN "OK".

        IF NOT AVAIL es_api_notificacao_pedido THEN
        DO:
            CREATE es_api_notificacao_pedido.
            ASSIGN  es_api_notificacao_pedido.nome_abrev      = cCliente      
                    es_api_notificacao_pedido.nr_pedcli       = cPedido
                .
        END.
        ASSIGN  es_api_notificacao_pedido.dat_ult_envio = TODAY
                es_api_notificacao_pedido.hor_ult_envio = TIME
            .

        RELEASE es_api_notificacao_pedido.

    END.


    


    IF AVAIL ped-venda AND NOT AVAIL emitente THEN
        FOR FIRST emitente NO-LOCK WHERE emitente.nome-abrev        = ped-venda.nome-abrev:
            ASSIGN cCliente =    emitente.nome-emit.
        END.


    

    FOR FIRST es_api_mensagem NO-LOCK WHERE
              es_api_mensagem.desEvento     = pEvento:
    END.
    IF NOT AVAIL es_api_mensagem  THEN
    DO:
        RUN gerarRowErro(SUBSTITUTE("Erro ao gerar notificaá∆o - evento n∆o cadastrado (&1)"), pEvento ).
        RETURN "NOK".
    END.

    FOR FIRST es_api_param_ped NO-LOCK:
    END.
    IF NOT AVAIL es_api_param_ped THEN
    DO:
        RUN gerarRowErro("Erro ao gerar notificaá∆o. ParÉmetros da integraá∆o de pedidos n∆o encontrados." ).
        RETURN "NOK".
    END.


    CASE pEvento:
        WHEN "Confirmacao" THEN     
            ASSIGN      cPara = AcrescentarEmail(cPara, es_api_param_ped.lst_mail_log )
                        cPara = AcrescentarEmail(cPara, es_api_param_ped.lst_mail_comercial ).
    
        WHEN "Financeiro" THEN     
            ASSIGN      cPara = AcrescentarEmail(cPara, es_api_param_ped.lst_mail_fin ).
    
        WHEN "Validacao" OR WHEN "Restricao" THEN     
            ASSIGN      cPara = AcrescentarEmail(cPara, es_api_param_ped.lst_mail_comercial ).
    
        WHEN "Financeiro" THEN     
            ASSIGN      cPara = AcrescentarEmail(cPara, es_api_param_ped.lst_mail_fin ).
    

    END CASE.



    FOR EACH rowErrors:

        FOR FIRST es_notificacoes NO-LOCK WHERE 
                  es_notificacoes.cod_Mensagem    = rowErrors.errorNumber:

            IF es_notificacoes.flg_Logistica    THEN cPara  = AcrescentarEmail(cPara, es_api_param_ped.lst_mail_log         ).
            IF es_notificacoes.flg_Comercial    THEN cPara  = AcrescentarEmail(cPara, es_api_param_ped.LST_mail_comercial         ).
            IF es_notificacoes.flg_Financeiro   THEN cPara  = AcrescentarEmail(cPara, es_api_param_ped.lst_mail_fin         ).
            IF es_notificacoes.flg_Fiscal       THEN cPara  = AcrescentarEmail(cPara, es_api_param_ped.lst_mail_fis   ).

            //cErro = cErro + rowErrors.errorDesc + CHR(10).
            
        END.

    END.


    IF pEvento = "confirmacao" THEN
    DO:

        IF AVAIL tt-ped-venda-import OR AVAIL web-ped-venda OR AVAIL web-ped-doacao THEN
        DO:

            IF AVAIL tt-ped-venda-import    THEN RUN gerarLog("tt-ped-venda-import.email " + tt-ped-venda-import.email).
            IF AVAIL web-ped-venda          THEN RUN gerarLog("web-ped-venda.e-mail      " + web-ped-venda.e-mail).
            IF AVAIL web-ped-doacao         THEN RUN gerarLog("web-ped-doacao.e-mail     " + web-ped-doacao.e-mail).

            ASSIGN  cCopia      = cPara
                    cPara       = IF AVAIL tt-ped-venda-import 
                                  THEN tt-ped-venda-import.email
                                  ELSE IF AVAIL web-ped-venda THEN web-ped-venda.e-mail
                                                              ELSE web-ped-doacao.e-mail.

            IF cPara = "" THEN
                cPara = cCopia.

        END.

    END.

    ASSIGN  cAssunto    = obterTextoNotificacao(es_api_mensagem.desAssunto )
            cCorpo      = obterTextoNotificacao(es_api_mensagem.desCorpo   ) 
            .

    RUN gerarLog("cPara                               " + tratarString(cPara                               ) ).
    RUN gerarLog("cCopia                              " + tratarString(cCopia                              ) ).
    RUN gerarLog("es_api_param_ped.email_remetente    " + tratarString(es_api_param_ped.email_remetente    ) ).
    RUN gerarLog("cAssunto                            " + tratarString(cAssunto                            ) ).
    RUN gerarLog("cCorpo                              " + tratarString(cCorpo                              ) ).
        

    RUN enviaremail
        (cPara,
         cCopia,
         es_api_param_ped.email_remetente,
         cAssunto,
         cCorpo,
         ""
         ) .

    RUN gerarLog("OK? " + tratarString(RETURN-VALUE )).
    

    RETURN "OK".

END PROCEDURE.



/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE EnviarEmail:
DEFINE INPUT PARAMETER pDestino      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pCopia        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pRemetente    AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pAssunto      AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pCorpo        AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pAnexo        AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h-utapi019 AS HANDLE      NO-UNDO.

    run utp/utapi019.p persistent set h-utapi019.           
    for each tt-envio2:                                     
        delete tt-envio2.                                   
    END.                                                    
    for each tt-mensagem:                                   
        delete tt-mensagem.                                 
    END.     

    //MESSAGE pdestino VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

    CREATE tt-envio2.                                       
    ASSIGN tt-envio2.versao-integracao = 1                  
           tt-envio2.destino           = pDestino           
           tt-envio2.copia             = pCopia             
           tt-envio2.remetente         = pRemetente         
           tt-envio2.assunto           = pAssunto           
           tt-envio2.mensagem          = pCorpo             
           tt-envio2.importancia       = 2                  
           tt-envio2.log-enviada       = no                 
           tt-envio2.log-lida          = no                 
           tt-envio2.acomp             = no                 
           tt-envio2.formato           = "text"             
           tt-envio2.arq-anexo         = pAnexo.            
                                                            
    CREATE tt-mensagem.                                     
    ASSIGN tt-mensagem.seq-mensagem = 1                     
           tt-mensagem.mensagem = pCorpo.                   
                                                            
    run pi-execute2 in h-utapi019 (input  table tt-envio2  ,
                                  input  table tt-mensagem, 
                                  output table tt-erros)  . 
    IF RETURN-VALUE <> "OK" THEN
    DO:
        DELETE PROCEDURE h-utapi019.

        RUN gerarRowError("Houver erro ao enviar notificaá‰es").
        FOR EACH tt-erros:
            RUN gerarRowError(SUBSTITUTE("&1 (&2)", tt-erros.desc-erro, tratarString(STRING(tt-erros.cod-erro)) ) ).
        END.
        RETURN "NOK".
    END.

    DELETE PROCEDURE h-utapi019.
                                                            
    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE EnviarNotificacoesPedido:

    FOR FIRST tt-ped-venda-import:
    END.

    IF AVAIL ped-venda THEN
    DO:

        IF tt-ped-venda-import.tipoPedido <> "D" THEN 
        DO:

            FOR FIRST web-ped-venda OF ped-venda NO-LOCK WHERE web-ped-venda.flg_restricao:
            END.
            IF AVAIL web-ped-venda THEN
            DO:
                RUN EnviarNotificacao ("Restricao"). 
            END.

            FOR FIRST web-ped-venda OF ped-venda NO-LOCK WHERE web-ped-venda.situacao = "PEND FIN":
            END.
            IF AVAIL web-ped-venda THEN
            DO:
                RUN EnviarNotificacao ("Financeiro"). 
            END.

            FOR FIRST web-ped-venda OF ped-venda NO-LOCK WHERE web-ped-venda.situacao = "PEND VOL":
            END.
            IF AVAIL web-ped-venda THEN
            DO:
                RUN EnviarNotificacao ("Confirmacao"). 
            END.

        END.

        IF tt-ped-venda-import.tipoPedido = "D" THEN 
        DO:
            FOR FIRST web-ped-doacao OF ped-venda NO-LOCK WHERE web-ped-doacao.situacao = "PEND FIN":
            END.
            IF AVAIL web-ped-doacao THEN
            DO:
                RUN EnviarNotificacao ("Financeiro"). 
            END.

            FOR FIRST web-ped-doacao OF ped-venda NO-LOCK WHERE web-ped-doacao.situacao = "PEND VOL":
            END.
            IF AVAIL web-ped-doacao THEN
            DO:
                RUN EnviarNotificacao ("Confirmacao"). 
            END.

        END.



    END.

END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE gerarDadosClienteNovo:
DEFINE VARIABLE cCNPJ AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iID AS INTEGER     NO-UNDO.

    cCnpj   = REPLACE(REPLACE(REPLACE(REPLACE(tt-ped-venda-import.CNPJ,".",""),",",""),"/",""),"-","").

    FOR FIRST ws-cliente NO-LOCK WHERE
              ws-cliente.cnpj       = cCnpj:
    END.
    IF AVAIL ws-cliente THEN
        RETURN "OK".

    iID = 1.
    FOR LAST ws-cliente BY ws-cliente.id_cliente:
        iID = ws-cliente.id_cliente + 1.
    END.


    CREATE ws-cliente.
    ASSIGN ws-cliente.bairro       = ""                                                   
           ws-cliente.cep          = ""                                                   
           ws-cliente.cidade       = ""                                                   
           ws-cliente.cnpj         = cCNPJ                                                
           ws-cliente.cod-emitente = ?                                                    
           ws-cliente.cod-status   = 1 /* Pendente VF */                                  
           ws-cliente.email        = tt-ped-venda-import.email                            
           ws-cliente.endereco     = ""                                                   
           ws-cliente.estado       = ""                                                   
           ws-cliente.id_cliente   = iID                                                  
           ws-cliente.natureza     = IF tt-ped-venda-import.natureza BEGINS "F" THEN 1 ELSE 2
           ws-cliente.nome-abrev   = tt-ped-venda-import.CNPJ                             
           ws-cliente.nome-emit    = tt-ped-venda-import.CNPJ                             
           ws-cliente.rg           = ""                                                   
           ws-cliente.situacao     = "ATIVO"                                              
           ws-cliente.telefone1    = tt-ped-venda-import.telefone                         
           ws-cliente.telefone2    = tt-ped-venda-import.telefone               .

    ASSIGN ws-cliente.cod-rep      = INTEGER(tt-ped-venda-import.NomeRepresentante) NO-ERROR.

    RETURN "OK".

END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE avaliarAlocacao:


    ASSIGN deAloc = 0.
    FOR EACH ped-ent OF ped-item NO-LOCK:

        ASSIGN deAloc   = deAloc + ped-ent.qt-log-aloca .

/*             MESSAGE                                */
/*                 ped-ent.qt-pedida SKIP             */
/*                 ped-ent.qt-log-aloca               */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */


    END.

    IF deAloc = ped-item.qt-pedida THEN DO:
        RUN atualizarSituacaoWeb ("PEND VOL").
    END.
    
    RETURN "OK".

END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE receberNumeroPedidoBO:
DEFINE INPUT  PARAMETER pPedido AS CHARACTER   NO-UNDO.

    ASSIGN cNumPedidoBO  = pPedido.

END PROCEDURE.

