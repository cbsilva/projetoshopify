/*----------------------------------------------------------------------------------------------/
 Programa..: esint0022a.p
 Objetivo..: Interface Integra��o Pedido Venda - Importa��o
 Data......: 10/08/2021
 Autor.....: 4Make Consultoria
 Vers�o....: 2.000.001
-----------------------------------------------------------------------------------------------*/

/******************************* Definitions **************************************************/
/* --------------------------------------------------------------------------------------------
    Import Objects - JSON Definitions
----------------------------------------------------------------------------------------------*/
USING PROGRESS.Json.ObjectModel.*.

/* --------------------------------------------------------------------------------------------
    Temp-Tables Definitions
----------------------------------------------------------------------------------------------*/
{method/dbotterr.i}

/* --------------------------------------------------------------------------------------------
    Global  Variable Definitions
----------------------------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------------------------
    Local Variable Definitions
----------------------------------------------------------------------------------------------*/
DEFINE VARIABLE cLongJson        AS LONGCHAR          NO-UNDO.
DEFINE VARIABLE lRetJson         AS LOGICAL           NO-UNDO.
DEFINE VARIABLE body             AS JsonObject        NO-UNDO.
DEFINE VARIABLE jsonOutput       AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonObject      AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonObjectMain  AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonObjectSec   AS JsonObject        NO-UNDO.
DEFINE VARIABLE oJsonArrayMain   AS JsonArray         NO-UNDO.
DEFINE VARIABLE oJsonArraySec    AS JsonArray         NO-UNDO.
DEFINE VARIABLE iCountMain       AS INTEGER           NO-UNDO.
DEFINE VARIABLE iCountSec        AS INTEGER           NO-UNDO.
DEFINE VARIABLE mJson            AS MEMPTR            NO-UNDO.
DEFINE VARIABLE myParser         AS ObjectModelParser NO-UNDO. 
DEFINE VARIABLE pJsonInput       AS JsonObject        NO-UNDO.

def var iMoeda as int.
def var cEstab  as char.
def var iCondPagto as int.

{utp/ut-glob.i}
{esp/esspf022.i}
{esp/esspf022ped.i}


DEF VAR lDebuga AS LOG INITIAL no.

/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM TABLE FOR ttPedido.
DEFINE INPUT  PARAM TABLE FOR ttItensPedido.
DEFINE OUTPUT PARAM TABLE FOR RowErrors.

/******************************* Main Block **************************************************/
find first es-api-param-ped-spf no-lock no-error.

if avail es-api-param-ped-spf then
    assign iMoeda = es-api-param-ped-spf.mo-codigo
           cEstab = es-api-param-ped-spf.cod-estabel.
           
if not can-find(first mgcad.moeda where moeda.mo-codigo = iMoeda) then
    iMoeda = 0.

if not can-find(first estabelec where estabelec.cod-estabel = cEstab) then
    cEstab = '1'.

iCondPagto = 1.

IF lDebuga THEN 
    message 'esspf022a.p' view-as alert-box.

criaPedido:

DO TRANS 
    ON ENDKEY UNDO criaPedido, RETURN "NOK"
    ON ERROR  UNDO criaPedido, RETURN "NOK"
    ON STOP   UNDO criaPedido, RETURN "NOK":


    FOR EACH ttPedido no-lock.
    
        IF lDebuga THEN
            message 'CNPJ: ' ttPedido.cnpjEmitente skip
                    'PEDIDO: ' ttPedido.PedidoCliente skip
                    'SHOPIFY: ' ttPedido.pedidoShopify view-as alert-box.
    
        FIND FIRST emitente 
             WHERE emitente.cgc = ttPedido.cnpjEmitente NO-LOCK NO-ERROR.
    
        IF NOT AVAIL emitente THEN DO:
    
            CREATE rowErrors.
            ASSIGN rowerrors.errornumber     = 999
                   rowErrors.ErrorType = 'Error'
                   rowerrors.errordescription = 'N�o encontrado cliente com CNPJ informado: ' + ttPedido.cnpjEmitente.
                   
              UNDO criaPedido, RETURN "NOK":U.         
                   
    
        END.
        ELSE DO:
        
            find first ped-venda 
                 where ped-venda.nome-abrev = emitente.nome-abrev 
                   and ped-venda.nr-pedcli = 'DG' + ttPedido.PedidoCliente no-lock no-error.
            
            
            IF AVAIL ped-venda THEN DO:
            
                CREATE rowErrors.
                ASSIGN rowerrors.errornumber     = 999
                       rowErrors.ErrorType = 'Error'
                       rowerrors.errordescription = 'J� registrado pedido ' +  'DG' + ttPedido.PedidoCliente +  ' para o cliente ' +  emitente.nome-abrev.
        
                UNDO criaPedido, RETURN "NOK":U.         

            
            END.
            ELSE DO:
            
                IF lDebuga THEN
                    message 'validou' SKIP EMITENTE.COD-EMITENTE SKIP EMITENTE.NOME-ABREV view-as alert-box.
            
        
                empty temp-table tt-ped-venda.
        
                CREATE tt-ped-venda.
                ASSIGN //tt-ped-venda.cod-estabel       = cEstab
                       tt-PED-VENDA.cod-emitente      = emitente.cod-emitente
                       tt-ped-venda.nome-abrev        = emitente.nome-abrev
                       tt-ped-venda.nr-pedcli         = 'DG' + ttPedido.pedidoCliente 
                       tt-ped-venda.nr-pedrep         = ttPedido.pedidoCliente
                       tt-ped-venda.mo-codigo         = iMoeda
                       tt-ped-venda.mo-FATUR          = iMoeda
                       tt-ped-venda.cod-cond-pag      = iCondPagto
                       tt-ped-venda.nr-pedido         = next-value(seq-nr-pedido).
        
                if  not valid-handle(hbodi159sdf) or
                
                    hbodi159sdf:type      <> "PROCEDURE":U or
                    hbodi159sdf:file-name <> "dibo/bodi159sdf.p":U then
                    run dibo/bodi159sdf.p persistent set hBOdi159sdf.
        
        
                run inputTable in hBOdi159sdf (INPUT TABLE tt-ped-venda).
                
                
                RUN setDefaultCustomerName IN hBOdi159sdf. 
                RUN setDefaultDelivery  IN hBOdi159sdf.
                RUN setDefaultDstChannel IN hBOdi159sdf.
                RUN setDefaultPaymentTerm IN hBOdi159sdf.
                RUN setDefaultPriceTable IN hBOdi159sdf.
                RUN setDefaultTransactionType IN hBOdi159sdf. 
                RUN setDefaultCentralSales IN hBOdi159sdf.
                RUN outputTable IN hBOdi159sdf (OUTPUT TABLE tt-ped-venda).
                
        
                FIND FIRST tt-ped-venda.
                
                ASSIGN tt-ped-venda.tp-preco                       = 1 // informado
                       tt-ped-venda.nr-tabpre                      = ''
      //               tt-ped-venda.no-ab-reppri                   = '1'
                       tt-ped-venda.perc-desco1                    = 0
                       tt-ped-venda.perc-desco2                    = 0
                       tt-ped-venda.val-desconto-total             = 0
                       tt-ped-venda.val-pct-desc-politic-bonifi    = 0 
                       tt-ped-venda.val-pct-desconto-total         = 0 
                       tt-ped-venda.val-pct-desconto-tab-preco     = 0 
                       tt-ped-venda.val-pct-desconto-valor         = 0
                       tt-ped-venda.vl-desconto                    = 0
                       tt-ped-venda.log-usa-tabela-desconto        = FALSE
                       tt-ped-venda.cod-cond-pag                   = iCondPagto
                       tt-ped-venda.mo-codigo                      = iMoeda            
                       tt-ped-venda.mo-FATUR                       = iMoeda
                       tt-ped-venda.cod-estabel                    = cEstab.
       
                if  valid-handle(hbodi159sdf) then
                    DELETE PROCEDURE hbodi159sdf.
        
                CREATE tt-ped-param.
        
                if  not valid-handle(hbodi159) or
                    hbodi159:type      <> "PROCEDURE":U or
                    hbodi159:file-name <> "dibo/bodi159.p":U then
                    run dibo/bodi159.p persistent set hbodi159.
        
                Run emptyRowObject in hbodi159.
                RUN inputRowParam in hbodi159(input table tt-ped-param).
        
                run openQueryStatic in hbodi159(input "Main":U).
                run setRecord in hbodi159(input table tt-ped-venda).
                //run emptyRowErrors in hbodi159.
                run createRecord   in hbodi159.
                run getRowErrors   in hbodi159(output table RowErrors).
        
                if  can-find(first RowErrors where RowErrors.ErrorSubType = "ERROR":U  ) then do:
        
                      UNDO criaPedido, RETURN "NOK":U.         

                    IF lDebuga then do:
                        for each rowerrors:
            
            
                            MESSAGE  rowerrors.errornumber
                                     rowerrors.errordescription
                                     rowerrors.ErrorParameters 
                                     rowerrors.ErrorType VIEW-AS ALERT-BOX
                                     TITLE "Erros na Implantacao de Pedidos". 
            
                        end. 
                    end.
                    
                end.
                ELSE DO:
        
        
                    FIND FIRST tt-ped-venda.
        
                    FOR EACH ttItensPedido NO-LOCK.
        
                        IF lDebuga THEN            
                            MESSAGE ttItensPedido.nrSEqPed skip ttItensPedido.codigoITem skip ttItensPedido.precounit skip ttItensPedido.qtdpedida view-as alert-box.
        
                        FIND FIRST ITEM 
                             WHERE ITEM.it-codigo = ttItensPedido.codigoItem NO-LOCK NO-ERROR.
                             
                        if not available item then do:
                            
                            CREATE rowErrors.
                            ASSIGN rowerrors.errornumber     = 999
                                   rowErrors.ErrorType = 'Error'
                                   rowerrors.errordescription = 'N�o encontrado item com codigo informado: ' + ttItensPedido.codigoItem.
                    
                            
                              UNDO criaPedido, RETURN "NOK":U.         

                        
                        end.                     
                        else do:
                                             
                            EMPTY TEMP-TABLE tt-ped-item.
                            create  tt-ped-item.
                            assign  tt-ped-item.nome-abrev         = tt-ped-venda.nome-abrev
                                    tt-ped-item.nr-pedcli          = tt-ped-venda.nr-pedcli
                                    tt-ped-item.nr-sequencia       = ttItensPedido.nrSeqPed
                                    tt-ped-item.it-codigo          = ITEM.it-codigo
                                    tt-ped-item.nat-operacao       = tt-ped-venda.nat-operacao
                                    tt-ped-item.vl-preuni          = ttItensPedido.precoUnit
                                    tt-ped-item.vl-preori          = ttItensPedido.precoUnit
                                    tt-ped-item.qt-un-fat          = ttItensPedido.qtdPedida
                                    tt-ped-item.qt-pedida          = ttItensPedido.qtdPedida
                                    tt-ped-item.cod-unid-neg       = ITEM.cod-unid-neg.
            
            
                            FIND FIRST natur-oper 
                                 WHERE natur-oper.nat-operacao = tt-ped-venda.nat-operacao NO-LOCK NO-ERROR.
            
                              if  not valid-handle(hbodi154) or
                                  hbodi154:type <> "PROCEDURE":U or
                                  hbodi154:file-name <> "dibo/bodi154.p":U then do:           
                                  run dibo/bodi154.p persistent set hbodi154.
                                  run openQueryStatic in hbodi154(input  "Default":U).
                              end.
            
                              //EMPTY TEMP-TABLE rowerrors.
            
            
                              if  not valid-handle(hbodi154sdf) or
                                  hbodi154sdf:type      <> "PROCEDURE":U or
                                  hbodi154sdf:file-name <> "dibo/bodi154sdf.p":U then
                                  run dibo/bodi154sdf.p persistent set hBOdi154sdf.
            
            
                              run inputParentTable in hBOdi154sdf (INPUT TABLE tt-ped-venda).
                              run inputTable in hBOdi154sdf (INPUT TABLE tt-ped-item).
            
                              RUN setDefaultItem IN hBOdi154sdf. 
                              
                              RUN setDefaultDiscount  IN hBOdi154sdf.
                              RUN setDefaultDescontoICMS IN hBOdi154sdf.
                              RUN setDefaultPriceTable IN hBOdi154sdf. 
                              
                              RUN setDefaultTransactionType IN hBOdi154sdf.
                              RUN setDefaultQuantity IN hBOdi154sdf.
                              
                              RUN setDefaultUOM IN hBOdi154sdf.
                              RUN outputTable IN hBOdi154sdf (OUTPUT TABLE tt-ped-item).
            
                              find first tt-ped-item.
                              assign tt-ped-item.nr-tabpre          = ''
                                   tt-ped-item.qt-un-fat          = ttItensPedido.qtdPedida
                                     tt-ped-item.qt-pedida          = ttItensPedido.qtdPedida
                                     tt-ped-item.vl-preuni          = ttItensPedido.precoUnit
                                     tt-ped-item.vl-preori          = ttItensPedido.precoUnit
                                     tt-ped-item.tp-preco           = 1.
            
                              run openQueryStatic     in hbodi154(input  "Default":U).
                              //run emptyRowErrors      in hbodi154.
                              run setRecord           in hbodi154(input table tt-ped-item).
                              run createRecord        in hbodi154.
                              run getRowErrors        in hbodi154(output table RowErrors).
            
                              if  can-find(first RowErrors where RowErrors.ErrorSubType = "ERROR":U  ) then do:
            
                                  UNDO criaPedido, RETURN "NOK":U.         

                                  IF lDebuga then do:
    
                                    for each rowerrors:
            
                                          MESSAGE  rowerrors.errornumber
                                                   rowerrors.errordescription
                                                   rowerrors.ErrorParameters 
                                                   rowerrors.ErrorType VIEW-AS ALERT-BOX
                                                   TITLE "Erros na Implantacao de Itens do Pedido". 
                                      end. 
                                      
                                    END.
                                    
                              END. /* can-find(First rowerrors */
            
                              /*elimina todos os handles usado na bodi154*/
                              if  valid-handle(hbodi154) then do:
                                  run destroyBO in hbodi154.
                                  RUN destroy IN hbodi154.
                                  ASSIGN hbodi154 = ?.
                              end. 
            
                              if  valid-handle(hbodi154sdf) then
                                  DELETE PROCEDURE hbodi154sdf.
                                  
                           end.
        
                    END. // EACH ttItensPedido
                    
                    
        //            message 'fim execucao bo ' view-as alert-box.
                    
        
                if not can-find(First rowerrors) then do:
        
                        if  not valid-handle(hbodi157) or
                            hbodi157:type      <> "PROCEDURE":U or
                            hbodi157:file-name <> "dibo/bodi157.p":U then
                            run dibo/bodi157.p persistent set hbodi157.
                        
                        
                            FIND FIRST ped-venda 
                                 WHERE ped-venda.nome-abrev = tt-ped-venda.nome-abrev 
                                   AND ped-venda.nr-pedcli  = tt-ped-venda.nr-pedcli NO-LOCK NO-ERROR.
                        
                        
                            IF AVAIL ped-venda THEN DO:
                            
                                run createordersrepresentatives in hbodi157(INPUT rowid(ped-venda)).
                            
                                if  valid-handle(hbodi157) then
                                    DELETE PROCEDURE hbodi157.
                            
                                if  not valid-handle(hbodi159com) or
                                    hbodi159com:type      <> "PROCEDURE":U or
                                    hbodi159com:file-name <> "dibo/bodi159com.p":U then
                                    run dibo/bodi159com.p persistent set hbodi159com.
                            
                                run completeOrder  in hbodi159com(INPUT rowid(ped-venda),
                                                                  OUTPUT TABLE RowErrors).
                            
                                RUN getRowErrors        in hbodi159com(output table RowErrors).  
    
                                RUN piAprovarCredito (INPUT ROWID(ped-venda),
                                                      INPUT "Pedidos Shopify").
                                
                                /*
                                FIND CURRENT ped-venda SHARE-LOCK NO-ERROR.
                                
                               ASSIGN ped-venda.dsp-pre-fat = YES
                                      ped-venda.dt-apr-cred = TODAY
                                      ped-venda.cod-sit-aval = 3.
                                */
                                
                            END.
                        
                        // DESABILITA MENSAGEM ERRO DO CREDITO             
                        for each rowerrors where rowerrors.errornumber = 8259.
                            
                            DELETE ROWERRORS.
                            
                        end.
                        
                        IF NOT CAN-FIND(FIRST rowErrors WHERE rowErrors.ErrorType = 'Error') THEN DO:
            
                            IF lDebuga then
                                message 'criando web-ped-venda' skip tt-ped-venda.nome-abrev skip tt-ped-venda.nr-pedcli view-as alert-box warning.    
                
                            CREATE web-ped-venda.
                            ASSIGN web-ped-venda.cod-estabel                = tt-ped-venda.cod-estabel
                                   web-ped-venda.nome-abrev                 = tt-ped-venda.nome-abrev
                                   web-ped-venda.nr-pedcli                  = tt-ped-venda.nr-pedcli
                                   web-ped-venda.it-codigo                  = ITEM.it-codigo
                                   web-ped-venda.arquivo-descricao          = "Pedido SHOPFIY"
                                   web-ped-venda.nr-sequencia               = tt-ped-item.nr-sequencia
                                   web-ped-venda.nr-nota-fisc               = ""
                                   web-ped-venda.serie                      = "" 
                                   web-ped-venda.nr-seq-fat                 = 0 
                                   web-ped-venda.situacao                   = 'PEND FAT'
                                   web-ped-venda.dt-emis-ped-venda          = TODAY 
                                   web-ped-venda.dt-emis-nota-fiscal        = ? 
                                   web-ped-venda.log-saldo-fisico           = FALSE 
                                   web-ped-venda.dat-saldo-fisico           = ?
                                   web-ped-venda.aprov-saldo-fis            = "" 
                                   web-ped-venda.quantidade                 = 1
                                   web-ped-venda.qtde-saldo-fisico          = 1
                                   web-ped-venda.qtde-confirmada-fat        = 1
                                   web-ped-venda.dt-gera-pedido             = TODAY                                                                                                                                                                       
                                   web-ped-venda.hr-pedido-web              = STRING(TIME, "HH:MM")                                                                                                                                                        
                                   web-ped-venda.vlr-unit                   = tt-ped-item.vl-preuni
                                   web-ped-venda.e-mail                     = emitente.e-mail
                                   web-ped-venda.telefone                   = emitente.telefone[1]
                                   web-ped-venda.cnpj                       = emitente.cgc
                                   web-ped-venda.nr-conhecimento            = ""
                                   web-ped-venda.dt-embarque                = ?
                                   web-ped-venda.nro-pedido                 = ''
                                   web-ped-venda.flg_restricao              = NO
                                   web-ped-venda.des_restricao              = ''
                             
                                  web-ped-venda.qtd_importada              = 1.
                                  
                                  
                            create es-ped-venda-spf.
                            assign es-ped-venda-spf.nome-abrev = tt-ped-venda.nome-abrev
                                   es-ped-venda-spf.nr-pedcli  = tt-ped-venda.nr-pedcli
                                   es-ped-venda-spf.nr-shopify = ttPedido.pedidoShopify
                                   es-ped-venda-spf.dt-pagamento = today. //date(ttPedido.dataPagamento).
                            
                                    
                        END.
                        ELSE DO:
                            UNDO criaPedido, RETURN "NOK":U.         
                        END.
            
                    END. // NOT CAN-FIND(fIRST ROWERRORS) 
        
                END. // NOT CAN-FIND(FIRST ROWERRORS)
        
            END.  // NOT AVAIL PED-VENDA
            
        END. /* AVAIL EMITENTE WHERE EMITENTE.CGC */
    
    END. // EACH TTPEDIDO  

END. // TRANS


PROCEDURE piAprovarCredito:
    DEFINE INPUT PARAM rw-ped-venda        AS ROWID       NO-UNDO.
    DEFINE INPUT PARAM c-motivo-aprovacao  AS CHARACTER   NO-UNDO.


    def var  tb-aprovbloq-desconto     as logical no-undo. /* Toggle-box Desconto */   
    def var  dat-lib-desconto          as date    no-undo. /* Data Desconto */
    def var  desc-lib-desconto         as char    no-undo. /* Motivo Bloqueio Desconto */
    def var  usuario-desconto          as char    no-undo. /* Usuario Desconto - tela */
    def var  tb-aprovbloq-cotas        as logical no-undo. /* Toggle-box Cotas */ 
    def var  dat-alter-sit             as date    no-undo. /* Data Alteracao Cotas */
    def var  motivo-quota              as char    no-undo. /* Motivo Alteracao Cotas */
    def var  usuario-cotas             as char    no-undo. /* Usuario Cotas - tela */   
    def var  tb-aprovbloq-preco        as logical no-undo. /* Toggle-box Preco */   
    def var  dat-aprov-preco           as date    no-undo. /* Data Preco */
    def var  desc-lib-preco            as char    no-undo. /* Motivo Preco */
    def var  usuario-preco             as char    no-undo. /* Usuario Preco - tela */


    

    RUN openQueryStatic        IN hbodi159 (INPUT "Default":U).
    RUN setUserLog IN hbodi159(INPUT "super").

    RUN validaPermissoesUsuario IN hbodi159( input YES                  ,  /* Toggle-box Credito */
                                             input TODAY                ,  /* Data Credito */ 
                                             input c-motivo-aprovacao   ,  /* Motivo Alteracao Credito */
                                             input "super"              ,  /* Usuario Credito - tela */
                                             input tb-aprovbloq-desconto,  /* Toggle-box Desconto */   
                                             input dat-lib-desconto     ,  /* Data Desconto */
                                             input desc-lib-desconto    ,  /* Motivo Bloqueio Desconto */
                                             input usuario-desconto     ,  /* Usuario Desconto - tela */
                                             input tb-aprovbloq-cotas   ,  /* Toggle-box Cotas */ 
                                             input dat-alter-sit        ,  /* Data Alteracao Cotas */
                                             input motivo-quota         ,  /* Motivo Alteracao Cotas */
                                             input usuario-cotas        ,  /* Usuario Cotas - tela */
                                             input tb-aprovbloq-preco   ,  /* Toggle-box Preco */   
                                             input dat-aprov-preco      ,  /* Data Preco */
                                             input desc-lib-preco       ,  /* Motivo Preco */
                                             input usuario-preco        ,  /* Usuario Preco - tela */
                                             input "apr"                ,  /* Variavel informa se aprovacao ou reprovacao */
                                             input STRING(rw-ped-venda)      ,  
                                             output table RowErrors).


    IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR") THEN DO:
        LOG-MANAGER:WRITE-MESSAGE("Hove problema ao tentar aprovar credito") NO-ERROR.
    END.

     
    IF VALID-HANDLE(hbodi159) THEN
        DELETE PROCEDURE hbodi159.

    RETURN "OK":U.

END PROCEDURE.
    

