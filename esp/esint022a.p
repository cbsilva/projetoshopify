/*----------------------------------------------------------------------------------------------/
 Programa..: esint0022a.p
 Objetivo..: Interface Integraá∆o Pedido Venda - Importaá∆o
 Data......: 10/08/2021
 Autor.....: 4Make Consultoria
 Vers∆o....: 2.000.001
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
def var cNatureza as char.
def var iTotItem as int.

{utp/ut-glob.i}
{esp/esint022.i}
{esp/esint022ped.i}


DEF VAR lDebuga AS LOG INITIAL no.

/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM TABLE FOR ttPedido.
DEFINE INPUT  PARAM TABLE FOR ttItensPedido.
DEFINE OUTPUT PARAM TABLE FOR RowErrors.

def var cPedido as char.

/******************************* Main Block **************************************************/
find first es-api-param-ped no-lock no-error.

if avail es-api-param-ped then
    assign iMoeda = es-api-param-ped.mo-codigo
           cEstab = es-api-param-ped.cod-estabel.
           
if not can-find(first mgcad.moeda where moeda.mo-codigo = iMoeda) then
    iMoeda = 0.

if not can-find(first estabelec where estabelec.cod-estabel = cEstab) then
    cEstab = '1'.

iCondPagto = 1.


IF lDebuga THEN 
    message 'esint022a.p' view-as alert-box.

criaPedido:

DO TRANS 

    ON ENDKEY UNDO criaPedido, RETURN "NOK"
    ON ERROR  UNDO criaPedido, RETURN "NOK"
    ON STOP   UNDO criaPedido, RETURN "NOK":

    find first es-api-param-cliente no-lock no-error.

    RUN carregarBO.

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
                   rowerrors.errordescription = 'N∆o encontrado cliente com CNPJ informado: ' + ttPedido.cnpjEmitente.

              RUN descarregarBO.                   
              UNDO criaPedido, RETURN "NOK":U.         
                   
    
        END.
        ELSE DO:
        
        
            assign cPedido = 'DG000' + ttPedido.pedidoCliente.
            
            IF AVAIL es-api-param-cliente THEN
                cNatureza = if emitente.estado = 'SP' THEN es-api-param-cliente.nat-operacao else es-api-param-cliente.nat-ope-ext.
                
                /*
            if not can-find(first natur-oper where natur-oper.nat-operacao = cNatureza) then
                cNatureza = emitente.nat-operacao.                
                  */  
        
            find first ped-venda 
                 where ped-venda.nome-abrev = emitente.nome-abrev 
                   and ped-venda.nr-pedcli =  cPedido no-lock no-error.
            
            
            IF AVAIL ped-venda THEN DO:
            
                CREATE rowErrors.
                ASSIGN rowerrors.errornumber     = 999
                       rowErrors.ErrorType = 'Error'
                       rowerrors.errordescription = 'J† registrado pedido ' +  cPedido  + ttPedido.PedidoCliente  +  ' para o cliente ' +  emitente.nome-abrev.
        
                RUN descarregarBO.                   
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
                       tt-ped-venda.nr-pedcli         = cPedido 
                       tt-ped-venda.nr-pedrep         = ttPedido.pedidoCliente
                       tt-ped-venda.mo-codigo         = iMoeda
                       tt-ped-venda.mo-FATUR          = iMoeda
                       tt-ped-venda.cod-cond-pag      = iCondPagto
                       tt-ped-venda.nr-pedido         = next-value(seq-nr-pedido)
                       tt-ped-venda.nat-operacao      = cNatureza.
                       
        
                /*
                if  not valid-handle(h_bodi159sdf) or
                
                    h_bodi159sdf:type      <> "PROCEDURE":U or
                    h_bodi159sdf:file-name <> "dibo/bodi159sdf.p":U then
                    run dibo/bodi159sdf.p persistent set h_bodi159sdf.
                */ 
        
                run inputTable in h_bodi159sdf (INPUT TABLE tt-ped-venda).
                
                
                RUN setDefaultCustomerName IN h_bodi159sdf. 
                RUN setDefaultDelivery  IN h_bodi159sdf.
                RUN setDefaultDstChannel IN h_bodi159sdf.
                RUN setDefaultPaymentTerm IN h_bodi159sdf.
                RUN setDefaultPriceTable IN h_bodi159sdf.
                RUN setDefaultTransactionType IN h_bodi159sdf. 
                RUN setDefaultCentralSales IN h_bodi159sdf.
                RUN outputTable IN h_bodi159sdf (OUTPUT TABLE tt-ped-venda).
                
        
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
                       tt-ped-venda.cod-estabel                    = cEstab
                       tt-ped-venda.nat-operacao                   = cNatureza.
       
                /*
                if  valid-handle(h_bodi159sdf) then
                    DELETE PROCEDURE h_bodi159sdf.
                */
                  
                CREATE tt-ped-param.
        
                /*
                if  not valid-handle(h_bodi159) or
                    h_bodi159:type      <> "PROCEDURE":U or
                    h_bodi159:file-name <> "dibo/bodi159.p":U then
                    run dibo/bodi159.p persistent set h_bodi159.
                */
                
                Run emptyRowObject in h_bodi159.
                RUN inputRowParam in h_bodi159(input table tt-ped-param).
        
                run openQueryStatic in h_bodi159(input "Main":U).
                run setRecord in h_bodi159(input table tt-ped-venda).
                //run emptyRowErrors in h_bodi159.
                run createRecord   in h_bodi159.
                run getRowErrors   in h_bodi159(output table RowErrors).
        
                if  can-find(first RowErrors where RowErrors.ErrorSubType = "ERROR":U  ) then do:        
                      
                    IF lDebuga then do:
                        for each rowerrors:
            
            
                            MESSAGE  rowerrors.errornumber
                                     rowerrors.errordescription
                                     rowerrors.ErrorParameters 
                                     rowerrors.ErrorType VIEW-AS ALERT-BOX
                                     TITLE "Erros na Implantacao de Pedidos". 
            
                        end. 
                    end.

                    RUN descarregarBO.                   
                    UNDO criaPedido, RETURN "NOK":U.         

                    
                end.
                ELSE DO:
        
        
                    FIND FIRST tt-ped-venda.
        
                    iTotItem = 0.
                    FOR EACH ttItensPedido NO-LOCK.
        
                        IF lDebuga THEN            
                            MESSAGE ttItensPedido.nrSEqPed skip ttItensPedido.codigoITem skip ttItensPedido.precounit skip ttItensPedido.qtdpedida view-as alert-box.
        
                        FIND FIRST ITEM 
                             WHERE ITEM.it-codigo = ttItensPedido.codigoItem NO-LOCK NO-ERROR.
                             
                        if not available item then do:
                            
                            CREATE rowErrors.
                            ASSIGN rowerrors.errornumber     = 999
                                   rowErrors.ErrorType = 'Error'
                                   rowerrors.errordescription = 'N∆o encontrado item com codigo informado: ' + ttItensPedido.codigoItem.
                        
                              RUN descarregarBO.                   
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
            
                            iTotItem = iTotItem + tt-ped-item.qt-pedida.
            
                            FIND FIRST natur-oper 
                                 WHERE natur-oper.nat-operacao = tt-ped-venda.nat-operacao NO-LOCK NO-ERROR.
            
                              /*
                              if  not valid-handle(h_bodi154) or
                                  h_bodi154:type <> "PROCEDURE":U or
                                  h_bodi154:file-name <> "dibo/bodi154.p":U then do:           
                                  run dibo/bodi154.p persistent set h_bodi154.
                                  run openQueryStatic in h_bodi154(input  "Default":U).
                              end.
                              */
            
                              //EMPTY TEMP-TABLE rowerrors.
            
                              /*
                              if  not valid-handle(h_bodi154sdf) or
                                  h_bodi154sdf:type      <> "PROCEDURE":U or
                                  h_bodi154sdf:file-name <> "dibo/bodi154sdf.p":U then
                                  run dibo/bodi154sdf.p persistent set h_bodi154sdf.
                              */
            
                              run inputParentTable in h_bodi154sdf (INPUT TABLE tt-ped-venda).
                              run inputTable in h_bodi154sdf (INPUT TABLE tt-ped-item).
            
                              RUN setDefaultItem IN h_bodi154sdf. 
                              
                              RUN setDefaultDiscount  IN h_bodi154sdf.
                              RUN setDefaultDescontoICMS IN h_bodi154sdf.
                              RUN setDefaultPriceTable IN h_bodi154sdf. 
                              
                              RUN setDefaultTransactionType IN h_bodi154sdf.
                              RUN setDefaultQuantity IN h_bodi154sdf.
                              
                              RUN setDefaultUOM IN h_bodi154sdf.
                              RUN outputTable IN h_bodi154sdf (OUTPUT TABLE tt-ped-item).
            
                              find first tt-ped-item.
                              assign tt-ped-item.nr-tabpre          = ''
                                   tt-ped-item.qt-un-fat          = ttItensPedido.qtdPedida
                                     tt-ped-item.qt-pedida          = ttItensPedido.qtdPedida
                                     tt-ped-item.vl-preuni          = ttItensPedido.precoUnit
                                     tt-ped-item.vl-preori          = ttItensPedido.precoUnit
                                     tt-ped-item.tp-preco           = 1.
            
                              run openQueryStatic     in h_bodi154(input  "Default":U).
                              //run emptyRowErrors      in h_bodi154.
                              run setRecord           in h_bodi154(input table tt-ped-item).
                              run createRecord        in h_bodi154.
                              run getRowErrors        in h_bodi154(output table RowErrors).
            
                              if  can-find(first RowErrors where RowErrors.ErrorSubType = "ERROR":U  ) then do:
            
                                 IF lDebuga then do:
   
                                   for each rowerrors:
           
                                         MESSAGE  rowerrors.errornumber
                                                  rowerrors.errordescription
                                                  rowerrors.ErrorParameters 
                                                  rowerrors.ErrorType VIEW-AS ALERT-BOX
                                                  TITLE "Erros na Implantacao de Itens do Pedido". 
                                     end. 
                                     
                                  END.
                                  RUN descarregarBO.                   
                                  UNDO criaPedido, RETURN "NOK":U.         

                                    
                              END. /* can-find(First rowerrors */
            
                              /*elimina todos os handles usado na bodi154
                              if  valid-handle(h_bodi154) then do:
                                  run destroyBO in h_bodi154.
                                  RUN destroy IN h_bodi154.
                                  ASSIGN h_bodi154 = ?.
                              end. 
            
                              if  valid-handle(h_bodi154sdf) then
                                  DELETE PROCEDURE h_bodi154sdf.
                               */
                                  
                           end.
        
                    END. // EACH ttItensPedido
                    
                    
        //            message 'fim execucao bo ' view-as alert-box.
                    
        
                    /*
                if  valid-handle(h_bodi159) then
                    DELETE PROCEDURE h_bodi159.
                    */
                    
                
                    
                    
                if not can-find(First rowerrors) then do:
        
                        /*
                        if  not valid-handle(h_bodi157) or
                            h_bodi157:type      <> "PROCEDURE":U or
                            h_bodi157:file-name <> "dibo/bodi157.p":U then
                            run dibo/bodi157.p persistent set h_bodi157.
                        */
                        
                            FIND FIRST ped-venda 
                                 WHERE ped-venda.nome-abrev = tt-ped-venda.nome-abrev 
                                   AND ped-venda.nr-pedcli  = tt-ped-venda.nr-pedcli NO-LOCK NO-ERROR.
                        
                        
                            IF AVAIL ped-venda THEN DO:
                            
                                run createordersrepresentatives in h_bodi157 (INPUT rowid(ped-venda)).
                            
                            /*
                                if  valid-handle(h_bodi157) then
                                    DELETE PROCEDURE h_bodi157.
                            
                                if  not valid-handle(h_bodi159com) or
                                    h_bodi159com:type      <> "PROCEDURE":U or
                                    h_bodi159com:file-name <> "dibo/bodi159com.p":U then
                                    run dibo/bodi159com.p persistent set h_bodi159com.
                            */
                            
                                run completeOrder  in h_bodi159com(INPUT rowid(ped-venda),
                                                                  OUTPUT TABLE RowErrors).
                            
                                RUN getRowErrors        in h_bodi159com(output table RowErrors).  
                                
                                /*
                                if  valid-handle(h_bodi159com) then
                                    DELETE PROCEDURE h_bodi159com.
                                
    
                                
                                RUN piAprovarCredito (INPUT ROWID(ped-venda),
                                                      INPUT "Pedidos Shopify").
                                */
                                
                                FIND CURRENT ped-venda SHARE-LOCK NO-ERROR.
                                
                               ASSIGN ped-venda.dsp-pre-fat = YES
                                      ped-venda.dt-apr-cred = TODAY
                                      ped-venda.cod-sit-aval = 3.
                                
                                
                            END.
                        
                        // DESABILITA MENSAGEM ERRO DO CREDITO             
                        for each rowerrors where rowerrors.errornumber = 8259.
                            
                            DELETE ROWERRORS.
                            
                        end.
                        
                        IF NOT CAN-FIND(FIRST rowErrors WHERE rowErrors.ErrorType = 'Error') THEN DO:
            
                            IF lDebuga then
                                message 'criando web-ped-venda' skip tt-ped-venda.nome-abrev skip tt-ped-venda.nr-pedcli view-as alert-box warning.    
                
                
                            /* SOMENTE UM REGISTRO POR PEDIDO"
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
                                   web-ped-venda.quantidade                 = iTotItem
                                   web-ped-venda.qtde-saldo-fisico          = iTotItem
                                   web-ped-venda.qtde-confirmada-fat        = iTotItem
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
                             
                                  web-ped-venda.qtd_importada              = iTotItem.
                            */
                            
                            FOR EACH ttItensPedido NO-LOCK.
        
                                
                                FIND FIRST ITEM 
                                     WHERE ITEM.it-codigo = ttItensPedido.codigoItem NO-LOCK NO-ERROR.
                                     
                                CREATE web-ped-venda.
                                ASSIGN web-ped-venda.cod-estabel                = tt-ped-venda.cod-estabel
                                       web-ped-venda.nome-abrev                 = tt-ped-venda.nome-abrev
                                       web-ped-venda.nr-pedcli                  = tt-ped-venda.nr-pedcli
                                       web-ped-venda.it-codigo                  = ITEM.it-codigo
                                       web-ped-venda.arquivo-descricao          = "Pedido SHOPFIY"
                                       web-ped-venda.nr-sequencia               = ttItensPedido.nrSeqPed
                                       web-ped-venda.nr-nota-fisc               = ""
                                       web-ped-venda.serie                      = "" 
                                       web-ped-venda.nr-seq-fat                 = 0 
                                       web-ped-venda.situacao                   = 'PEND FAT'
                                       web-ped-venda.dt-emis-ped-venda          = TODAY 
                                       web-ped-venda.dt-emis-nota-fiscal        = ? 
                                       web-ped-venda.log-saldo-fisico           = FALSE 
                                       web-ped-venda.dat-saldo-fisico           = ?
                                       web-ped-venda.aprov-saldo-fis            = "" 
                                       web-ped-venda.quantidade                 = ttItensPedido.qtdPedida
                                       web-ped-venda.qtde-saldo-fisico          = ttItensPedido.qtdPedida
                                       web-ped-venda.qtde-confirmada-fat        = ttItensPedido.qtdPedida
                                       web-ped-venda.dt-gera-pedido             = TODAY                                                                                                                                                                       
                                       web-ped-venda.hr-pedido-web              = STRING(TIME, "HH:MM")                                                                                                                                                        
                                       web-ped-venda.vlr-unit                   = ttItensPedido.precoUni
                                       web-ped-venda.e-mail                     = emitente.e-mail
                                       web-ped-venda.telefone                   = emitente.telefone[1]
                                       web-ped-venda.cnpj                       = emitente.cgc
                                       web-ped-venda.nr-conhecimento            = ""
                                       web-ped-venda.dt-embarque                = ?
                                       web-ped-venda.nro-pedido                 = ''
                                       web-ped-venda.flg_restricao              = NO
                                       web-ped-venda.des_restricao              = ''
                                 
                                      web-ped-venda.qtd_importada              = ttItensPedido.qtdPedida.
                                     

                            END.                          
                                  
                            create es-spf-ped-venda.
                            assign es-spf-ped-venda.nome-abrev = tt-ped-venda.nome-abrev
                                   es-spf-ped-venda.nr-pedcli  = tt-ped-venda.nr-pedcli
                                   es-spf-ped-venda.nr-shopify = ttPedido.pedidoShopify
                                   es-spf-ped-venda.dt-pagamento = today. //date(ttPedido.dataPagamento).
                            
                            RUN descarregarBO.                   

                                    
                        END.
                        ELSE DO:
                        
                            RUN descarregarBO.                   
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


    

    RUN openQueryStatic        IN h_bodi159 (INPUT "Default":U).
    RUN setUserLog IN h_bodi159(INPUT "super").

    RUN validaPermissoesUsuario IN h_bodi159( input YES                  ,  /* Toggle-box Credito */
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

     
    IF VALID-HANDLE(h_bodi159) THEN
        DELETE PROCEDURE h_bodi159.

    RETURN "OK":U.

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

/*     IF NOT VALID-HANDLE(hAlocacao) THEN                         */
/*         RUN lib/PedidoVendaAlocacao.p PERSISTENT SET hAlocacao. */

END PROCEDURE.
    
/*
 *------------------------------------------------------------------------------
 *      Descarrega BOs de pedido pedido de venda
 * ------------------------------------------------------------------------------
 */
PROCEDURE descarregarBO:

    IF VALID-HANDLE(h_bodi159sdf) THEN DO:
//        RUN destroyBO IN h_bodi159sdf NO-ERROR.
        DELETE PROCEDURE h_bodi159sdf.
        ASSIGN h_bodi159sdf = ?.
    END.
    
    IF VALID-HANDLE(h_bodi159) THEN DO:
        RUN destroyBO IN h_bodi159.
        DELETE PROCEDURE h_bodi159.
        ASSIGN h_bodi159 = ?.
    END.

    IF VALID-HANDLE(h_bodi157) THEN DO:
        //RUN destroyBO IN h_bodi157 NO-ERROR.
        DELETE PROCEDURE h_bodi157.
        ASSIGN h_bodi157 = ?.
    END.
    
    IF VALID-HANDLE(h_bodi154sdf) THEN DO:
        //RUN destroyBO IN h_bodi154sdf.
        DELETE PROCEDURE h_bodi154sdf.
        ASSIGN h_bodi154sdf = ?.
    END.
    
    IF VALID-HANDLE(h_bodi154) THEN DO:
        RUN destroyBO IN h_bodi154.
        DELETE PROCEDURE h_bodi154.
        ASSIGN h_bodi154 = ?.
    END.
    
    IF VALID-HANDLE(h_bodi159cal) THEN DO:
        //RUN destroyBO IN h_bodi159cal.
        DELETE PROCEDURE h_bodi159cal.
        ASSIGN h_bodi159cal = ?.
    END.
    
    IF VALID-HANDLE(h_bodi159com) THEN DO:
        //RUN destroyBO IN h_bodi159com.
        DELETE PROCEDURE h_bodi159com.
        ASSIGN h_bodi159com = ?.
    END.

/*     IF VALID-HANDLE(hAlocacao) THEN DO: */
/*         //RUN destroyBO IN hAlocacao.   */
/*         DELETE PROCEDURE hAlocacao.     */
/*         ASSIGN hAlocacao = ?.           */
/*     END.                                */

    IF VALID-HANDLE(h_bodi154can) THEN DO:
        //RUN destroyBO IN h_bodi154can.
        DELETE PROCEDURE h_bodi154can.
        ASSIGN h_bodi154can = ?.
    END.

    
    
END PROCEDURE.

