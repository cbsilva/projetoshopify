/*
 *------------------------------------------------------------------------------
 *  PROGRAMA        LIB/PedidoVendaAlocacao.p
 *  OBJETIVO        Rotinas para integraá∆o de pedidos de venda MACMILLAN - ALOCAÄ«O
 *  AUTOR           TOTVS - LASF
 *  DATA            12/2020
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *                                DEFINIÄÂES
 *
 *------------------------------------------------------------------------------
 */
DEFINE VARIABLE h-api-alocacao AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE ttSaldoEstoq LIKE saldo-estoq
    FIELD qt-disponivel         LIKE saldo-estoq.qt-alocada
    FIELD r-rowid               AS ROWID.

DEFINE BUFFER localizacao FOR mgadm.localizacao.

DEFINE VARIABLE deSaldoTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE deAlocar AS DECIMAL     NO-UNDO.

/* Definicao temp-table RowErrors */
{method/dbotterr.i}     


{lib/utilidades.i}
{lib/log2.i}
{lib/mensagens2.i}
{cdp/cd0666.i}

{lib/PedidoVenda.i}


/*
 *------------------------------------------------------------------------------
 *
 *                                FUNÄÂES
 *
 *------------------------------------------------------------------------------
 */


/*
 *------------------------------------------------------------------------------
 *
 *                              BLOCO PRINCIPAL
 *                                MAIN BLOCK 
 *
 *------------------------------------------------------------------------------
 */


/* RUN obterSaldoItem ("9788551101285").                         */
/* MESSAGE 'l'                                                   */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */
/* FOR EACH ttsaldoestoq:                                        */
/*     // DISPLAY ttSaldoEstoq.qt-disponivel.                    */
/*     deSaldoTotal = deSaldoTotal + ttSaldoEstoq.qt-disponivel. */
/* END.                                                          */
/* DISP desaldototal.                                            */


/* DEFINE VARIABLE deSaldoTotal AS DECIMAL     NO-UNDO.              */
/*      // RUN obterSaldoItem ("9786685743180").                     */
/*     // RUN obterSaldoItem ("9786685743197").                      */
/*      RUN obterSaldoItem ("9786685743463").                        */
/*      //RUN obterSaldoItem ("9786685743470").                      */
/*     //RUN obterSaldoItem ("9788582762486").                       */
/*     FOR EACH ttsaldoestoq:                                        */
/*         // DISPLAY ttSaldoEstoq.qt-disponivel.                    */
/*         deSaldoTotal = deSaldoTotal + ttSaldoEstoq.qt-disponivel. */
/*     END.                                                          */
/*     DISPLAY deSaldoTotal.                                         */



/*     FOR FIRST ped-venda NO-LOCK WHERE                                                        */
/*               ped-venda.nome-abrev    =   "LIV GRANJA V" AND ped-venda.nr-pedcli = "WS1038": */
/*                                                                                              */
/*         DISP ped-venda.completo                                                              */
/*             ped-venda.cod-sit-ped                                                            */
/*             SKIP(2)                                                                          */
/*             WITH SCROLLABLE                                                                  */
/*             .                                                                                */
/*                                                                                              */
/* /*         FOR FIRST ext_ped_venda EXCLUSIVE-LOCK WHERE                   */                 */
/* /*                   ext_ped_venda.nr_pedido       = ped-venda.nr-pedido: */                 */
/* /*         END.                                                           */                 */
/* /*         IF NOT AVAIL ext_ped_venda THEN                                */                 */
/* /*         DO:                                                            */                 */
/* /*             CREATE ext_ped_venda.                                      */                 */
/* /*             ASSIGN ext_ped_venda.nr_pedido      = ped-venda.nr-pedido. */                 */
/* /*         END.                                                           */                 */
/* /*         ASSIGN ext_ped_venda.tip_pedido     = "S".                     */                 */
/*                                                                                              */
/*                                                                                              */
/*         RUN     alocarpedido(ROWID(ped-venda)) .                                             */
/*                                                                                              */
/*         FOR EACH rowerrors:                                                                  */
/*             DISPLAY rowErrors.errornumber rowerrors.errordesc VIEW-AS EDITOR SIZE 60 BY 6 .  */
/*         END.                                                                                 */
/*                                                                                              */
/*     END.                                                                                     */

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
PROCEDURE alocarPedido:
DEFINE INPUT  PARAMETER pPedido AS ROWID       NO-UNDO.

    FOR FIRST ped-venda NO-LOCK WHERE
              ROWID(ped-venda)  = pPedido:
    END.
    IF NOT AVAIL ped-venda THEN
    DO:
        RUN gerarRowError("Pedido n∆o encontrado.").
        RETURN "NOK".        
    END.

    BLOCO:
    DO TRANSACTION:
        FOR EACH ped-item OF ped-venda NO-LOCK:
    
            RUN alocarItemPedido (ROWID(ped-item)).

            IF RETURN-VALUE <> "OK" THEN
            DO:
                RUN gerarRowError("Erro ao alocar ITEM DO pedido - " + ped-item.it-codigo).
                UNDO BLOCO, RETURN "NOK".
            END.
                

        END.
    END.

    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE alocarItemPedido:
DEFINE INPUT  PARAMETER pItemPedido AS ROWID       NO-UNDO.

     FOR FIRST ped-item NO-LOCK WHERE
               ROWID(ped-item)  = pItemPedido:
     END.
     IF NOT AVAIL ped-item THEN
     DO:
         RUN gerarRowError("Item do pedido n∆o encontrada.").
         RETURN "NOK".        
     END.
         

    // RUN localizarSaldos.
    RUN obterSaldoItem (ped-item.it-codigo).
    IF RETURN-VALUE <> "OK" THEN
        RETURN "NOK".
    

    ASSIGN deSaldoTotal =  0.
    FOR EACH ttSaldoEstoq    :
        deSaldoTotal = deSaldoTOtal + ttSaldoEstoq.qt-disponivel.
    END.


/*     MESSAGE "na alocacao " SKIP(1) */
/*         'ped-item.it-codigo: '  ped-item.it-codigo  SKIP */
/*         'desaldototal      : '  desaldototal        SKIP */
/*         'ped-item.qt-pedida: '  ped-item.qt-pedida */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*    */
    IF deSaldoTotal < ped-item.qt-pedida THEN
    DO:
        RUN gerarMensagem
            (SUBSTITUTE("N∆o h† saldo suficiente para alocaá∆o (Item: &1 | Qt Ped: &2| Qt Estoq: &3)", ped-item.it-codigo, tratarString(STRING(ped-item.qt-pedida)), tratarString(STRING(deSaldoTotal)) ) ,
             17600,
             "INFO"
             ).
        NEXT.
    END.

    FOR EACH ped-ent OF ped-item NO-LOCK:
        
        FOR EACH ttSaldoEstoq:

            RUN alocarPedEnt.
            IF RETURN-VALUE <> "OK" THEN
            DO:
                RUN gerarRowError("Erro ao alocar Entrega DO ITEM DO pedido- " + ped-item.it-codigo).
                RETURN "NOK".
            END.

        END.

    END.

    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE localizarSaldos:

DEFINE VARIABLE cAux AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

    EMPTY TEMP-TABLE ttSaldoEstoq.

/*     FOR FIRST ITEM NO-LOCK  WHERE                  */
/*               ROWID(ITEM)   = pItem:               */
/*     END.                                           */
/*     IF NOT AVAIL ITEM THEN                         */
/*     DO:                                            */
/*         RUN gerarRowError("Item n∆o encontrado."). */
/*         RETURN "NOK".                              */
/*     END.                                           */

    IF NOT AVAIL ped-item THEN
    DO:
        RUN gerarRowError("Item do pedido n∆o dispon°vel para localizar saldos.").
        RETURN "NOK".
    END.

    FOR FIRST ITEM  NO-LOCK WHERE 
               ITEM.it-codigo       = ped-item.it-codigo:    
    END.
    IF NOT AVAIL ITEM THEN
    DO:
        RUN gerarRowError("Item n∆o encontrado.").
        RETURN "NOK".
    END.

/*     IF NOT AVAIL ped-ent THEN                                                                            */
/*     DO:                                                                                                  */
/*         RUN gerarRowError("Erro. Registro de entrega do pedido n∆o dispon°vel para rotina de alocaá∆o"). */
/*         RETURN "NOK".                                                                                    */
/*     END.                                                                                                 */


    /*
    IF ext_ped_venda.tip_pedido     = "D" THEN
        ASSIGN cAux = TRIM(REPLACE(es_api_param_ped.lst_cod_depos_doa, ";", ",")) .
    ELSE
        ASSIGN cAux = TRIM(REPLACE(es_api_param_ped.lst_cod_depos_sales, ";", ",")) .

    DO iCont = 1 TO NUM-ENTRIES(cAux):  

        FOR FIRST deposito NO-LOCK WHERE
                  deposito.cod-depos     = ENTRY(iCont, cAux)
            AND   deposito.cons-saldo:
        END.
        IF AVAIL deposito THEN
        DO:
            FOR EACH estabelec NO-LOCK,
                EACH localizacao NO-LOCK WHERE
                     localizacao.cod-estabel    = estabelec.cod-estabel
                 AND localizacao.cod-depos      = deposito.cod-depos,
                EACH saldo-estoq NO-LOCK WHERE 
                     saldo-estoq.it-codigo      = ped-item.it-codigo
                AND  saldo-estoq.cod-estabel    = estabelec.cod-estabel
                AND  saldo-estoq.cod-refer      = ITEM.cod-refer
                AND  saldo-estoq.cod-depos      = deposito.cod-depos
                AND  saldo-estoq.cod-estabel    = estabelec.cod-estabel
                AND  saldo-estoq.cod-localiz    = localizacao.cod-localiz  :


                    CREATE ttSaldoEstoq.
                    BUFFER-COPY saldo-estoq TO ttSaldoEstoq
                        ASSIGN ttSaldoEstoq.qt-disponivel        = saldo-estoq.qtidade-atu  - 
                                                                   saldo-estoq.qt-alocada   - 
                                                                   saldo-estoq.qt-aloc-prod - 
                                                                   saldo-estoq.qt-aloc-ped
                               ttSaldoEstoq.r-rowid              = ROWID(saldo-estoq).   

                
            END.
        END.


    END.
        */


    RUN obtersaldoItem(ped-item.it-codigo).


    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE alocarPedEnt:
DEFINE VARIABLE h-api-alocacao AS HANDLE      NO-UNDO.

    IF NOT AVAIL ttSaldoEstoq THEN
    DO:
        RUN gerarRowError("Saldo de Estoque n∆o dispon°vel para Alocaá∆o").
        RETURN "NOK".
    END.

/*     IF NOT AVAIL ext_ped_venda THEN                                                    */
/*     DO:                                                                                */
/*         RUN gerarRowError("Extens∆o do Pedido de Venda n∆o dispon°vel para Alocaá∆o"). */
/*         RETURN "NOK".                                                                  */
/*     END.                                                                               */

    FOR FIRST para-ped NO-LOCK:
    END.
    IF NOT AVAIL para-ped THEN
    DO:
        RUN gerarRowError("ParÉmetros do pedido n∆o encontrados").
        RETURN "NOK".
    END.

    IF NOT VALID-HANDLE(h-api-alocacao) THEN
        RUN pdp/pdapi002.p PERSISTENT SET h-api-alocacao.

//    &IF  DEFINED(bf_dis_allocate_blanket_orders) &THEN 
         run pi-validacao in h-api-alocacao(INPUT ROWID(ped-ent)).
//    &ELSE
//         run pi-validacao in h-api-alocacao(input rowid(ped-item)).
//    &ENDIF
    
    EMPTY TEMP-TABLE RowErrors.
    EMPTY TEMP-TABLE tt-erro.

    RUN pi-retorna-erro IN h-api-alocacao(OUTPUT TABLE tt-erro).
    IF CAN-FIND(FIRST tt-erro) THEN DO:
        FOR EACH tt-erro:            

            CREATE RowErrors.
            ASSIGN RowErrors.ErrorNumber      = tt-erro.cd-erro
                   RowErrors.ErrorDescription = tt-erro.mensagem
                   RowErrors.ErrorType        = "EMS":U
                   RowErrors.ErrorSubType     = "ERROR":U.
        END.
        DELETE PROCEDURE h-api-alocacao.

/*         FOR EACH RowErrors:   */
/*             DELETE RowErrors. */
/*         END.                  */

        RETURN 'NOK':U.
    END.
    
    RUN allocateQuantity IN THIS-PROCEDURE.
      
/*     IF NOT AVAIL ped-item THEN                           */
/*         FIND FIRST ped-item OF ped-ent NO-LOCK NO-ERROR. */


            
    IF para-ped.dec-1 = 1 AND ped-item.qt-log-aloca > 0  AND ped-item.qt-log-aloca < ped-item.qt-pedida - ped-item.qt-alocada  THEN DO:
        
        //RUN utp/ut-msgs.p ("msg", 17554, "").
        
        CREATE RowErrors.
        ASSIGN RowErrors.ErrorSequence = 1
               RowErrors.ErrorNumber = 17554
               RowErrors.ErrorDescription = RETURN-VALUE 
               RowErrors.ErrorType = "EMS":U
               RowErrors.ErrorSubType = "Information":U.
        
//        RUN utp/ut-msgs.p ("help", 17554, "").
//        ASSIGN RowErrors.ErrorHelp = RETURN-VALUE.
        
/*         IF CAN-FIND(FIRST RowErrors                                      */
/*                     WHERE Rowerrors.ErrorType <> "INTERNAL":U) THEN DO : */
/*         END.                                                             */
/*         DELETE rowErrors.                                                */
        
        /* Desaloca a quantidade informada */
        /*run pi-desaloca-fisica-man in h-api-alocacao(input rowid(ped-ent),
                                                     input-output de-qt-aux,
                                                     input rowid(saldo-estoq)).*/
        
        /*assign de-qt-log-aloca:screen-value in frame fpage0 = string(ped-ent.qt-log-aloca)
               de-qt-aux = 0. */           
    END.

    IF VALID-HANDLE(h-api-alocacao) then
	    DELETE PROCEDURE h-api-alocacao.

    RETURN "OK".

END PROCEDURE.



/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE allocateQuantity :

DEFINE VARIABLE de-qt-alocar    LIKE ped-ent.qt-log-aloca label "Qtde a Alocar" NO-UNDO.
DEFINE VARIABLE c-unid-neg      AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE VARIABLE c-unid-neg-aux  AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE VARIABLE c-cod-depos     AS CHARACTER NO-UNDO.
DEFINE VARIABLE h-alocacao      AS HANDLE      NO-UNDO.

    IF ttSaldoEstoq.qt-Disponivel > ped-ent.qt-pedida - ped-ent.qt-log-aloca THEN 
        ASSIGN de-qt-alocar    = ped-ent.qt-pedida - ped-ent.qt-log-aloca .
    ELSE
        ASSIGN de-qt-alocar    = ttSaldoEstoq.qt-Disponivel.

    IF de-qt-alocar <= 0 THEN
        RETURN "OK".

    RUN pdp/pdapi002.p PERSISTENT SET h-alocacao.

    EMPTY TEMP-TABLE tt-erro.
    EMPTY TEMP-TABLE RowErrors.

/*     MESSAGE "na allocateQunatity " SKIP */
/*         "qt alocar"         de-qt-alocar */
/*         VIEW-AS ALERT-BOX. */


    RUN pi-aloca-fisica-man IN h-alocacao(INPUT ROWID(ped-ent),
                                          INPUT-OUTPUT de-qt-alocar, 
                                          INPUT ttSaldoEstoq.r-rowid).
    DELETE PROCEDURE h-alocacao.

    
    RETURN "OK".


END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *      
 * ------------------------------------------------------------------------------
 */
PROCEDURE obterSaldoItem:
DEFINE INPUT  PARAMETER pItem AS CHARACTER   NO-UNDO.   




    EMPTY TEMP-TABLE ttSaldoEstoq.
/*     FOR EACH deposito NO-LOCK WHERE                                                       */
/*              deposito.ind-disp-saldo = true           :                                   */
/*                                                                                           */
/*         FOR EACH estabelec NO-LOCK,                                                       */
/* /*             EACH localizacao NO-LOCK WHERE                          */                 */
/* /*                  localizacao.cod-estabel    = estabelec.cod-estabel */                 */
/* /*              AND localizacao.cod-depos      = deposito.cod-depos    */                 */
/* /*             ,                                                       */                 */
/*                                                                                           */
/*             EACH saldo-estoq NO-LOCK WHERE                                                */
/*                  saldo-estoq.it-codigo      = pItem                                       */
/*             AND  saldo-estoq.cod-estabel    = estabelec.cod-estabel                       */
/*             //AND  saldo-estoq.cod-refer      = ""                                        */
/*             AND  saldo-estoq.cod-depos      = deposito.cod-depos                          */
/* //AND  saldo-estoq.cod-localiz    = localizacao.cod-localiz                               */
/*             :                                                                             */
/*                                                                                           */
/*                 CREATE ttSaldoEstoq.                                                      */
/*                 BUFFER-COPY saldo-estoq TO ttSaldoEstoq                                   */
/*                     ASSIGN ttSaldoEstoq.qt-disponivel        = saldo-estoq.qtidade-atu  - */
/*                                                                saldo-estoq.qt-alocada   - */
/*                                                                saldo-estoq.qt-aloc-prod - */
/*                                                                saldo-estoq.qt-aloc-ped    */
/*                            ttSaldoEstoq.r-rowid              = ROWID(saldo-estoq).        */
/*                                                                                           */
/*         END.    // EACH estabelec                                                         */
/*                                                                                           */
/*     END.    // EACH deposito                                                              */






   FOR EACH saldo-estoq WHERE
             saldo-estoq.it-codigo = pitem AND 
          CAN-FIND ( deposito WHERE
                     deposito.cod-depos      = saldo-estoq.cod-depos   AND
                     // deposito.ind-disp-saldo = true                  
                     deposito.cons-saldo = TRUE 
                     ) 
            NO-LOCK

        BREAK BY saldo-estoq.it-codigo:

         CREATE ttSaldoEstoq.    
         BUFFER-COPY saldo-estoq TO ttSaldoEstoq                                   
             ASSIGN ttSaldoEstoq.qt-disponivel        = saldo-estoq.qtidade-atu  - 
                                                        saldo-estoq.qt-alocada   - 
                                                        saldo-estoq.qt-aloc-prod - 
                                                        saldo-estoq.qt-aloc-ped    
                    ttSaldoEstoq.r-rowid              = ROWID(saldo-estoq).        

        
    END.


    RETURN "OK".
    
END PROCEDURE.

