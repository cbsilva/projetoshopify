/*
 *-----------------------------------------------------------------------------`
 *  PROGRAMA        esspf111a.p
 *  OBJETIVO        Importaá∆o AMAZON
 *  AUTOR           TOTVS - LASF
 *  DATA            04/2021
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *
 *                                DEFINIÄÂES
 *
 *------------------------------------------------------------------------------
 */
DEFINE INPUT PARAMETER pArquivo    AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pDirOutbox  AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pDirLog     AS CHARACTER   NO-UNDO.
{utp/ut-glob.i}


{lib/Utilidades.i}
{lib/log2.i}
{lib/RowErrors.i}
{lib/PedidoVenda.i}

DEFINE VARIABLE hproc AS HANDLE      NO-UNDO.

DEFINE VARIABLE c-campo-aux      AS CHARACTER FORMAT "x(300)" NO-UNDO.
DEFINE VARIABLE c-campo-aux-2    AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nome-arquivo-2 AS CHARACTER .
DEFINE VARIABLE i-seq            AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-sub-seq        AS INTEGER   NO-UNDO.

DEFINE VARIABLE cErro AS CHARACTER   NO-UNDO.

DEFINE VARIABLE c-pedido-cli     AS CHAR FORMAT "x(12)".
DEFINE VARIABLE c-cnpj-cli       AS CHAR FORMAT "x(14)".
DEFINE VARIABLE c-cnpj-ent       AS CHAR FORMAT "x(14)".
define variable c-it-codigo      AS CHAR FORMAT "x(16)".
define variable i-quantidade     AS DECIMAL.
DEFINE VARIABLE c-nr-pedcli      AS CHAR.
DEFINE VARIABLE c-inicial-cli    AS CHAR FORMAT "XX".
DEFINE VARIABLE c-campo-pedido   AS CHARACTER.
DEFINE VARIABLE i-campo-pedido   AS INTEGER.
DEFINE VARIABLE c-campo-depara   AS CHAR    NO-UNDO.
DEFINE VARIABLE c-custo-unit     AS DECIMAL.
DEFINE VARIABLE c-dt-entrega     AS CHAR.

/*
 *------------------------------------------------------------------------------
 *
 *                                FUNÄÂES
 *
 *------------------------------------------------------------------------------
 */

FUNCTION obterNRPedcli RETURNS CHARACTER 
    ():

DEFINE VARIABLE cNrPedcli AS CHARACTER   NO-UNDO.

/*     IF substr(emitente.cgc,1,8) = "15436940" */
/*      THEN  c-inicial-cli = "AM".             */
/*     ELSE  c-inicial-cli = "SV".              */

    c-inicial-cli = "AM".

    FIND web-param-cliente-esp
        WHERE web-param-cliente-esp.cnpj = SUBSTR(emitente.cgc,1,8) NO-ERROR.
    IF AVAIL web-param-cliente-esp THEN DO:
        //c-nr-pedcli = STRING(web-param-cliente-esp.nr-seq-ped + 1,"999999") + c-inicial-cli .
        cNrPedcli = c-inicial-cli + STRING(web-param-cliente-esp.nr-seq-ped + 1,"999999") .
        web-param-cliente-esp.nr-seq-ped = web-param-cliente-esp.nr-seq-ped + 1.
        RELEASE web-param-cliente-esp.
    END.

    RETURN cNrPedcli.

END FUNCTION.


/*
 *------------------------------------------------------------------------------
 *
 *                                BLOCO PRINCIPAL
 *
 *------------------------------------------------------------------------------
 */

RUN pi-verifica-arquivo.

RETURN "OK".


/*
 *------------------------------------------------------------------------------
 *
 *                                PROCDURES 
 *
 *------------------------------------------------------------------------------
 */

/*
 *------------------------------------------------------------------------------
 *------------------------------------------------------------------------------
 */
PROCEDURE pi-verifica-arquivo:

DEFINE VARIABLE carqAux AS CHARACTER   NO-UNDO.
        
    cArqAux = tratarArquivo(pArquivo).
    cArqaux = ENTRY(NUM-ENTRIES(cArqAux, "/"), cArqAux, "/").

    RUN abrirLog(tratarDiretorio(pdirLog) + "LogEDI_" + cArqAux /*Stamp()*/  + ".txt").
    RUN gerarLog("Iniciando importaá∆o arquivo " + tratarString(pArquivo) ).
            
    /* Zera variaveis temporarias */
    c-pedido-cli = "".
    c-cnpj-cli   = "".
    c-cnpj-ent   = "".
    c-nr-pedcli  = "".    
    ASSIGN i-seq = 1 i-sub-seq = 0.
    INPUT FROM VALUE(pArquivo) NO-CONVERT.
    
    ARQUIVO:
    REPEAT :
        import UNFORMATTED c-campo-aux.
    
        /* Trata informaá‰es Cabec */
        IF SUBSTRING(c-campo-aux,1,2) = "01" THEN DO:
    
            ASSIGN c-pedido-cli = trim(SUBSTR(c-campo-aux,9,20))
                   /* 02/12/2016 - Alterado programa conforme soliciado por Fabiana Godeck*/
                   c-cnpj-cli   = SUBSTR(c-campo-aux,195,14)
                   /* c-cnpj-cli   = SUBSTR(c-campo-aux,181,14) */
                   c-cnpj-ent   = SUBSTR(c-campo-aux,209,14)
                   c-dt-entrega = SUBSTR(c-campo-aux,67,2) + SUBSTR(c-campo-aux,65,2) + SUBSTR(c-campo-aux,61,4)
                   //c-dt-entrega = SUBSTR(c-campo-aux,79,2) + SUBSTR(c-campo-aux,77,2) + SUBSTR(c-campo-aux,73,4)
                   .
            
            RUN gerarLog("CNPJ cliente " +  c-cnpj-cli).
            
            
            FIND emitente NO-LOCK
                WHERE emitente.cgc = c-cnpj-cli NO-ERROR.
            IF NOT AVAIL emitente THEN
            DO:
                RUN gerarLog("Cliente n∆o encontrado." ).
                UNDO ARQUIVO, RETURN "NOK".
            END.
    
            RUN gerarFilaImportacao.
            IF RETURN-VALUE <> "OK" THEN
            DO:
                RUN gerarLog("Erro ao gerar fila de importaá∆o." ).
                UNDO ARQUIVO, RETURN "NOK".
            END.

/*             MESSAGE                                                              */
/*                 "LASF DEBUG"                                                SKIP */
/*                 "es-api-import-spf.id-movto   "    es-api-import-spf.id-movto       SKIP */
/*                 "es-api-import-spf.cod-status "    es-api-import-spf.cod-status     SKIP */
/*                 "es-api-import-spf.chave-alt  "    es-api-import-spf.chave-alt      SKIP */
/*                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                        */

            IF es-api-import-spf.cod-status = 9 THEN
                RETURN "OK".
    

/*                                                                                      */
/*             IF AVAIL emitente AND c-nr-pedcli = ""            /* Amazon - Saraiva */ */
/*                THEN DO:                                                              */
/*                                                                                      */
/*                 RUN obterNrPedCli.                                                   */
/*                                                                                      */
/*             END.                                                                     */
    
        END.
        /* Trata informaá‰es Detalhe */
        ELSE IF SUBSTRING(c-campo-aux,1,2) = "04" THEN DO:
            ASSIGN c-it-codigo = ""
                   i-quantidade = 0
                   i-quantidade = int(SUBSTR(c-campo-aux,100,15)) / 100
                   c-it-codigo  = SUBSTR(c-campo-aux,18,14).
    
            /* Varifica o item se existe, busca tabela de preáo e gerar registro na tt-dados */
            c-campo-depara = "".
            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.it-codigo = c-it-codigo NO-ERROR.
    
            /* Verifica se existe algum registro de-para */
            IF NOT AVAIL ITEM THEN do:
                RUN include/param_tab_de_para.p (INPUT c-nr-pedcli,
                                                 INPUT c-it-codigo,
                                                 OUTPUT c-campo-depara). 
    
                IF c-campo-depara <> "" THEN DO:
                    FIND FIRST ITEM NO-LOCK
                        WHERE ITEM.it-codigo = c-campo-depara NO-ERROR.
                    IF AVAIL item THEN ASSIGN c-it-codigo = c-campo-depara.
                END.
                /* Enviar e-mail avisando sobre problema */
                ELSE DO:
                    ASSIGN c-campo-aux-2 = "Informativo do Sistema - Importaá∆o Pedido Amazon" + CHR(10) + CHR(10) + ~
                                           "O pedido do cliente abaixo n∆o ser† processado completamente pois o item informado" + CHR(10) + ~
                                           "n∆o existe no cadastro do sistema e nem relacionado ao controle DE-PARA." + CHR(10) + CHR(10) + ~
                                           "Pedido Livraria Saraiva.: " + c-pedido-cli + CHR(10) + CHR(10) + ~
                                           "Codigo Livraria Saraiva.: " + c-it-codigo + CHR(10) + CHR(10) + ~
                                           "Lembre-se que o pedido n∆o ficar† completo no sistema." + CHR(10) + CHR(10).
                    RUN pi-envia-email (INPUT "Pedido Amazon - ITEM N∆o Localizado",
                                        INPUT c-campo-aux-2,
                                        INPUT "michele.alves@macmillaneducation.com ; emerson.marques@macmillaneducation.com ; daniela.goncalves@macmillaneducation.com ; maria.diniz@macmillaneducation.com").  
                END.
    
    
            END.
            /********************************************************/
    
            IF AVAIL emitente  THEN
                RUN gerarLog("Emitente " + emitente.nome-abrev).
            ELSE
                RUN gerarLog("Emitente inexistente").
    
            IF AVAIL ITEM THEN DO:
    
                CREATE tt-dados.
                ASSIGN tt-dados.nr-pedcli  = c-nr-pedcli
                       tt-dados.data-hora  = TODAY
                       tt-dados.it-codigo  = ITEM.it-codigo
                       tt-dados.desc-item  = ITEM.desc-item
                       tt-dados.vlr-unit   =  INT(SUBSTR(c-campo-aux,198,15)) / 100  /* / 100),">>>,>>>,>>9.99")    preco-item.preco-fob */
                       tt-dados.quantidade = i-quantidade
                       tt-dados.vlr-total  = i-quantidade * tt-dados.vlr-unit      /* preco-item.preco-fob */
                       tt-dados.pedido-cli = c-pedido-cli
                       tt-dados.nome-cli   = emitente.nome-emit WHEN AVAIL emitente
                       tt-dados.email-cli  = emitente.e-mail    WHEN AVAIL emitente
                       tt-dados.telefone   = emitente.telefone[1] WHEN AVAIL emitente
                       tt-dados.cnpj       = IF AVAIL emitente THEN emitente.cgc ELSE c-cnpj-cli
                       tt-dados.cod-entreg = c-cnpj-ent
                       tt-dados.dt-entrega = DATE(c-dt-entrega) + 5.    
                // 24/10/2019 - Se for pedido AMAZON, data de entrega deve ser adicionado 5 dias (Fabiana)
                //IF SUBSTR(c-nr-pedcli,7,2) = "AM" THEN  tt-dados.dt-entrega = tt-dados.dt-entrega + 5.
    
    
                ASSIGN  c-nome-arquivo-2 = tratarDiretorio(pDirOUTBOX ) + tt-dados.nr-pedcli + ".CSV".
    
    
            END.
        END.
    END.
    INPUT CLOSE.
    

    RUN gerarPedidoEDI.
    IF RETURN-VALUE <> "OK" THEN
    DO:
        RUN gerarLog("Houve erro ao immportar pedido").
        RETURN "NOK".
    END.


/* /***************** 16/01/2017 - Validar se existe o clliente no sistema */                                                */
/* ASSIGN c-tipo = "PV-CES".                                                                                                 */
/* FOR FIRST tt-dados:                                                                                                       */
/*     IF NOT CAN-FIND(FIRST emitente                                                                                        */
/*                     WHERE emitente.cgc = tt-dados.cnpj) THEN DO:                                                          */
/*         /* Mandar mensagem por e-mail sobre cliente n∆o cadastrado */                                                     */
/*         ASSIGN c-campo-aux-2 = "Informativo do Sistema - Importaá∆o SARAIVA" + CHR(10) + CHR(10) + ~                      */
/*                                "Foi enviado um arquivo pela SARAIVA com um CNPJ " + CHR(10) + ~                           */
/*                                "que n∆o existe no cadastro de clientes da Macmillan." + CHR(10) + CHR(10) + ~             */
/*                                "Cliente Pedido SARAIVA.: " + tt-dados.cnpj + CHR(10) + CHR(10) + ~                        */
/*                                "Lembre-se que o pedido n∆o ir† integrar com o sistema atÇ correá∆o." + CHR(10) + CHR(10). */
/*         RUN pi-envia-email (INPUT "Pedido SARAIVA - Cliente N∆o Cadastrado",                                              */
/*                             INPUT c-campo-aux-2,                                                                          */
/*                             INPUT "michele.alves@macmillaneducation.com ; maria.diniz@macmillaneducation.com").           */
/*         ASSIGN c-tipo = "".                                                                                               */
/*     END.                                                                                                                  */
/* END.                                                                                                                      */
/* /************************************************************************/                                                */



    
    RUN gerarLog("Iniciando exportaá∆o arquivo " + tratarString(c-nome-arquivo-2) ).

    OUTPUT TO VALUE (c-nome-arquivo-2) NO-CONVERT.
    FOR EACH tt-dados:

        PUT UNFORMATTED
            tt-dados.nr-pedcli  ";"
            string(year(tt-dados.data-hora),"9999") "-"
            string(MONTH(tt-dados.data-hora),"99") "-"
            string(day(tt-dados.data-hora),"99") " "
            STRING(TIME,"HH:MM:SS")  ";"
            tt-dados.it-codigo  ";"
            tt-dados.desc-item  ";"
            tt-dados.vlr-unit   ";"
            tt-dados.quantidade ";"
            tt-dados.vlr-total  ";"
            tt-dados.pedido-cli ";"
            tt-dados.nome-cli   ";"
            tt-dados.email-cli  ";"
            tt-dados.telefone   ";"
            tt-dados.cnpj       ";"
            tt-dados.cod-entreg ";" 
            tt-dados.dt-entrega SKIP.
    END.
    OUTPUT CLOSE.


    IF SEARCH(c-nome-arquivo-2) = ? THEN DO:

        RUN gerarLog("O arquivo de sa°da n∆o foi gerado, verificar.").

    END.



    //OS-COMMAND VALUE(SUBSTITUTE("move &1 &2", pArquivo, pDirlog))
    OS-COMMAND SILENT VALUE("del " + pArquivo).

    RUN gerarLog("CONCLU÷DO").


            
END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *------------------------------------------------------------------------------
 */
PROCEDURE gerarPedidoEDI:

DEFINE VARIABLE cChave AS CHARACTER   NO-UNDO.

EMPTY TEMP-TABLE tt-ped-venda-import.
EMPTY TEMP-TABLE tt-ped-item-import.
    
    FOR FIRST tt-dados:
    END.
    IF NOT AVAIL tt-dados THEN
    DO:
        RUN gerarLog("Nenhum item importado para pedido EDI").
        RETURN "NOK".
    END.


    CREATE tt-ped-venda-import.
    ASSIGN  
            tt-ped-venda-import.NumeroPedidoSistemaCliente      = tt-dados.pedido-cli 
            tt-ped-venda-import.NumeroPedidoCliente             = tt-dados.nr-pedcli  
            tt-ped-venda-import.NomeCliente                     = "" // tt-dados.nome-cli   
            tt-ped-venda-import.Email                           = tt-dados.email-cli  
            tt-ped-venda-import.Telefone                        = tt-dados.telefone   
            tt-ped-venda-import.CNPJ                            = tt-dados.cnpj 
            tt-ped-venda-import.DataEntrega                     = tt-dados.dt-entrega 
            tt-ped-venda-import.tipoPedido                      = "E"
            .                            
                            

    FOR EACH tt-dados:

        CREATE  tt-ped-item-import.
        ASSIGN  tt-ped-item-import.CodigoItem                     = tt-dados.it-codigo 
                tt-ped-item-import.ValorUnitario                  = tt-dados.vlr-unit   
                tt-ped-item-import.QuantidadePedida               = tt-dados.quantidade .
                
    END.
                            

    RUN lib/PedidoVenda.p PERSISTENT SET hProc.                                                                                                      
    RUN receberPedidos IN hProc (INPUT TABLE tt-ped-venda-import,
                                 INPUT TABLE tt-ped-item-import).

    BLOCO:
    DO  
/*         TRANSACTION                       */
/*         ON ERROR  UNDO BLOCO, LEAVE BLOCO */
/*         ON QUIT   UNDO BLOCO, LEAVE BLOCO */
/*         ON STOP   UNDO BLOCO, LEAVE BLOCO */
        :

        RUN processarDados IN hProc
            (OUTPUT cChave).
        IF RETURN-VALUE <> "OK" THEN
        DO:
            RUN retornarErros IN hProc (OUTPUT TABLE rowErrors).
            RUN gerarLog("Houve erro ao gerar pedido").
            FOR EACH rowErrors:
                cErro = cErro + SUBSTITUTE(rowErrors.Errordesc + "(&1)[&2]", rowErrors.errorNum, rowErrors.errorsubType) + "|".
                RUN gerarLog(cErro).

            END.
            
            RUN gerarLogImportacao(cErro).
            RUN atualizarFilaImportacao("NOK").

            DELETE PROCEDURE hProc.                                                                                                                          
            //UNDO BLOCO, RETURN "NOK".
            RETURN "NOK".
        END.    

        RUN gerarLogImportacao("Pedido Importado com Sucesso").

        RUN atualizarFilaImportacao("OK").

        DELETE PROCEDURE hProc.                                                                                                                          

    END. // TRANS
    

    RETURN "OK".

END PROCEDURE.

/*
 *------------------------------------------------------------------------------
 *------------------------------------------------------------------------------
 */
PROCEDURE pi-envia-email:
DEFINE INPUT PARAMETER p-assunto  AS CHAR.
DEFINE INPUT PARAMETER p-mensagem AS CHAR.
DEFINE INPUT PARAMETER p-mailto   AS CHAR.

    CREATE es-control-email.
    ASSIGN 
    es-control-email.dt-registro  = TODAY
    es-control-email.hr-registro  = string(TIME,"HH:MM:SS")
    es-control-email.email-de     = "pedidos@macmillaneducation.com"
    es-control-email.email-para   = p-mailto
    es-control-email.email-copia  = ""
    es-control-email.assunto      = p-assunto
    es-control-email.conteudo     = p-mensagem
    es-control-email.anexo1       = ""
    es-control-email.anexo2       = ""
    es-control-email.cod-usuario  = c-seg-usuario
    es-control-email.programa     = "MAC0251GS"
    es-control-email.dt-envio     = ? 
    es-control-email.hr-envio     = ""
    es-control-email.situacao     = "PENDENTE".
END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *------------------------------------------------------------------------------
 */
PROCEDURE gerarFilaImportacao:

    FOR FIRST es-api-import-spf NO-LOCK WHERE
              es-api-import-spf.cd-tipo-integr      = 5
          //AND es-api-import-spf.chave               = pArquivo
          AND es-api-import-spf.chave-alt           = pArquivo
        :
    END.

    IF NOT AVAIL es-api-import-spf THEN
    DO:

        c-nr-pedcli = obterNRPedCli().

        IF c-nr-pedcli = "" OR c-nr-pedcli = ? THEN
        DO:
            RUN gerarLog("N∆o foi poss°vel determinar n£mero do pedido do cliente").
            RETURN "NOK".
        END.

        CREATE  es-api-import-spf.
        ASSIGN  es-api-import-spf.id-movto          = NEXT-VALUE(seq_import)
                es-api-import-spf.cd-tipo-integr    = 5
                //es-api-import-spf.chave             = pArquivo
                es-api-import-spf.chave-alt         = pArquivo
                es-api-import-spf.data-movto        = NOW
                es-api-import-spf.data-inicio       = NOW
                es-api-import-spf.data-fim          = ?
                es-api-import-spf.ind-situacao      = 0 /*--- Pendente ---*/
                es-api-import-spf.cod-status        = 0 /*--- sem status ---*/
                //es-api-import-spf.c-json            = c-nr-pedcli.
                es-api-import-spf.chave             = c-nr-pedcli  + "|" + c-pedido-cli.
    END.
    ELSE
    DO:
        IF es-api-import-spf.cod-status = 9 THEN
            RETURN "OK".

        //ASSIGN  c-nr-pedcli                     = STRING(es-api-import-spf.c-json).
        ASSIGN  c-nr-pedcli                     = ENTRY(1, es-api-import-spf.chave, "|").

    END.


    RETURN "OK".


END PROCEDURE.


/*
 *------------------------------------------------------------------------------
 *------------------------------------------------------------------------------
 */
PROCEDURE gerarLogImportacao:
DEFINE INPUT PARAMETER pMensagem AS CHARACTER NO-UNDO.

DEFINE VARIABLE daAux AS DATETIME        NO-UNDO.
DEFINE VARIABLE i-nr-seq AS INTEGER NO-UNDO.

    IF NOT AVAIL es-api-import-spf THEN
        RETURN.

    FIND LAST es-api-import-log-spf NO-LOCK OF es-api-import-spf NO-ERROR.
    IF AVAIL es-api-import-log-spf THEN
        ASSIGN i-nr-seq = es-api-import-log-spf.nr-seq + 1.
    ELSE i-nr-seq = 1.

    daAux  = NOW.
    IF NOT AVAIL es-api-import-log-spf OR DATE(es-api-import-log-spf.data-log) <> DATE(daAux) THEN
    DO:
        CREATE es-api-import-log-spf.
        ASSIGN es-api-import-log-spf.cd-tipo-integr = es-api-import-spf.cd-tipo-integr
               es-api-import-log-spf.id-movto       = es-api-import-spf.id-movto      
               es-api-import-log-spf.data-log       = NOW
               es-api-import-log-spf.nr-seq         = i-nr-seq. 
    END.
    ELSE
        FIND CURRENT es-api-import-log-spf EXCLUSIVE-LOCK.
        

    ASSIGN es-api-import-log-spf.des-log        = pMensagem
           es-api-import-log-spf.data-log       = daAux.

/*     CREATE es-api-import-log-spf.                                                                                  */
/*     ASSIGN es-api-import-log-spf.cd-tipo-integr = es-api-import-spf.cd-tipo-integr                                     */
/*            es-api-import-log-spf.id-movto       = es-api-import-spf.id-movto                                           */
/*            es-api-import-log-spf.data-log       = NOW                                                              */
/*            es-api-import-log-spf.des-log        = IF c-erro = "" THEN "Registro integrado com sucesso" ELSE c-erro */
/*            es-api-import-log-spf.nr-seq         = i-nr-seq.                                                        */

    RELEASE es-api-import-log-spf.

END PROCEDURE.

PROCEDURE atualizarFilaImportacao:
DEFINE INPUT  PARAMETER pStatus AS CHARACTER   NO-UNDO.

    IF NOT AVAIL es-api-import-spf THEN
        RETURN "NOK".

    FIND CURRENT es-api-import-spf EXCLUSIVE-LOCK.

    IF pStatus = "OK" THEN 
        ASSIGN  es-api-import-spf.data-fim      = NOW
                es-api-import-spf.ind-situacao  = 2
                es-api-import-spf.cod-status    = 1
                // es-api-import-spf.chave         = IF c-chave = "" THEN es-api-import-spf.chave ELSE c-chave
        .

    IF pStatus = "NOK" THEN 
        ASSIGN  es-api-import-spf.data-fim      = NOW
                es-api-import-spf.ind-situacao  = 2
                es-api-import-spf.cod-status    = 2
                // es-api-import-spf.chave         = IF c-chave = "" THEN es-api-import-spf.chave ELSE c-chave
        .




    FIND CURRENT es-api-import-spf NO-LOCK.
    IF AVAIL es-api-import-spf THEN
        RELEASE es-api-import-spf.

    RETURN "OK".

END PROCEDURE.
