

// Dados oriundos da integra‡Æo
DEFINE TEMP-TABLE tt-dados
    FIELD nr-pedcli  AS CHAR FORMAT "X(12)"
    FIELD data-hora  AS DATE
    FIELD it-codigo  AS CHAR FORMAT "x(16)"
    FIELD desc-item  AS CHAR FORMAT "x(60)"
    FIELD vlr-unit   AS DECIMAL
    FIELD quantidade AS INTEGER
    FIELD vlr-total  AS DECIMAL
    FIELD pedido-cli AS CHAR FORMAT "x(12)"
    FIELD nome-cli   AS CHAR FORMAT "x(60)"
    FIELD email-cli  AS CHAR FORMAT "x(100)"
    FIELD telefone   AS CHAR FORMAT "x(20)"
    FIELD cnpj       AS CHAR FORMAT "x(19)"
    FIELD cod-entreg AS CHAR FORMAT "x(19)"
    FIELD dt-entrega AS DATE FORMAT "99/99/9999".

/* DEFINE TEMP-TABLE tt-dados                                                            */
/*     FIELD nr-pedcli  AS CHAR                                                          */
/*     FIELD tip-pedido    AS INTEGER                                                    */
/*     FIELD data-hora  AS CHAR                                                          */
/*     FIELD it-codigo  AS CHAR                                                          */
/*     FIELD desc-item  AS CHAR                                                          */
/*     FIELD vlr-unit   AS DEC                                                           */
/*     FIELD quantidade AS decimal                                                       */
/*     FIELD vlt-tot    AS DEC                                                           */
/*     FIELD nro-pedido AS CHAR                                                          */
/*     FIELD nome-emit  AS CHAR                                                          */
/*     FIELD e-mail     AS CHAR                                                          */
/*     FIELD telefone   AS CHAR                                                          */
/*     FIELD cnpj       AS CHAR                                                          */
/*     FIELD cod-cond-pag AS CHAR. /* 22/08/2016 Campo a Considerar Pagamento a Vista */ */


DEFINE TEMP-TABLE tt-ped-venda-import
    FIELD NumeroPedidoCliente           LIKE ped-venda.nr-pedcli
    FIELD CodigoEmitente                LIKE emitente.cod-emitente
    FIELD NumeroPedidoSistemaCliente    AS CHAR
    FIELD DataHoraEnvio                 AS CHAR
    FIELD TipoPedido                    AS CHAR         // (S)ales | (D)onation | (E)DI
    FIELD NomeCliente                   LIKE emitente.nome-emit
    FIELD ValorTotal                    AS DECIMAL 
    FIELD Endereco                      LIKE emitente.endereco
    FIELD Email                         LIKE emitente.e-mail
    FIELD Telefone                      AS CHAR
    FIELD CNPJ                          LIKE emitente.cgc
    FIELD PagamentoAV                   AS INTEGER      // 0 - NÆo |  1 - Sim
    FIELD NomeTransportador             AS CHAR
    FIELD DataEntrega                   AS DATE
    FIELD Natureza                      AS CHAR         // (F)isica | (J)uridica
    FIELD NomeRepresentante             AS CHAR
    FIELD Observacao                    AS CHAR
    FIELD r-rowid                       AS ROWID    
    FIELD valLiquido                    AS DECIMAL


    .                                   


DEFINE TEMP-TABLE tt-ped-item-import
    FIELD numSeq                    AS INTEGER 
    FIELD CodigoItem                LIKE ped-item.it-codigo
    FIELD ValorUnitario             AS DECIMAL
    FIELD QuantidadePedida          LIKE ped-item.qt-pedida
    FIELD nat-oper                      LIKE ped-item.nat-oper
    FIELD r-rowid                       AS ROWID
    FIELD flgRestricao                  AS LOGICAL
    FIELD desRestricao                  AS CHAR
    FIELD QuantidadeDisponivel      AS DECIMAL

    .



DEFINE TEMP-TABLE ttParam
    FIELD flgCompletarPedido    AS LOGICAL
    FIELD flgValidarPreco       AS LOGICAL
    FIELD flgValidartrib        AS LOGICAL
    .
