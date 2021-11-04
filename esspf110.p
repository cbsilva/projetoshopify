/*
 *------------------------------------------------------------------------------
 *  PROGRAMA        esp/esspf110.p
 *  OBJETIVO        Impmortaá∆o de Pedido de Venda a partir de JSON recebido
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

USING Progress.Json.OBJECTModel.*.

{lib/utilidades.i}
{lib/PedidoVenda.i}

{method/dbotterr.i}     


DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER pErro AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER pChave AS CHARACTER   NO-UNDO.

/* ------- Definiá∆o de Vari†veis ------ */
DEFINE VARIABLE cLongJson        AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lRetJson         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE body             AS JsonObject NO-UNDO.
DEFINE VARIABLE jsonOutput       AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObject      AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectMain  AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObjectSec   AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonArrayMain   AS JsonArray  NO-UNDO.
DEFINE VARIABLE oJsonArraySec    AS JsonArray  NO-UNDO.
DEFINE VARIABLE iCountMain       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCountSec        AS INTEGER    NO-UNDO.
DEFINE VARIABLE cprop            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-natureza       AS CHARACTER  NO-UNDO INITIAL "Pessoa F°sica,Pessoa Jur°dica,Estrangeiro,Trading". 
DEFINE VARIABLE c-tipocredito    AS CHARACTER  NO-UNDO INITIAL "Normal,Autom†tico,Suspenso,S¢ Imp Ped,Pg Ö Vista".
DEFINE VARIABLE m-json           AS MEMPTR     NO-UNDO.
DEFINE VARIABLE myParser         AS ObjectModelParser NO-UNDO. 
DEFINE VARIABLE pJsonInput       AS JsonObject NO-UNDO.

DEFINE VARIABLE hProc AS HANDLE      NO-UNDO.



    FOR FIRST es-api-import-spf NO-LOCK WHERE
              ROWID(es-api-import) = pRowid:
    END.
    IF NOT AVAIL es-api-import-spf THEN
    DO:
       ASSIGN pErro = "Registro n∆o encontrado".
       RETURN "NOK".
    END.

    FIX-CODEPAGE(cLongJson) = "UTF-8".

    COPY-LOB es-api-import-spf.c-json TO m-json.
    COPY-LOB m-json TO cLongJson NO-CONVERT.

     myParser = NEW ObjectModelParser(). 
     pJsonInput = CAST(myParser:Parse(cLongJson),JsonObject).
     oJsonArrayMain = pJsonInput:GetJsonArray("req":U).    

    DO iCountMain = 1 TO oJsonArrayMain:LENGTH:

        CREATE tt-ped-venda-import.
        
        oJsonObjectMain =  oJsonArrayMain:GetJsonObject(iCountMain).
    
        IF oJsonObjectMain:Has(TRIM("NumeroPedidoCliente         "))  THEN ASSIGN     tt-ped-venda-import.NumeroPedidoCliente         = oJsonObjectMain:GetCharacter(TRIM("NumeroPedidoCliente       ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("CodigoEmitente              "))  THEN ASSIGN     tt-ped-venda-import.CodigoEmitente              = oJsonObjectMain:GetInteger  (TRIM("CodigoEmitente            ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("NumeroPedidoSistemaCliente  "))  THEN ASSIGN     tt-ped-venda-import.NumeroPedidoSistemaCliente  = oJsonObjectMain:GetCharacter(TRIM("NumeroPedidoSistemaCliente")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("DataHoraEnvio               "))  THEN ASSIGN     tt-ped-venda-import.DataHoraEnvio               = oJsonObjectMain:GetCharacter(TRIM("DataHoraEnvio             ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("TipoPedido                  "))  THEN ASSIGN     tt-ped-venda-import.TipoPedido                  = oJsonObjectMain:GetCharacter(TRIM("TipoPedido                ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("NomeCliente                 "))  THEN ASSIGN     tt-ped-venda-import.NomeCliente                 = oJsonObjectMain:GetCharacter(TRIM("NomeCliente               ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("ValorTotal                  "))  THEN ASSIGN     tt-ped-venda-import.ValorTotal                  = oJsonObjectMain:GetDecimal  (TRIM("ValorTotal                ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("Endereco                    "))  THEN ASSIGN     tt-ped-venda-import.Endereco                    = oJsonObjectMain:GetCharacter(TRIM("Endereco                  ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("Email                       "))  THEN ASSIGN     tt-ped-venda-import.Email                       = oJsonObjectMain:GetCharacter(TRIM("Email                     ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("Telefone                    "))  THEN ASSIGN     tt-ped-venda-import.Telefone                    = oJsonObjectMain:GetCharacter(TRIM("Telefone                  ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("CNPJ                        "))  THEN ASSIGN     tt-ped-venda-import.CNPJ                        = oJsonObjectMain:GetCharacter(TRIM("CNPJ                      ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("PagamentoAV                 "))  THEN ASSIGN     tt-ped-venda-import.PagamentoAV                 = oJsonObjectMain:GetInteger  (TRIM("PagamentoAV               ")) NO-ERROR.
	    IF oJsonObjectMain:Has(TRIM("NomeTransportador           "))  THEN ASSIGN     tt-ped-venda-import.NomeTransportador           = oJsonObjectMain:GetCharacter(TRIM("NomeTransportador         ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("DataEntrega                 "))  THEN ASSIGN     tt-ped-venda-import.DataEntrega                 = oJsonObjectMain:GetDate     (TRIM("DataEntrega               ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("Natureza                    "))  THEN ASSIGN     tt-ped-venda-import.Natureza                    = oJsonObjectMain:GetCharacter(TRIM("Natureza                  ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("NomeRepresentante           "))  THEN ASSIGN     tt-ped-venda-import.NomeRepresentante           = oJsonObjectMain:GetCharacter(TRIM("NomeRepresentante         ")) NO-ERROR.
        IF oJsonObjectMain:Has(TRIM("Observacao                  "))  THEN ASSIGN     tt-ped-venda-import.Observacao                  = oJsonObjectMain:GetCharacter(TRIM("Observacao                ")) NO-ERROR.

        IF oJsonObjectMain:Has("ItemPedidoList") THEN DO:  
            oJsonArraySec = oJsonObjectMain:GetJsonArray("ItemPedidoList").
    
            DO iCountSec = 1 TO oJsonArraySec:LENGTH:
                oJsonObjectSec =  oJsonArraySec:GetJsonObject(iCountSec).          
    
                CREATE tt-ped-item-import.
    
                IF oJsonObjectSec:Has(TRIM("CodigoItem          ")) THEN ASSIGN	tt-ped-item-import.CodigoItem             = oJsonObjectSec:GetCharacter (TRIM("CodigoItem        ")) NO-ERROR.            
                IF oJsonObjectSec:Has(TRIM("ValorUnitario       ")) THEN ASSIGN	tt-ped-item-import.ValorUnitario          = oJsonObjectSec:GetDecimal   (TRIM("ValorUnitario     ")) NO-ERROR.            
                IF oJsonObjectSec:Has(TRIM("QuantidadePedida    ")) THEN ASSIGN	tt-ped-item-import.QuantidadePedida       = oJsonObjectSec:GetDecimal   (TRIM("QuantidadePedida  ")) NO-ERROR.            
            END.                                                                                                                                         
                                                                                                                                                         
        END.

        IF NOT CAN-FIND(FIRST tt-ped-venda-import) THEN
        DO:
            ASSIGN pErro        = "Erro. N∆o encontrado pedido no arquivo importado".            
            RETURN "NOK".
        END.

        IF NOT CAN-FIND(FIRST tt-ped-item-import) THEN
        DO:
            ASSIGN pErro        = "Erro. N∆o encontrado item de pedido no arquivo importado".            
            RETURN "NOK".
        END.

        RUN lib/PedidoVenda.p PERSISTENT SET hProc.                                                                                                      
        RUN receberPedidos IN hProc (INPUT TABLE tt-ped-venda-import,
                                     INPUT TABLE tt-ped-item-import).

/*         BLOCO:                                */
/*         DO  TRANSACTION                       */
/*             ON ERROR  UNDO BLOCO, LEAVE BLOCO */
/*             ON QUIT   UNDO BLOCO, LEAVE BLOCO */
/*             ON STOP   UNDO BLOCO, LEAVE BLOCO */
/*             :                                 */

            RUN processarDados IN hProc
                (OUTPUT pChave).
/*             MESSAGE "esspf110 - depoips do processar dados " RETURN-VALUE */
/*                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                 */
            IF RETURN-VALUE <> "OK" THEN
            DO:
                RUN retornarErros IN hProc (OUTPUT TABLE rowErrors).
                ASSIGN pErro = "Erro ao criar pedido.".
                FOR EACH rowErrors:
                    pErro = pErro + SUBSTITUTE(rowErrors.Errordesc + "(&1)[&2]", rowErrors.errorNum, rowErrors.errorsubType) + "|" + CHR(10) + CHR(13) .
    
                END.
                DELETE PROCEDURE hProc.

                //RUN checarPersistentes.

                // UNDO BLOCO, RETURN "NOK".
                RETURN "NOK".
            END.    
    

            DELETE PROCEDURE hProc.                                                                                                                          

/*         END. // TRANS */


    END.

//    RUN checarPersistentes.


    RETURN "OK".                                                                                                                                                                                         


/*
 *------------------------------------------------------------------------------
 *
 *------------------------------------------------------------------------------
 */
PROCEDURE checarPersistentes:

DEFINE VARIABLE hProcedureHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE hAux AS HANDLE      NO-UNDO.
DEFINE VARIABLE cProgramName AS CHARACTER NO-UNDO.


    LOG-MANAGER:WRITE-MESSAGE("INICIO - Tratando procedures persistentes").
    LOG-MANAGER:WRITE-MESSAGE("LISTANDO...").

    ASSIGN
        hProcedureHandle = SESSION:FIRST-PROCEDURE NO-ERROR.

    DO WHILE VALID-HANDLE(hProcedureHandle):
        LOG-MANAGER:WRITE-MESSAGE("Procedure: " + STRING(hProcedureHandle:FILE-NAME)).
        hProcedureHandle = hProcedureHandle:NEXT-SIBLING NO-ERROR.
    END.

    LOG-MANAGER:WRITE-MESSAGE("EXCLUINDO...").
    ASSIGN
        hProcedureHandle = SESSION:FIRST-PROCEDURE NO-ERROR.

    DO WHILE VALID-HANDLE(hProcedureHandle):
        IF VALID-HANDLE(hProcedureHandle) AND NOT hProcedureHandle:FILE-NAME MATCHES "*esspf100*" THEN
        DO:
            LOG-MANAGER:WRITE-MESSAGE("Excluindo procedure: " + STRING(hProcedureHandle:FILE-NAME)).
            hAux = hProcedureHandle.
            hProcedureHandle = hProcedureHandle:NEXT-SIBLING NO-ERROR.
            DELETE PROCEDURE hAux.            
        END.
        ELSE
            hProcedureHandle = hProcedureHandle:NEXT-SIBLING NO-ERROR.

    END.

    LOG-MANAGER:WRITE-MESSAGE("FIM - Tratando procedures persistentes").

END PROCEDURE.
