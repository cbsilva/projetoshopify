/*----------------------------------------------------------------------------------------------/
 Programa..: esint301
 Objetivo..: Rotina de envio de e-mail
 Data......: 08.11.2021
 Autor.....: 4Make Consultoria
 Vers∆o....: 2.000.001
-----------------------------------------------------------------------------------------------*/


/******************************* Definitions **************************************************/

/* --------------------------------------------------------------------------------------------
    Temp-Tables Definitions
----------------------------------------------------------------------------------------------*/
{utp/utapi019.i1} 
{method/dbotterr.i}

/* --------------------------------------------------------------------------------------------
    Global  Variable Definitions
----------------------------------------------------------------------------------------------*/



/* --------------------------------------------------------------------------------------------
    Local Variable Definitions
----------------------------------------------------------------------------------------------*/
DEFINE VARIABLE e-mensagem     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE e-destinatario AS CHARACTER   NO-UNDO.
DEFINE VARIABLE e-assunto      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cChaveRegistro AS CHARACTER   NO-UNDO.


/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM pRowTable AS ROWID       NO-UNDO.
DEFINE INPUT  PARAM pCodigo   AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAM pMensagem AS CHARACTER   NO-UNDO.



/******************************* Main Block **************************************************/

FOR FIRST es_notificacoes NO-LOCK WHERE es_notificacoes.cod_mensagem = pCodigo:

    //IF es_notificacoes.flg_Logistica AND pTipo = "Logistica"   THEN
    //    RUN piEmailLogistica(INPUT pMensagem,INPUT es_notificacoes.lst_mail).
    //
    //IF es_notificacoes.flg_Comercial AND pTipo = "Comercial"  THEN 
    //    RUN piEmailComercial(INPUT pMensagem,INPUT es_notificacoes.lst_mail).
    //
    IF es_notificacoes.flg_Financeiro THEN 
        RUN piMsgFinanceiro(INPUT pMensagem, INPUT es_notificacoes.lst_mail, INPUT pRowTable).

    //IF es_notificacoes.flg_Fiscal     AND pTipo = "Fiscal"     THEN 
    //    RUN piEmailFiscal(INPUT pMensagem, INPUT es_notificacoes.lst_mail).
END.


RETURN "OK":U.


PROCEDURE piMsgFinanceiro:
    DEFINE INPUT PARAM pMsg          AS CHAR  NO-UNDO.
    DEFINE INPUT PARAM pDestinatario AS CHAR  NO-UNDO.
    DEFINE INPUT PARAM pRowTable     AS ROWID NO-UNDO.


    FIND FIRST es-api-import WHERE rowid(es-api-import) = pRowTable NO-LOCK NO-ERROR.
    IF NOT AVAIL es-api-import THEN LEAVE.

     ASSIGN e-destinatario = pDestinatario
            e-destinatario =  replace(e-destinatario,";",",") .   
            e-destinatario =  replace(e-destinatario,",,",",") .
            e-destinatario =  replace(e-destinatario,chr(13),""). 
            e-destinatario =  replace(e-destinatario,chr(10),"").
            e-destinatario =  replace(e-destinatario,chr(9),"").
            e-destinatario =  replace(e-destinatario,chr(32),"").  
            e-destinatario =  replace(e-destinatario,chr(32),"").  
            e-destinatario =  replace(e-destinatario,",,",",") .
            e-destinatario =  replace(e-destinatario," ","").
            e-destinatario =  lc(e-destinatario) .



     ASSIGN e-mensagem = "Informativo do Sistema SHOPIFY <BR><BR><BR>"
                       + "Seguem mensagens geradas ao criar/atualizar o cliente :<BR><BR>"
                       + SUBSTITUTE("Cliente...: &1<br>",es-api-import.chave)
                       + SUBSTITUTE("Data......: &1<br>",STRING(TODAY,"99/99/9999"))
                       + SUBSTITUTE("Hora......: &1<br><br>",STRING(TIME,"HH:MM"))
                       + pMsg.

     ASSIGN e-assunto = "[MACMILLAN][SHOPIFY] IMPORTAÄ«O/ATUALIZAÄ«O COM ERROS".


     RUN piEnviaEmail.




END PROCEDURE.

PROCEDURE piEnviaEmail:

    
    IF NOT VALID-HANDLE(h-utapi019) THEN
        run utp/utapi019.p persistent set h-utapi019.


    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-mensagem. 
    EMPTY TEMP-TABLE tt-paramEmail.
    EMPTY TEMP-TABLE tt-erros.

    FIND FIRST param-global NO-LOCK NO-ERROR.

    create tt-envio2.
    ASSIGN tt-envio2.versao-integracao = 1
           tt-envio2.exchange          = TRUE
           tt-envio2.servidor          = param-global.serv-mail
           tt-envio2.porta             = param-global.porta-mail
           tt-envio2.destino           = e-destinatario 
           tt-envio2.log-enviada       = NO 
           tt-envio2.log-lida          = NO 
           tt-envio2.acomp             = NO 
           tt-envio2.remetente         = "suporte@macmillaneducation.com" 
           tt-envio2.assunto           = e-assunto
           tt-envio2.formato           = "HTML".



    create tt-mensagem.
    assign tt-mensagem.seq-mensagem = 1
           tt-mensagem.mensagem     = e-mensagem. 


    run pi-execute2 in h-utapi019 (input  table tt-envio2,
                                   input  table tt-mensagem,
                                   output table tt-erros).



    if return-value = "NOK" then do:

        output to value(session:temp-directory + "envemail_integracao.txt").       
           for each tt-erros:
               disp tt-erros with 1 column width 300.
           end.                               
        output close.
        
    end.






    delete procedure h-utapi019.







END PROCEDURE.





       




