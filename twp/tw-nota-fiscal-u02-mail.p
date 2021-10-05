{utp/ut-glob.i}
{utp/utapi019.i}

define temp-table tt-param-aux
        field destino              as integer
        field destino-bloq         as integer
        field arquivo              as char
        field arquivo-bloq         as char
        field usuario              as char
        field data-exec            as date
        field hora-exec            as integer
        field parametro            as logical
        field formato              as integer
        field cod-layout           as character
        field des-layout           as character
        field log-impr-dados       as logical  
        field v_num_tip_aces_usuar as integer
        field ep-codigo            AS CHAR
        field c-cod-estabel        like nota-fiscal.cod-estabel
        field c-serie              like nota-fiscal.serie
        field c-nr-nota-fis-ini    like nota-fiscal.nr-nota-fis
        field c-nr-nota-fis-fim    like nota-fiscal.nr-nota-fis
        field de-cdd-embarq-ini    like nota-fiscal.cdd-embarq
        field de-cdd-embarq-fim    like nota-fiscal.cdd-embarq
        field da-dt-saida          like nota-fiscal.dt-saida
        field c-hr-saida           like nota-fiscal.hr-confirma
        field banco                as integer
        field cod-febraban         as integer      
        field cod-portador         as integer      
        field prox-bloq            as char         
        field c-instrucao          as char extent 5
        field imprime-bloq         as logical
        field rs-imprime           as INTEGER
        FIELD impressora-so        AS CHAR
        FIELD impressora-so-bloq   AS CHAR.
    
def shared var c-programa-mg97 as char format "x(08)" no-undo.
def shared var c-versao-mg97 as char format "x(08)" no-undo.

define new shared temp-table tt-notas-impressas
    field r-nota as rowid.
    
define temp-table tt-raw-digita
    field raw-digita as raw.

def parameter buffer b-nota for nota-fiscal.

RUN envia-email.

RETURN "OK".

PROCEDURE envia-email.

    DEF VAR c-destino-mail AS CHAR.
    DEF VAR c-remetente    AS CHAR.
    DEF VAR c-assunto      AS CHAR.
    DEF VAR c-arq-email    AS CHAR.

    RUN pi-imprime-nota (output c-arq-email).
    
    FIND FIRST emitente 
         WHERE emitente.cod-emitente = b-nota.cod-emitente NO-LOCK NO-ERROR.

    ASSIGN c-remetente    = 'suporte@macmillan.com'
           c-assunto      = 'EMISSÇO NFSE - MACMILLAN'
           c-destino-mail = emitente.e-mail.

    FOR EACH tt-envio2.
        DELETE tt-envio2.
    END.

    FOR EACH tt-mensagem.
        DELETE tt-mensagem.
    END.

    run utp/utapi019.p persistent set h-utapi019.

    FIND FIRST param-global NO-LOCK NO-ERROR.
    
    create tt-envio2.
    assign tt-envio2.versao-integracao = 1
           tt-envio2.exchange    = YES
           tt-envio2.log-lida    = YES
           tt-envio2.servidor    = IF AVAIL param-global THEN param-global.serv-mail ELSE " "
           tt-envio2.porta       = IF AVAIL param-global THEN param-global.porta-mail ELSE 25
           tt-envio2.destino     = c-destino-mail
           tt-envio2.remetente   = c-remetente
           tt-envio2.assunto     = c-assunto
           tt-envio2.arq-anexo   = c-arq-email
           tt-envio2.formato     = "HTML".

    create tt-mensagem.
    assign tt-mensagem.seq-mensagem = 1
           tt-mensagem.mensagem     = "".

    run pi-execute2 in h-utapi019 (input  table tt-envio2,
                                   input  table tt-mensagem,
                                   output table tt-erros).
    
    if return-value = "NOK" then do:
    
        output to value(session:temp-directory + "envemail.txt").       
           for each tt-erros:
               disp tt-erros with 1 column width 300.
           end.                               
        output close.
        
    end.
    
    delete procedure h-utapi019.
   
END PROCEDURE.


PROCEDURE pi-imprime-nota.

    DEFINE VARIABLE c-dir-spool         AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER c-pdf-file  AS CHARACTER  NO-UNDO.
    
    {UTP/ut-glob.i}
    {include/i_dbvers.i}
    def var raw-param        as raw no-undo.  
    
    ASSIGN c-dir-spool = session:temp-directory + "nfe.docx" 
           c-pdf-file  = session:temp-directory + "nfe.pdf".
           
    CREATE tt-notas-impressas.
    ASSIGN tt-notas-impressas.r-nota = ROWID(b-nota).
    
    create tt-param-aux.
    assign tt-param-aux.usuario              = c-seg-usuario
           tt-param-aux.destino              = 2
           tt-param-aux.data-exec            = today
           tt-param-aux.hora-exec            = time
           tt-param-aux.arquivo              = 'c:/temp/nfe.docx'
           tt-param-aux.ep-codigo            = i-ep-codigo-usuario
           tt-param-aux.c-cod-estabel        = b-nota.cod-estabel
           tt-param-aux.c-serie              = b-nota.serie
           tt-param-aux.c-nr-nota-fis-ini    = b-nota.nr-nota-fis
           tt-param-aux.c-nr-nota-fis-fim    = b-nota.nr-nota-fis
           tt-param-aux.de-cdd-embarq-ini    = 0
           tt-param-aux.de-cdd-embarq-fim    = 999999
           tt-param-aux.rs-imprime           = 2 
           tt-param-aux.da-dt-saida          = TODAY.
    
    FIND FIRST tt-param-aux.
    raw-transfer tt-param-aux    to raw-param.
                                            
    run value("ftp\ft051627.r") (input raw-param,
                                 input table tt-raw-digita).
    
    def var ch-AppWord   as com-handle no-undo.
    def var ch-DocWord   as com-handle no-undo.
    
    
    /* Cria docto word */
    create "word.application" ch-AppWord.
    ch-AppWord:visible        = false.
    ch-AppWord:DisplayAlerts  = 0.
    ch-DocWord      = ch-AppWord:Documents:Add(c-dir-spool).
    
    ch-DocWord:ExportAsFixedFormat(c-pdf-file , 17, false , 0 ,,,,,,,,,,) . 
    
    ch-AppWord:Documents:CLOSE NO-ERROR.
    ch-AppWord:APPLICATION:QUIT.

END PROCEDURE.
