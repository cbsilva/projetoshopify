/*----------------------------------------------------------------------------------------------/
 Programa..: esint0021.p
 Objetivo..: Interface Integraá∆o Clientes SHOPIFY - Importaá∆o
 Data......: 27/07/2021
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
{esp/esint021.i}
{method/dbotterr.i}
{include/boini.i} /** tt-bo-erro **/

/* --------------------------------------------------------------------------------------------
    Global  Variable Definitions
----------------------------------------------------------------------------------------------*/



/* --------------------------------------------------------------------------------------------
    Local Variable Definitions
----------------------------------------------------------------------------------------------*/
DEFINE VARIABLE cLongJson        AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE lRetJson         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iCountMain       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCountSec        AS INTEGER    NO-UNDO.
DEFINE VARIABLE h-boad098        AS HANDLE     NO-UNDO.
DEFINE VARIABLE iErro            AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCodEmitente     AS INTEGER    NO-UNDO.
DEFINE VARIABLE rw-emitente      AS ROWID      NO-UNDO.


/* --------------------------------------------------------------------------------------------
    Functions
----------------------------------------------------------------------------------------------*/
FUNCTION fnNatureza RETURNS INTEGER (INPUT pParam AS CHAR):
    /*1  = Pessoa Fisica | 2 = Pessoa Juridica | 3 = Extrangeiro*/
    CASE LENGTH(pParam):
        WHEN 11 THEN
            RETURN 1.
        WHEN 14 THEN
            RETURN 2.
        OTHERWISE
            RETURN 3.

    END CASE.  
END FUNCTION.


/* --------------------------------------------------------------------------------------------
    Define input parameters
----------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAM TABLE FOR ttCustomer.
DEFINE OUTPUT PARAM TABLE FOR RowErrors.


/******************************* Main Block **************************************************/


IF NOT VALID-HANDLE(h-boad098) THEN
    RUN adbo/boad098.p PERSISTENT SET h-boad098.


FIND FIRST es-api-param-cliente-spf NO-LOCK NO-ERROR.
IF NOT AVAIL es-api-param-cliente-spf THEN
DO:
    RUN piErro("ParÉmetro do cliente n∆o cadastrado no ESINT016","").
    RETURN "NOK":U.
END.


FOR FIRST ttCustomer:

    EMPTY TEMP-TABLE tt-emitente.

    RUN findCGC IN h-boad098 (INPUT ttCustomer.CNPJ, OUTPUT c-return).

    IF RETURN-VALUE = "OK" THEN 
        RUN piAlteraCliente.
    ELSE
        RUN piCriaCliente.

END.

IF VALID-HANDLE(h-boad098) THEN
    DELETE PROCEDURE h-boad098.


PROCEDURE piAlteraCliente:

    FOR FIRST emitente WHERE emitente.cgc = ttCustomer.CNPJ NO-LOCK:

        ASSIGN iCodEmitente = emitente.cod-emitente.

        CREATE tt-emitente.
        BUFFER-COPY emitente TO tt-emitente
            ASSIGN tt-emitente.nome-emit        = ttCustomer.RazaoSocial
                   tt-emitente.Telefone         = ttCustomer.telefone      
                   tt-emitente.e-mail           = ttCustomer.email
                   tt-emitente.endereco         = ttCustomer.Endereco         
                   tt-emitente.bairro           = ttCustomer.bairro           
                   tt-emitente.cidade           = ttCustomer.cidade           
                   tt-emitente.estado           = ttCustomer.estado           
                   tt-emitente.pais             = ttCustomer.pais             
                   tt-emitente.CEP              = ttCustomer.cep             
                   tt-emitente.endereco-cob     = ttCustomer.Endereco         
                   tt-emitente.bairro-cob       = ttCustomer.bairro           
                   tt-emitente.cidade-cob       = ttCustomer.cidade           
                   tt-emitente.estado-cob       = ttCustomer.estado           
                   tt-emitente.pais-cob         = ttCustomer.pais             
                   tt-emitente.cep-cob          = ttCustomer.cep
                   tt-emitente.r-rowid          = ROWID(emitente).

        RUN openQuery      IN h-boad098 (INPUT 1).
        RUN validateUpdate IN h-boad098 (INPUT TABLE tt-emitente,
                                         INPUT tt-emitente.r-rowid,
                                         OUTPUT TABLE tt-bo-erro).

        IF CAN-FIND(FIRST tt-bo-erro NO-LOCK) THEN     
        DO:                                            
            FOR EACH tt-bo-erro NO-LOCK:               
                RUN piErro(tt-bo-erro.mensagem,"").    
            END.                                       
                                                       
                                                       
        END.                                           
        ELSE RUN pi-AtualizaEMS5.                      
        
    END.

END PROCEDURE.


PROCEDURE piCriaCliente:

    /*-- recupera o c¢digo do cliente --*/
    RUN cdp/cd9960.p (OUTPUT iCodEmitente).

    CREATE tt-emitente.
    ASSIGN tt-emitente.cod-emitente                = iCodEmitente
           tt-emitente.nome-abrev                  = SUBSTRING(ttCustomer.cnpj,1,12)
           tt-emitente.Nome-matriz                 = SUBSTRING(ttCustomer.cnpj,1,12)
           tt-emitente.cgc                         = ttCustomer.cnpj
           tt-emitente.identific                   = 1 /*-- cliente --*/
           tt-emitente.natureza                    = fnNatureza(ttCustomer.cnpj)
           tt-emitente.nome-emit                   = ttCustomer.RazaoSocial
           tt-emitente.endereco                    = ttCustomer.Endereco
           tt-emitente.bairro                      = ttCustomer.bairro
           tt-emitente.cidade                      = ttCustomer.cidade
           tt-emitente.estado                      = ttCustomer.estado
           tt-emitente.pais                        = ttCustomer.pais
           tt-emitente.CEP                         = ttCustomer.cep  
           tt-emitente.End-cobranca                = iCodEmitente
           tt-emitente.endereco-cob                = ttCustomer.Endereco           
           tt-emitente.bairro-cob                  = ttCustomer.bairro               
           tt-emitente.cidade-cob                  = ttCustomer.cidade               
           tt-emitente.estado-cob                  = ttCustomer.estado               
           tt-emitente.pais-cob                    = ttCustomer.pais                 
           tt-emitente.cep-cob                     = ttCustomer.cep
           tt-emitente.Ins-estadual                = 'ISENTO'                                           
           tt-emitente.Taxa-financ                 = 0                                                  
           tt-emitente.Cod-transp                  = es-api-param-cliente-spf.cod-transp
           tt-emitente.Telefone                    = ttCustomer.telefone
           tt-emitente.e-mail                      = ttCustomer.email
           tt-emitente.data-implant                = TODAY
           tt-emitente.cod-rep                     = 1 /*-- incluir na tela de paramtros um campo para informar o codigo do repres*/
           tt-emitente.Cod-gr-cli                  = es-api-param-cliente-spf.cod-gr-cli   
           tt-emitente.Perc-fat-ped                = es-api-param-cliente-spf.perc-fat-ped 
           tt-emitente.Portador                    = es-api-param-cliente-spf.portador    
           tt-emitente.Modalidade                  = es-api-param-cliente-spf.modalidade   
           tt-emitente.Ind-fat-par                 = es-api-param-cliente-spf.ind-fat-par  
           tt-emitente.Ind-cre-cli                 = 1                                 
           tt-emitente.Ind-apr-cred                = YES  
           tt-emitente.Nat-operacao                = es-api-param-cliente-spf.nat-operacao 
           tt-emitente.cod-cond-pag                = es-api-param-cliente-spf.cod-cond-pag 
           tt-emitente.emite-bloq                  = es-api-param-cliente-spf.emite-bloq
           tt-emitente.port-prefer                 = es-api-param-cliente-spf.port-prefer
           tt-emitente.mod-prefer                  = es-api-param-cliente-spf.mod-prefer 
           tt-emitente.nat-ope-ext                 = es-api-param-cliente-spf.nat-ope-ext
           tt-emitente.esp-pd-venda                = es-api-param-cliente-spf.esp-pd-venda 
           tt-emitente.agente-retencao             = es-api-param-cliente-spf.agente-retencao 
           tt-emitente.log-calcula-pis-cofins-unid = es-api-param-cliente-spf.log-calcula-pis-cofins-unid
           tt-emitente.log-optan-suspens-ipi       = es-api-param-cliente-spf.log-optan-suspens-ipi 
           tt-emitente.log-nf-eletro               = es-api-param-cliente-spf.log-nf-eletro 
           tt-emitente.cod-email-nfe               = es-api-param-cliente-spf.cod-email-nfe 
           tt-emitente.tp-rec-padrao               = es-api-param-cliente-spf.tp-rec-padrao 
           tt-emitente.ins-banc                    = es-api-param-cliente-spf.ins-banc. 




    RUN openQuery      IN h-boad098 (INPUT 1).                                   
    RUN validateCreate IN h-boad098 (INPUT TABLE tt-emitente,                                                       
                                     OUTPUT TABLE tt-bo-erro,
                                     OUTPUT rw-emitente).


    IF CAN-FIND(FIRST tt-bo-erro NO-LOCK) THEN 
    DO:
        FOR EACH tt-bo-erro NO-LOCK:
            RUN piErro(tt-bo-erro.mensagem,"").
        END.
    
        
    END.
    ELSE RUN pi-AtualizaEMS5.


END PROCEDURE.


PROCEDURE pi-AtualizaEMS5:
                   
   /************* Integracao 2.00 X 5.00 *****************/      
   IF CAN-FIND(funcao WHERE funcao.cd-funcao = "adm-cdc-ems-5.00"
       AND funcao.ativo = YES                                    
       AND funcao.log-1 = YES) THEN DO:                          
       FIND FIRST param-global NO-LOCK NO-ERROR.                 
       IF  param-global.log-2 = YES THEN DO:                     
           VALIDATE emitente NO-ERROR.                           
           RUN cdp/cd1608.p (INPUT tt-emitente.cod-emitente,     
                             INPUT tt-emitente.cod-emitente,     
                             INPUT tt-emitente.identific,        
                             INPUT YES,                          
                             INPUT 1,                            
                             INPUT 0,                            
                             INPUT "utb765zb.tmp",               
                             INPUT "Terminal":U,                 
                             INPUT "").                          
                                                                 
       END.                                                      
   END.                                                          
   /*********** Fim Integracao 2.00 X 5.00 ****************/ 

END PROCEDURE.



PROCEDURE piErro:
    DEFINE INPUT PARAM cErrorDescription   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAM cErrorHelp          AS CHARACTER NO-UNDO.

    CREATE RowErrors.
    ASSIGN iErro            = iErro + 1
           ErrorSequence    = iErro
           ErrorNumber      = 17006
           ErrorType        = "Error"
           ErrorDescription = cErrorDescription
           ErrorHelp        = cErrorHelp.

END PROCEDURE.
