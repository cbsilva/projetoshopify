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
DEFINE INPUT  PARAM TABLE FOR ttEnderecoList.
DEFINE OUTPUT PARAM TABLE FOR RowErrors.


/******************************* Main Block **************************************************/


IF NOT VALID-HANDLE(h-boad098) THEN
    RUN adbo/boad098.p PERSISTENT SET h-boad098.


FIND FIRST es-api-param-cliente NO-LOCK NO-ERROR.
IF NOT AVAIL es-api-param-cliente THEN
DO:
    RUN piErro("ParÉmetro do cliente n∆o cadastrado no ESINT016","").
    RETURN "NOK":U.
END.




FOR FIRST ttCustomer:

    EMPTY TEMP-TABLE tt-emitente.

    RUN findCGC IN h-boad098 (INPUT ttCustomer.CNPJ, OUTPUT c-return).

    /*-- ver uma forma de amarrar as duas temp-tables --*/
    FIND FIRST ttEnderecoList NO-LOCK NO-ERROR.
    IF NOT AVAIL ttEnderecoList THEN
    DO:
        RUN piErro("SHOPIFY | Endereáo do cliente n∆o informado.","").
        RETURN "NOK":U.
    END.

    IF RETURN-VALUE = "OK" THEN 
        RUN piAlteraCliente.
    ELSE
        RUN piCriaCliente.

END.

IF VALID-HANDLE(h-boad098) THEN
    DELETE PROCEDURE h-boad098.


PROCEDURE piAlteraCliente:


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
           tt-emitente.endereco                    = ttEnderecoList.logradouro
           tt-emitente.bairro                      = ttEnderecoList.bairro
           tt-emitente.cidade                      = ttEnderecoList.cidade
           tt-emitente.estado                      = ttEnderecoList.estado
           tt-emitente.pais                        = ttEnderecoList.pais
           tt-emitente.CEP                         = ttEnderecoList.cep  
           tt-emitente.End-cobranca                = iCodEmitente
           tt-emitente.endereco-cob                = ttEnderecoList.logradouro           
           tt-emitente.bairro-cob                  = ttEnderecoList.bairro               
           tt-emitente.cidade-cob                  = ttEnderecoList.cidade               
           tt-emitente.estado-cob                  = ttEnderecoList.estado               
           tt-emitente.pais-cob                    = ttEnderecoList.pais                 
           tt-emitente.cep-cob                     = ttEnderecoList.cep
           tt-emitente.Ins-estadual                = 'ISENTO'                                           
           tt-emitente.Taxa-financ                 = 0                                                  
           tt-emitente.Cod-transp                  = es-api-param-cliente.cod-transp
           tt-emitente.Contato                     = ""
           tt-emitente.Telefone                    = ttCustomer.telefone
           tt-emitente.e-mail                      = ttCustomer.email
           tt-emitente.data-implant                = TODAY
           tt-emitente.cod-rep                     = 1 /*-- incluir na tela de paramtros um campo para informar o codigo do repres*/
           tt-emitente.Cod-gr-cli                  = es-api-param-cliente.cod-gr-cli   
           tt-emitente.Perc-fat-ped                = es-api-param-cliente.perc-fat-ped 
           tt-emitente.Portador                    = es-api-param-cliente.portador    
           tt-emitente.Modalidade                  = es-api-param-cliente.modalidade   
           tt-emitente.Ind-fat-par                 = es-api-param-cliente.ind-fat-par  
           tt-emitente.Ind-cre-cli                 = 1                                 
           tt-emitente.Ind-apr-cred                = YES  
           tt-emitente.Nat-operacao                = es-api-param-cliente.nat-operacao 
           tt-emitente.cod-cond-pag                = es-api-param-cliente.cod-cond-pag 
           tt-emitente.emite-bloq                  = es-api-param-cliente.emite-bloq
           tt-emitente.port-prefer                 = es-api-param-cliente.port-prefer
           tt-emitente.mod-prefer                  = es-api-param-cliente.mod-prefer 
           tt-emitente.nat-ope-ext                 = es-api-param-cliente.nat-ope-ext
           tt-emitente.esp-pd-venda                = es-api-param-cliente.esp-pd-venda 
           tt-emitente.agente-retencao             = es-api-param-cliente.agente-retencao 
           tt-emitente.log-calcula-pis-cofins-unid = es-api-param-cliente.log-calcula-pis-cofins-unid
           tt-emitente.log-optan-suspens-ipi       = es-api-param-cliente.log-optan-suspens-ipi 
           tt-emitente.log-nf-eletro               = es-api-param-cliente.log-nf-eletro 
           tt-emitente.cod-email-nfe               = es-api-param-cliente.cod-email-nfe 
           tt-emitente.tp-rec-padrao               = es-api-param-cliente.tp-rec-padrao 
           tt-emitente.ins-banc                    = es-api-param-cliente.ins-banc. 





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
