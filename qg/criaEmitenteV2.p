DEFINE VARIABLE cDados          AS CHARACTER        NO-UNDO.
DEFINE VARIABLE hBo             AS HANDLE           NO-UNDO.
DEFINE VARIABLE rw-emitente     AS ROWID            NO-UNDO.
DEFINE VARIABLE i-emitente      AS INTEGER          NO-UNDO.
DEFINE VARIABLE c-cep           LIKE emitente.cep   NO-UNDO.
DEFINE VARIABLE c-rua           AS CHAR             NO-UNDO.
DEFINE VARIABLE c-nro           AS CHAR             NO-UNDO.
DEFINE VARIABLE c-comp          AS CHAR             NO-UNDO.

DEF VAR h-cdapi704     AS HANDLE              NO-UNDO.

DEFINE TEMP-TABLE tt-emitente NO-UNDO
    LIKE emitente
    FIELD r-rowid AS ROWID.

/** tt-bo-erro **/
{include/boini.i}

IF NOT VALID-HANDLE(hbO) THEN
    RUN adbo/boad098.p PERSISTENT SET hbo.

EMPTY TEMP-TABLE tt-emitente.

RUN findCGC IN hbo (INPUT 15015073000124, OUTPUT c-return).

//UPDATE cDados.

IF RETURN-VALUE = 'OK' THEN DO: //Emitente já cadastrado

END.
ELSE DO: //Cadastrar Emitente

    FIND FIRST es-api-param-cliente NO-LOCK NO-ERROR.

    IF NOT AVAIL es-api-param-cliente THEN DO:
        MESSAGE "Tabela de parâmetros não encontrada!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.

    RUN cdp/cd9960.p (OUTPUT i-emitente).

    CREATE tt-emitente.
    ASSIGN
        tt-emitente.cod-emitente                = i-emitente
        tt-emitente.Nome-abrev                  = 'teste123'
        tt-emitente.Cgc                         = '15015073000124'
        tt-emitente.Identific                   = 1 // 1-Cliente 2-Fornecedor 3-Ambos
        tt-emitente.Natureza                    = 1 //es-api-param-cliente.natureza //programa param
        tt-emitente.Nome-emit                   = 'teste teste'
        tt-emitente.Endereco                    = 'rua teste, 87'
        tt-emitente.Bairro                      = 'bairro teste'
        tt-emitente.Cidade                      = 'Jundiaí'
        tt-emitente.Estado                      = 'SP'
        tt-emitente.Pais                        = 'Brasil'
        tt-emitente.CEP                         = '13520000'
        tt-emitente.caixa-postal                = ''
        tt-emitente.End-cobranca                = i-emitente
        tt-emitente.endereco-cob                = 'rua teste, 87'
        tt-emitente.bairro-cob                  = 'bairro teste'
        tt-emitente.cidade-cob                  = 'Jundiaí'
        tt-emitente.estado-cob                  = 'SP' 
        tt-emitente.pais-cob                    = 'Brasil'
        tt-emitente.cep-cob                     = '13520000'
        tt-emitente.cx-post-cob                 = ''
        tt-emitente.Ins-estadual                = 'ISENTO'
        tt-emitente.Taxa-financ                 = 0
        tt-emitente.Cod-transp                  = es-api-param-cliente.cod-transp //programa param
        tt-emitente.Linha-produt                = ''
        tt-emitente.Atividade                   = ''
        tt-emitente.Contato                     = 'aaaaa'
        tt-emitente.Telefone                    = 'bbbbb'
        tt-emitente.Ramal                       = 'ccccc'
        tt-emitente.e-mail                      = 'teste@teste.com.br'
        tt-emitente.Data-implant                = TODAY
        tt-emitente.Cod-rep                     = 1
        tt-emitente.Categoria                   = ''
        tt-emitente.Bonificacao                 = 0
        tt-emitente.Istr                        = 1
        tt-emitente.Cod-gr-cli                  = es-api-param-cliente.cod-gr-cli //programa param
        tt-emitente.Lim-credito                 = 0
        tt-emitente.Perc-fat-ped                = es-api-param-cliente.perc-fat-ped //programa param
        tt-emitente.Portador                    = es-api-param-cliente.portador //programa param
        tt-emitente.Modalidade                  = es-api-param-cliente.modalidade //programa param
        tt-emitente.Ind-fat-par                 = es-api-param-cliente.ind-fat-par //programa param
        tt-emitente.Ind-cre-cli                 = 1
        tt-emitente.Ind-apr-cred                = YES
        tt-emitente.Nat-operacao                = es-api-param-cliente.nat-operacao //programa param
        tt-emitente.Nome-matriz                 = 'teste123'
        tt-emitente.Agencia                     = ''
        tt-emitente.per-max-canc                = 0
        tt-emitente.cod-cond-pag                = es-api-param-cliente.cod-cond-pag //programa param
        tt-emitente.emite-bloq                  = es-api-param-cliente.emite-bloq //programa param
        tt-emitente.port-prefer                 = es-api-param-cliente.port-prefer //programa param
        tt-emitente.mod-prefer                  = es-api-param-cliente.mod-prefer //programa param
        tt-emitente.nat-ope-ext                 = es-api-param-cliente.nat-ope-ext //programa param
        tt-emitente.esp-pd-venda                = es-api-param-cliente.esp-pd-venda //programa param
        tt-emitente.agente-retencao             = es-api-param-cliente.agente-retencao //programa param
        tt-emitente.log-calcula-pis-cofins-unid = es-api-param-cliente.log-calcula-pis-cofins-unid //programa param
        tt-emitente.log-optan-suspens-ipi       = es-api-param-cliente.log-optan-suspens-ipi //programa param
        tt-emitente.log-nf-eletro               = es-api-param-cliente.log-nf-eletro //programa param
        tt-emitente.cod-email-nfe               = es-api-param-cliente.cod-email-nfe //programa param
        tt-emitente.tp-rec-padrao               = es-api-param-cliente.tp-rec-padrao //programa param
        tt-emitente.ins-banc                    = es-api-param-cliente.ins-banc. //programa param

        RUN piValida.

        IF RETURN-VALUE = "NOK" THEN LEAVE.

        //Integracao EMS5                                                 
        RUN openQuery IN hbo (INPUT 1).                                   
        RUN validateCreate IN hbo (INPUT TABLE tt-emitente,                                                       
                                   OUTPUT TABLE tt-bo-erro,
                                   OUTPUT rw-emitente).                                                                                                                                                                                                       
                                                                                                                         
        IF NOT CAN-FIND(FIRST tt-bo-erro NO-LOCK) THEN DO:                
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
                                                                          
        END.                                                              
        ELSE DO:                                                          
            FOR EACH tt-bo-erro:                                          
                DISP tt-bo-erro WITH WIDTH 333 1 COL.                     
            END.                                                          
        END.                                               

END.


/******************************************************************/
/*************************** PROCEDURES ***************************/
/******************************************************************/

PROCEDURE piValida:

    FIND FIRST param-global NO-LOCK NO-ERROR.

    //Valida Cidade
    IF tt-emitente.cidade = "" THEN DO:
        MESSAGE "Cidade inválido!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK":U.
    END.

    FIND FIRST mgcad.cidade
        WHERE cidade.cidade = tt-emitente.cidade NO-LOCK NO-ERROR.

    IF NOT AVAIL cidade THEN DO:
        MESSAGE "Cidade não cadastrada!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK":U.
    END.

    FIND FIRST mgcad.cidade
        WHERE cidade.cidade = tt-emitente.cidade 
          AND cidade.estado = tt-emitente.estado
          AND cidade.pais   = tt-emitente.pais NO-LOCK NO-ERROR.

    IF NOT AVAIL cidade THEN DO:
        MESSAGE "Cidade não pertence a PAIS/ESTADO!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK":U.
    END.

    //Valida País
    FIND FIRST mgcad.PAIS 
       WHERE PAIS.NOME-PAIS = tt-emitente.PAIS NO-LOCK NO-ERROR.
    
    IF  NOT AVAIL PAIS THEN DO:
        MESSAGE "País não cadastrado!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK":U.
    END.

    //Valida UF
    FIND FIRST mgcad.UNID-FEDER 
        WHERE UNID-FEDER.PAIS   = tt-emitente.PAIS
        AND   UNID-FEDER.ESTADO = tt-emitente.ESTADO NO-LOCK NO-ERROR.

    IF NOT AVAIL UNID-FEDER THEN DO:
        MESSAGE "Unidade de Federação não cadastrada!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK":U.
    END.

    //Valida CEP
    IF tt-emitente.natureza < 3 THEN DO:

        IF LENGTH(REPLACE(tt-emitente.cep, '-', '')) <> 8 THEN DO:
            MESSAGE "CEP inválido!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN "NOK":U.
        END.

        IF LENGTH(REPLACE(tt-emitente.cep-cob, '-', '')) <> 8 THEN DO:
            MESSAGE "CEP inválido!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN "NOK":U.
        END.
                                                                                  
    END.

    //Valida Endereço
    IF tt-emitente.endereco = "" THEN DO:
        MESSAGE "Endereço inválido!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK":U.
    END. 

    IF NOT VALID-HANDLE(h-cdapi704) THEN
        RUN cdp/cdapi704.p PERSISTENT SET h-cdapi704.

    RUN pi-trata-endereco IN h-cdapi704 (INPUT tt-emitente.endereco, OUTPUT c-rua, OUTPUT c-nro, OUTPUT c-comp).

    IF c-nro = "" THEN DO:
        MESSAGE "Endereço inválido!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK":U.
    END.

    RUN pi-trata-endereco IN h-cdapi704 (INPUT tt-emitente.endereco-cob, OUTPUT c-rua, OUTPUT c-nro, OUTPUT c-comp).

    IF c-nro = "" THEN DO:
        MESSAGE "Endereço de Cobrança inválido!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK":U.
    END.

    IF VALID-HANDLE(h-cdapi704) THEN DO:
        DELETE PROCEDURE h-cdapi704.
        ASSIGN h-cdapi704 = ?.
    END.

    RETURN "OK":U.

END PROCEDURE.
