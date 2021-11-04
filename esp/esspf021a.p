/*----------------------------------------------------------------------------------------------/
 Programa..: esspf0021.p
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
{esp/esspf021.i}
{method/dbotterr.i}
{include/boini.i} /** tt-bo-erro **/
{utp/utapi019.i}

/* --------------------------------------------------------------------------------------------
    Global  Variable Definitions
----------------------------------------------------------------------------------------------*/



/* --------------------------------------------------------------------------------------------
    Local Variable Definitions
----------------------------------------------------------------------------------------------*/
DEFINE VARIABLE cLongJson       AS LONGCHAR              NO-UNDO.
DEFINE VARIABLE lRetJson        AS LOGICAL               NO-UNDO.
DEFINE VARIABLE iCountMain      AS INTEGER               NO-UNDO.
DEFINE VARIABLE iCountSec       AS INTEGER               NO-UNDO.
DEFINE VARIABLE h-boad098       AS HANDLE                NO-UNDO.
DEFINE VARIABLE h-cdapi704      AS HANDLE                NO-UNDO.
DEFINE VARIABLE h-cd1608        AS HANDLE                NO-UNDO.
DEFINE VARIABLE iErro           AS INTEGER               NO-UNDO.
DEFINE VARIABLE iCodEmitente    AS INTEGER               NO-UNDO.
DEFINE VARIABLE rw-emitente     AS ROWID                 NO-UNDO.
DEFINE VARIABLE c-cep           LIKE emitente.cep        NO-UNDO.
DEFINE VARIABLE c-rua           AS CHAR                  NO-UNDO.
DEFINE VARIABLE c-nro           AS CHAR                  NO-UNDO.
DEFINE VARIABLE c-comp          AS CHAR                  NO-UNDO.
DEFINE VARIABLE iGrupoCli       AS INTEGER               NO-UNDO.
DEFINE VARIABLE lUpdate         AS LOGICAL  INITIAL NO   NO-UNDO.


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


FUNCTION fnNomeAbrev RETURN CHARACTER (INPUT pNome AS CHAR):
    DEFINE VARIABLE iCount          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iniciais        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ultSobrenome    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE novoNome        AS CHARACTER NO-UNDO.

    ASSIGN novoNome = REPLACE(REPLACE(REPLACE(REPLACE(pNome, ' dos ', ' '), ' da ', ' '), ' de ', ' '), ' do ', ' ').

    //Concatena iniciais dos nomes/sobrenomes (sem contar o £ltimo sobrenome)
    DO iCount = 1  TO NUM-ENTRIES(novoNome, ' ') - 1:
        ASSIGN iniciais = iniciais + SUBSTRING(ENTRY(iCount, novoNome, ' '), 1, 1).
    END.

    //Pega £ltimo sobrenome
    ASSIGN ultSobrenome = ENTRY(NUM-ENTRIES(novoNome, ' '), novoNome, ' ').

    RETURN iniciais + ' ' + ultSobrenome.
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
    RUN piErro("ParÉmetro do cliente n∆o cadastrado no ESSPF016").
    RUN pi-delete-object.
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
    ASSIGN lUpdate = YES.
    RUN piValidaEmitente.
    IF RETURN-VALUE = "NOK" THEN 
    DO:

       PUT "OCORREU ERRO NA VALIDAÄ«O DO CLIENTE" SKIP.
      //RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "cleberson.silva@4make.com.br", "Erro").  
      RETURN "NOK":U.
   END.
   ELSE
   DO:
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
    
          IF tt-emitente.pais = 'Brazil' THEN
              ASSIGN tt-emitente.pais = 'Brasil'.

          IF tt-emitente.identific = 2 THEN
              ASSIGN tt-emitente.Nat-operacao  = es-api-param-cliente-spf.nat-operacao 
                     tt-emitente.identific     = 3. //Ambos
    
          RUN openQuery      IN h-boad098 (INPUT 1).
          RUN validateUpdate IN h-boad098 (INPUT TABLE tt-emitente,
                                           INPUT tt-emitente.r-rowid,
                                           OUTPUT TABLE tt-bo-erro).
    
          IF CAN-FIND(FIRST tt-bo-erro NO-LOCK) THEN     
          DO:                                            
             FOR EACH tt-bo-erro NO-LOCK:         
                 RUN piErro(tt-bo-erro.mensagem).    
             END.  
             //RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "cleberson.silva@4make.com.br", "Erro"). 
             RETURN "NOK":U.
          END.
          ELSE                                           
          DO:
             RUN pi-AtualizaEMS5.
             IF RETURN-VALUE = "NOK" THEN 
             DO:
                 PUT UNFORMATTED "OCORREU ERRO NA GERAÄ«O DO EMITENTE NO EMS5" SKIP.
                //RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "cleberson.silva@4make.com.br", "Erro").  

                 RETURN "NOK":U.
             END.
             //RETURN "OK":U.
          END.            
       END.
       RETURN "OK":U.
   END.
    
   //RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "cleberson.silva@4make.com.br", "Sucesso").  
   //RETURN "OK":U. 
   

END PROCEDURE.


PROCEDURE piCriaCliente:

   RUN piValidaEmitente.
   IF RETURN-VALUE = "NOK" THEN 
   DO:

       PUT "OCORREU ERRO NA VALIDAÄ«O DO CLIENTE" SKIP.
      //RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "cleberson.silva@4make.com.br", "Erro").  
      RETURN "NOK":U.
   END.


   PUT "INICIO DA ROTINA DE CRIACAO DE CLIENTES" SKIP.

   criaEmitente:
   DO TRANS 
       ON ENDKEY UNDO criaEmitente, RETURN "NOK"
       ON ERROR  UNDO criaEmitente, RETURN "NOK"
       ON STOP   UNDO criaEmitente, RETURN "NOK":

      /*-- recupera o c¢digo do cliente --*/
      RUN cdp/cd9960.p (OUTPUT iCodEmitente).

      PUT UNFORMATTED "NOVO EMITENTE " + string(iCodEmitente) SKIP.

      if (fnNatureza(ttCustomer.cnpj) = 1) THEN
         ASSIGN iGrupoCli = es-api-param-cliente-spf.cod-gr-cli-fisica.
      else
      ASSIGN iGrupoCli = es-api-param-cliente-spf.cod-gr-cli.


      CREATE tt-emitente.
      ASSIGN tt-emitente.cod-emitente                = iCodEmitente
             tt-emitente.nome-abrev                  = fnNomeAbrev(ttCustomer.RazaoSocial) //SUBSTRING(ttCustomer.cnpj,1,12)
             tt-emitente.Nome-matriz                 = fnNomeAbrev(ttCustomer.RazaoSocial)
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
             tt-emitente.Cod-gr-cli                  = iGrupoCli  
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
             tt-emitente.ins-banc                    = es-api-param-cliente-spf.ins-banc
             tt-emitente.cod-canal-venda             = es-api-param-cliente-spf.cod-canal-venda.

          IF tt-emitente.pais = 'Brazil' THEN
            tt-emitente.pais = 'Brasil'.


      RUN openQuery      IN h-boad098 (INPUT 1).                                   
      RUN validateCreate IN h-boad098 (INPUT TABLE tt-emitente,                                                       
                                      OUTPUT TABLE tt-bo-erro,
                                      OUTPUT rw-emitente).


      IF CAN-FIND(FIRST tt-bo-erro NO-LOCK) THEN 
      DO:
          PUT UNFORMATTED "OCORREU ERRO NA GERAÄ«O DO EMITENTE" SKIP.
          PUT UNFORMATTED tt-bo-erro.mensagem SKIP.
          FOR EACH tt-bo-erro NO-LOCK:
              RUN piErro(tt-bo-erro.mensagem).
          END.
          //RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "cleberson.silva@4make.com.br", "Erro").  
          UNDO criaEmitente, RETURN "NOK":U.         
      END.
      ELSE 
      DO:
         RUN pi-AtualizaEMS5.
         IF RETURN-VALUE = "NOK" THEN 
         DO:
             PUT UNFORMATTED "OCORREU ERRO NA GERAÄ«O DO EMITENTE NO EMS5" SKIP.
            //RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "cleberson.silva@4make.com.br", "Erro").  
            //UNDO criaEmitente, RETURN "NOK":U.  
         END.
      END.     
   END.

   PUT UNFORMATTED "EMITENTE " + STRING(iCodEmitente) + " Criado com sucesso" SKIP.
   //RUN piEnviaNotificacaoUsuario("suporte@macmillan.com.br", "cleberson.silva@4make.com.br", "Sucesso").  
   RETURN "OK":U. 


END PROCEDURE.


PROCEDURE pi-AtualizaEMS5:

    LOG-MANAGER:WRITE-MESSAGE("### ROTINA PARA ATUALIZAR REGISTRO NO EMS5") NO-ERROR.
                   
   /************* Integracao 2.00 X 5.00 *****************/      
   IF CAN-FIND(funcao WHERE funcao.cd-funcao = "adm-cdc-ems-5.00"
       AND funcao.ativo = YES                                    
       AND funcao.log-1 = YES) THEN DO:                          
       FIND FIRST param-global NO-LOCK NO-ERROR.                 
       IF  param-global.log-2 = YES THEN DO:                     
           VALIDATE emitente NO-ERROR.     
           LOG-MANAGER:WRITE-MESSAGE("CHAMANDO CD1608") NO-ERROR.
           RUN cdp/cd1608.p persistent set h-cd1608  
                            (INPUT tt-emitente.cod-emitente,     
                             INPUT tt-emitente.cod-emitente,     
                             INPUT tt-emitente.identific,        
                             INPUT YES,                          
                             INPUT 1,                            
                             INPUT 0,                            
                             INPUT "",               
                             INPUT "Arquivo":U,                 
                             INPUT "").   
            RUN pi-erros IN h-cd1608 (OUTPUT TABLE tt_erros_conexao). 
            DELETE PROCEDURE h-cd1608.
            
            IF CAN-FIND(FIRST tt_erros_conexao) THEN
            DO:
                LOG-MANAGER:WRITE-MESSAGE("OCORRERAM ERROS AO INTEGRAR REGISTRO COM EMS5") NO-ERROR.
                FOR EACH tt_erros_conexao NO-LOCK:
                    PUT UNFORMATTED tt_erros_conexao.mensagem SKIP.
                    RUN piErro(tt_erros_conexao.mensagem).                
                END.  
                RETURN "NOK".
            END.
            ELSE
            DO:
                /*-- cria cd0705 --*/
                RUN pi-loc-entr.

                LOG-MANAGER:WRITE-MESSAGE("NENHUM ERRO ENCONTRADO NA ROTINA DE CREATE/UPDATE DE CLIENTES") NO-ERROR.

                IF NOT CAN-FIND(FIRST spf-emitente WHERE spf-emitente.cod-emitente = tt-emitente.cod-emitente) THEN
                DO: 
                    PUT UNFORMATTED "GERANDO TABELA AUXILIAR SPF-EMITENTE" SKIP.
                    CREATE spf-emitente.                                                                
                    ASSIGN spf-emitente.cod-emitente = tt-emitente.cod-emitente                            
                           spf-emitente.shopify-id   = ttCustomer.shopifyId.                            
                END.                                                                                    
            END.
       END.                                                      
   END.

   RETURN "OK":U.
   /*********** Fim Integracao 2.00 X 5.00 ****************/ 

END PROCEDURE.



PROCEDURE piErro:
    DEFINE INPUT PARAM cErrorDescription   AS CHARACTER NO-UNDO.
    

    CREATE RowErrors.
    ASSIGN iErro            = iErro + 1
           ErrorSequence    = iErro
           ErrorNumber      = 17006
           ErrorType        = "Error"
           ErrorDescription = cErrorDescription
           ErrorHelp        = cErrorDescription.

END PROCEDURE.


PROCEDURE piValidaEmitente:

   FIND FIRST param-global NO-LOCK NO-ERROR.

   IF ttCustomer.ShopifyID = "" THEN
   DO:
      RUN piErro("ERRO: C¢digo do Shopify n∆o informado.").
      RETURN "NOK":U.

   END.

   IF ttCustomer.ShopifyID <> "" AND CAN-FIND(FIRST spf-emitente WHERE spf-emitente.shopify-id = ttCustomer.ShopifyID) AND NOT lUpdate THEN
   DO: 
      RUN piErro(SUBSTITUTE("ERRO: C¢digo do Shopify &1 j† registrado para outro emitente",ttCustomer.ShopifyID)).
      RETURN "NOK":U.

   END.

   IF ttCustomer.RazaoSocial = "" THEN
   DO:
      RUN piErro("ERRO: Razao social n∆o informada.").
      RETURN "NOK":U.
   END.

   IF ttCustomer.endereco = "" THEN
   DO:
      RUN piErro("ERRO: Endereáo informado inv†lido.").
      RETURN "NOK":U.
   END.

   IF ttCustomer.cidade = "" THEN
   DO:
      RUN piErro("ERRO: Cidade n∆o informada.").
      RETURN "NOK":U.
   END.

   FIND FIRST mguni.cidade NO-LOCK 
        WHERE mguni.cidade.cidade = ttCustomer.cidade 
          AND mguni.cidade.estado = ttCustomer.estado
          AND mguni.cidade.pais   = ttCustomer.pais NO-ERROR.
   IF NOT AVAIL mguni.cidade THEN
   DO:
      RUN piErro(SUBSTITUTE("ERRO: Cidade &1 informada n∆o perntece ao Pais &2/Estado &3", 
                            ttCustomer.cidade, ttCustomer.pais, ttCustomer.estado)).
      RETURN "NOK":U.
   END.

   FIND FIRST mgcad.pais WHERE pais.nome-pais = ttCustomer.pais NO-LOCK NO-ERROR.
   IF NOT AVAIL mgcad.pais THEN
   DO:
      RUN piErro(SUBSTITUTE("ERRO: Pais &1 informado n∆o cadastrado.",ttCustomer.pais)).
      RETURN "NOK":U.
   END.

   FIND FIRST mgcad.unid-feder NO-LOCK
        WHERE unid-feder.pais = ttCustomer.pais 
          AND unid-feder.estado = ttCustomer.estado
   NO-ERROR.
   IF NOT AVAIL mgcad.unid-feder THEN
   DO:
      RUN piErro("ERRO: Unidade de federaá∆o n∆o cadastrada.").
      RETURN "NOK":U.
   END.

   IF LENGTH(REPLACE(ttCustomer.cep, '-', '')) <> 8 THEN DO:
      RUN piErro("ERRO: CEP inv†lido.").
      RETURN "NOK":U.
  END.

  IF NOT VALID-HANDLE(h-cdapi704) THEN
      RUN cdp/cdapi704.p PERSISTENT SET h-cdapi704.

   RUN pi-trata-endereco IN h-cdapi704 (INPUT ttCustomer.endereco, OUTPUT c-rua, OUTPUT c-nro, OUTPUT c-comp).
   IF c-nro = "" THEN
   DO:
      RUN piErro("ERRO: Endereáo inv†lido.").
      RETURN "NOK":U.
   END.

   IF VALID-HANDLE(h-cdapi704) THEN DO:
      DELETE PROCEDURE h-cdapi704.
      ASSIGN h-cdapi704 = ?.
  END.
END PROCEDURE.

PROCEDURE piEnviaNotificacaoUsuario:
   DEFINE INPUT PARAM pRemetente  AS CHAR NO-UNDO.
   DEFINE INPUT PARAM pDestino    AS CHAR NO-UNDO.
   DEFINE INPUT PARAM pAcaoEmail  AS CHAR NO-UNDO.
   
   

   DEFINE VARIABLE cMensagem   AS CHAR NO-UNDO.
   DEFINE VARIABLE cAssunto    AS CHAR NO-UNDO.
   DEFINE VARIABLE cDestino    AS CHAR NO-UNDO.
   DEFINE VARIABLE cRemetente  AS CHAR NO-UNDO.
   
   /** rotina comentada, pois em nenhum dos servidores 192.168.0.131 e 134, est† funcionando corretamente.
   portanto esta rotina esta sendo suprimida, para garatir os testes dos demais requisitos

   run utp/utapi019.p persistent set h-utapi019.           
   for each tt-envio2:                                     
       delete tt-envio2.                                   
   END.                                                    
   for each tt-mensagem:                                   
       delete tt-mensagem.                                 
   END.

   
   ASSIGN cAssunto  = "Integraá∆o de Clientes SHOPIFY"
          cDestino  = pDestino.

   IF pAcaoEmail = "SUCESSO" THEN
   DO:
      ASSIGN cMensagem = SUBSTITUTE("Prezado(s), Informo que foi registrado no CD0704, o cliente &1 - CPF/CNPJ &1",
                         ttCustomer.RazaoSocial, ttCustomer.cnpj).            

   END.
   ELSE
   DO:
      ASSIGN cMensagem = SUBSTITUTE("Prezado(s), Informo que foram gerados, erros ao tentar integrar o cliente &1 - CPF/CNPJ &1, verifique o monitor de integraá∆o.", ttCustomer.RazaoSocial, ttCustomer.cnpj).
   END.

   CREATE tt-envio2.                                       
   ASSIGN tt-envio2.versao-integracao = 1                  
            tt-envio2.destino           = cDestino     
            tt-envio2.remetente         = cRemetente         
            tt-envio2.assunto           = cAssunto           
            tt-envio2.mensagem          = cMensagem             
            tt-envio2.importancia       = 2                  
            tt-envio2.log-enviada       = no                 
            tt-envio2.log-lida          = no                 
            tt-envio2.acomp             = no                 
            tt-envio2.formato           = "HTML"             
            tt-envio2.arq-anexo         = "".            
    
                                                            
   CREATE tt-mensagem.                                     
   ASSIGN tt-mensagem.seq-mensagem = 1                     
            tt-mensagem.mensagem = cMensagem.                   
                                                            
   run pi-execute2 in h-utapi019 (input  table tt-envio2  ,
                                  input  table tt-mensagem, 
                                  output table tt-erros).
   IF RETURN-VALUE <> "OK" THEN
   DO:
      DELETE PROCEDURE h-utapi019.

      RUN piErro("Houver erro ao enviar notificaá‰es").
      FOR EACH tt-erros:
         RUN piErro(SUBSTITUTE("&1 - &2", tt-erros.desc-erro)).      
      END.
      RETURN "NOK".
   END.

   DELETE PROCEDURE h-utapi019.
   */
                                                         
   RETURN "OK".
END.

PROCEDURE pi-loc-entr:
    DEFINE VARIABLE c-cod-entrega AS CHARACTER   NO-UNDO.

    FIND FIRST emitente WHERE emitente.cod-emitente = tt-emitente.cod-emitente EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL emitente THEN
    DO:
        {utp/ut-liter.i Padr∆o} 
    
        assign c-cod-entrega        = trim(return-value)
               emitente.cod-entrega = c-cod-entrega.

        find first loc-entr use-index ch-entrega
             where loc-entr.nome-abrev  = emitente.nome-abrev
             and   loc-entr.cod-entrega = c-cod-entrega exclusive-lock no-error.

        IF NOT AVAIL loc-entr THEN
        DO:
            CREATE loc-entr.
            ASSIGN loc-entr.nome-abrev = emitente.nome-abrev
                   loc-entr.cod-entrega = c-cod-entrega.
        END.

        ASSIGN loc-entr.endereco     = emitente.endereco
               loc-entr.bairro       = emitente.bairro
               loc-entr.cidade       = emitente.cidade
               loc-entr.estado       = emitente.estado
               loc-entr.cep          = emitente.cep
               loc-entr.caixa-postal = emitente.caixa-postal
               loc-entr.pais         = emitente.pais
               loc-entr.ins-estadual = emitente.Ins-estadual
               loc-entr.zip-code     = emitente.zip-code
               loc-entr.cgc          = emitente.cgc.


    END.
    FIND CURRENT emitente NO-LOCK NO-ERROR.

END PROCEDURE.


PROCEDURE pi-delete-object:

    IF VALID-HANDLE(h-utapi019) THEN
        DELETE PROCEDURE h-utapi019.

    IF VALID-HANDLE(h-boad098) THEN
        DELETE PROCEDURE h-boad098.

    IF VALID-HANDLE(h-cdapi704) THEN DO:
      DELETE PROCEDURE h-cdapi704.
      ASSIGN h-cdapi704 = ?.
    END.

END PROCEDURE.
