&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEF VAR v_des_cta AS CHAR.
DEF VAR v_ind_finalid_cta AS CHAR.
def var h_api_cta          as handle no-undo.
def var v_cod_cta          as char   no-undo.
DEF VAR c-cod-empresa     AS CHAR NO-UNDO.

/* Local Temp-Tables Definitions ---                                    */
def temp-table tt_log_erro no-undo
    field ttv_num_cod_erro  as integer format ">>>>,>>9" label "Nœmero" column-label "Nœmero"
    field ttv_des_msg_ajuda as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro  as character format "x(60)" label "Mensagem Erro" column-label "Inconsist¼ncia".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-api-param-acr-spf
&Scoped-define FIRST-EXTERNAL-TABLE es-api-param-acr-spf


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-api-param-acr-spf.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-api-param-acr-spf.cod_portador ~
es-api-param-acr-spf.cod_cart_bcia es-api-param-acr-spf.cod_ccusto ~
es-api-param-acr-spf.cod_ser_docto es-api-param-acr-spf.cod_espec_docto ~
es-api-param-acr-spf.cod_plano_cta_ctbl es-api-param-acr-spf.cod_unid_neg ~
es-api-param-acr-spf.cod_plano_ccusto ~
es-api-param-acr-spf.cod_tip_fluxo_financ es-api-param-acr-spf.seq_refer ~
es-api-param-acr-spf.cod_matriz_trad_org_ext ~
es-api-param-acr-spf.cta_trans_fatur ~
es-api-param-acr-spf.log_assume_dat_emis ~
es-api-param-acr-spf.log_atualiza_refer_acr 
&Scoped-define ENABLED-TABLES es-api-param-acr-spf
&Scoped-define FIRST-ENABLED-TABLE es-api-param-acr-spf
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es-api-param-acr-spf.cod_empresa ~
es-api-param-acr-spf.cod_estab es-api-param-acr-spf.cod_portador ~
es-api-param-acr-spf.cod_cart_bcia es-api-param-acr-spf.cod_ccusto ~
es-api-param-acr-spf.cod_ser_docto es-api-param-acr-spf.cod_espec_docto ~
es-api-param-acr-spf.cod_plano_cta_ctbl es-api-param-acr-spf.cod_unid_neg ~
es-api-param-acr-spf.cod_plano_ccusto ~
es-api-param-acr-spf.cod_tip_fluxo_financ es-api-param-acr-spf.seq_refer ~
es-api-param-acr-spf.cod_matriz_trad_org_ext ~
es-api-param-acr-spf.cta_trans_fatur ~
es-api-param-acr-spf.log_assume_dat_emis ~
es-api-param-acr-spf.log_atualiza_refer_acr 
&Scoped-define DISPLAYED-TABLES es-api-param-acr-spf
&Scoped-define FIRST-DISPLAYED-TABLE es-api-param-acr-spf
&Scoped-Define DISPLAYED-OBJECTS desc-empresa desc-especie desc-unid-negoc ~
desc-estab desc-portador desc-plan-contas desc-carteira desc-plan-ccusto ~
desc-ccusto desc-flux-financ desc-mat-trad-org-ext desc-cta-transit ~
desc-serie 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-api-param-acr-spf.cod_empresa ~
es-api-param-acr-spf.cod_estab 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE desc-carteira AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-ccusto AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-cta-transit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-especie AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-flux-financ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-mat-trad-org-ext AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-plan-ccusto AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-plan-contas AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-portador AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-serie AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE desc-unid-negoc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 2.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 14.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-api-param-acr-spf.cod_empresa AT ROW 1.17 COL 22 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-api-param-acr-spf.cod_estab AT ROW 2.17 COL 22 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     es-api-param-acr-spf.cod_portador AT ROW 3.67 COL 22 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     es-api-param-acr-spf.cod_cart_bcia AT ROW 4.67 COL 22 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-api-param-acr-spf.cod_ccusto AT ROW 5.67 COL 22 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     es-api-param-acr-spf.cod_ser_docto AT ROW 6.67 COL 22 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-api-param-acr-spf.cod_espec_docto AT ROW 7.67 COL 22 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-api-param-acr-spf.cod_plano_cta_ctbl AT ROW 8.67 COL 22 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     es-api-param-acr-spf.cod_unid_neg AT ROW 9.67 COL 22 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-api-param-acr-spf.cod_plano_ccusto AT ROW 10.67 COL 22 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     es-api-param-acr-spf.cod_tip_fluxo_financ AT ROW 11.67 COL 22 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     es-api-param-acr-spf.seq_refer AT ROW 12.67 COL 22 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     es-api-param-acr-spf.cod_matriz_trad_org_ext AT ROW 13.67 COL 39 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     es-api-param-acr-spf.cta_trans_fatur AT ROW 14.67 COL 30 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .88
     desc-empresa AT ROW 1.17 COL 29.57 COLON-ALIGNED NO-LABEL WIDGET-ID 34 NO-TAB-STOP 
     desc-especie AT ROW 7.67 COL 29.57 COLON-ALIGNED NO-LABEL WIDGET-ID 46 NO-TAB-STOP 
     es-api-param-acr-spf.log_assume_dat_emis AT ROW 15.67 COL 23.72 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es-api-param-acr-spf.log_atualiza_refer_acr AT ROW 16.67 COL 28.43 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     desc-unid-negoc AT ROW 9.67 COL 29.57 COLON-ALIGNED NO-LABEL WIDGET-ID 50 NO-TAB-STOP 
     desc-estab AT ROW 2.17 COL 31.57 COLON-ALIGNED NO-LABEL WIDGET-ID 36 NO-TAB-STOP 
     desc-portador AT ROW 3.67 COL 31.57 COLON-ALIGNED NO-LABEL WIDGET-ID 38 NO-TAB-STOP 
     desc-plan-contas AT ROW 8.67 COL 34.57 COLON-ALIGNED NO-LABEL WIDGET-ID 48 NO-TAB-STOP 
     desc-carteira AT ROW 4.67 COL 29.57 COLON-ALIGNED NO-LABEL WIDGET-ID 40 NO-TAB-STOP 
     desc-plan-ccusto AT ROW 10.67 COL 34.72 COLON-ALIGNED NO-LABEL WIDGET-ID 54 NO-TAB-STOP 
     desc-ccusto AT ROW 5.67 COL 31.72 COLON-ALIGNED NO-LABEL WIDGET-ID 42 NO-TAB-STOP 
     desc-flux-financ AT ROW 11.67 COL 34.72 COLON-ALIGNED NO-LABEL WIDGET-ID 56 NO-TAB-STOP 
     desc-mat-trad-org-ext AT ROW 13.67 COL 51.57 COLON-ALIGNED NO-LABEL WIDGET-ID 52 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.72 BY 17.5 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     desc-cta-transit AT ROW 14.67 COL 51.57 COLON-ALIGNED NO-LABEL WIDGET-ID 58 NO-TAB-STOP 
     desc-serie AT ROW 6.67 COL 29.57 COLON-ALIGNED NO-LABEL WIDGET-ID 60 NO-TAB-STOP 
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.72 BY 17.5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgesp.es-api-param-acr-spf
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.5
         WIDTH              = 101.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN es-api-param-acr-spf.cod_empresa IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es-api-param-acr-spf.cod_estab IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN desc-carteira IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-ccusto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-cta-transit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-empresa IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-especie IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-flux-financ IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-mat-trad-org-ext IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-plan-ccusto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-plan-contas IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-portador IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-serie IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN desc-unid-negoc IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME es-api-param-acr-spf.cod_cart_bcia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_cart_bcia V-table-Win
ON F5 OF es-api-param-acr-spf.cod_cart_bcia IN FRAME f-main /* Carteira */
DO:
  
    {include/zoomvar.i  &prog-zoom=esp/esspf019z12.w
                        &campo=es-api-param-acr-spf.cod_cart_bcia
                        &campozoom=cod_cart_bcia}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_cart_bcia V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_cart_bcia IN FRAME f-main /* Carteira */
DO:
  
    FIND FIRST emscad.cart_bcia 
        WHERE cart_bcia.cod_cart_bcia = input frame {&frame-name} es-api-param-acr-spf.cod_cart_bcia NO-LOCK NO-ERROR.

    IF AVAIL cart_bcia THEN
        DISP cart_bcia.des_cart_bcia @ desc-carteira WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-carteira WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_cart_bcia V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_cart_bcia IN FRAME f-main /* Carteira */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_ccusto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_ccusto V-table-Win
ON F5 OF es-api-param-acr-spf.cod_ccusto IN FRAME f-main /* Centro de custo */
DO:
  
    {include/zoomvar.i  &prog-zoom=esp/esspf019z06.w
                        &campo=es-api-param-acr-spf.cod_ccusto
                        &campozoom=cod_ccusto}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_ccusto V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_ccusto IN FRAME f-main /* Centro de custo */
DO:
  
    FIND FIRST emscad.ccusto 
        WHERE ccusto.cod_ccusto = input frame {&frame-name} es-api-param-acr-spf.cod_ccusto NO-LOCK NO-ERROR.

    IF AVAIL ccusto THEN
        DISP ccusto.des_tit_ctbl @ desc-ccusto WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-ccusto WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_ccusto V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_ccusto IN FRAME f-main /* Centro de custo */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_empresa V-table-Win
ON F5 OF es-api-param-acr-spf.cod_empresa IN FRAME f-main /* Empresa */
DO:

    {include/zoomvar.i  &prog-zoom=esp/esspf019z02.w
                        &campo=es-api-param-acr-spf.cod_empresa
                        &campozoom=cod_empresa}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_empresa V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_empresa IN FRAME f-main /* Empresa */
DO:

    FIND FIRST emscad.empresa WHERE empresa.cod_empresa = input frame {&frame-name} es-api-param-acr-spf.cod_empresa NO-LOCK NO-ERROR.

    IF AVAIL empresa THEN
        DISP empresa.nom_abrev @ desc-empresa WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-empresa WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_empresa IN FRAME f-main /* Empresa */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_espec_docto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_espec_docto V-table-Win
ON F5 OF es-api-param-acr-spf.cod_espec_docto IN FRAME f-main /* Esp‚cie Docto ACR */
DO:
  
        {include/zoomvar.i  &prog-zoom=esp/esspf019z09.w
                            &campo=es-api-param-acr-spf.cod_espec_docto
                            &campozoom=cod_espec_docto}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_espec_docto V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_espec_docto IN FRAME f-main /* Esp‚cie Docto ACR */
DO:
  
    FIND FIRST emscad.espec_docto
        WHERE espec_docto.cod_espec_docto = input frame {&frame-name} es-api-param-acr-spf.cod_espec_docto NO-LOCK NO-ERROR.

    IF AVAIL espec_docto THEN
        DISP espec_docto.des_espec_docto @ desc-especie WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-especie WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_espec_docto V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_espec_docto IN FRAME f-main /* Esp‚cie Docto ACR */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_estab V-table-Win
ON F5 OF es-api-param-acr-spf.cod_estab IN FRAME f-main /* Estabelecimento */
DO:
  
    {include/zoomvar.i  &prog-zoom=esp/esspf019z03.w
                        &campo=es-api-param-acr-spf.cod_estab
                        &campozoom=cod_estab}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_estab V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_estab IN FRAME f-main /* Estabelecimento */
DO:
  
    FIND FIRST emscad.estabelecimento WHERE estabelecimento.cod_estab = input frame {&frame-name} es-api-param-acr-spf.cod_estab NO-LOCK NO-ERROR.

    IF AVAIL estabelecimento THEN
        DISP estabelecimento.nom_abrev @ desc-estab WITH FRAME {&FRAME-NAME}.
    ELSE 
        DISP '' @ desc-estab WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_estab V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_estab IN FRAME f-main /* Estabelecimento */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_matriz_trad_org_ext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_matriz_trad_org_ext V-table-Win
ON F5 OF es-api-param-acr-spf.cod_matriz_trad_org_ext IN FRAME f-main /* Matriz Tradu‡Æo Organizacional Externa */
DO:
  
    {include/zoomvar.i  &prog-zoom=esp/esspf019z10.w
                        &campo=es-api-param-acr-spf.cod_matriz_trad_org_ext
                        &campozoom=cod_matriz_trad_org_ext}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_matriz_trad_org_ext V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_matriz_trad_org_ext IN FRAME f-main /* Matriz Tradu‡Æo Organizacional Externa */
DO:
  
    FIND FIRST emscad.matriz_trad_org_ext 
        WHERE matriz_trad_org_ext.cod_matriz_trad_org_ext = input frame {&frame-name} es-api-param-acr-spf.cod_matriz_trad_org_ext NO-LOCK NO-ERROR.

    IF AVAIL matriz_trad_org_ext THEN
        DISP matriz_trad_org_ext.des_matriz_trad_org_ext @ desc-mat-trad-org-ext WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-mat-trad-org-ext WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_matriz_trad_org_ext V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_matriz_trad_org_ext IN FRAME f-main /* Matriz Tradu‡Æo Organizacional Externa */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_plano_ccusto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_plano_ccusto V-table-Win
ON F5 OF es-api-param-acr-spf.cod_plano_ccusto IN FRAME f-main /* Plano Centro de custo */
DO:
  
        {include/zoomvar.i  &prog-zoom=esp/esspf019z08.w
                            &campo=es-api-param-acr-spf.cod_plano_ccusto
                            &campozoom=cod_plano_ccusto}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_plano_ccusto V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_plano_ccusto IN FRAME f-main /* Plano Centro de custo */
DO:
  
    FIND FIRST emscad.plano_ccusto 
        WHERE plano_ccusto.cod_plano_ccusto = input frame {&frame-name} es-api-param-acr-spf.cod_plano_ccusto NO-LOCK NO-ERROR.

    IF AVAIL plano_ccusto THEN
        DISP plano_ccusto.des_tit_ctbl @ desc-plan-ccusto WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-plan-ccusto WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_plano_ccusto V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_plano_ccusto IN FRAME f-main /* Plano Centro de custo */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_plano_cta_ctbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_plano_cta_ctbl V-table-Win
ON F5 OF es-api-param-acr-spf.cod_plano_cta_ctbl IN FRAME f-main /* Plano de Contas */
DO:
  
        {include/zoomvar.i  &prog-zoom=esp/esspf019z07.w
                            &campo=es-api-param-acr-spf.cod_plano_cta_ctbl
                            &campozoom=cod_plano_cta_ctbl}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_plano_cta_ctbl V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_plano_cta_ctbl IN FRAME f-main /* Plano de Contas */
DO:
  
    FIND FIRST emscad.plano_cta_ctbl 
        WHERE plano_cta_ctbl.cod_plano_cta_ctbl = input frame {&frame-name} es-api-param-acr-spf.cod_plano_cta_ctbl NO-LOCK NO-ERROR.

    IF AVAIL plano_cta_ctbl THEN
        DISP plano_cta_ctbl.des_tit_ctbl @ desc-plan-contas WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-plan-contas WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_plano_cta_ctbl V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_plano_cta_ctbl IN FRAME f-main /* Plano de Contas */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_portador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_portador V-table-Win
ON F5 OF es-api-param-acr-spf.cod_portador IN FRAME f-main /* Portador */
DO:
  
    {include/zoomvar.i  &prog-zoom=esp/esspf019z05.w
                        &campo=es-api-param-acr-spf.cod_portador
                        &campozoom=cod_portador}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_portador V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_portador IN FRAME f-main /* Portador */
DO:
  
    FIND FIRST emscad.portador 
        WHERE portador.cod_portador = input frame {&frame-name} es-api-param-acr-spf.cod_portador NO-LOCK NO-ERROR.

    IF AVAIL portador THEN
        DISP portador.nom_abrev @ desc-portador WITH FRAME {&FRAME-NAME}.
    ELSE 
        DISP '' @ desc-portador WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_portador V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_portador IN FRAME f-main /* Portador */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_ser_docto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_ser_docto V-table-Win
ON F5 OF es-api-param-acr-spf.cod_ser_docto IN FRAME f-main /* S‚rie */
DO:
  
    {include/zoomvar.i  &prog-zoom=esp/esspf019z13.w
                        &campo=es-api-param-acr-spf.cod_ser_docto
                        &campozoom=cod_ser_fisc_nota}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_ser_docto V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_ser_docto IN FRAME f-main /* S‚rie */
DO:
  
    FIND FIRST emscad.ser_fisc_nota 
        WHERE ser_fisc_nota.cod_ser_fisc_nota = input frame {&frame-name} es-api-param-acr-spf.cod_ser_docto NO-LOCK NO-ERROR.

    IF AVAIL ser_fisc_nota THEN
        DISP ser_fisc_nota.des_ser_fisc_nota @ desc-serie WITH FRAME {&FRAME-NAME}.
    ELSE 
        DISP '' @ desc-portador WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_ser_docto V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_ser_docto IN FRAME f-main /* S‚rie */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_tip_fluxo_financ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_tip_fluxo_financ V-table-Win
ON F5 OF es-api-param-acr-spf.cod_tip_fluxo_financ IN FRAME f-main /* Fluxo Financeiro */
DO:
  
    {include/zoomvar.i  &prog-zoom=esp/esspf019z11.w
                        &campo=es-api-param-acr-spf.cod_tip_fluxo_financ
                        &campozoom=cod_tip_fluxo_financ}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_tip_fluxo_financ V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_tip_fluxo_financ IN FRAME f-main /* Fluxo Financeiro */
DO:
  
    FIND FIRST emscad.tip_fluxo_financ 
        WHERE tip_fluxo_financ.cod_tip_fluxo_financ = input frame {&frame-name} es-api-param-acr-spf.cod_tip_fluxo_financ NO-LOCK NO-ERROR.

    IF AVAIL tip_fluxo_financ THEN
        DISP tip_fluxo_financ.des_tip_fluxo_financ @ desc-flux-financ WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-flux-financ WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_tip_fluxo_financ V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_tip_fluxo_financ IN FRAME f-main /* Fluxo Financeiro */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cod_unid_neg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_unid_neg V-table-Win
ON F5 OF es-api-param-acr-spf.cod_unid_neg IN FRAME f-main /* Unidade Neg¢cio */
DO:
  
    {include/zoomvar.i  &prog-zoom=esp/esspf019z04.w
                        &campo=es-api-param-acr-spf.cod_unid_neg
                        &campozoom=cod_unid_negoc}                      

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_unid_neg V-table-Win
ON LEAVE OF es-api-param-acr-spf.cod_unid_neg IN FRAME f-main /* Unidade Neg¢cio */
DO:
  
    FIND FIRST emscad.unid_negoc WHERE unid_negoc.cod_unid_negoc = input frame {&frame-name} es-api-param-acr-spf.cod_unid_neg NO-LOCK NO-ERROR.

    IF AVAIL unid_negoc THEN
        DISP unid_negoc.des_unid_negoc @ desc-unid-negoc WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-unid-negoc WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cod_unid_neg V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cod_unid_neg IN FRAME f-main /* Unidade Neg¢cio */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-api-param-acr-spf.cta_trans_fatur
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cta_trans_fatur V-table-Win
ON F5 OF es-api-param-acr-spf.cta_trans_fatur IN FRAME f-main /* Conta Transit¢ria Faturamento */
DO:

    RUN prgint/utb/utb743za.py PERSISTENT SET h_api_cta.

    run pi_zoom_cta_ctbl_integr in h_api_cta (input  c-cod-empresa,          // EMPRESA EMS2 /
                                              input  "FTP",                  // M…DULO /
                                              input  "",                     // PLANO DE CONTAS /
                                              input  "(nenhum)",             // FINALIDADES /
                                              input  TODAY,                  // DATA TRANSACAO /
                                              output v_cod_cta,              // CODIGO CONTA /
                                              output v_des_cta,              // DESCRICAO CONTA /
                                              output v_ind_finalid_cta,      // FINALIDADE DA CONTA /
                                              output table tt_log_erro).     // ERROS / 
     
    IF NOT CAN-FIND(FIRST tt_log_erro) OR RETURN-VALUE = "OK" THEN 
        IF v_cod_cta <> "" THEN
            ASSIGN es-api-param-acr-spf.cta_trans_fatur:screen-value in frame {&FRAME-NAME} = v_cod_cta.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cta_trans_fatur V-table-Win
ON LEAVE OF es-api-param-acr-spf.cta_trans_fatur IN FRAME f-main /* Conta Transit¢ria Faturamento */
DO:
  
    FIND FIRST emscad.cta_ctbl
        WHERE cta_ctbl.cod_cta_ctbl = input frame {&frame-name} es-api-param-acr-spf.cta_trans_fatur NO-LOCK NO-ERROR.

    IF AVAIL cta_ctbl THEN
        DISP cta_ctbl.des_tit_ctbl @ desc-cta-transit WITH FRAME {&FRAME-NAME}.
    ELSE
        DISP '' @ desc-carteira WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-api-param-acr-spf.cta_trans_fatur V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-api-param-acr-spf.cta_trans_fatur IN FRAME f-main /* Conta Transit¢ria Faturamento */
DO:
  
    APPLY 'F5' TO SELF.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "es-api-param-acr-spf"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-api-param-acr-spf"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    if not frame {&frame-name}:validate() then
                return 'ADM-ERROR':U.
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    RUN PI-validate.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior.  */
    

    /* Dispatch standard ADM method.                                      */
    RUN dispatch IN THIS-PROCEDURE (INPUT 'display-fields':U).

    /* Code placed here will execute AFTER standard behavior.    */
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_empresa IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_estab IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_unid_neg IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_portador IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_ccusto IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_plano_cta_ctbl IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_plano_ccusto IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_espec_docto IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_matriz_trad_org_ext IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_tip_fluxo_financ IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_cart_bcia IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cta_trans_fatur IN FRAME {&FRAME-NAME}.
    APPLY 'LEAVE' TO es-api-param-acr-spf.cod_ser_docto IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    //if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

    FIND FIRST emscad.empresa NO-LOCK
        WHERE empresa.cod_empresa = es-api-param-acr-spf.cod_empresa:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL empresa THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_empresa IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Empresa inv lida.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.estabelecimento NO-LOCK
        WHERE estabelecimento.cod_estab = es-api-param-acr-spf.cod_estab:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL estabelecimento THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_estab IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Estabelecimento inv lido.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.portador NO-LOCK
        WHERE portador.cod_portador = es-api-param-acr-spf.cod_portador:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL portador THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_portador IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Portador inv lido.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.cart_bcia NO-LOCK
        WHERE cart_bcia.cod_cart_bcia = es-api-param-acr-spf.cod_cart_bcia:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL cart_bcia THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_cart_bcia IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Carteira inv lida.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.ccusto NO-LOCK
        WHERE ccusto.cod_ccusto = es-api-param-acr-spf.cod_ccusto:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL ccusto THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_ccusto IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Centro de Custo inv lido.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.ser_fisc_nota NO-LOCK
        WHERE ser_fisc_nota.cod_ser_fisc_nota = es-api-param-acr-spf.cod_ser_docto:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL ser_fisc_nota THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_ser_docto IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"S‚rie inv lida.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.espec_docto NO-LOCK
        WHERE espec_docto.cod_espec_docto = es-api-param-acr-spf.cod_espec_docto:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL espec_docto THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_espec_docto IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Esp‚cie inv lida.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.plano_cta_ctbl NO-LOCK
        WHERE plano_cta_ctbl.cod_plano_cta_ctbl = es-api-param-acr-spf.cod_plano_cta_ctbl:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL plano_cta_ctbl THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_plano_cta_ctbl IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Plano de Contas inv lido.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.unid_negoc NO-LOCK
        WHERE unid_negoc.cod_unid_negoc = es-api-param-acr-spf.cod_unid_neg:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL unid_negoc THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_unid_neg IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Unidade de Neg¢cio inv lida.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.plano_ccusto NO-LOCK
        WHERE plano_ccusto.cod_plano_ccusto = es-api-param-acr-spf.cod_plano_ccusto:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL plano_ccusto THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_plano_ccusto IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Plano Centro de Custo inv lido.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.tip_fluxo_financ NO-LOCK
        WHERE tip_fluxo_financ.cod_tip_fluxo_financ = es-api-param-acr-spf.cod_tip_fluxo_financ:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL tip_fluxo_financ THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_tip_fluxo_financ IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Tipo de Fluxo Financeiro inv lido.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.matriz_trad_org_ext NO-LOCK
        WHERE matriz_trad_org_ext.cod_matriz_trad_org_ext = es-api-param-acr-spf.cod_matriz_trad_org_ext:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL matriz_trad_org_ext THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cod_matriz_trad_org_ext IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Matriz Trad Org Externa inv lida.").
      RETURN 'ADM-ERROR':U.
    END.

    FIND FIRST emscad.cta_ctbl NO-LOCK
        WHERE cta_ctbl.cod_cta_ctbl = es-api-param-acr-spf.cta_trans_fatur:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.

    IF NOT AVAIL cta_ctbl THEN DO:
      APPLY "ENTRY" TO es-api-param-acr-spf.cta_trans_fatur IN FRAME {&FRAME-NAME}.
      RUN utp/ut-msgs.p ("show",17006,"Conta Cont bil inv lida.").
      RETURN 'ADM-ERROR':U.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-api-param-acr-spf"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

