&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esspf006 2.09.00.004} /*** "019004" ***/

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
//    {include/i-license-manager.i esspf006 <m¢dulo>}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


DEF TEMP-TABLE tt-log NO-UNDO
    FIELD id-movto AS INTEGER                  column-label "Id Movto"
    FIELD cd-tipo  AS INTEGER column-label "Tipo Movto"
    FIELD nr-seq   AS INTEGER                  column-label "Seq"
    FIELD c-log    AS CHARACTER format "X(1000)"   column-label "Log"
    FIELD data     AS DATETIME  COLUMN-LABEL "Data"
    INDEX i1 IS PRIMARY UNIQUE id-movto cd-tipo nr-seq.

DEFINE VARIABLE c-string AS CHARACTER   NO-UNDO.

DEFINE VARIABLE c-rs-transacao      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cb-status         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cb-estado         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cb-sistema        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-fi-chave          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-cb-tipo-integr    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE c-status            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-string-brlog      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE v_hdl_col           AS HANDLE      NO-UNDO.
DEFINE VARIABLE v_predicado         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l_ordena            AS LOGICAL     NO-UNDO.

DEFINE BUFFER bf-es-api-export-spf FOR es-api-export-spf.
DEFINE BUFFER bf-es-api-import-spf FOR es-api-import-spf.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-exp

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-api-export-spf es-api-param-spf es-api-import-spf ~
tt-log

/* Definitions for BROWSE br-exp                                        */
&Scoped-define FIELDS-IN-QUERY-br-exp es-api-export-spf.id-movto es-api-param-spf.des-tipo-integr es-api-export-spf.chave es-api-export-spf.data-inicio es-api-export-spf.data-fim c-status es-api-export-spf.data-movto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-exp   
&Scoped-define SELF-NAME br-exp
&Scoped-define QUERY-STRING-br-exp FOR EACH es-api-export-spf NO-LOCK       WHERE DATE(es-api-export-spf.data-movto) = TODAY , ~
             EACH es-api-param-spf OF es-api-export-spf NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-exp OPEN QUERY {&SELF-NAME} FOR EACH es-api-export-spf NO-LOCK       WHERE DATE(es-api-export-spf.data-movto) = TODAY , ~
             EACH es-api-param-spf OF es-api-export-spf NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-exp es-api-export-spf es-api-param-spf
&Scoped-define FIRST-TABLE-IN-QUERY-br-exp es-api-export
&Scoped-define SECOND-TABLE-IN-QUERY-br-exp es-api-param-spf


/* Definitions for BROWSE br-imp                                        */
&Scoped-define FIELDS-IN-QUERY-br-imp es-api-import-spf.id-movto es-api-param-spf.des-tipo-integr es-api-import-spf.chave es-api-import-spf.data-inicio es-api-import-spf.data-fim c-status es-api-import-spf.data-movto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-imp   
&Scoped-define SELF-NAME br-imp
&Scoped-define QUERY-STRING-br-imp FOR EACH es-api-import-spf NO-LOCK     WHERE DATE(es-api-import-spf.data-movto) = TODAY , ~
             EACH es-api-param-spf OF es-api-import-spf NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-imp OPEN QUERY {&SELF-NAME} FOR EACH es-api-import-spf NO-LOCK     WHERE DATE(es-api-import-spf.data-movto) = TODAY , ~
             EACH es-api-param-spf OF es-api-import-spf NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-imp es-api-import-spf es-api-param-spf
&Scoped-define FIRST-TABLE-IN-QUERY-br-imp es-api-import
&Scoped-define SECOND-TABLE-IN-QUERY-br-imp es-api-param-spf


/* Definitions for BROWSE br-log                                        */
&Scoped-define FIELDS-IN-QUERY-br-log tt-log.id-movto tt-log.data tt-log.cd-tipo tt-log.nr-seq tt-log.c-log   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-log   
&Scoped-define SELF-NAME br-log
&Scoped-define QUERY-STRING-br-log FOR EACH tt-log
&Scoped-define OPEN-QUERY-br-log OPEN QUERY {&SELF-NAME} FOR EACH tt-log.
&Scoped-define TABLES-IN-QUERY-br-log tt-log
&Scoped-define FIRST-TABLE-IN-QUERY-br-log tt-log


/* Definitions for FRAME f-cad                                          */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-canc bt-rep rt-button RECT-4 RECT-5 ~
IMAGE-7 IMAGE-8 cb-sistema cb-tipo-integr rs-transacao bt-pesquisar ~
cb-estado cb-status fi-dt-movto-ini fi-dt-movto-fim fi-chave br-imp br-exp ~
br-log edLog 
&Scoped-Define DISPLAYED-OBJECTS cb-sistema cb-tipo-integr rs-transacao ~
cb-estado cb-status fi-dt-movto-ini fi-dt-movto-fim fi-chave edLog 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .

DEFINE MENU POPUP-MENU-br-exp 
       MENU-ITEM m_Copia_Chave_para_rea_de_tra LABEL "Copia Chave para †rea de transferància"
       MENU-ITEM m_Visualiza_Json2 LABEL "Visualiza Json"
       MENU-ITEM m_Visualiza_Retorno2 LABEL "Visualiza Retorno".

DEFINE MENU POPUP-MENU-br-imp 
       MENU-ITEM m_Copia_Chave_para_rea_de_tra1 LABEL "Copia Chave para †rea de transferància"
       MENU-ITEM m_Visualiza_Json1 LABEL "Visualiza Json"
       MENU-ITEM m_Visualiza_Retorno1 LABEL "Visualiza Retorno".


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-canc 
     IMAGE-UP FILE "image/toolbar/im-can.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Reprocessar" 
     SIZE 4.14 BY 1.13 TOOLTIP "Reprocessar".

DEFINE BUTTON bt-pesquisar 
     IMAGE-UP FILE "image/toolbar/im-sea1.bmp":U
     LABEL "Pesquisar" 
     SIZE 4.57 BY 1.21.

DEFINE BUTTON bt-rel 
     IMAGE-UP FILE "image/im-solic.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Relat¢rio" 
     SIZE 4.14 BY 1.13 TOOLTIP "Relat¢rio Integraá‰es".

DEFINE BUTTON bt-rep 
     IMAGE-UP FILE "image/toolbar/im-tick.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Reprocessar" 
     SIZE 4.14 BY 1.13 TOOLTIP "Reprocessar".

DEFINE VARIABLE cb-estado AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 99 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todos",99,
                     "Pendente",0,
                     "Processando",1,
                     "Processado",2,
                     "Cancelado",9
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE cb-sistema AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 99 
     LABEL "Sistema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todos",99
     DROP-DOWN-LIST
     SIZE 25 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cb-status AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 99 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todos",99,
                     "N∆o Verificado",0,
                     "Integrado (OK)",1,
                     "N∆o Integrado (Erro)",2,
                     "Cancelado",9
     DROP-DOWN-LIST
     SIZE 28.14 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tipo-integr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 99 
     LABEL "Tipo Integraá∆o" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todas",99
     DROP-DOWN-LIST
     SIZE 25 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE edLog AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 138 BY 5.21 NO-UNDO.

DEFINE VARIABLE fi-chave AS CHARACTER FORMAT "X(256)":U 
     LABEL "Chave" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-movto-fim AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-movto-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Dt. Movto" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-transacao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Importaá∆o", 1,
"Exportaá∆o", 2
     SIZE 32.43 BY .63 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 138 BY 1.83.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 137.86 BY 1.25.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 137.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-exp FOR 
      es-api-export-spf, 
      es-api-param-spf SCROLLING.

DEFINE QUERY br-imp FOR 
      es-api-import-spf, 
      es-api-param-spf SCROLLING.

DEFINE QUERY br-log FOR 
      tt-log SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-exp w-livre _FREEFORM
  QUERY br-exp NO-LOCK DISPLAY
        es-api-export-spf.id-movto                        COLUMN-LABEL "Id Movto"     WIDTH 7   
        es-api-param-spf.des-tipo-integr  FORMAT 'X(30)'  COLUMN-LABEL "Descriá∆o"  WIDTH 18  
        es-api-export-spf.chave           FORMAT 'x(80)'  COLUMN-LABEL "Chave"
        es-api-export-spf.data-inicio                     COLUMN-LABEL "Data In°cio"
        es-api-export-spf.data-fim                        COLUMN-LABEL "Data TÇrmino"
        c-status                      FORMAT 'X(20)'  COLUMN-LABEL "Status"
       es-api-export-spf.data-movto                       COLUMN-LABEL "Data Movto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137.86 BY 13.29
         FONT 1 FIT-LAST-COLUMN.

DEFINE BROWSE br-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-imp w-livre _FREEFORM
  QUERY br-imp NO-LOCK DISPLAY
      es-api-import-spf.id-movto                        COLUMN-LABEL "Id Movto"         WIDTH 7
      es-api-param-spf.des-tipo-integr  FORMAT 'X(30)'  COLUMN-LABEL "Descriá∆o"        WIDTH 18
      es-api-import-spf.chave           FORMAT 'x(80)'  COLUMN-LABEL "Chave"
      es-api-import-spf.data-inicio                     COLUMN-LABEL "Data In°cio"
      es-api-import-spf.data-fim                        COLUMN-LABEL "Data TÇrmino"
      c-status                      FORMAT 'X(20)'  COLUMN-LABEL "Status"
      es-api-import-spf.data-movto                      COLUMN-LABEL "Data Movto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137.86 BY 13.29
         FONT 1 ROW-HEIGHT-CHARS .46 FIT-LAST-COLUMN.

DEFINE BROWSE br-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-log w-livre _FREEFORM
  QUERY br-log DISPLAY
      tt-log.id-movto 
tt-log.data
tt-log.cd-tipo  
tt-log.nr-seq   
tt-log.c-log WIDTH 200
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137.86 BY 4.17
         FONT 1 ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-rel AT ROW 1.21 COL 11.29 WIDGET-ID 24
     bt-canc AT ROW 1.21 COL 6.29 WIDGET-ID 58
     bt-rep AT ROW 1.21 COL 1.86 WIDGET-ID 18
     cb-sistema AT ROW 3.13 COL 52.57 COLON-ALIGNED WIDGET-ID 48
     cb-tipo-integr AT ROW 3.17 COL 110.86 COLON-ALIGNED WIDGET-ID 10
     rs-transacao AT ROW 3.25 COL 3.57 NO-LABEL WIDGET-ID 30
     bt-pesquisar AT ROW 4.75 COL 133.86 WIDGET-ID 16
     cb-estado AT ROW 4.88 COL 7 COLON-ALIGNED WIDGET-ID 8
     cb-status AT ROW 4.88 COL 36.86 COLON-ALIGNED WIDGET-ID 12
     fi-dt-movto-ini AT ROW 4.88 COL 73.43 COLON-ALIGNED WIDGET-ID 46
     fi-dt-movto-fim AT ROW 4.88 COL 89 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     fi-chave AT ROW 4.88 COL 104.86 COLON-ALIGNED WIDGET-ID 14
     br-imp AT ROW 6.29 COL 1.14 WIDGET-ID 500
     br-exp AT ROW 6.29 COL 1.14 WIDGET-ID 600
     br-log AT ROW 19.88 COL 1 WIDGET-ID 300
     edLog AT ROW 24.38 COL 1 NO-LABEL WIDGET-ID 60
     " Transaá∆o" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 2.58 COL 2.72 WIDGET-ID 42
     " Filtros" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 4.25 COL 2.29 WIDGET-ID 44
     rt-button AT ROW 1 COL 1.29
     RECT-4 AT ROW 4.42 COL 1.29 WIDGET-ID 38
     RECT-5 AT ROW 2.92 COL 1.14 WIDGET-ID 40
     IMAGE-7 AT ROW 4.88 COL 85.57 WIDGET-ID 54
     IMAGE-8 AT ROW 4.88 COL 88.57 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 139.14 BY 28.88
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Monitor de Integraá‰es"
         HEIGHT             = 28.88
         WIDTH              = 139.14
         MAX-HEIGHT         = 28.96
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 28.96
         VIRTUAL-WIDTH      = 194.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-imp fi-chave f-cad */
/* BROWSE-TAB br-exp br-imp f-cad */
/* BROWSE-TAB br-log br-exp f-cad */
ASSIGN 
       br-exp:POPUP-MENU IN FRAME f-cad             = MENU POPUP-MENU-br-exp:HANDLE
       br-exp:ALLOW-COLUMN-SEARCHING IN FRAME f-cad = TRUE.

ASSIGN 
       br-imp:POPUP-MENU IN FRAME f-cad             = MENU POPUP-MENU-br-imp:HANDLE
       br-imp:ALLOW-COLUMN-SEARCHING IN FRAME f-cad = TRUE.

/* SETTINGS FOR BUTTON bt-rel IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       bt-rel:HIDDEN IN FRAME f-cad           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-exp
/* Query rebuild information for BROWSE br-exp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH es-api-export-spf NO-LOCK
      WHERE DATE(es-api-export-spf.data-movto) = TODAY ,
      EACH es-api-param-spf OF es-api-export-spf NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE br-exp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-imp
/* Query rebuild information for BROWSE br-imp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH es-api-import-spf NO-LOCK
    WHERE DATE(es-api-import-spf.data-movto) = TODAY ,
      EACH es-api-param-spf OF es-api-import-spf NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE br-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-log
/* Query rebuild information for BROWSE br-log
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-log.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE br-log */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME f-cad:HANDLE
       ROW             = 1
       COLUMN          = 54
       HEIGHT          = 1.75
       WIDTH           = 6
       WIDGET-ID       = 50
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Monitor de Integraá‰es */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Monitor de Integraá‰es */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-exp
&Scoped-define SELF-NAME br-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-exp w-livre
ON MOUSE-SELECT-CLICK OF br-exp IN FRAME f-cad
DO:
    /*
    FOR EACH es-api-export-log-spf OF es-api-export-spf NO-LOCK:
        FIND FIRST tt-log
             WHERE tt-log.id-movto = int64(es-api-export-spf.id-movto)
               AND tt-log.cd-tipo  = es-api-export-spf.cd-tipo-integr
               AND tt-log.nr-seq   = es-api-export-log-spf.nr-seq    NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-log THEN DO:
            CREATE tt-log.
            ASSIGN tt-log.id-movto = int64(es-api-export-spf.id-movto)
                   tt-log.cd-tipo  = es-api-export-spf.cd-tipo-integr
                   tt-log.nr-seq   = es-api-export-log-spf.nr-seq.
        END.

       ASSIGN 
           tt-log.data     = es-api-export-log-spf.data
           tt-log.c-log    = es-api-export-log-spf.des-log.
    END.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-exp w-livre
ON ROW-DISPLAY OF br-exp IN FRAME f-cad
DO:

DEFINE BUFFER bLogE FOR tt-log.

DEFINE VARIABLE iCor AS INTEGER     NO-UNDO.

    CASE es-api-export-spf.cod-status:
        WHEN 0 THEN c-status = "N∆o Verificado".
        WHEN 1 THEN c-status = "Integrado (OK)".
        WHEN 2 THEN c-status = "N∆o Integrado (Erro)".
        WHEN 9 THEN c-status = "Cancelado".
    END CASE.



    FOR FIRST bLogE NO-LOCK WHERE bLogE.id-movto = INT(es-api-export-spf.id-movto):
       c-log = bLogE.c-log.
    END.

/*     IF es-api-export-spf.cd-tipo-integ = 1 THEN           */
/*     DO:                                               */
/*         DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.    */
/*         cc = STRING(es-api-export-spf.c-json).            */
/*         IF cc MATCHES TRIM('*"TipoPedido":"S"*') THEN */
/*             ASSIGN c-tipo-ped   = "Sales".            */
/*         ELSE                                          */
/*             ASSIGN c-tipo-ped   = "Doaá∆o".           */
/*     END.                                              */
/*     ELSE                                              */
/*         ASSIGN c-tipo-ped   = "EDI".                  */
            



    IF es-api-export-spf.cod-status = 0 THEN
        ASSIGN iCor     = 1.
    ELSE IF es-api-export-spf.cod-status = 1 THEN
         ASSIGN iCor =  9.
    ELSE IF es-api-export-spf.cod-status = 2 THEN
         ASSIGN iCor = 12.
    ELSE IF es-api-export-spf.cod-status = 9 THEN
         ASSIGN iCor = 7.

        ASSIGN es-api-export-spf.id-movto           :fgcolor in browse br-exp = iCor
               es-api-param-spf.des-tipo-integr     :fgcolor in browse br-exp = iCor
               es-api-export-spf.chave              :fgcolor in browse br-exp = iCor
               es-api-export-spf.data-movto         :fgcolor in browse br-exp = iCor
               es-api-export-spf.data-inicio        :fgcolor in browse br-exp = iCor
               es-api-export-spf.data-fim           :fgcolor in browse br-exp = iCor
               c-status                         :fgcolor in browse br-exp = iCor
//               c-tipo-ped                       :fgcolor in browse br-imp = iCor
//               c-log                            :fgcolor in browse br-imp = iCor
            . 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-exp w-livre
ON START-SEARCH OF br-exp IN FRAME f-cad
DO:
    ASSIGN v_hdl_col   = br-exp:current-column
           v_predicado = "".

    ASSIGN c-rs-transacao   = STRING(rs-transacao   )
           c-fi-chave       = STRING(fi-chave       )
           c-cb-status      = STRING(cb-status      )
           c-cb-estado      = STRING(cb-estado      )
           c-cb-tipo-integr = STRING(cb-tipo-integr )
           c-cb-sistema     = STRING(cb-sistema     ).

    IF c-cb-status      = "99" THEN c-cb-status      = "". ELSE c-cb-status       = ' AND es-api-export-spf.cod-status     = ' + c-cb-status      .
    IF c-cb-estado      = "99" THEN c-cb-estado      = "". ELSE c-cb-estado       = ' AND es-api-export-spf.ind-situacao   = ' + c-cb-estado      .
    IF c-cb-tipo-integr = "99" THEN c-cb-tipo-integr = "". ELSE c-cb-tipo-integr  = ' AND es-api-export-spf.cd-tipo-integr = ' + c-cb-tipo-integr .

    IF v_hdl_col:NAME <> "c-status" THEN DO:
        ASSIGN v_predicado = "   FOR EACH es-api-export-spf NO-LOCK "
                        + " WHERE DATE(es-api-export-spf.data-movto) >= DATE('" + fi-dt-movto-ini:screen-value in frame {&frame-name} + "')" 
                        + "   AND DATE(es-api-export-spf.data-movto) <= DATE('" + fi-dt-movto-fim:screen-value in frame {&frame-name} + "')" 
                        + " " +   c-cb-status                                                                         
                        + " " +   c-cb-estado
                        + " " +   c-cb-tipo-integr 
                        + "  ,EACH es-api-param-spf OF es-api-export-spf NO-LOCK " 
                        + " WHERE es-api-param-spf.cd-tipo-integr = es-api-export-spf.cd-tipo-integr " 
                       /* + " " +   c-cb-sistema.*/
                        + " by " + v_hdl_col:name.
    END.
    ELSE DO:
        ASSIGN v_predicado = "   FOR EACH es-api-export-spf NO-LOCK "
                        + " WHERE DATE(es-api-export-spf.data-movto) >= DATE('" + fi-dt-movto-ini:screen-value in frame {&frame-name} + "')" 
                        + "   AND DATE(es-api-export-spf.data-movto) <= DATE('" + fi-dt-movto-fim:screen-value in frame {&frame-name} + "')" 
                        + " " +   c-cb-status                                                                         
                        + " " +   c-cb-estado
                        + " " +   c-cb-tipo-integr 
                        + "  ,EACH es-api-param-spf OF es-api-export-spf NO-LOCK " 
                        + " WHERE es-api-param-spf.cd-tipo-integr = es-api-export-spf.cd-tipo-integr " 
                       /* + " " +   c-cb-sistema.*/
                        + " by es-api-export-spf.cod-status".
    END.

    APPLY 'value-changed' TO br-exp.

    QUERY br-exp:QUERY-PREPARE(v_predicado).
    QUERY br-exp:QUERY-OPEN(). 

    RUN pi-lista-br-log .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-exp w-livre
ON VALUE-CHANGED OF br-exp IN FRAME f-cad
DO:
    FOR EACH es-api-export-log-spf OF es-api-export-spf NO-LOCK:
        FIND FIRST tt-log
             WHERE tt-log.id-movto = int64(es-api-export-spf.id-movto)
               AND tt-log.cd-tipo  = es-api-export-spf.cd-tipo-integr
               AND tt-log.nr-seq   = es-api-export-log-spf.nr-seq    NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-log THEN DO:
            CREATE tt-log.
            ASSIGN tt-log.id-movto = int64(es-api-export-spf.id-movto)
                   tt-log.cd-tipo  = es-api-export-spf.cd-tipo-integr
                   tt-log.nr-seq   = es-api-export-log-spf.nr-seq.
        END.

       ASSIGN 
           tt-log.data     = es-api-export-log-spf.data
           tt-log.c-log    = es-api-export-log-spf.des-log.
    END.

    OPEN QUERY br-log
      FOR EACH tt-log 
         WHERE int(tt-log.id-movto) = es-api-export-spf.id-movto
           AND tt-log.cd-tipo  = es-api-export-spf.cd-tipo-integr BY tt-log.nr-seq DESC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-imp
&Scoped-define SELF-NAME br-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-imp w-livre
ON ROW-DISPLAY OF br-imp IN FRAME f-cad
DO:


DEFINE BUFFER bLog FOR tt-log.

DEFINE VARIABLE iCor AS INTEGER     NO-UNDO.

    CASE es-api-import-spf.cod-status:
        WHEN 0 THEN c-status = "N∆o Verificado".
        WHEN 1 THEN c-status = "Integrado (OK)".
        WHEN 2 THEN c-status = "N∆o Integrado (Erro)".
        WHEN 9 THEN c-status = "Cancelado".
    END CASE.



    FOR FIRST bLOg NO-LOCK WHERE blog.id-movto = INT(es-api-import-spf.id-movto):
       c-log = bLog.c-log.
    END.

/*     IF es-api-import-spf.cd-tipo-integ = 1 THEN           */
/*     DO:                                               */
/*         DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.    */
/*         cc = STRING(es-api-import-spf.c-json).            */
/*         IF cc MATCHES TRIM('*"TipoPedido":"S"*') THEN */
/*             ASSIGN c-tipo-ped   = "Sales".            */
/*         ELSE                                          */
/*             ASSIGN c-tipo-ped   = "Doaá∆o".           */
/*     END.                                              */
/*     ELSE                                              */
/*         ASSIGN c-tipo-ped   = "EDI".                  */
            



    IF es-api-import-spf.cod-status = 0 THEN
        ASSIGN iCor     = 1.
    ELSE IF es-api-import-spf.cod-status = 1 THEN
         ASSIGN iCor =  9.
    ELSE IF es-api-import-spf.cod-status = 2 THEN
         ASSIGN iCor = 12.
    ELSE IF es-api-import-spf.cod-status = 9 THEN
         ASSIGN iCor = 7.

        ASSIGN es-api-import-spf.id-movto           :fgcolor in browse br-imp = iCor
               es-api-param-spf.des-tipo-integr     :fgcolor in browse br-imp = iCor
               es-api-import-spf.chave              :fgcolor in browse br-imp = iCor
               es-api-import-spf.data-movto         :fgcolor in browse br-imp = iCor
               es-api-import-spf.data-inicio        :fgcolor in browse br-imp = iCor
               es-api-import-spf.data-fim           :fgcolor in browse br-imp = iCor
               c-status                         :fgcolor in browse br-imp = iCor
//               c-tipo-ped                       :fgcolor in browse br-imp = iCor
//               c-log                            :fgcolor in browse br-imp = iCor
            . 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-imp w-livre
ON START-SEARCH OF br-imp IN FRAME f-cad
DO:
    ASSIGN v_hdl_col   = br-imp:current-column
           v_predicado = "".

    ASSIGN c-rs-transacao   = STRING(rs-transacao   )
           c-fi-chave       = STRING(fi-chave       )
           c-cb-status      = STRING(cb-status      )
           c-cb-estado      = STRING(cb-estado      )
           c-cb-tipo-integr = STRING(cb-tipo-integr ) 
           c-cb-sistema     = STRING(cb-sistema     ).

    IF l_ordena THEN
        ASSIGN l_ordena = NO.
    ELSE ASSIGN l_ordena = YES.

    IF c-cb-status      = "99" THEN c-cb-status      = "". ELSE c-cb-status       = ' AND es-api-import-spf.cod-status     = ' + c-cb-status      .
    IF c-cb-estado      = "99" THEN c-cb-estado      = "". ELSE c-cb-estado       = ' AND es-api-import-spf.ind-situacao   = ' + c-cb-estado      .
    IF c-cb-tipo-integr = "99" THEN c-cb-tipo-integr = "". ELSE c-cb-tipo-integr  = ' AND es-api-import-spf.cd-tipo-integr = ' + c-cb-tipo-integr .

    IF v_hdl_col:NAME <> "c-status" THEN DO:
        ASSIGN v_predicado = "   FOR EACH es-api-import-spf NO-LOCK "
                          + " WHERE DATE(es-api-import-spf.data-movto) >= DATE('" + fi-dt-movto-ini:screen-value in frame {&frame-name} + "')" 
                          + "   AND DATE(es-api-import-spf.data-movto) <= DATE('" + fi-dt-movto-fim:screen-value in frame {&frame-name} + "')" 
                          + " " +   c-cb-status                                                                         
                          + " " +   c-cb-estado
                          + " " +   c-cb-tipo-integr 
                          + "  ,EACH es-api-param-spf OF es-api-import-spf NO-LOCK " 
                          + " WHERE es-api-param-spf.cd-tipo-integr = es-api-import-spf.cd-tipo-integr " 
                          /*+ " " +   c-cb-sistema*/
                          + " by " + v_hdl_col:name.
      END.
      ELSE DO:
           ASSIGN v_predicado = "   FOR EACH es-api-import-spf NO-LOCK "
                          + " WHERE DATE(es-api-import-spf.data-movto) >= DATE('" + fi-dt-movto-ini:screen-value in frame {&frame-name} + "')" 
                          + "   AND DATE(es-api-import-spf.data-movto) <= DATE('" + fi-dt-movto-fim:screen-value in frame {&frame-name} + "')" 
                          + " " +   c-cb-status                                                                         
                          + " " +   c-cb-estado
                          + " " +   c-cb-tipo-integr 
                          + "  ,EACH es-api-param-spf OF es-api-import-spf NO-LOCK " 
                          + " WHERE es-api-param-spf.cd-tipo-integr = es-api-import-spf.cd-tipo-integr " 
                          /*+ " " +   c-cb-sistema*/
                          + " by es-api-import-spf.cod-status".
      END.

      APPLY 'value-changed' TO br-imp.


      QUERY br-imp:QUERY-PREPARE(v_predicado).
      QUERY br-imp:QUERY-OPEN().
          
      RUN pi-lista-br-log .

    QUERY br-imp:QUERY-PREPARE(v_predicado).
    QUERY br-imp:QUERY-OPEN(). 

    RUN pi-lista-br-log .


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-imp w-livre
ON VALUE-CHANGED OF br-imp IN FRAME f-cad
DO:

    FOR EACH es-api-import-log-spf OF es-api-import-spf NO-LOCK:

        FIND FIRST tt-log
             WHERE tt-log.id-movto = int64(es-api-import-spf.id-movto)
               AND tt-log.cd-tipo  = es-api-import-spf.cd-tipo-integr
               AND tt-log.nr-seq   = es-api-import-log-spf.nr-seq    NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-log THEN DO:
            CREATE tt-log.
            ASSIGN tt-log.id-movto = int64(es-api-import-spf.id-movto)
                   tt-log.cd-tipo  = es-api-import-spf.cd-tipo-integr
                   tt-log.nr-seq   = es-api-import-log-spf.nr-seq.
        END.

       ASSIGN 
           tt-log.data     = es-api-import-log-spf.data
           tt-log.c-log    = es-api-import-log-spf.des-log.
    END.


    OPEN QUERY br-log
      FOR EACH tt-log 
         WHERE tt-log.id-movto = INT64(es-api-import-spf.id-movto)
           AND tt-log.cd-tipo  = es-api-import-spf.cd-tipo-integr BY tt-log.nr-seq DESC.

    /* foráa clique noprimeiro do browse para carregar log */
    //APPLY "MOUSE-SELECT-CLICK" TO br-imp IN FRAME f-cad.

    APPLY "value-changed" TO br-log.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-log
&Scoped-define SELF-NAME br-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-log w-livre
ON MOUSE-SELECT-DBLCLICK OF br-log IN FRAME f-cad
DO:
    //IF AVAIL tt-log THEN
    
    MESSAGE tt-log.c-log /*:SCREEN-VALUE IN BROWSE br-log */ 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

    APPLY "value-changed" TO br-log.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-log w-livre
ON VALUE-CHANGED OF br-log IN FRAME f-cad
DO:
    IF AVAIL tt-log THEN
        edLog:SCREEN-VALUE = REPLACE( tt-log.c-log, "|", CHR(10)) .
    ELSE 
        edLog:SCREEN-VALUE = "" .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-canc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-canc w-livre
ON CHOOSE OF bt-canc IN FRAME f-cad /* Reprocessar */
DO:  
    DEF BUFFER bf-es-api-import-spf FOR es-api-import-spf.
    DEF BUFFER bf-es-api-export-spf FOR es-api-export-spf.

    IF rs-transacao = 2 THEN DO:
        IF AVAIL es-api-export-spf THEN DO:

            IF es-api-export-spf.cod-status = 2 OR es-api-export-spf.cod-status = 0 THEN DO:
                FIND bf-es-api-export-spf WHERE ROWID(bf-es-api-export-spf) = ROWID(es-api-export-spf) EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL bf-es-api-export-spf THEN DO:
                    MESSAGE "Registro cancelado" VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    ASSIGN bf-es-api-export-spf.cod-status   = 9
                           //bf-es-api-export-spf.ind-situacao = 0
                           //bf-es-api-export-spf.cd-agente    = 0 
                           //bf-es-api-export-spf.nm-appserv   = "".
                        .
                END.
            END.            
        END.


        br-exp:REFRESH().
    END.
    ELSE DO:
        IF AVAIL es-api-import-spf THEN DO:

            IF es-api-import-spf.cod-status = 2 OR es-api-import-spf.cod-status = 0 THEN DO:
                FIND bf-es-api-import-spf WHERE ROWID(bf-es-api-import-spf) = rowid(es-api-import-spf) EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL bf-es-api-import-spf THEN DO:
                    MESSAGE "Registro cancelado" VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    ASSIGN bf-es-api-import-spf.cod-status   = 9
                           bf-es-api-import-spf.ind-situacao = 9
                           .
                END.
            END.
            
            FIND CURRENT es-api-import-spf NO-LOCK NO-ERROR.
            
        END.
        br-imp:REFRESH().
    END.

    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pesquisar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pesquisar w-livre
ON CHOOSE OF bt-pesquisar IN FRAME f-cad /* Pesquisar */
DO:     
    EMPTY TEMP-TABLE tt-log.
    ASSIGN FRAME {&FRAME-NAME} rs-transacao cb-status cb-estado cb-sistema fi-chave cb-tipo-integr.

    ASSIGN c-rs-transacao   = STRING(rs-transacao   )
           c-fi-chave       = STRING(fi-chave       )
           c-cb-status      = STRING(cb-status      )
           c-cb-estado      = STRING(cb-estado      )
           c-cb-tipo-integr = STRING(cb-tipo-integr )
           c-cb-sistema     = STRING(cb-sistema     ).


    SESSION:SET-WAIT-STATE("General").

    IF c-cb-sistema     = "99" THEN c-cb-sistema     = "". ELSE c-cb-sistema      = ' AND es-api-param-spf.cd-sistema      = ' + c-cb-sistema     .

    IF fi-chave <> "" THEN DO:
        /* Importaá∆o */
        IF rs-transacao = 1 THEN DO:   

            ASSIGN c-string = "FOR EACH es-api-import-spf NO-LOCK " +
                                  " WHERE es-api-import-spf.chave MATCHES " + "'*" + fi-chave:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "*' ," + 
                                  " EACH es-api-param-spf OF es-api-import-spf NO-LOCK BY es-api-import-spf.id-movto desc".

            APPLY 'value-changed' TO br-imp.
            QUERY br-imp:QUERY-PREPARE(c-string).
            QUERY br-imp:QUERY-OPEN().

            RUN pi-lista-br-log .

        END.

        /* Exportaá∆o */
        ELSE DO:
            ASSIGN c-string = "FOR EACH es-api-export-spf NO-LOCK " +
                                  " WHERE es-api-export-spf.chave MATCHES " + "'*" + fi-chave:SCREEN-VALUE IN FRAME {&FRAME-NAME} + "*' ," + 
                                  " EACH es-api-param-spf OF es-api-export-spf NO-LOCK BY es-api-export-spf.id-movto desc".
            APPLY 'value-changed' TO br-exp.
            QUERY br-exp:QUERY-PREPARE(c-string).
            QUERY br-exp:QUERY-OPEN().
            
            RUN pi-lista-br-log .

        END.
    END.
    ELSE DO:
        
        /* Importaá∆o */
        IF rs-transacao = 1 THEN DO:   

            ASSIGN c-string = ''.
        
            IF c-cb-status      = "99" THEN c-cb-status      = "". ELSE c-cb-status       = ' AND es-api-import-spf.cod-status     = ' + c-cb-status      .
            IF c-cb-estado      = "99" THEN c-cb-estado      = "". ELSE c-cb-estado       = ' AND es-api-import-spf.ind-situacao   = ' + c-cb-estado      .
            IF c-cb-tipo-integr = "99" THEN c-cb-tipo-integr = "". ELSE c-cb-tipo-integr  = ' AND es-api-import-spf.cd-tipo-integr = ' + c-cb-tipo-integr .

            ASSIGN c-string = "   FOR EACH es-api-import-spf NO-LOCK "
                            + " WHERE DATE(es-api-import-spf.data-movto) >= DATE('" + fi-dt-movto-ini:screen-value in frame {&frame-name} + "')" 
                            + "   AND DATE(es-api-import-spf.data-movto) <= DATE('" + fi-dt-movto-fim:screen-value in frame {&frame-name} + "')" 
                            + " " +   c-cb-status                                                                         
                            + " " +   c-cb-estado
                            + " " +   c-cb-tipo-integr 
                            + "  ,EACH es-api-param-spf OF es-api-import-spf NO-LOCK " 
                            + " WHERE es-api-param-spf.cd-tipo-integr = es-api-import-spf.cd-tipo-integr " 
                            + " " +   c-cb-sistema
                            + " BY es-api-import-spf.id-movto desc ".

            APPLY 'value-changed' TO br-imp.

            QUERY br-imp:QUERY-PREPARE(c-string).
            QUERY br-imp:QUERY-OPEN().
                
            RUN pi-lista-br-log .

        END.

        /* Exportaá∆o */
        ELSE DO:
            .MESSAGE 4
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            IF c-cb-status      = "99" THEN c-cb-status      = "". ELSE c-cb-status       = ' AND es-api-export-spf.cod-status     = ' + c-cb-status      .
            IF c-cb-estado      = "99" THEN c-cb-estado      = "". ELSE c-cb-estado       = ' AND es-api-export-spf.ind-situacao   = ' + c-cb-estado      .
            IF c-cb-tipo-integr = "99" THEN c-cb-tipo-integr = "". ELSE c-cb-tipo-integr  = ' AND es-api-export-spf.cd-tipo-integr = ' + c-cb-tipo-integr .

            ASSIGN c-string = "   FOR EACH es-api-export-spf NO-LOCK "
                            + " WHERE DATE(es-api-export-spf.data-movto) >= DATE('" + fi-dt-movto-ini:screen-value in frame {&frame-name} + "')" 
                            + "   AND DATE(es-api-export-spf.data-movto) <= DATE('" + fi-dt-movto-fim:screen-value in frame {&frame-name} + "')" 
                            + " " +   c-cb-status                                                                         
                            + " " +   c-cb-estado
                            + " " +   c-cb-tipo-integr 
                            + "  ,EACH es-api-param-spf OF es-api-export-spf NO-LOCK " 
                            + " WHERE es-api-param-spf.cd-tipo-integr = es-api-export-spf.cd-tipo-integr " 
                            + " " +   c-cb-sistema
                            + " BY es-api-export-spf.id-movto desc ".

            APPLY 'value-changed' TO br-exp.

            .MESSAGE c-string
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            QUERY br-exp:QUERY-PREPARE(c-string).
            QUERY br-exp:QUERY-OPEN(). 

            RUN pi-lista-br-log .

        END.
    END.

    ASSIGN fi-chave:SCREEN-VALUE = "".

    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-rel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-rel w-livre
ON CHOOSE OF bt-rel IN FRAME f-cad /* Relat¢rio */
DO:
  RUN esp/esspf013.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-rep w-livre
ON CHOOSE OF bt-rep IN FRAME f-cad /* Reprocessar */
DO:  
    DEF BUFFER bf-es-api-import-spf FOR es-api-import-spf.
    DEF BUFFER bf-es-api-export-spf FOR es-api-export-spf.

    IF rs-transacao = 2 THEN DO:
        IF AVAIL es-api-export-spf THEN DO:

            IF es-api-export-spf.cod-status = 2 OR es-api-export-spf.cod-status = 9 THEN DO:
                FIND bf-es-api-export-spf WHERE ROWID(bf-es-api-export-spf) = ROWID(es-api-export-spf) EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL bf-es-api-export-spf THEN DO:
                    MESSAGE "Registro ser† reprocessado!" VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    ASSIGN bf-es-api-export-spf.cod-status   = 0
                           bf-es-api-export-spf.ind-situacao = 0
                           //bf-es-api-export-spf.cd-agente    = 0 
                           //bf-es-api-export-spf.nm-appserv   = "".
                        .
                END.
            END.            
        END.


        br-exp:REFRESH().
    END.
    ELSE DO:
        IF AVAIL es-api-import-spf THEN DO:

            IF es-api-import-spf.cod-status = 2 OR es-api-import-spf.cod-status = 9 THEN DO:
                FIND bf-es-api-import-spf WHERE ROWID(bf-es-api-import-spf) = rowid(es-api-import-spf) EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL bf-es-api-import-spf THEN DO:
                    MESSAGE "Registro ser† reprocessado!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    ASSIGN bf-es-api-import-spf.cod-status   = 0
                           bf-es-api-import-spf.ind-situacao = 0
                           .
                END.
            END.
            
            FIND CURRENT es-api-import-spf NO-LOCK NO-ERROR.
            
        END.
        br-imp:REFRESH().
    END.

    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-sistema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-sistema w-livre
ON VALUE-CHANGED OF cb-sistema IN FRAME f-cad /* Sistema */
DO:

    DEF VAR c-integr  AS CHARACTER NO-UNDO.
    DEF VAR l-ret     AS LOGICAL   NO-UNDO.

    ASSIGN FRAME f-cad rs-transacao.

    ASSIGN 
       c-integr  = "Todas,99".

    FIND FIRST es-api-app-spf NO-LOCK
         WHERE es-api-app-spf.cd-sistema = SELF:INPUT-VALUE
         NO-ERROR.

    CASE rs-transacao:
        WHEN 1 THEN DO:
            /*DISABLE cb-tipo-integr WITH FRAME f-cad.           */
            ENABLE cb-tipo-integr WITH FRAME f-cad. 
            FOR EACH es-api-param-spf WHERE es-api-param-spf.ind-tipo-trans = 1 NO-LOCK:
                IF es-api-param-spf.cd-sistema = SELF:INPUT-VALUE
                OR SELF:INPUT-VALUE = 99
                THEN ASSIGN 
                   c-integr = c-integr + "," + es-api-param-spf.des-tipo-integr + "," + string(es-api-param-spf.cd-tipo-integr).
            END.
        END.
        WHEN 2 THEN DO:
            ENABLE cb-tipo-integr WITH FRAME f-cad. 
            FOR EACH es-api-param-spf WHERE es-api-param-spf.ind-tipo-trans = 2 NO-LOCK:
                IF es-api-param-spf.cd-sistema = SELF:INPUT-VALUE
                OR SELF:INPUT-VALUE = 99
                THEN ASSIGN 
                   c-integr = c-integr + "," + es-api-param-spf.des-tipo-integr + "," + string(es-api-param-spf.cd-tipo-integr).
            END.
        END.
        WHEN 3 THEN DO:
            ENABLE cb-tipo-integr WITH FRAME f-cad. 
            FOR EACH es-api-param-spf WHERE es-api-param-spf.ind-tipo-trans = 2 NO-LOCK:
                IF es-api-param-spf.cd-sistema = SELF:INPUT-VALUE
                OR SELF:INPUT-VALUE = 99
                THEN ASSIGN 
                   c-integr = c-integr + "," + es-api-param-spf.des-tipo-integr + "," + string(es-api-param-spf.cd-tipo-integr) .
            END.
        END.

    END CASE.

    ASSIGN 
       cb-tipo-integr:LIST-ITEM-PAIRS = c-integr
       cb-tipo-integr:SCREEN-VALUE = "99".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame w-livre OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

    
    SESSION:SET-WAIT-STATE("General").

    IF AVAIL es-api-import-spf THEN 
        br-imp:REFRESH() IN FRAME f-cad.

    IF AVAIL es-api-export-spf THEN
        br-exp:REFRESH() IN FRAME f-cad.

    SESSION:SET-WAIT-STATE("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edLog w-livre
ON ENTRY OF edLog IN FRAME f-cad
DO:
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-chave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-chave w-livre
ON RETURN OF fi-chave IN FRAME f-cad /* Chave */
DO:
    APPLY "choose" TO bt-pesquisar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copia_Chave_para_rea_de_tra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copia_Chave_para_rea_de_tra w-livre
ON CHOOSE OF MENU-ITEM m_Copia_Chave_para_rea_de_tra /* Copia Chave para †rea de transferància */
DO:
    IF AVAIL es-api-export-spf THEN
        CLIPBOARD:VALUE = es-api-export-spf.chave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Copia_Chave_para_rea_de_tra1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Copia_Chave_para_rea_de_tra1 w-livre
ON CHOOSE OF MENU-ITEM m_Copia_Chave_para_rea_de_tra1 /* Copia Chave para †rea de transferància */
DO:
    IF AVAIL es-api-import-spf THEN
        CLIPBOARD:VALUE = es-api-import-spf.chave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visualiza_Json1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visualiza_Json1 w-livre
ON CHOOSE OF MENU-ITEM m_Visualiza_Json1 /* Visualiza Json */
DO:
  
    
    IF AVAIL es-api-import-spf THEN 
        RUN visualizarJSON ("es-api-import-spf", es-api-import-spf.id-movto).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visualiza_Json2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visualiza_Json2 w-livre
ON CHOOSE OF MENU-ITEM m_Visualiza_Json2 /* Visualiza Json */
DO:
  
    
    IF AVAIL es-api-export-spf THEN 
        RUN visualizarJSON ("es-api-export-spf", es-api-export-spf.id-movto).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visualiza_Retorno1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visualiza_Retorno1 w-livre
ON CHOOSE OF MENU-ITEM m_Visualiza_Retorno1 /* Visualiza Retorno */
DO:
    DEF VAR cLongJson AS LONGCHAR  NO-UNDO.
    DEF VAR h-buffer  AS HANDLE    NO-UNDO.
    DEF VAR h-query   AS HANDLE    NO-UNDO.
    DEF VAR i-count   AS INTEGER   NO-UNDO.

    IF AVAIL es-api-import-spf THEN DO:
        //ASSIGN clongjson = es-api-import-spf.text-retorno.
        //RUN esp\esspf006j.r (INPUT es-api-import-spf.ind-tipo-trans, INPUT clongjson).
                
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visualiza_Retorno2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visualiza_Retorno2 w-livre
ON CHOOSE OF MENU-ITEM m_Visualiza_Retorno2 /* Visualiza Retorno */
DO:
    DEF VAR cLongJson AS LONGCHAR  NO-UNDO.
    DEF VAR h-buffer  AS HANDLE    NO-UNDO.
    DEF VAR h-query   AS HANDLE    NO-UNDO.
    DEF VAR i-count   AS INTEGER   NO-UNDO.
    
    IF AVAIL es-api-export-spf THEN DO:

        IF es-api-export-spf.text-retorno <> ? AND es-api-export-spf.text-retorno <> "" THEN 
            ASSIGN clongjson = es-api-export-spf.text-retorno.
        ELSE
            ASSIGN clongjson = es-api-export-spf.clob-retorno.


        FOR FIRST es-api-param-spf NO-LOCK WHERE
                  es-api-param-spf.cd-tipo-integr        = es-api-export-spf.cd-tipo-integr:

        END.

        RUN esp\esspf006j.r (INPUT es-api-param-spf.ind-tipo-trans, INPUT clongjson).              
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-transacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-transacao w-livre
ON VALUE-CHANGED OF rs-transacao IN FRAME f-cad
DO:
    DEF VAR c-sistema AS CHARACTER NO-UNDO.
    DEF VAR l-ret     AS LOGICAL   NO-UNDO.

    EMPTY TEMP-TABLE tt-log.

    ASSIGN FRAME f-cad rs-transacao.

    ASSIGN 
       c-sistema = "Todos,99".

    FOR EACH es-api-app-spf NO-LOCK:
        ASSIGN 
           c-sistema = c-sistema
                     + "," 
                     + es-api-app-spf.des-sistema
                     + "," 
                     + string(es-api-app-spf.cd-sistema).
    END.

    
    ASSIGN 
       cb-sistema    :LIST-ITEM-PAIRS = c-sistema
       cb-sistema    :SCREEN-VALUE = "99".

    APPLY "VALUE-CHANGED" TO cb-sistema IN FRAME {&FRAME-NAME}.

    IF rs-transacao = 1 THEN
        ASSIGN 
        br-imp:VISIBLE = TRUE
        br-exp:VISIBLE = FALSE.
    ELSE
        ASSIGN 
        br-exp:VISIBLE = TRUE
        br-imp:VISIBLE = FALSE.


        EMPTY TEMP-TABLE tt-log.
        ASSIGN c-string-brlog = "For each tt-log" .
        QUERY br-log:QUERY-PREPARE(c-string-brlog).            
        QUERY br-log:QUERY-OPEN().                       

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-exp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 122.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load w-livre  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "esspf006.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "esspf006.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cb-sistema cb-tipo-integr rs-transacao cb-estado cb-status 
          fi-dt-movto-ini fi-dt-movto-fim fi-chave edLog 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE bt-canc bt-rep rt-button RECT-4 RECT-5 IMAGE-7 IMAGE-8 cb-sistema 
         cb-tipo-integr rs-transacao bt-pesquisar cb-estado cb-status 
         fi-dt-movto-ini fi-dt-movto-fim fi-chave br-imp br-exp br-log edLog 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

{utp/ut9000.i "esspf006" "2.09.00.004"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* seta posicao importaá∆o */
  ASSIGN rs-transacao = 1.

  /* foráa aplicar bot∆o rs-transacao */
  APPLY 'value-changed' TO rs-transacao.

  /* foráa abrir log */
  APPLY "VALUE-CHANGED" TO br-imp IN FRAME f-cad.

  ASSIGN fi-dt-movto-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999").
  ASSIGN fi-dt-movto-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999").

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-lista-br-log w-livre 
PROCEDURE pi-lista-br-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt-log.

    ASSIGN FRAME {&FRAME-NAME} rs-transacao.

    IF rs-transacao = 2 THEN DO:
        IF AVAIL es-api-export-spf THEN DO:
            FOR EACH es-api-export-log-spf OF es-api-export-spf NO-LOCK:                               
                FIND FIRST tt-log                                                              
                     WHERE tt-log.id-movto = int64(es-api-export-spf.id-movto)                     
                       AND tt-log.cd-tipo  = es-api-export-spf.cd-tipo-integr                      
                       AND tt-log.nr-seq   = es-api-export-log-spf.nr-seq    NO-LOCK NO-ERROR.     
                                                                                               
                IF NOT AVAIL tt-log THEN DO:                                                   
                    CREATE tt-log.                                                             
                    ASSIGN tt-log.id-movto = int64(es-api-export-spf.id-movto)                     
                           tt-log.cd-tipo  = es-api-export-spf.cd-tipo-integr                      
                           tt-log.nr-seq   = es-api-export-log-spf.nr-seq.                         
                END.                                                                           
                                                                                               
                ASSIGN                                                                         
                   tt-log.data     = es-api-export-log-spf.data                                    
                   tt-log.c-log    = es-api-export-log-spf.des-log.                                
            END.                                                                               
        END.
    END. 
    ELSE do:
        IF AVAIL es-api-import-spf THEN DO:
            FOR EACH es-api-import-log-spf OF es-api-import-spf NO-LOCK:                               
                FIND FIRST tt-log                                                              
                     WHERE tt-log.id-movto = int64(es-api-import-spf.id-movto)                     
                       AND tt-log.cd-tipo  = es-api-import-spf.cd-tipo-integr                      
                       AND tt-log.nr-seq   = es-api-import-log-spf.nr-seq    NO-LOCK NO-ERROR.     
                                                                                               
                IF NOT AVAIL tt-log THEN DO:                                                   
                    CREATE tt-log.                                                             
                    ASSIGN tt-log.id-movto = int64(es-api-import-spf.id-movto)                     
                           tt-log.cd-tipo  = es-api-import-spf.cd-tipo-integr                      
                           tt-log.nr-seq   = es-api-import-log-spf.nr-seq.                         
                END.                                                                           
                                                                                               
                ASSIGN                                                                         
                   tt-log.data     = es-api-import-log-spf.data                                    
                   tt-log.c-log    = es-api-import-log-spf.des-log.                                
            END.
        END.
    END. 
    ASSIGN c-string-brlog = "FOR EACH tt-log BY tt-log.nr-seq DESC" .
    QUERY br-log:QUERY-PREPARE(c-string-brlog).
    QUERY br-log:QUERY-OPEN(). 

    IF rs-transacao = 2 THEN
        APPLY 'value-changed' TO br-exp.
    ELSE
        APPLY 'value-changed' TO br-imp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-log"}
  {src/adm/template/snd-list.i "es-api-import-spf"}
  {src/adm/template/snd-list.i "es-api-param-spf"}
  {src/adm/template/snd-list.i "es-api-export-spf"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visualizarJSON w-livre 
PROCEDURE visualizarJSON :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pTabela     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pMovto      AS INTEGER     NO-UNDO.

DEF VAR h-buffer  AS HANDLE    NO-UNDO.
DEF VAR h-query   AS HANDLE    NO-UNDO.
DEF VAR cLongJson AS LONGCHAR  NO-UNDO.


    CREATE BUFFER h-buffer FOR TABLE pTabela.
    //CREATE BUFFER h-buffer FOR TABLE es-api-param-spf.nome-tabela-integr.
                                                          
    CREATE QUERY h-query.
    h-query:SET-BUFFERS(h-buffer).
    h-query:QUERY-PREPARE("FOR EACH " + pTabela + " where id-movto = " + STRING(pMovto)).
    h-query:QUERY-OPEN().
    
    REPEAT:
        h-query:GET-NEXT().  
        if h-query:QUERY-OFF-END THEN LEAVE.

        COPY-LOB h-buffer:BUFFER-FIELD("c-json"):BUFFER-VALUE() TO cLongJson.                 
    END.
    
    h-query:QUERY-CLOSE().
    h-buffer:BUFFER-RELEASE().
    DELETE OBJECT h-buffer.
    DELETE OBJECT h-query.

    IF clongjson = "" THEN
        MESSAGE 'sem registros' VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RUN esp\esspf006j.w (INPUT 1,
                       INPUT clongjson).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

