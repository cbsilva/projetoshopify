&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i esspf100 2.09.00.002}

{lib/utilidades.i}
/* {lib/logsfa.i} */

DEFINE VARIABLE lAtivo AS LOGICAL     NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE TOTVSDRGSP_ExecutandoRobo AS LOGICAL NO-UNDO.
                                                  
/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esspf100 mpd}
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

DEF VAR h-esspf100 AS HANDLE NO-UNDO.

DEFINE VARIABLE lInterromper AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 cb_sistema cb_Entrada ~
cb_saida fi_Intervalo buAtivar buClientLog 
&Scoped-Define DISPLAYED-OBJECTS cb_sistema cb_Entrada cb_saida ~
fi_Intervalo fiStatus 

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


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON buAtivar 
     LABEL "Ativar" 
     SIZE 14 BY 1.

DEFINE BUTTON buClientLog 
     LABEL "Client Log" 
     SIZE 14 BY 1.

DEFINE VARIABLE cb_Entrada AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 999 
     LABEL "Entrada" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todos",999
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE cb_saida AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 999 
     LABEL "Sa¡da" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todos",999
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE cb_sistema AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Sistema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",1
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 86 BY .88 NO-UNDO.

DEFINE VARIABLE fi_Intervalo AS INTEGER FORMAT ">>,>>9":U INITIAL 30 
     LABEL "Intervalo (s.)" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 8.33.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE toAS AS LOGICAL INITIAL no 
     LABEL "AppServer" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     cb_sistema AT ROW 3 COL 18 COLON-ALIGNED WIDGET-ID 26
     toAS AT ROW 3 COL 56 WIDGET-ID 48
     cb_Entrada AT ROW 4.25 COL 18 COLON-ALIGNED WIDGET-ID 50
     cb_saida AT ROW 5.5 COL 18 COLON-ALIGNED WIDGET-ID 52
     fi_Intervalo AT ROW 6.75 COL 18 COLON-ALIGNED WIDGET-ID 56
     buAtivar AT ROW 8.25 COL 20 WIDGET-ID 2
     buClientLog AT ROW 8.25 COL 37 WIDGET-ID 58
     fiStatus AT ROW 9.75 COL 3 NO-LABEL WIDGET-ID 40
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.67 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04 WIDGET-ID 100.


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
         TITLE              = "Processo de Integra‡Æo SFA"
         HEIGHT             = 10.17
         WIDTH              = 89.14
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 105.14
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 105.14
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
/* SETTINGS FOR FILL-IN fiStatus IN FRAME f-cad
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR TOGGLE-BOX toAS IN FRAME f-cad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       toAS:HIDDEN IN FRAME f-cad           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME f-cad:HANDLE
       ROW             = 4.5
       COLUMN          = 81
       HEIGHT          = 1.92
       WIDTH           = 7.29
       WIDGET-ID       = 36
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(cb_Entrada:HANDLE IN FRAME f-cad).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON CTRL-D OF w-livre /* Processo de Integra‡Æo SFA */
DO:
/*     FOR EACH es-api-exec WHERE es-api-exec.cd-sistema = integer( /*fi-cd-sistema:SCREEN-VALUE IN FRAME {&FRAME-NAME}*/ cb_sistema:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) */
/*                             AND es-api-exec.data-fim = ?:                                                                                                               */
/*                                                                                                                                                                         */
/*          ASSIGN es-api-exec.data-fim = NOW.                                                                                                                             */
/*      END.                                                                                                                                                               */
/*                                                                                                                                                                         */
/*      APPLY 'ESC' TO THIS-PROCEDURE.                                                                                                                                     */
/*                                                                                                                                                                         */
/*      ASSIGN bt-ativa:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.                                                                                                           */
/*                                                                                                                                                                         */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Processo de Integra‡Æo SFA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Processo de Integra‡Æo SFA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buAtivar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buAtivar w-livre
ON CHOOSE OF buAtivar IN FRAME f-cad /* Ativar */
DO:
    DEFINE VARIABLE lop AS LOGICAL     NO-UNDO.


    IF LOG-MANAGER:LOGFILE-NAME     = "" OR LOG-MANAGER:LOGFILE-NAME     = ? THEN
    RUN ativarClientLOg.


    ASSIGN  chCtrlFrame:pSTimer:interval = INTEGER(fi_intervalo:SCREEN-VALUE)  * 1000.

    IF lAtivo  THEN
    DO:
        MESSAGE "Confirmar interromper integra‡Æo?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOP.
        IF NOT lop THEN RETURN NO-APPLY.

        ASSIGN  lAtivo                      = NO
                buAtivar:LABEL              = "Ativar"
                fiStatus:SCREEN-VALUE       = timeStamp() + " Integra‡Æo interrompida".

        
    END.
    ELSE
    DO:

        ASSIGN  lAtivo                      = YES
                buAtivar:LABEL              = "Desativar"
                fiStatus:SCREEN-VALUE       = timeStamp() + " Integra‡Æo Ativada".

        // RUN ConectarAS.

    END.

        

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME buClientLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL buClientLog w-livre
ON CHOOSE OF buClientLog IN FRAME f-cad /* Client Log */
DO:

    DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.


    IF LOG-MANAGER:LOGFILE-NAME = "" OR LOG-MANAGER:LOGFILE-NAME = ? THEN
    DO:

        MESSAGE "Confirmar ativar ClientLOG?"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE lOK.
        IF lOK THEN
            RUN ativarClientLog.

    END.
    ELSE
    DO:
        MESSAGE "Confirmar desativar ClientLOG?"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE lOK.
        IF lOK THEN
            RUN desativarClientLog.
    END.


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

    LOG-MANAGER:WRITE-MESSAGE("LASF MAC - ROBO EM EXEC? " + STRING(TOTVSDRGSP_ExecutandoRobo )).

    IF NOT TOTVSDRGSP_ExecutandoRobo THEN DO:

        LOG-MANAGER:WRITE-MESSAGE("LASF MAC - CHAMANDO ROTINA CONEXAO ").

        TOTVSDRGSP_ExecutandoRobo = YES.

        IF lAtivo THEN
            RUN conectarAS.


        TOTVSDRGSP_ExecutandoRobo = NO.

    END.


END PROCEDURE.

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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

  RUN ativarClientLOg.

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
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             cb_sistema:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ativarClientLog w-livre 
PROCEDURE ativarClientLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cHora AS CHARACTER   NO-UNDO.

cHora = STRING(TIME, "HH:MM:SS").

SESSION:DEBUG-ALERT = YES.

ASSIGN LOG-MANAGER:LOGFILE-NAME     = 
    "c:\temp\clientlog_"  + 
    STRING(YEAR(today) , "9999") + 
    STRING(MONTH(today) , "9999") + 
    STRING(DAY(today) , "9999") + 
    SUBSTRING(chora, 1, 2) +
    SUBSTRING(chora, 4, 2) +
    SUBSTRING(chora, 7, 2) + ".txt"
    .

ASSIGN LOG-MANAGER:LOG-ENTRY-TYPES = "4GLTrace".
ASSIGN LOG-MANAGER:LOGGING-LEVEL = 2            .



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE conectarAS w-livre 
PROCEDURE conectarAS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                                        
    LOG-MANAGER:WRITE-MESSAGE("LASF MAC - CONECTAR AS").

    DO WITH FRAME {&FRAME-NAME}:
        
        fiStatus:SCREEN-VALUE = timeStamp() + " Chamando Rotina Processamento...".

        RUN esp\esspf100rp.p PERSISTENT SET h-esspf100.
        RUN pi-processa IN h-esspf100 (INPUT integer(cb_sistema:SCREEN-VALUE  ) ,  INTEGER(cb_entrada:SCREEN-VALUE)  , INTEGER(cb_saida:SCREEN-VALUE), fiStatus:HANDLE, NO /*toAS:CHECKED*/ ).
        IF VALID-HANDLE(h-esspf100) THEN DELETE PROCEDURE h-esspf100.

    END.


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

OCXFile = SEARCH( "esspf100.wrx":U ).
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
ELSE MESSAGE "esspf100.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE desativarClientLog w-livre 
PROCEDURE desativarClientLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN LOG-MANAGER:LOGFILE-NAME     = "".

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
  DISPLAY cb_sistema cb_Entrada cb_saida fi_Intervalo fiStatus 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 cb_sistema cb_Entrada cb_saida fi_Intervalo buAtivar 
         buClientLog 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE interromper w-livre 
PROCEDURE interromper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    ASSIGN lInterromper = YES.

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

DEFINE VARIABLE cSistema AS CHARACTER   NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "esspf100" "1.00.00.000"}



      cb_sistema:DELETE(1) NO-ERROR.

      FOR EACH es-api-app-spf NO-LOCK BREAK BY es-api-app-spf.cd-sistema:
          cb_sistema:ADD-LAST(es-api-app-spf.des-sistema,  es-api-app-spf.cd-sistema).
          IF FIRST-OF(es-api-app-spf.cd-sistema) THEN 
              ASSIGN cSistema = STRING( es-api-app-spf.cd-sistema)    .
      END.

      cb_sistema:SCREEN-VALUE = cSistema.

      
      //cb_entrada:DELETE(1) NO-ERROR.
      FOR EACH es-api-param-spf NO-LOCK 
          WHERE es-api-param-spf.ind-tipo-trans = 1
          BREAK BY es-api-param-spf.cd-tipo-integr:
          cb_entrada:ADD-LAST(es-api-param-spf.des-tipo-integr,  es-api-param-spf.cd-tipo-integr).
      END.
      cb_entrada:ADD-LAST("Nenhum", 0).
      
      //cb_saida:DELETE(1) NO-ERROR.
      FOR EACH es-api-param-spf NO-LOCK 
          WHERE es-api-param-spf.ind-tipo-trans = 2
          BREAK BY es-api-param-spf.cd-tipo-integr:
          cb_saida:ADD-LAST(es-api-param-spf.des-tipo-integr,  es-api-param-spf.cd-tipo-integr).
      END.
      cb_saida:ADD-LAST("Nenhum", 0).

    
    /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

    ASSIGN  chCtrlFrame:pSTimer:interval = 30000
            toAs:CHECKED = YES
            .

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

