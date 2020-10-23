&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VAR i-status AS INT NO-UNDO.
DEFINE VAR i-contador AS INT NO-UNDO.

DEFINE QUERY q-dados FOR testeJson.

DEFINE TEMP-TABLE tt-json
    FIELD id LIKE testeJson.id
    FIELD produto LIKE testeJson.produto
    FIELD quantidade LIKE testeJson.quantidade
    FIELD unMedida LIKE testeJson.unMedida
    FIELD valor LIKE testeJson.valor
    FIELD codigo LIKE testeJson.codigo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME h_b-browser

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-json

/* Definitions for BROWSE h_b-browser                                   */
&Scoped-define FIELDS-IN-QUERY-h_b-browser tt-json.id tt-json.produto tt-json.quantidade tt-json.unMedida tt-json.valor tt-json.codigo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-h_b-browser   
&Scoped-define SELF-NAME h_b-browser
&Scoped-define QUERY-STRING-h_b-browser FOR EACH tt-json
&Scoped-define OPEN-QUERY-h_b-browser OPEN QUERY h_b-browser FOR EACH tt-json.
&Scoped-define TABLES-IN-QUERY-h_b-browser tt-json
&Scoped-define FIRST-TABLE-IN-QUERY-h_b-browser tt-json


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-h_b-browser}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 btn-nav-first btn-nav-prev ~
btn-nav-next btn-nav-last btn-add btn-edit btn-nav-trash btn-confirmar ~
btn-cancelar btn-imprimir btn-sair h_b-browser tbn-gera-relatorio 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_v-viewer-2 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-add 
     LABEL "Add" 
     SIZE 4 BY 1.13.

DEFINE BUTTON btn-cancelar 
     LABEL "Cancelar" 
     SIZE 9 BY 1.13.

DEFINE BUTTON btn-confirmar 
     LABEL "Confirmar" 
     SIZE 9 BY 1.13.

DEFINE BUTTON btn-edit 
     LABEL "Edit" 
     SIZE 4 BY 1.13.

DEFINE BUTTON btn-imprimir 
     LABEL "Imprimir" 
     SIZE 9 BY 1.13.

DEFINE BUTTON btn-nav-first 
     LABEL "<<-" 
     SIZE 4 BY 1.13.

DEFINE BUTTON btn-nav-last 
     LABEL "->>" 
     SIZE 4 BY 1.13.

DEFINE BUTTON btn-nav-next 
     LABEL "->" 
     SIZE 4 BY 1.13.

DEFINE BUTTON btn-nav-prev 
     LABEL "<-" 
     SIZE 4 BY 1.13.

DEFINE BUTTON btn-nav-trash 
     LABEL "Trash" 
     SIZE 5 BY 1.13.

DEFINE BUTTON btn-sair 
     LABEL "Sair" 
     SIZE 6 BY 1.13.

DEFINE BUTTON tbn-gera-relatorio 
     LABEL "Gerar Relatorio" 
     SIZE 15 BY 1.13.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 1.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY h_b-browser FOR 
      tt-json SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE h_b-browser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS h_b-browser W-Win _FREEFORM
  QUERY h_b-browser DISPLAY
      tt-json.id WIDTH 8
 tt-json.produto WIDTH 15
 tt-json.quantidade WIDTH 8
 tt-json.unMedida WIDTH 15
 tt-json.valor WIDTH 10
 tt-json.codigo WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 8.25 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btn-nav-first AT ROW 1.25 COL 2 WIDGET-ID 4
     btn-nav-prev AT ROW 1.25 COL 6 WIDGET-ID 10
     btn-nav-next AT ROW 1.25 COL 11 WIDGET-ID 6
     btn-nav-last AT ROW 1.25 COL 15 WIDGET-ID 8
     btn-add AT ROW 1.25 COL 25 WIDGET-ID 12
     btn-edit AT ROW 1.25 COL 29 WIDGET-ID 14
     btn-nav-trash AT ROW 1.25 COL 33 WIDGET-ID 16
     btn-confirmar AT ROW 1.25 COL 38.72 WIDGET-ID 22
     btn-cancelar AT ROW 1.25 COL 47.72 WIDGET-ID 24
     btn-imprimir AT ROW 1.25 COL 64 WIDGET-ID 18
     btn-sair AT ROW 1.25 COL 73 WIDGET-ID 20
     h_b-browser AT ROW 10 COL 1 WIDGET-ID 200
     tbn-gera-relatorio AT ROW 18.5 COL 65 WIDGET-ID 26
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.72 BY 19.21 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 19.13
         WIDTH              = 79.72
         MAX-HEIGHT         = 29.38
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 29.38
         VIRTUAL-WIDTH      = 195.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB h_b-browser btn-sair F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE h_b-browser
/* Query rebuild information for BROWSE h_b-browser
     _START_FREEFORM
OPEN QUERY h_b-browser FOR EACH tt-json.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE h_b-browser */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-add W-Win
ON CHOOSE OF btn-add IN FRAME F-Main /* Add */
DO:
    ASSIGN
        i-status = 1.
    
    RUN local-enable-fields IN h_v-viewer-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancelar W-Win
ON CHOOSE OF btn-cancelar IN FRAME F-Main /* Cancelar */
DO:
  RUN local-disable-fields IN h_v-viewer-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-confirmar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-confirmar W-Win
ON CHOOSE OF btn-confirmar IN FRAME F-Main /* Confirmar */
DO:
    IF i-status = 1 THEN
    DO:
        RUN add-fields IN h_v-viewer-2.
        RUN local-disable-fields IN h_v-viewer-2.
        
        FOR EACH testeJson NO-LOCK.
            CREATE tt-json.
            ASSIGN
                tt-json.id = testeJson.id
                tt-json.produto = testeJson.produto
                tt-json.quantidade = testeJson.quantidade
                tt-json.unMedida = testeJson.unMedida
                tt-json.valor = testeJson.valor
                tt-json.codigo = testeJson.codigo.
        END.
        ELSE
            MESSAGE "ELSE FOR" VIEW-AS ALERT-BOX.
        {&open-query-h_b-browser}
    END.
        
    ELSE 
        IF i-status = 2 THEN
            MESSAGE "Status: Edit" VIEW-AS ALERT-BOX.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-edit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-edit W-Win
ON CHOOSE OF btn-edit IN FRAME F-Main /* Edit */
DO:
    ASSIGN
        i-status = 2.
    
    //RUN local-enable-fields IN h_v-viewer-2.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-sair W-Win
ON CHOOSE OF btn-sair IN FRAME F-Main /* Sair */
DO:
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME h_b-browser
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */


{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
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
             INPUT  'v-viewer.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-viewer-2 ).
       RUN set-position IN h_v-viewer-2 ( 2.50 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.75 , 79.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-viewer-2 ,
             btn-sair:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  ENABLE RECT-1 btn-nav-first btn-nav-prev btn-nav-next btn-nav-last btn-add 
         btn-edit btn-nav-trash btn-confirmar btn-cancelar btn-imprimir 
         btn-sair h_b-browser tbn-gera-relatorio 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-json"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

