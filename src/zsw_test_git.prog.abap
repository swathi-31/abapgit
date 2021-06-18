*&---------------------------------------------------------------------*
*& Report ZSW_TESTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSW_TEST_GIT.

DATA: lv_trkorr TYPE trkorr.

PARAMETERS: p_trkorr TYPE trkorr.

START-OF-SELECTION.

  SELECT SINGLE trkorr FROM e070 INTO lv_trkorr WHERE trkorr EQ p_trkorr.

  IF sy-subrc EQ 0.

    WRITE 'Transport Exists'.

  ELSE.

    WRITE 'Transport does not Exists'.

  ENDIF.

*----------------------------------------------------------------------*
*       CLASS lcl_check DEFINITION
*----------------------------------------------------------------------*

CLASS lcl_check DEFINITION.
  PUBLIC SECTION.
    METHODS: check IMPORTING iv_trkorr TYPE trkorr
                 RETURNING value(rv_result) TYPE char1.
ENDCLASS.                    "lcl_check DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_sum IMPLEMENTATION
*----------------------------------------------------------------------*

CLASS lcl_check IMPLEMENTATION.
  METHOD check.

    SELECT SINGLE trkorr FROM e070 INTO lv_trkorr WHERE trkorr EQ iv_trkorr.

    IF sy-subrc EQ 0.

      rv_result = abap_true.

    ENDIF.

  ENDMETHOD.                    "check
ENDCLASS.                    "lcl_check IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration   Short
.
  PUBLIC SECTION.
    METHODS: m_check FOR TESTING.
ENDCLASS.                    "lcl_test DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_test IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
  METHOD m_check.
    DATA: lc_check TYPE REF TO lcl_check.
    DATA: lv_result TYPE char1.

    CREATE OBJECT lc_check.
    lv_result = lc_check->check('S7HK902197')."EXPORTING iv_trkorr = p_trkorr ).

    cl_aunit_assert=>assert_equals(
        exp                  = abap_true
        act                  = lv_result
        msg                  = 'Transport does not exist'
           ).
  ENDMETHOD.                    "m_check

ENDCLASS.                    "lcl_test IMPLEMENTATION
