*&---------------------------------------------------------------------*
*& Report  ZTEMP3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_hello_salv_treee.


FIELD-SYMBOLS: <fs_line> TYPE spfli.
INCLUDE <icon>.

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      go_alv_tree     TYPE REF TO cl_salv_tree,
      gt_empty_tab    TYPE STANDARD TABLE OF spfli,
      gt_full_tab     TYPE STANDARD TABLE OF spfli.
    CLASS-METHODS:
      create_tree,    " Создаем и настраиваем объект ALV
      get_data,       " Получение данных из БД
      setup_functions," Настройка тулбара
      create_nodes,   " Заполнение дерева данными (создание иерархии)
      setup_columns,  " Настройка колонок
      on_link_click FOR EVENT link_click OF cl_salv_events_tree IMPORTING node_key,
      setup_events,
      show_tree.      " Отображение (обновление) ALV дерева
ENDCLASS.                    "lcl_report DEFINITION

START-OF-SELECTION.
  lcl_report=>get_data( ).
  lcl_report=>create_tree( ).
  lcl_report=>create_nodes( ).
  lcl_report=>setup_functions( ).
  lcl_report=>setup_columns( ).
  lcl_report=>setup_events( ).
  lcl_report=>show_tree( ).

CLASS lcl_report IMPLEMENTATION.
  METHOD show_tree.
    go_alv_tree->display( ).
  ENDMETHOD.                    "show_tree

  METHOD create_nodes.
    DATA:
      lo_nodes  TYPE REF TO cl_salv_nodes,
      lo_node   TYPE REF TO cl_salv_node,
      lv_carrid_key TYPE lvc_nkey,
      lv_expand_icon TYPE salv_de_tree_image,
      lv_collapse_icon TYPE salv_de_tree_image,
      lv_hier_icon    TYPE salv_de_tree_image,
      lo_item     TYPE REF TO cl_salv_item,
      lv_count TYPE i,
      lo_settings TYPE REF TO cl_salv_tree_settings,
      lv_carrid TYPE spfli-carrid.

    lv_expand_icon = icon_closed_folder.
    lv_collapse_icon = icon_open_folder.
    lv_hier_icon = icon_tree.

    lo_settings = go_alv_tree->get_tree_settings( ).
    lo_settings->set_hierarchy_header( 'Иерархия' ).
    lo_settings->set_hierarchy_size( 30 ).
    lo_settings->set_hierarchy_icon( lv_hier_icon ).

    " Получаем ссылку на экземпляр класса упрвления узлами дерева
    lo_nodes = go_alv_tree->get_nodes( ).
    SORT gt_full_tab BY carrid.
    " Заполняем дерево
    LOOP AT gt_full_tab ASSIGNING <fs_line>.
      IF lv_carrid <> <fs_line>-carrid.
        TRY.
            lv_count = 0.
            LOOP AT gt_full_tab TRANSPORTING NO FIELDS WHERE carrid = <fs_line>-carrid.
              lv_count = lv_count + 1.
            ENDLOOP.
            IF lv_count > 1.
              lo_node = lo_nodes->add_node(
                                    related_node   = ''
                                    relationship   = cl_gui_column_tree=>relat_last_child
                                    data_row       = <fs_line>
                                    collapsed_icon = lv_expand_icon
                                    expanded_icon  = lv_collapse_icon
                                    row_style      = if_salv_c_tree_style=>emphasized_a
                                    text           = 'Родительский узел' ).
            ELSE.
              lo_node = lo_nodes->add_node(
                                    related_node   = ''
                                    relationship   = cl_gui_column_tree=>relat_last_child
                                    data_row       = <fs_line>
                                    collapsed_icon = lv_collapse_icon
                                    row_style      = if_salv_c_tree_style=>emphasized_a
                                    text           = 'Родительский узел' ).
            ENDIF.

            lv_carrid_key = lo_node->get_key( ).
            IF lv_count > 1.
              lo_item = lo_node->get_item( 'CARRID' ).
              lo_item->set_type(  if_salv_c_item_type=>button ).
              lo_item->set_value( 'Все' ).
            ENDIF.

            lo_item = lo_node->get_item( 'CITYFROM' ).
            lo_item->set_font( if_salv_c_item_font=>fixed_size ).
            lo_item->set_enabled( abap_false ).
          CATCH cx_salv_msg.
        ENDTRY.
      ELSE.
        TRY.
            lo_node = lo_nodes->add_node(
                                  related_node   = lv_carrid_key
                                  relationship   = cl_gui_column_tree=>relat_last_child
                                  data_row       = <fs_line>
                                  row_style      = if_salv_c_tree_style=>emphasized_negative
                                  text           = 'Дочерний узел - Дочерний узел' ).
            lo_item = lo_node->get_item( 'CARRID' ).
            lo_item->set_type(  if_salv_c_item_type=>checkbox ).
            lo_item->set_editable( abap_true ).
          CATCH cx_salv_msg.
        ENDTRY.
      ENDIF.
      lv_carrid = <fs_line>-carrid.
    ENDLOOP.
  ENDMETHOD.                    "create_nodes

  METHOD get_data.
    SELECT * FROM spfli INTO CORRESPONDING FIELDS OF TABLE gt_full_tab.
  ENDMETHOD.                    "get_data

  METHOD create_tree.
    TRY.
        cl_salv_tree=>factory(
          IMPORTING
            r_salv_tree = go_alv_tree
          CHANGING
            t_table     = gt_empty_tab ).
      CATCH cx_salv_error.
        MESSAGE 'Ошибка при создании объекта ALV' TYPE 'E'.
    ENDTRY.
  ENDMETHOD.                    "create_tree

  METHOD setup_functions.
    DATA: lo_functions TYPE REF TO cl_salv_functions_tree.
    lo_functions = go_alv_tree->get_functions( ).
    lo_functions->set_all( ).
  ENDMETHOD.

  METHOD setup_columns.
    DATA:
      lo_columns TYPE REF TO cl_salv_columns_tree,
      lo_column  TYPE REF TO cl_salv_column_tree.

    TRY.
        lo_columns = go_alv_tree->get_columns( ).
        lo_columns->set_optimize( abap_true ).
        lo_column ?= lo_columns->get_column( 'MANDT' ).
        lo_column->set_technical( abap_true ).
        lo_column ?= lo_columns->get_column( 'FLTYPE' ).
        lo_column->set_technical( abap_true ).
        lo_column ?= lo_columns->get_column( 'PERIOD' ).
        lo_column->set_technical( abap_true ).
      CATCH cx_salv_msg cx_salv_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD on_link_click.
    DATA: lo_nodes      TYPE REF TO cl_salv_nodes,
          lo_main_node  TYPE REF TO cl_salv_node,
          lo_item       TYPE REF TO cl_salv_item,
          lo_child_node TYPE REF TO cl_salv_node.

    lo_nodes = go_alv_tree->get_nodes( ).

    TRY.
        lo_main_node  = lo_nodes->get_node( node_key ).
        lo_child_node = lo_main_node->get_first_child( ).
        WHILE lo_child_node IS BOUND.
          lo_item = lo_child_node->get_item( 'CARRID' ).

          IF lo_item->is_checked( ) = abap_true.
            lo_item->set_checked( abap_false ).
          ELSE.
            lo_item->set_checked( abap_true ).
          ENDIF.

          lo_child_node = lo_child_node->get_next_sibling( ).
        ENDWHILE.
      CATCH cx_salv_msg.
        EXIT.
    ENDTRY.

  ENDMETHOD.

  METHOD setup_events.
    DATA: lo_events TYPE REF TO cl_salv_events_tree.

    lo_events = go_alv_tree->get_event( ).
    SET HANDLER on_link_click FOR lo_events.
  ENDMETHOD.

ENDCLASS.                    "lcl_report IMPLEMENTATION
