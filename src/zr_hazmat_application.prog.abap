************************************************************************************************************************
* Copyright 2024 Google LLC                                                                                            *
* ABAP SDK for Google Cloud is made available as "Software" under the agreement governing your use of                  *
* Google Cloud Platform including the Service Specific Terms available at                                              *
*                                                                                                                      *
* https://cloud.google.com/terms/service-terms                                                                         *
*                                                                                                                      *
* Without limiting the generality of the above terms, you may not modify or distribute ABAP SDK for Google Cloud       *
* without express written permission from Google.                                                                      *
************************************************************************************************************************

REPORT zr_hazmat_application.

INCLUDE zi_hazmat_application_sel.
INCLUDE zi_hazmat_application_cls.

INITIALIZATION.
  DATA(lo_hazmat_application) = NEW lcl_hazmat_application( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_scen.
  p_scen = lo_hazmat_application->get_scenario_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_prod.
  p_prod = lo_hazmat_application->get_product_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_prom.
  p_prom = lo_hazmat_application->get_prompt_f4( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_imag.
  p_imag = lo_hazmat_application->get_file_name_f4( ).

AT SELECTION-SCREEN OUTPUT.
  lo_hazmat_application->validate( ).

START-OF-SELECTION.
  lo_hazmat_application->execute( ).
