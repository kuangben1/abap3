*&---------------------------------------------------------------------*
*& Report  ZFIR030
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report zfir030.
include zfir030_data.
include zfir030_form.
include zfir030_alv.

start-of-selection.
  perform get_partner_data.
  perform get_ranl_data.
  perform get_alv_data.

end-of-selection.
  perform build_alv.
  perform display_alv.
