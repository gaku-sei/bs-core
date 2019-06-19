#if OCAML_VERSION =~ "<4.03.0" then
type ('ok, 'error) result =
  | Ok of 'ok
  | Error of 'error
#end