@del *.cmi
@del *.cmo
ocamlc -g -c type.ml
ocamlc -g -c utility.ml
ocaml type.cmo utility.cmo quendor.ml
@del *.cmi
@del *.cmo