@del *.cmi
@del *.cmo
del quendor.exe
ocamlc -g -c type.ml
ocamlc -g -c utility.ml
ocamlc -g -c immutable_bytes.ml
ocamlc -g -c story.ml
ocamlc -g -c zstring.ml
ocamlc -g -c dictionary.ml
ocamlc -g -c object.ml
ocamlc -g -c instruction.ml
ocamlc -g -o quendor.exe type.cmo utility.cmo immutable_bytes.cmo story.cmo zstring.cmo dictionary.cmo object.cmo instruction.cmo quendor.ml
ocamldebug quendor.exe
@del *.cmi
@del *.cmo
del quendor.exe