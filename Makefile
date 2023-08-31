.PHONY: clean, stream

clean:
	rm -rf *.o
	rm -rf *.cmi
	rm -rf *.cmx
	rm -rf *.cmo
	rm -rf a.out

build:
	ocamlfind opt -package base,angstrom,alcotest,lwt,lwt.unix,lwt_domain,ounit2 -thread -linkpkg $(file).ml
	./a.out
	

.DEFAULT_GOAL := clean