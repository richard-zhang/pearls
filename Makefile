.PHONY: clean

clean:
	rm -rf **/*.o
	rm -rf **/*.cmi
	rm -rf **/*.cmx
	rm -rf **/*.cmo
	rm -rf **/a.out

.DEFAULT_GOAL := clean