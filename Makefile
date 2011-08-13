JS=pong.js
HTML=index.html

all : $(JS) $(HTML)

index.html : index.haml
	haml index.haml > index.html

pong.js : pong.ml
	ocamlbuild $(JS); ln -s _build/$(JS) .	

clean:
	ocamlbuild -clean
	rm -f index.html
