
all: meld

meld:
	buildapp --asdf-path $(PWD) \
		--load-system cl-meld \
		--eval '(defun main (args) (cl-meld:meld-compile (second args) (third args)))' \
		--entry main \
		--output meld

clean:
	rm -f meld
