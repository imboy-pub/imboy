
# New make commands for efmt
fmt:
	# Format files.
	./efmt --print-width 72 -w ./src/*.erl
