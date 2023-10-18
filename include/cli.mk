
# New make commands for efmt
# https://github.com/sile/efmt/releases
efmt:
	# Format files.
	./efmt -w ./src/*.erl
	./efmt -w ./apps/*/src/*.erl
