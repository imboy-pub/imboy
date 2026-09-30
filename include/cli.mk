
# New make commands for efmt
# https://github.com/sile/efmt/releases
efmt:
	# Format files.
	./efmt -w ./src/*.erl
	./efmt -w ./apps/*/src/*.erl


# Node runner with all customizable parameters
#
# make start node=node1 port=9801 cookie=<your-cookie> exclude="app1,app2" daemon=daemon
# make start node=node2 port=9802 cookie=<your-cookie> exclude="imadm,imcron" daemon=daemon
.PHONY: start
start:
	./scripts/start_node.sh $(node) $(cookie) $(port) $(exclude) $(daemon)

# Stop running node
# Example: make stop node=node1
stop:
	./scripts/stop_node.sh $(node)

# Clean all .beam files recursively
# Example: make clean-beam
.PHONY: clean-beam
clean-beam:
	find . -type f -name "*.beam" -delete
