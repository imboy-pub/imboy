
# New make commands for efmt
# https://github.com/sile/efmt/releases
efmt:
	# Format files.
	./efmt -w ./src/*.erl
	./efmt -w ./apps/*/src/*.erl


# make gen-appup OLD_VERSION=0.6.1 PROJECT_VERSION=0.6.2
.PHONY: gen-appup
gen-appup:
	bash script/gen_appup.sh $(OLD_VERSION) $(PROJECT_VERSION)

# Node runner with all customizable parameters
#
# make start node=node1 port=9801 cookie=imboycookie exclude="app1,app2" daemon=daemon
# make start node=node2 port=9802 cookie=imboycookie exclude="imadm,imcron" daemon=daemon
.PHONY: start
start:
	./script/start_node.sh $(node) $(cookie) $(port) $(exclude) $(daemon)

# Stop running node
# Example: make stop node=node1
stop:
	./script/stop_node.sh $(node)
