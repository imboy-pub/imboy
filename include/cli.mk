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
