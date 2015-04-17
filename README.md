# talaria
Talaria allows you to build a distributed network for information exchange without special knowledge and skills

Network based on the "nodes" - they can connect with each other and exchange information. Members can connect only to the "nodes". Connected together "nodes" form a network segment. Inside network segment operates automatic routing and broadcast messages. Each "node" can have miltiple child clients - "points".



Network members addressing done by network address in form of X.Y, where X is "node" indentifier and Y is "point" indentifier. Also, every member have globally unique indentifier (GUID), that not related to network address, but can be used as his synonym. Possible direct addressing up to 232 "nodes" and up to 232 "points" on every "node".

The basic unit of network information exchange is "net message". Net message consist of header, parameters section and data section. Header contains messgae type, source and target addresses, timestamp. Parameters section contain random text parameters in form of name=value. Data section store any arbitrary data. When message transfered by nodes, in tail of message added "seen-by" - transit nodes indentifiers.
