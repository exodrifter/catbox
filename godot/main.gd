extends GraphEdit

const CATBOX_GRAPH_NODE = preload("res://catbox_node.tscn")

@export_multiline var api_json: String

@export_multiline var graph_json: String

var api: CatboxApi
var graph: CatboxGraph
var nodes: Dictionary

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
	api = CatboxApi.from_json(api_json)
	graph = CatboxGraph.from_json(graph_json)

	# Special input node
	var input_function := CatboxFunction.new()
	input_function.function_name = "in"
	input_function.variable_outputs = true

	var input_node = CATBOX_GRAPH_NODE.instantiate()
	input_node.name = "in"
	input_node.parent_graph = graph
	input_node.function = input_function
	for input in graph.inputs:
		input_node.add_slot(input.name, input.type, CatboxSlot.SlotType.OutputSlot)
	add_child(input_node)
	nodes["in"] = input_node

	# Special output node
	var output_function := CatboxFunction.new()
	output_function.function_name = "out"
	output_function.variable_inputs = true

	var output_node = CATBOX_GRAPH_NODE.instantiate()
	output_node.name = "out"
	output_node.parent_graph = graph
	output_node.function = output_function
	for output in graph.outputs:
		output_node.add_slot(output.name, output.type, CatboxSlot.SlotType.InputSlot)
	add_child(output_node)
	nodes["out"] = output_node

	# Add nodes
	for n in graph.nodes:
		var node: CatboxGraphNode = CATBOX_GRAPH_NODE.instantiate()
		node.name = n.id
		node.parent_graph = graph
		node.function = api.get_function(n.function_name)
		add_child(node)
		nodes[n.id] = node

	# Add parameters
	for param in graph.parameters:
		match param.source.type:
			"connection":
				var to = resolve_key(param.key, CatboxSlot.SlotType.InputSlot)
				var from = resolve_key(param.source.value, CatboxSlot.SlotType.OutputSlot)
				connect_node(
					from.id,
					from.port,
					to.id,
					to.port
				)
			"constant":
				var info = param.key.split(".")
				for slot in nodes[info[0]].slots:
					if slot.slot_type == CatboxSlot.SlotType.InputSlot and slot.slot_name == info[1]:
						slot.value = param.source.value.value
						break

	# If we don't wait, the graph ends up being spread out a lot more than it
	# would be if the user pressed the arrange nodes button.
	await get_tree().create_timer(0).timeout
	arrange_nodes()

func resolve_key(key: String, type: CatboxSlot.SlotType) -> Dictionary:
	var info = key.split(".")
	var slots: Array[CatboxSlot] = nodes[info[0]].slots
	var index = -1
	for i in slots.size():
		var slot = slots[i]
		if slot.slot_name == info[1] and slot.slot_type == type:
			index = i

			var count = 0
			if type == CatboxSlot.SlotType.OutputSlot:
				for s in slots:
					if s.slot_type == CatboxSlot.SlotType.InputSlot:
						count += 1
			index -= count
			break

	return {
		"id": info[0],
		"port": index,
	}

func _on_connection_request(from_node: StringName, from_port: int, to_node: StringName, to_port: int) -> void:
	# Update connection in data model
	var from: CatboxGraphNode = nodes[from_node]
	var from_slot: CatboxSlot = from.slots[from.get_input_port_slot(from_port)]
	var to: CatboxGraphNode = nodes[to_node]
	var to_slot: CatboxSlot = to.slots[to.get_input_port_slot(to_port)]
	var param_to_update: CatboxParameter = null
	for i in graph.parameters.size():
		var param = graph.parameters[i]
		if param.key == to_node + "." + to_slot.slot_name:
			param_to_update = param
			break
	if param_to_update == null:
		param_to_update = CatboxParameter.new()
		param_to_update.key = to_node + "." + to_slot.slot_name
		graph.parameters.push_back(param_to_update)
	param_to_update.source = CatboxSource.new()
	param_to_update.source.type = "connection"
	param_to_update.source.value = from_node + "." + from_slot.slot_name

	# Remove old connection from graph editor
	for c in get_connection_list():
		if c.to_node == to_node and c.to_port == to_port:
			disconnect_node(c.from_node, c.from_port, c.to_node, c.to_port)

	# Create new connection in graph editor
	connect_node(from_node, from_port, to_node, to_port)

func _on_disconnection_request(from_node: StringName, from_port: int, to_node: StringName, to_port: int) -> void:
	# Remove connection from data model
	var to: CatboxGraphNode = nodes[to_node]
	var to_slot: CatboxSlot = to.slots[to.get_input_port_slot(to_port)]
	var param_to_remove: int = -1
	for i in graph.parameters.size():
		var param = graph.parameters[i]
		if param.key == to_node + "." + to_slot.slot_name:
			param_to_remove = i
			break
	if param_to_remove != -1:
		graph.parameters.remove_at(param_to_remove)

	# Remove connection from graph editor
	disconnect_node(from_node, from_port, to_node, to_port)
