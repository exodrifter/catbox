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
