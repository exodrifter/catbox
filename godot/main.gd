extends GraphEdit

const CATBOX_NODE = preload("res://catbox_node.tscn")

@export_multiline var api_json: String

@export_multiline var graph_json: String

var functions: Dictionary

var input_node: CatboxNode
var nodes: Dictionary
var output_node: CatboxNode

# Called when the node enters the scene tree for the first time.
func _ready() -> void:
	for function_resource in from_api_json(api_json):
		functions[function_resource.function] = function_resource

	var graph = JSON.parse_string(graph_json)

	# Special input node
	var input_function := FunctionResource.new()
	input_function.function = "in"
	input_function.variable_outputs = true

	input_node = CATBOX_NODE.instantiate()
	input_node.name = "in"
	input_node.function = input_function
	for input in graph.inputs:
		input_node.add_slot(input.name, input.type, CatboxSlot.SlotType.OutputSlot)
	add_child(input_node)
	nodes["in"] = input_node

	# Special output node
	var output_function := FunctionResource.new()
	output_function.function = "out"
	output_function.variable_inputs = true

	output_node = CATBOX_NODE.instantiate()
	output_node.name = "out"
	output_node.function = output_function
	for output in graph.outputs:
		output_node.add_slot(output.name, output.type, CatboxSlot.SlotType.InputSlot)
	add_child(output_node)
	nodes["out"] = output_node

	# Add nodes
	for n in graph.nodes:
		var node: CatboxNode = CATBOX_NODE.instantiate()
		node.name = n.id
		node.function = functions[n.function]
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
				pass # TODO

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

static func from_api_json(json: String) -> Array[FunctionResource]:
	var data = JSON.parse_string(json)
	var results: Array[FunctionResource] = []
	for item in data:
		var function := FunctionResource.new()
		function.function = item.name
		for key in item.inputs.keys():
			function.input_names.push_back(key)
			function.input_types.push_back(item.inputs[key])
		for key in item.outputs.keys():
			function.output_names.push_back(key)
			function.output_types.push_back(item.outputs[key])
		function.variable_inputs = item.variable_inputs
		function.variable_outputs = item.variable_outputs
		results.push_back(function)
	return results
