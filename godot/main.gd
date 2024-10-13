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
	input_node.function = input_function
	for input in graph.inputs:
		input_node.add_slot(input.name, input.type, CatboxSlot.SlotType.OutputSlot)
	add_child(input_node)

	# Add nodes
	for n in graph.nodes:
		var node: CatboxNode = CATBOX_NODE.instantiate()
		node.function = functions[n.function]
		add_child(node)

	# Add connections
	for n in graph.nodes:
		for param in n.parameters:
			match param.source.type:
				"connection":
					var connection_info = param.source.value.split(".")
					var from_node: StringName = ""
					var from_port: int = -1
					for o in graph.nodes:
						if o.id == connection_info[0]:
							from_node = o.function
							from_port = functions[o.function].output_names.find(connection_info[1])
					print(connection_info[0], " # ", from_node, " ", from_port, " ", n.function, " ", functions[n.function].input_names.find(param.name))
					var err = connect_node(
						from_node,
						from_port,
						n.function,
						functions[n.function].input_names.find(param.name)
					)
					print("!! ", err)
				"constant":
					pass # TODO

	# Special output node
	var output_function := FunctionResource.new()
	output_function.function = "out"
	output_function.variable_inputs = true

	output_node = CATBOX_NODE.instantiate()
	output_node.function = output_function
	# TODO: load outputs
	add_child(output_node)

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

func _on_connection_request(from_node: StringName, from_port: int, to_node: StringName, to_port: int) -> void:
	print(from_node, " ", from_port, " ", to_node, " ", to_port)
