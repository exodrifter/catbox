@tool
class_name CatboxGraph
extends RefCounted

var inputs: Array[CatboxSignature]
var outputs: Array[CatboxSignature]
var nodes: Array[CatboxNode]
var parameters: Array[CatboxParameter]

static func from_json(json: String) -> CatboxGraph:
	return CatboxGraph.from_dict(JSON.parse_string(json))

static func from_dict(dict: Dictionary) -> CatboxGraph:
	var result := CatboxGraph.new()
	for a in dict["inputs"]:
		result.inputs.push_back(CatboxSignature.from_dict(a))
	for a in dict["outputs"]:
		result.outputs.push_back(CatboxSignature.from_dict(a))
	for a in dict["nodes"]:
		result.nodes.push_back(CatboxNode.from_dict(a))
	for a in dict["parameters"]:
		result.parameters.push_back(CatboxParameter.from_dict(a))
	return result

func to_json() -> String:
	return JSON.stringify(self.to_dict())

func to_dict() -> Dictionary:
	var input_dicts: Array[Dictionary] = []
	for a in inputs:
		input_dicts.push_back(a.to_dict())

	var output_dicts: Array[Dictionary] = []
	for a in outputs:
		output_dicts.push_back(a.to_dict())

	var node_dicts: Array[Dictionary] = []
	for a in nodes:
		node_dicts.push_back(a.to_dict())

	var parameter_dicts: Array[Dictionary] = []
	for a in parameters:
		parameter_dicts.push_back(a.to_dict())

	return {
		"inputs": input_dicts,
		"outputs": output_dicts,
		"nodes": node_dicts,
		"parameters": parameter_dicts,
	}
