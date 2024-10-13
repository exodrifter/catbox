class_name CatboxFunction
extends RefCounted

var function_name: String
var inputs: Array[CatboxSignature]
var outputs: Array[CatboxSignature]
var variable_inputs: bool
var variable_outputs: bool

static func from_dict(dict: Dictionary) -> CatboxFunction:
	var result := CatboxFunction.new()
	result.function_name = dict["name"]
	for a in dict["inputs"]:
		result.inputs.push_back(CatboxSignature.from_dict(a))
	for a in dict["outputs"]:
		result.outputs.push_back(CatboxSignature.from_dict(a))
	result.variable_inputs = dict["variable_inputs"]
	result.variable_outputs = dict["variable_outputs"]
	return result

func to_dict() -> Dictionary:
	var input_dicts: Array[Dictionary] = []
	for a in inputs:
		input_dicts.push_back(a.to_dict())

	var output_dicts: Array[Dictionary] = []
	for a in outputs:
		output_dicts.push_back(a.to_dict())

	return {
		"function": function_name,
		"inputs": input_dicts,
		"outputs": output_dicts,
		"variable_inputs": variable_inputs,
		"variable_outputs": variable_outputs,
	}
