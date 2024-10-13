class_name CatboxApi
extends RefCounted

var functions: Array[CatboxFunction]

func get_function(function_name: String) -> CatboxFunction:
	for function in functions:
		if function.function_name == function_name:
			return function
	return

static func from_json(json: String) -> CatboxApi:
	var result := CatboxApi.new()
	for a in JSON.parse_string(json):
		result.functions.push_back(CatboxFunction.from_dict(a))
	return result

func to_json() -> String:
	var function_dicts: Array[Dictionary] = []
	for a in functions:
		function_dicts.push_back(a.to_dict())
	return JSON.stringify(function_dicts)
