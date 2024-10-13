@tool
class_name CatboxNode
extends RefCounted

var id: String
var function_name: String

static func from_dict(dict: Dictionary) -> CatboxNode:
	var result := CatboxNode.new()
	result.id = dict["id"]
	result.function_name = dict["function"]
	return result

func to_dict() -> Dictionary:
	return {
		"id": id,
		"function": function_name
	}
