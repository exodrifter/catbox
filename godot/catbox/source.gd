@tool
class_name CatboxSource
extends RefCounted

var type: String
var value: Variant

static func from_dict(dict: Dictionary) -> CatboxSource:
	var result := CatboxSource.new()
	result.type = dict["type"]
	result.value = dict["value"]
	return result

func to_dict() -> Dictionary:
	return {
		"type": type,
		"value": value
	}
