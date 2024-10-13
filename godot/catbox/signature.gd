@tool
class_name CatboxSignature
extends RefCounted

var name: String
var type: String

static func from_dict(dict: Dictionary) -> CatboxSignature:
	var result := CatboxSignature.new()
	result.name = dict["name"]
	result.type = dict["type"]
	return result

func to_dict() -> Dictionary:
	return {
		"name": name,
		"type": type
	}
