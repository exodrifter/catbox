@tool
class_name CatboxParameter
extends RefCounted

var key: String
var source: CatboxSource

static func from_dict(dict: Dictionary) -> CatboxParameter:
	var result := CatboxParameter.new()
	result.key = dict["key"]
	result.source = CatboxSource.from_dict(dict["source"])
	return result

func to_dict() -> Dictionary:
	return {
		"key": key,
		"source": source.to_dict()
	}
