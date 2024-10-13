class_name FunctionResource
extends Resource

@export var function: String
@export var input_names: Array[String]
@export var input_types: Array[String]
@export var output_names: Array[String]
@export var output_types: Array[String]
@export var variable_inputs: bool
@export var variable_outputs: bool

var input_count: int:
	get:
		return mini(input_names.size(), input_types.size())

var output_count: int:
	get:
		return mini(output_names.size(), output_types.size())
