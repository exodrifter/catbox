## Responsible for representing a node in a function graph
class_name CatboxGraphNode
extends GraphNode

const SLOT = preload("res://catbox_slot.tscn")

var parent_graph: CatboxGraph
var function: CatboxFunction

var slots: Array[CatboxSlot]

@onready var slot_editor: HBoxContainer = $CatboxSlotEditor

func _enter_tree() -> void:
	if not is_instance_valid(function):
		return

	title = function.function_name

	for input in function.inputs:
		add_slot(
			input.name,
			input.type,
			CatboxSlot.SlotType.InputSlot
		)

	for output in function.outputs:
		add_slot(
			output.name,
			output.type,
			CatboxSlot.SlotType.OutputSlot
		)

	for i in slots.size():
		var slot = slots[i]
		match slot.slot_type:
			CatboxSlot.SlotType.InputSlot:
				set_slot_enabled_left(i, true)
				set_slot_enabled_right(i, false)
			CatboxSlot.SlotType.OutputSlot:
				set_slot_enabled_left(i, false)
				set_slot_enabled_right(i, true)

func _process(_delta: float) -> void:
	move_child(slot_editor, -1)
	slot_editor.visible = function.variable_inputs || function.variable_outputs
	slot_editor.variable_inputs = function.variable_inputs
	slot_editor.variable_outputs = function.variable_outputs

	if name == "in" or name == "out":
		for slot in slots:
			slot.hide_editor = true
	else:
		for slot in slots:
			if slot.slot_type == CatboxSlot.SlotType.InputSlot:
				var p = parent_graph.get_parameter(name + "." + slot.slot_name)
				slot.hide_editor = p.source.type == "connection"

	reset_size()

func add_slot(slot_name: String, value_type: String, slot_type: CatboxSlot.SlotType) -> void:
	# Don't create duplicate slots
	for slot in slots:
		if slot.slot_type == slot_type and slot.slot_name == slot_name:
			return

	var slot: CatboxSlot = SLOT.instantiate()
	slot.catbox_node = self
	slot.slot_name = slot_name
	slot.slot_type = slot_type
	slot.value_type = value_type
	add_child(slot)

	var i = slots.size()
	slots.push_back(slot)
	match slot_type:
		CatboxSlot.SlotType.InputSlot:
			set_slot_enabled_left(i, true)
			set_slot_enabled_right(i, false)
		CatboxSlot.SlotType.OutputSlot:
			set_slot_enabled_left(i, false)
			set_slot_enabled_right(i, true)
