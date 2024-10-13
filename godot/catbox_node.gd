## Responsible for representing a node in a function graph
class_name CatboxGraphNode
extends GraphNode

const SLOT = preload("res://catbox_slot.tscn")

var function: CatboxFunction

var slots: Array[CatboxSlot]

@onready var slot_editor: HBoxContainer = $CatboxSlotEditor

func _enter_tree() -> void:
	if is_instance_valid(function):
		setup()

func _process(_delta: float) -> void:
	for i in slots.size():
		var slot = slots[i]
		match slot.slot_type:
			CatboxSlot.SlotType.InputSlot:
				set_slot_enabled_left(i, true)
				set_slot_enabled_right(i, false)
			CatboxSlot.SlotType.OutputSlot:
				set_slot_enabled_left(i, false)
				set_slot_enabled_right(i, true)

	move_child(slot_editor, -1)
	slot_editor.visible = function.variable_inputs || function.variable_outputs
	slot_editor.variable_inputs = function.variable_inputs
	slot_editor.variable_outputs = function.variable_outputs

	reset_size()

func setup():
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
	slots.push_back(slot)
	add_child(slot)
