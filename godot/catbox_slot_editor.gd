class_name CatboxSlotEditor
extends HBoxContainer

signal add_slot(name: String, value_type: String, slot_type: CatboxSlot.SlotType)

@export var variable_outputs: bool
@export var variable_inputs: bool

@onready var line_edit: LineEdit = $LineEdit
@onready var slot_type_button: OptionButton = $SlotTypeButton
@onready var value_type_button: OptionButton = $ValueTypeButton

func _process(_delta: float) -> void:
	if variable_inputs and variable_outputs:
		slot_type_button.disabled = false
	elif variable_inputs:
		slot_type_button.disabled = true
		slot_type_button.selected = slot_type_button.get_item_index(0)
	elif variable_outputs:
		slot_type_button.disabled = true
		slot_type_button.selected = slot_type_button.get_item_index(1)

func _on_button_pressed() -> void:
	var slot_type := CatboxSlot.SlotType.InputSlot
	match slot_type_button.get_item_text(slot_type_button.selected):
		"input":
			slot_type = CatboxSlot.SlotType.InputSlot
		"output":
			slot_type = CatboxSlot.SlotType.OutputSlot
		_:
			assert(false, "Unknown slot type")

	add_slot.emit(
		line_edit.text,
		value_type_button.get_item_text(value_type_button.selected),
		slot_type,
	)
