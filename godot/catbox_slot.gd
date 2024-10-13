class_name CatboxSlot
extends HBoxContainer

enum SlotType { InputSlot, OutputSlot }

@export var catbox_node: CatboxGraphNode
@export var slot_name: String
@export var slot_type: SlotType
@export var value_type: String

@onready var label: RichTextLabel = $Label
@onready var line_edit: LineEdit = $LineEdit
@onready var no_editor_label: RichTextLabel = $NoEditorLabel

var value: Variant

func _ready() -> void:
	name = slot_name

func _process(_delta: float) -> void:
	var is_variable := true
	match slot_type:
		SlotType.InputSlot:
			label.text = slot_name
			_set_editor(value_type)
			if is_instance_valid(catbox_node):
				for input in catbox_node.function.inputs:
					if input.name == slot_name:
						is_variable = false
						break
		SlotType.OutputSlot:
			label.text = "[right]%s[/right]" % slot_name
			_disable_editors()
			if is_instance_valid(catbox_node):
				for output in catbox_node.function.outputs:
					if output.name == slot_name:
						is_variable = false
						break

	# Set variable style
	if is_variable:
		label.add_theme_color_override("default_color", Color.GRAY)

func _disable_editors() -> void:
	line_edit.visible = false
	no_editor_label.visible = false

func _set_editor(type: String) -> void:
	match type:
		"text":
			if value is String:
				line_edit.text = value
			line_edit.visible = true
			no_editor_label.visible = false

		"path":
			if value is String:
				line_edit.text = value
			line_edit.visible = true
			no_editor_label.visible = false

		_:
			line_edit.visible = false

			no_editor_label.visible = true
			no_editor_label.text = "no editor for type \"" + type + "\""
