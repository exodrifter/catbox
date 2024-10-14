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
@onready var spacer: Control = $Spacer

var hide_editor: bool = false:
	set(value):
		hide_editor = value
		_refresh_editor()
var value: Variant

func _ready() -> void:
	name = slot_name
	_refresh_editor()

	var is_variable := true
	match slot_type:
		SlotType.InputSlot:
			label.text = slot_name
			if is_instance_valid(catbox_node):
				for input in catbox_node.function.inputs:
					if input.name == slot_name:
						is_variable = false
						break
		SlotType.OutputSlot:
			label.text = "[right]%s[/right]" % slot_name
			if is_instance_valid(catbox_node):
				for output in catbox_node.function.outputs:
					if output.name == slot_name:
						is_variable = false
						break

	# Set variable style
	if is_variable:
		label.add_theme_color_override("default_color", Color.GRAY)

func _refresh_editor() -> void:
	if not is_node_ready():
		return

	var show_editor := not hide_editor and slot_type == SlotType.InputSlot
	var show_spacer := hide_editor and slot_type == SlotType.InputSlot
	match value_type:
		"text":
			spacer.visible = show_spacer
			line_edit.visible = show_editor
			if value is String:
				line_edit.text = value

			no_editor_label.visible = false

		"path":
			spacer.visible = show_spacer
			line_edit.visible = show_editor
			if value is String:
				line_edit.text = value

			no_editor_label.visible = false

		_:
			line_edit.visible = false

			spacer.visible = show_spacer
			no_editor_label.visible = show_editor
			no_editor_label.text = "no editor for type \"" + value_type + "\""
