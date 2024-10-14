class_name CatboxSlot
extends HBoxContainer

enum SlotType { InputSlot, OutputSlot }

@export var parent_node: CatboxGraphNode
@export var slot_name: String
@export var slot_type: SlotType
@export var value_type: String

@onready var label: RichTextLabel = $Label
@onready var line_edit: LineEdit = $LineEdit
@onready var no_editor_label: RichTextLabel = $NoEditorLabel
@onready var spacer: Control = $Spacer

var value: Variant

func _ready() -> void:
	name = slot_name
	refresh_editor()

	var is_variable := true
	match slot_type:
		SlotType.InputSlot:
			label.text = slot_name
			if is_instance_valid(parent_node):
				for input in parent_node.function.inputs:
					if input.name == slot_name:
						is_variable = false
						break
		SlotType.OutputSlot:
			label.text = "[right]%s[/right]" % slot_name
			if is_instance_valid(parent_node):
				for output in parent_node.function.outputs:
					if output.name == slot_name:
						is_variable = false
						break

	# Set variable style
	if is_variable:
		label.add_theme_color_override("default_color", Color.GRAY)

func refresh_editor() -> void:
	if not is_node_ready():
		return

	var hide_editor := false
	if parent_node.name == "in" or parent_node.name == "out":
		hide_editor = true
	else:
		match slot_type:
			CatboxSlot.SlotType.InputSlot:
				var parameter_name = parent_node.name + "." + slot_name
				var parameter = parent_node.parent_graph.get_parameter(parameter_name)
				hide_editor = parameter != null and parameter.source.type == "connection"
			CatboxSlot.SlotType.OutputSlot:
				hide_editor = true

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

	parent_node.reset_size()
