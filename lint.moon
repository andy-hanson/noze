#! /usr/local/bin/moon

require 'lfs'
import attributes, currentdir, dir from lfs

TAB_SIZE = 4

str_at = (str, index) ->
	str\sub(index, index)

n_tabs = (line) ->
	i = 1
	while str_at(line, i) == "\t"
		i += 1
	return i - 1

line_len = (line) ->
	n_tabs(line) * (TAB_SIZE - 1) + line\len()

MAX_LINE_LENGTH = 140

lint_line_length = (path) ->
	file = io.open path
	line_num = 1
	n_errors = 0
	while true
		line = file\read()
		if line == nil
			break
		else
			len = line_len(line)
			if len > MAX_LINE_LENGTH
				n_errors += 1
				print "#{path}: line #{line_num} is #{len} columns long, should be <= #{MAX_LINE_LENGTH}"
			line_num += 1
	file\close
	n_errors

lint_file = (path, ext) ->
	lint_line_length(path)

recur_dir = (path, filter_dir_name, cb) ->
	for child_name in dir path
		if child_name\sub(1, 1) ~= "."
			child_path = "#{path}/#{child_name}"
			if attributes(child_path).mode == "directory"
				if filter_dir_name child_name
					recur_dir child_path, filter_dir_name, cb
			else
				cb child_path, child_name
	nil

ends_with = (str, ending) ->
	(str\sub -ending\len(), -1) == ending

values = (t) ->
  i = 0
  return () ->
	i += 1
	t[i]


values = (t) ->
  i = 0
  return () ->
	i += 1
	t[i]

Set = (list) ->
  set = {}
  for v in values list
	set[v] = true
  set

IGNORE_EXTS = Set({ ".c", ".data", ".o", ".tmLanguage" })

get_ext = (name) ->
	name\match("^.+(%..+)$")

main = () ->
	src = currentdir()
	any_error = false

	filter_dir_name = (name) ->
		name ~= ".git" and name ~= "libfirm"

	n_files = 0
	n_errors = 0

	recur_dir src, filter_dir_name, (path, base_name) ->
		ext = get_ext(base_name)
		-- Can't add nil to IGNORE_EXTS, lua will ignore the rest of the list
		if ext != nil and not IGNORE_EXTS[ext]
			n_errors += lint_file(path, ext)
			n_files += 1

	if not any_error
		print "Linted #{n_files} files, #{n_errors} errors"

main()
