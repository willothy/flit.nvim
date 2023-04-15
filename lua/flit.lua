local api = vim.api

local state = { prev_input = nil }

-- INFO This method is necessary as opposed to a simple `:find`, to correctly
-- determine a word the
-- cursor is already standing on
---@param line string
---@param pattern string
---@param endOfWord boolean look for the end of the pattern instead of the start
---@param col number -- look for the first match after this number
---@nodiscard
---@return number|nil returns nil if none is found
local function firstMatchAfter(line, pattern, endOfWord, col)
	-- special case: pattern with ^/$, since there can only be one match
	-- and since gmatch won't work with them
	if pattern:find("^%^") or pattern:find("%$$") then
		if pattern:find("%$$") and col >= #line then
			return nil
		end -- checking for high col count for virtualedit
		if pattern:find("^%^") and col ~= 1 then
			return nil
		end
		local start, endPos = line:find(pattern)
		local pos = endOfWord and endPos or start
		if pos and not endOfWord then
			pos = pos - 1
		end
		return pos
	end

	if endOfWord then
		pattern = pattern .. "()" -- INFO "()" makes gmatch return the position of that group
	else
		pattern = "()" .. pattern
	end
	-- `:gmatch` will return all locations in the string where the pattern is
	-- found, the loop looks for the first one that is higher than the col to
	-- look from
	for pos in line:gmatch(pattern) do
		if endOfWord and pos > col then
			return pos - 1
		end
		if not endOfWord and pos >= col then
			return pos - 1
		end
	end
	return nil
end

---finds next word, which is lowercase, uppercase, or standalone punctuation
---@param line string input string where to find the pattern
---@param col number position to start looking from
---@param key "w"|"e"|"b"|"ge" the motion to perform
---@nodiscard
---@return number|nil pattern position, returns nil if no pattern was found
local function getNextPosition(line, col, key)
	-- `%f[set]` is roughly lua's equivalent of `\b`
	local patterns = {
		lowerWord = "%u?[%l%d]+", -- first char may be uppercase for CamelCase
		upperWord = "%f[%w][%u%d]+%f[^%w]", -- solely uppercase for SCREAMING_SNAKE_CASE
		punctuation = "%f[^%s]%p+%f[%s]", -- punctuation surrounded by whitespace
		punctAtStart = "^%p+%f[%s]", -- needed since lua does not allow for logical OR
		punctAtEnd = "%f[^%s]%p+$",
		onlyPunct = "^%p+$",
	}
	-- if not skipInsignificantPunc then
	-- 	patterns.punctuation = "%p+"
	-- end

	-- define motion properties
	local backwards = (key == "b") or (key == "ge")
	local endOfWord = (key == "ge") or (key == "e")
	if backwards then
		patterns.lowerWord = "[%l%d]+%u?" -- the other patterns are already symmetric
		line = line:reverse()
		endOfWord = not endOfWord
		if col == -1 then
			col = 1
		else
			col = #line - col + 1
		end
	end

	-- search for patterns, get closest one
	local matches = {}
	for _, pattern in pairs(patterns) do
		local match = firstMatchAfter(line, pattern, endOfWord, col)
		if match then
			table.insert(matches, match)
		end
	end
	if vim.tbl_isempty(matches) then
		return nil
	end -- none found in this line
	local nextPos = math.min(unpack(matches))

	if not endOfWord then
		nextPos = nextPos + 1
	end
	if backwards then
		nextPos = #line - nextPos + 1
	end
	return nextPos
end

local function next_n_positions(lines, col, n, startline)
	local key = "w"
	local positions = {}

	for linenr, line in ipairs(lines) do
		local c = linenr == 1 and col or 0
		local p = getNextPosition(line, c, key)
		while p ~= nil and #positions < n do
			table.insert(positions, { line = startline + linenr, col = p, char = string.char(string.byte(line, p)) })
			c = p + 1
			p = getNextPosition(line, c, key)
		end
		if #positions >= n then
			break
		end
	end
	return positions
end

local function make_positions_unique(positions)
	local unique = {}
	local res = {}
	for i, pos in ipairs(positions) do
		if unique[pos.char] == nil then
			unique[pos.char] = 0
		end
		if unique[pos.char] < 3 then
			unique[pos.char] = unique[pos.char] + 1
			table.insert(res, { line = pos.line, col = pos.col, char = pos.char, nr = unique[pos.char] })
		end
	end
	return res
end

local function get_positions()
	local cursor = vim.api.nvim_win_get_cursor(0)
	local col = cursor[2]
	local lines = vim.api.nvim_buf_get_lines(0, cursor[1] - 1, -1, false)
	local positions = next_n_positions(lines, col, 15, cursor[1] - 1)
	positions = make_positions_unique(positions)
	return positions
end

-- local ns = vim.api.nvim_create_namespace("leap_willothy")
local ns = require("leap.highlight").ns

local function hl_positions(positions, n)
	for i, pos in ipairs(positions) do
		if n and i > n then
			break
		end
		local group = "LeapMatch"
		if pos.nr == 1 then
			group = "LeapMatch"
		elseif pos.nr == 2 then
			group = "LeapLabelSecondary"
		end
		-- vim.api.nvim_buf_add_highlight(0, ns, "LeapMatch", pos.line - 1, pos.col, pos.col + 1)
		vim.highlight.range(
			0,
			ns,
			group,
			{ pos.line - 1, pos.col - 1 },
			{ pos.line - 1, pos.col },
			{ priority = 65535 }
		)
	end
end

local function clear_hl()
	vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)
end

local function find_hl()
	local positions = get_positions()
	hl_positions(positions)
end

local function flit(kwargs)
	-- Reinvent The Wheel #1
	-- Custom targets callback, ~90% of it replicating what Leap does by default.

	local function get_input()
		vim.cmd('echo ""')
		local hl = require("leap.highlight")
		clear_hl()
		if vim.v.count == 0 and not (kwargs.unlabeled and vim.fn.mode(1):match("o")) then
			hl["apply-backdrop"](hl, kwargs.cc.backward)
		end
		hl["highlight-cursor"](hl)
		find_hl()
		vim.cmd("redraw")
		local ch = require("leap.util")["get-input-by-keymap"]({ str = ">" })
		hl["cleanup"](hl, { vim.fn.win_getid() })
		if not ch then
			return
		end
		-- Repeat with the previous input?
		local repeat_key = require("leap.opts").special_keys.repeat_search
		if ch == api.nvim_replace_termcodes(repeat_key, true, true, true) then
			if state.prev_input then
				ch = state.prev_input
			else
				vim.cmd('echo "no previous search"')
				return
			end
		else
			state.prev_input = ch
		end
		return ch
	end

	local function get_pattern(input)
		-- See `expand-to-equivalence-class` in `leap`.
		-- Gotcha! 'leap'.opts redirects to 'leap.opts'.default - we want .current_call!
		local chars = require("leap.opts").eq_class_of[input]
		if chars then
			chars = vim.tbl_map(function(ch)
				if ch == "\n" then
					return "\\n"
				elseif ch == "\\" then
					return "\\\\"
				else
					return ch
				end
			end, chars or {})
			input = "\\(" .. table.concat(chars, "\\|") .. "\\)" -- "\(a\|b\|c\)"
		end
		return "\\V" .. (kwargs.multiline == false and "\\%.l" or "") .. input
	end

	local function get_targets(pattern)
		local search = require("leap.search")
		local bounds = search["get-horizontal-bounds"]()
		local get_char_at = require("leap.util")["get-char-at"]
		local match_positions = search["get-match-positions"](pattern, bounds, { ["backward?"] = kwargs.cc.backward })
		local targets = {}
		for _, pos in ipairs(match_positions) do
			table.insert(targets, { pos = pos, chars = { get_char_at(pos, {}) } })
		end
		return targets
	end

	-- The actual arguments for `leap` (would-be `opts.current_call`).
	local cc = kwargs.cc or {}

	cc.targets = function()
		local state = require("leap").state
		local pattern
		if state.args.dot_repeat then
			pattern = state.dot_repeat_pattern
		else
			local input = get_input()
			if not input then
				return
			end
			pattern = get_pattern(input)
			-- Do not save into `state.dot_repeat`, because that will be
			-- replaced by `leap` completely when setting dot-repeat.
			state.dot_repeat_pattern = pattern
		end
		return get_targets(pattern)
	end

	cc.opts = kwargs.opts or {}
	local key = kwargs.keys
	-- In any case, keep only safe labels.
	cc.opts.labels = {}
	if kwargs.unlabeled then
		cc.opts.safe_labels = {}
	else
		-- Remove labels conflicting with the next/prev keys.
		-- The first label will be the repeat key itself.
		-- Note: this doesn't work well for non-alphabetic characters.
		local filtered = { cc.t and key.t or key.f }
		local to_ignore = cc.t and { key.t, key.T } or { key.f, key.F }
		for _, label in ipairs(require("leap").opts.safe_labels) do
			if not vim.tbl_contains(to_ignore, label) then
				table.insert(filtered, label)
			end
		end
		cc.opts.safe_labels = filtered
	end
	-- Set the next/prev ("clever-f") keys.
	cc.opts.special_keys = vim.deepcopy(require("leap").opts.special_keys)
	if type(cc.opts.special_keys.next_target) == "string" then
		cc.opts.special_keys.next_target = { cc.opts.special_keys.next_target }
	end
	if type(cc.opts.special_keys.prev_target) == "string" then
		cc.opts.special_keys.prev_target = { cc.opts.special_keys.prev_target }
	end
	table.insert(cc.opts.special_keys.next_target, cc.t and key.t or key.f)
	table.insert(cc.opts.special_keys.prev_target, cc.t and key.T or key.F)

	require("leap").leap(cc)
end

local function setup(kwargs)
	local kwargs = kwargs or {}
	kwargs.cc = {} --> would-be `opts.current_call`
	kwargs.cc.ft = true
	kwargs.cc.inclusive_op = true

	-- Set keymappings.
	kwargs.keys = kwargs.keys or kwargs.keymaps or { f = "f", F = "F", t = "t", T = "T" }
	local key = kwargs.keys
	local motion_specific_args = {
		[key.f] = {},
		[key.F] = { backward = true },
		[key.t] = { offset = -1, t = true },
		[key.T] = { backward = true, offset = 1, t = true },
	}
	local labeled_modes = kwargs.labeled_modes and kwargs.labeled_modes:gsub("v", "x") or "x"
	for _, key in pairs(kwargs.keys) do
		for _, mode in ipairs({ "n", "x", "o" }) do
			-- Make sure to create a new table for each mode (and not pass the
			-- outer one by reference here inside the loop).
			local kwargs = vim.deepcopy(kwargs)
			kwargs.cc = vim.tbl_extend("force", kwargs.cc, motion_specific_args[key])
			kwargs.unlabeled = not labeled_modes:match(mode)
			vim.keymap.set(mode, key, function()
				flit(kwargs)
			end)
		end
	end

	-- Reinvent The Wheel #2
	-- Ridiculous hack to prevent having to expose a `multiline` flag in
	-- the core: switch Leap's backdrop function to our special one here :)
	if kwargs.multiline == false then
		local state = require("leap").state
		local function backdrop_current_line()
			local hl = require("leap.highlight")
			if pcall(api.nvim_get_hl_by_name, hl.group.backdrop, false) then
				local curline = vim.fn.line(".") - 1 -- API indexing
				local curcol = vim.fn.col(".")
				local startcol = state.args.backward and 0 or (curcol + 1)
				local endcol = state.args.backward and (curcol - 1) or -1
				vim.highlight.range(
					0,
					hl.ns,
					hl.group.backdrop,
					{ curline, startcol },
					{ curline, endcol },
					{ priority = hl.priority.backdrop }
				)
			end
		end
		api.nvim_create_augroup("Flit", {})
		api.nvim_create_autocmd("User", {
			pattern = "LeapEnter",
			group = "Flit",
			callback = function()
				if state.args.ft then
					state.saved_backdrop_fn = require("leap.highlight")["apply-backdrop"]
					require("leap.highlight")["apply-backdrop"] = backdrop_current_line
				end
			end,
		})
		api.nvim_create_autocmd("User", {
			pattern = "LeapLeave",
			group = "Flit",
			callback = function()
				if state.args.ft then
					require("leap.highlight")["apply-backdrop"] = state.saved_backdrop_fn
					state.saved_backdrop_fn = nil
				end
			end,
		})
	end
end

return { setup = setup }
