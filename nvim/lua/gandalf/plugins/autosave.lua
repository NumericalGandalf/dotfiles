local M = { "pocco81/auto-save.nvim" }

M.event = "BufEnter"

M.enabled = require("gandalf.prefs").enablings.autosave

function M.config()
	local autosave = require("auto-save")
	local utils = require("auto-save.utils.data")
	local format = require("gandalf.plugins.format")

	autosave.setup({
		execution_message = {
			message = function()
				vim.notify("auto-save: buf" .. vim.fn.bufnr("%") .. "-" .. vim.fn.strftime("%H:%M:%S"))
				return ""
			end,
			dim = 0,
			cleaning_interval = 0,
		},
		trigger_events = { "InsertLeave", "TextChanged" },
		condition = function(buf)
			if vim.fn.getbufvar(buf, "&modifiable") == 1 and utils.not_in(vim.fn.getbufvar(buf, "&filetype"), {}) then
				return true
			end
			return false
		end,
		write_all_buffers = false,
		debounce_delay = format.timeout,
		callbacks = {
			enabling = nil,
			disabling = nil,
			before_asserting_save = nil,
			before_saving = function()
				format.by_autosave = true
				format.invoke()
			end,
			after_saving = function()
				format.by_autosave = false
			end,
		},
	})

	autosave.off()
	vim.keymap.set("n", "<leader>s", autosave.toggle)
end

return M
