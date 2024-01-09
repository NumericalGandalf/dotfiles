return {
	"pocco81/auto-save.nvim",
	event = "BufEnter",
	config = function()
		local autosave = require("auto-save")
		autosave.setup({
			execution_message = {
				message = function()
					return "auto-save: saved buffer " .. vim.fn.bufnr("%") .. " at " .. vim.fn.strftime("%H:%M:%S")
				end,
				dim = 0.2,
				cleaning_interval = 500,
			},
			trigger_events = { "InsertLeave", "TextChanged" },
			condition = function(buf)
				local utils = require("auto-save.utils.data")
				if
					vim.fn.getbufvar(buf, "&modifiable") == 1 and utils.not_in(vim.fn.getbufvar(buf, "&filetype"), {})
				then
					return true
				end
				return false
			end,
			write_all_buffers = false,
			debounce_delay = 500,
			callbacks = {
				enabling = nil,
				disabling = nil,
				before_asserting_save = nil,
				before_saving = nil,
				after_saving = nil,
			},
		})
		autosave.off()
		vim.keymap.set("n", "<leader>s", autosave.toggle)
	end,
}
