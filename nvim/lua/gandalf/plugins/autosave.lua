local M = { "0x00-ketsu/autosave.nvim" }

M.cond = false

M.event = "BufEnter"

function M.config()
	local format = require("gandalf.plugins.format")
	local autosave = require("autosave")
	local actions = require("autosave.action")

	autosave.setup({
		enable = false,
		prompt_style = "notify",
		debounce_delay = format.timeout,
	})

	function autosave.hook_after_enable()
		vim.notify("Autsave: enabled")
	end
	function autosave.hook_after_disable()
		vim.notify("Autsave: disabled")
	end

	vim.keymap.set("n", "<leader>s", actions.toggle)
end

return M
