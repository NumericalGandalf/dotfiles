local M = {}

function M.setup()
	local settings = require("gandalf.settings")
	settings.setup()
	require("gandalf.lazy").setup()

	if settings.colorscheme == "default" then
		vim.cmd.ColDefault()
	else
		vim.cmd(":colorscheme " .. settings.colorscheme)
	end
end

return M
